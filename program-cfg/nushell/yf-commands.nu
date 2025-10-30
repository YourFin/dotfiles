def "nu-complete external-command" [context?: string] {
  let substr = $context | split words | last;
  bash -c $'IFS="\\t" compgen -c(if $substr == null { "" } else { $" ($substr)" })'
    | from tsv --noheaders
    | get column0
}

# which + readlink -f
export def whichr [cmd: string@"nu-complete external-command"] {
  let res = which $cmd;
  if $res == [] {
    error make { msg: $'($cmd): Command not found' }
  }
  let tbl = $res.0;
  if $tbl.type != external {
    error make { msg: $'Not an external command; ($tbl.type)' }
  } else {
    readlink -f $'($tbl.path)'
  }
}

def assert-pueue-group [name: string, --parallel: int] {
  let groups = pueue group --json | from json | transpose k v | get k;
  if ($groups | find $name | length) == 0 {
    pueue group add $name --parallel $parallel
  }
}

def emacs-socket-file [] {
  let socket_dir = match [($env | get --optional XDG_RUNTIME_DIR), $nu.os-info.name] {
    [null, "macos"] => (getconf DARWIN_USER_TEMP_DIR),
    [null, "linux"] => ($env.HOME | path join '.cache' run),
    [null, _] => (error make { msg: "Unsupported OS" }),
    [$it, _] => $it,
  }
  $socket_dir | path join emacs | path join emacsserver.sock
}

export def restart-emacs [] {
  while ((pkill emacs | complete).exit_code == 0) {}
  assert-pueue-group emacs-daemon --parallel 1
  let sock_file = emacs-socket-file
  mkdir (dirname $sock_file)
  chmod 700 (dirname $sock_file)
  rm -f $sock_file
  pueue add --group emacs-daemon -w ~ -- emacs --fg-daemon=($sock_file)
}

export def emacsclient-frame [file?: path] {
  let f = $file | default $env.HOME;
  assert-pueue-group emacs-frame --parallel 0
  pueue add --group emacs-frame -w . -- emacsclient --socket-name=(emacs-socket-file) -c $f
}

def "nu-git changes" [] {
 let ref = $in | default -e "HEAD";
 git show --numstat --format="" $ref | lines | each {
    (parse --regex "(?<additions>\\d+)\\s+(?<removals>\\d+)\\s+(?<filename>.+)"
        | get 0
        | update removals { into int }
        | update additions { into int })
   }
}

def "nu-git ref" [] {
  let ref = $in | default -e "HEAD";
  let items = {
    hash: "%H",
    time: "%at",
    refs: "%D",
    committerName: "%cN",
    committerEmail: "%cE",
    subject: "%s",
    notes: "%N",
  } | transpose name git;
  git log -1 $"--format=($items | get git | str join "%x1E")" $ref
    | parse ($items | each { $'{($in.name)}' } | str join "\u{1E}")
    | update time { into int | $in * 1_000_000_000 | into datetime }
    | insert changes ($ref | nu-git changes)
    | get 0
}

export def "nu-git log" [range?: string] {
  let refs = $in | default -e $range | default -e "HEAD~7..HEAD";
  git log --format=%H $refs | lines | par-each -k { nu-git ref }
}

export def "into epochsecond" [time?: datetime] {
  let inp = $in | default -e $time
  $inp | into int | $in / 1_000_000_000 | into int
}

export def "from epochsecond" [epochseconds?: int] {
  let inp = $in | default -e $epochseconds
  $inp | $in * 1_000_000_000 | into datetime
}

export def "into epochmilli" [time?: datetime] {
  let inp = $in | default -e $time
  $inp | into int | $in / 1_000_000 | into int
}

export def "from epochmilli" [epochmilliseconds?: int] {
  let inp = $in | default -e $epochmilliseconds
  $inp | $in * 1_000_000 | into datetime
}

export def with_sts_creds (closure) {
  let parsed = $in | from json;
  $env.AWS_ACCESS_KEY_ID = $parsed.Credentials.AccessKeyId;
  $env.AWS_SECRET_ACCESS_KEY = $parsed.Credentials.SecretAccessKey;
  $env.AWS_SESSION_TOKEN = $parsed.Credentials.SessionToken;
  do $closure
}
