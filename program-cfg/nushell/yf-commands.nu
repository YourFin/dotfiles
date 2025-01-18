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
  let socket_dir = match [($env | get --ignore-errors XDG_RUNTIME_DIR), $nu.os-info.name] {
    [null, "macos"] => (getconf DARWIN_USER_TEMP_DIR),
    [null, "linux"] => ($env.HOME | path join '.cache' run),
    [null, _] => (error make { msg: "Unsupported OS" }),
    [$it, _] => $it,
  }
  $socket_dir | path join emacsserver.sock
}

export def restart-emacs [] {
  while ((pkill emacs | complete).exit_code == 0) {}
  assert-pueue-group emacs-daemon --parallel 1
  let sock_file = emacs-socket-file
  mkdir (dirname $sock_file)
  rm -f $sock_file
  pueue add --group emacs-daemon -w ~ -- emacs --fg-daemon=(emacs-socket-file)
}

export def emacsclient-frame [file?: path] {
  let f = $file | default $env.HOME;
  assert-pueue-group emacs-frame --parallel 0
  pueue add --group emacs-frame -w . -- emacsclient --socket-name=(emacs-socket-file) -c $f
}
