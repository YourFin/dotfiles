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
