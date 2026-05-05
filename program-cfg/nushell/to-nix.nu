use std repeat

export def "to nix" [value?: any]: any -> string {
  let val = $in | default -e $value
  to-nix ($val | to json | from json) 0
}

def to-nix [val: any, depth: int] {
  let sep = "\n" + (" " | repeat $depth | str join)
  let innerSep = "\n" + (" " | repeat ($depth + 2) | str join)
  match ($val | describe | str replace --regex "<.*" "") {
        "record" => {
            let fields = $val | items { |k, v| if ($v | describe) != "nothing" { $"(to-nix-key $k) = (to-nix $v ($depth + 2));" } else { "" } }
            "{" + $innerSep + ($fields | str join $innerSep) + $sep + "}"
        }
        "list" => {
            "[" + $innerSep + ($val | each {|item| to-nix $item ($depth + 2) } | str join $innerSep) + $sep + "]"
        }
        "string" => { to-nix-string-literal $val }
        "int" | "float" | "number" => {
            $val | into string
        }
        "null" | "nothing" => {
            "null"
        }
        "bool" => {
            if $val { "true" } else { "false" }
        }
        $type => {
            error make {
              msg: $"Unexpected ($type)"
              labels: [ {text: "here" span: (metadata $val).span} ]
              help: "This is a bug in to-nix"
            }
        }

  }
}

def to-nix-key [val: string] {
  if $val =~ '^[_a-zA-Z][a-zA-Z0-9-_]*$' {
    $val
  } else {
    to-nix-string-literal $val
  }
}

def to-nix-string-literal [val: string] {
  $val | str replace -a '\' '\\' | str replace -a '$' '\$' | str replace -a '"' '\"' | $'"($in)"'
}
