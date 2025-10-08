{
  yf,
  tree-sitter,
  tree-sitter-grammars,
  writeText,
  fetchFromGitHub,
  lib,
}:
let
  grammars =
    with (lib.concatMapAttrs (
      k: v:
      let
        match = builtins.match "tree-sitter-(.+)" k;
      in
      if match == null then { } else { ${builtins.elemAt match 0} = v; }
    ) tree-sitter-grammars); {
      inherit
        bash
        c-sharp
        commonlisp
        clojure
        elisp
        elm
        go
        haskell
        java
        julia
        kotlin
        lua
        php
        python
        ruby
        rust
        scala
        scheme
        sql

        css
        graphql
        html
        javascript
        jsdoc
        svelte
        tsx
        typescript
        vue

        c
        llvm
        wgsl
        zig

        devicetree
        dockerfile
        gomod
        gowork
        hcl
        just
        make
        nix

        bibtex
        markdown
        rst

        hjson
        json
        json5
        kdl
        latex
        nickel
        toml
        yaml
        ;
      # ref: https://github.com/NixOS/nixpkgs/blob/nixos-unstable/pkgs/development/tools/parsing/tree-sitter/grammar.nix
      doxygen = tree-sitter.buildGrammar {
        language = "doxygen";
        version = "1.1.0";
        src = fetchFromGitHub {
          owner = "tree-sitter-grammars";
          repo = "tree-sitter-doxygen";
          rev = "ccd998f378c3f9345ea4eeb223f56d7b84d16687";
          hash = "sha256-Yh6FaRvWmeqnSnBgOojWbs1wJaeEoNJlvSEqgzjGh7o=";
        };
      };
    };
in
{
  tree-sitter-bundle =
    yf.nushell-builder
      {
        pname = "yf-tree-sitter-bundle";
        buildInputs = builtins.attrValues grammars;
        nuVars.grammars = builtins.mapAttrs (k: v: v.outPath) grammars;
        nuVars.isMac = (lib.systems.elaborate builtins.currentSystem).isDarwin;
      }
      ''
        let ext = if $isMac { "dylib" } else { "so" };
        mkdir ($env.out)/lib ($env.out)/etc/tree-sitter/queries;
        $grammars
          | transpose name path
          | each { |grammar|
              # per: https://www.gnu.org/software/emacs/manual/html_node/elisp/Language-Grammar.html#index-treesit_002dlanguage_002davailable_002dp
              ln -s $"($grammar.path)/parser" $"($env.out)/lib/libtree-sitter-($grammar.name).($ext)"
              let grammarQueries = $"($grammar.path)/queries";
              if ($grammarQueries | path exists) {
                ln -s ($grammarQueries) $"($env.out)/etc/tree-sitter/queries/($grammar.name)"
              }
            }
      '';
}
