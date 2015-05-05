#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "operf-macro" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "lib/macroperf";
    Pkg.bin ~dst:"operf-macro" ~auto:true "src/macrorun";
    Pkg.bin ~auto:true "src/injector";
    Pkg.man ~dst:"man1/operf-macro.1" "operf-macro.1"
  ]
