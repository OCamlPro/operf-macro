#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "operf-macro" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "lib/macroperf";
    Pkg.lib ~exts:Exts.module_library "lwt/macroperf_lwt";
    Pkg.bin ~auto:true "src/macrorun";
  ]
