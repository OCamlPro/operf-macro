open Ocamlbuild_pack.Rule
open Ocamlbuild_pack.Command

let () =
  rule
    "manpage"
    ~insert:`top
    ~prod:"operf-macro.1"
    ~dep:"src/macrorun.byte"
    (fun _ _ -> Cmd (Sh "src/macrorun.byte --help=groff > operf-macro.1"))
