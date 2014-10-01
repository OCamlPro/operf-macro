(* run with "ocaml generate_table.ml > t_table.ml" to update the tables *)

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;

#use "topfind";;
#require "gsl";;

let rec interval i j =
  if i > j then []
  else i :: (interval (i+1) j)

let probas =
  [| 0.75; 0.80; 0.85; 0.90; 0.95; 0.975; 0.99; 0.995; 0.999 |]

let values = Array.of_list (interval 1 100)

let student_table =
  Array.map (fun p ->
      Array.map (fun i -> Gsl.Cdf.tdist_Pinv ~p ~nu:(float i))
        values)
    probas

let output_array f oc vs =
  output_string oc "[|";
  Array.iteri (fun i v ->
      (f oc v:unit);
      if i <> Array.length vs - 1
      then output_string oc "; ") vs;
  output_string oc "|]\n"

let output_float oc f = output_string oc (string_of_float f)

let output_t oc t =
  output_array (output_array output_float) oc t

let output_def oc t =
  output_string oc "(* table.(i).(j): t distribution quantile for probability p.(i) and degree of liberty (j+1) *)\n";
  output_string oc "let p =\n";
  output_array output_float oc probas;
  output_string oc "let table =\n";
  output_t oc t

let _ = output_def stdout student_table
