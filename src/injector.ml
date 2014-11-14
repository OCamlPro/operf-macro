module Core_bench_data = struct
  type t = {
    runs: int;
    cycles: int64;
    nanos: int64;
    compactions: int;
    minor_allocated: int;
    major_allocated: int;
    promoted: int;
    major_collections: int;
    minor_collections: int;
  }
  let of_string s =
    Scanf.sscanf s "%d %Ld %Ld %d %d %d %d %d %d"
      (fun runs cycles nanos compactions minor_allocated
        major_allocated promoted major_collections minor_collections ->
        {
          runs; cycles; nanos; compactions; minor_allocated; major_allocated;
          promoted; major_collections; minor_collections;
        }
      )

  let to_string { runs; cycles; nanos; compactions; minor_allocated;
                  major_allocated; promoted; major_collections;
                  minor_collections } =
    Printf.sprintf "%d %Ld %Ld %d %d %d %d %d %d"
      runs cycles nanos compactions minor_allocated major_allocated
      promoted major_collections minor_collections

  let nanos t = t.nanos
  let cycles t = t.cycles

  let affine_adjustment ?(what=cycles) ts =
    let ts = List.map (fun t -> t.runs, what t |> Int64.to_float) ts in

    (* Computing min and max amortized measure *)
    let amortized = List.map (fun (nb_iter, v) -> v /. float nb_iter) ts in
    let min_a = List.fold_left (fun a v -> min a v) max_float amortized in
    let max_a = List.fold_left
        (fun a v -> if v <> infinity then max a v else a) min_float amortized in

    let affine_adjustment r =
      let len = float (List.length r) in
      let mean_x =
        let sum_x = List.fold_left (fun acc (x,_) -> x + acc) 0 r in
        (float sum_x) /. len in
      let mean_y =
        let sum_y = List.fold_left (fun acc (_,y) -> y +. acc) 0. r in
        sum_y /. len in
      let variance_x =
        let sumvar =
          List.fold_left
            (fun acc (x,_) ->
               let v = (float x) -. mean_x in v *. v +. acc)
            0. r
        in
        sumvar /. len
      in
      let covariance_x_y =
        let sumcovar =
          List.fold_left
            (fun acc (x,y) ->
               let v = ((float x) -. mean_x) *. (y -. mean_y) in
               v +. acc)
            0. r
        in
        sumcovar /. len
      in
      let a = covariance_x_y /. variance_x in
      let b = mean_y -. a *. mean_x in
      a, b, min_a, max_a
    in affine_adjustment ts
end

let import_core_bench_data ic =
  let rec inner acc =
    match input_line ic with
    | s when s.[0] = '#' -> inner acc
    | s -> inner (Core_bench_data.of_string s :: acc)
    | exception End_of_file -> List.rev acc
  in inner []

let import dn =
  let open Macroperf in
  Util.FS.fold_files (fun acc fn ->
      let name =
        let fn = Filename.basename fn in
        String.(sub fn 0 @@ index fn '-') in
      (name, Util.File.with_ic_safe import_core_bench_data fn) :: acc
    ) [] dn

let export
    ?(context_id = Macroperf.Util.Opam.switch)
    ?(weight = 1.) vs =
  let open Macroperf in
  let mkdir_openfile f fn =
    let (/) = Filename.concat in
    XDGBaseDir.(mkdir_openfile f @@
                Cache.user_dir ~exists:true () / "operf" / "micro" / fn)
  in
  let summaries = List.map
      (fun (name, data) ->
         let cy_a, cy_b, cy_min, cy_max = Core_bench_data.(affine_adjustment ~what:cycles data) in
         let na_a, na_b, na_min, na_max = Core_bench_data.(affine_adjustment ~what:nanos data) in
         let cy_aggr = Summary.Aggr.create cy_a 0. cy_min cy_max in
         let na_aggr = Summary.Aggr.create na_a 0. na_min na_max in
         Summary.{
           name; context_id; weight;
           data = TMap.add
               Topic.(Topic ("cycles", Perf)) cy_aggr
               (TMap.singleton Topic.(Topic (Time.Real, Time)) na_aggr)
         }
      ) vs in
  List.iter
    (fun s -> mkdir_openfile
        Summary.(fun fn -> save_hum fn s)
        Util.FS.(s.Summary.name / context_id ^ ".summary"))
    summaries

let () =
  if Array.length Sys.argv < 2 then
    (Printf.eprintf "Usage: %s <dir>\n" Sys.argv.(0); exit 1);

  let vs = import Sys.argv.(1) in
  export vs
