open Sexplib.Std

module Sexpable = struct
  module type S = sig
    type t with sexp
  end
  module type S1 = sig
    type 'a t with sexp
  end
  module type S2 = sig
    type ('a, 'b) t with sexp
  end
  module type S3 = sig
    type ('a, 'b, 'c) t with sexp
  end
end

module SSet = Set.Make(String)

module List = struct
  include List
  let filter_map f l =
    List.fold_left (fun a e -> match f e with Some v -> v::a | None -> a) [] l |> List.rev
end

module StringList = struct
  let settrip l = SSet.(of_list l |> elements)
end

module String = struct
  include String
  let prefix s s' =
    length s <= length s' && s = sub s' 0 @@ length s
end

module Util = struct
  module FS = struct
    let (/) = Filename.concat
    let home = Unix.getenv "HOME"
    let cache_dir = XDGBaseDir.Cache.user_dir ~exists:true () / "operf"
    let micro_dir = cache_dir / "micro"
    let macro_dir = cache_dir / "macro"

    let ls ?(preserve_order=false) ?(prefix=false) ?glob dirname =
      let dh = Unix.opendir dirname in
      let rec loop acc =
        match Unix.readdir dh with
        | n when n <> "." && n <> ".." ->
            let n' = if prefix then dirname / n else n
            in
            (match glob with
             | None -> loop (n'::acc)
             | Some pat ->
                 let re = Re_glob.globx ~anchored:() pat |> Re.compile in
                 if Re.execp re n then loop (n'::acc) else loop acc)
        | _ -> loop acc
        | exception End_of_file ->
            if preserve_order then List.rev acc else acc
      in loop []

    let rec iter f n =
      let open Unix in
      match (stat n).st_kind with
      | S_DIR -> List.iter f @@ ls ~prefix:true n; f n
      | _ -> f n

    let rec fold f acc n =
      let open Unix in
      match (stat n).st_kind with
      | S_DIR -> f (List.fold_left (fold f) acc @@ ls ~prefix:true n) n
      | _ -> f acc n

    let rec fold_files f acc n =
      let open Unix in
      match (stat n).st_kind with
      | S_DIR -> List.fold_left (fold_files f) acc @@ ls ~prefix:true n
      | _ -> f acc n

    let rm_r fns =
      List.iter
        (iter
           Unix.(fun n -> match (stat n).st_kind with
               | S_DIR -> rmdir n
               | _ -> unlink n)
        ) fns

    let kind_exn fn = Unix.((stat fn).st_kind)
    let is_file_exn fn = Unix.((stat fn).st_kind = S_REG)
    let is_dir_exn fn = Unix.((stat fn).st_kind = S_DIR)

    let kind fn = try Some (kind_exn fn) with _ -> None
    let is_file fn = try Some (is_file_exn fn) with _ -> None
    let is_dir fn = try Some (is_dir_exn fn) with _ -> None
  end

  module File = struct
    let string_of_ic ic =
      let buf = Buffer.create 4096 in
      let buf_str = Bytes.create 4096 in
      let rec drain () =
        let nb_read = input ic buf_str 0 4096 in
        if nb_read > 0 then
          (Buffer.add_subbytes buf buf_str 0 nb_read;
           drain ())
        else
          Buffer.contents buf
      in drain ()

    let lines_of_ic ic =
      let rec get_line acc = match input_line ic with
        | line -> get_line (line::acc)
        | exception End_of_file -> List.rev acc
      in get_line []

    let string_of_file filename =
      let ic = open_in filename in
      try
        let res = really_input_string ic @@ in_channel_length ic
        in close_in ic; res
      with exn ->
        close_in ic; raise exn

    let sexp_of_file_exn filename conv =
      let module SSA = Sexplib.Sexp.Annotated in
      match Sexplib.Sexp.load_sexp_conv filename conv
      with
      | `Error (exn, SSA.Atom ({SSA.start_pos; SSA.end_pos}, _))
      | `Error (exn, SSA.List ({SSA.start_pos; SSA.end_pos; _}, _, _)) ->
          let open SSA in
          Printf.eprintf "%s: Error at line %d, col %d-%d.\n"
            filename start_pos.line start_pos.col end_pos.col;
          raise exn
      | `Result b -> b

    let lines_of_file filename =
      let ic = open_in filename in
      try
        let res = lines_of_ic ic in close_in ic; res
      with exn ->
        close_in ic; raise exn

    let write_string_to_file ~fn str =
      let oc = open_out fn in
      try
        Printf.fprintf oc "%s" str;
        close_out oc
      with exn ->
        close_out oc; raise exn

    let with_oc_safe f fn =
      let oc = open_out fn in
      try let res = f oc in close_out oc; res
      with exn -> close_out oc; raise exn

    let with_ic_safe f fn =
      let ic = open_in fn in
      try let res = f ic in close_in ic; res
      with exn -> close_in ic; raise exn
  end

  module Cmd = struct
    let with_process_in_safe f cmd_string =
      let p_stdout = Unix.open_process_in cmd_string in
      try
        let res = f p_stdout in Unix.close_process_in p_stdout, res
      with exn ->
        let _ = Unix.close_process_in p_stdout in raise exn

    let with_process_full_safe f cmd_string =
      let p_stdout, p_stdin = Unix.open_process cmd_string in
      try
        let res = f p_stdout p_stdin in Unix.close_process (p_stdout, p_stdin), res
      with exn ->
        let _ = Unix.close_process (p_stdout, p_stdin) in raise exn

    let lines_of_cmd cmd_string = with_process_in_safe File.lines_of_ic cmd_string

    let path_of_exe n =
      List.hd @@ snd @@ lines_of_cmd @@ "command -v " ^ n
  end

  module Opam = struct
    include FS
    let root = try Unix.getenv "OPAMROOT" with Not_found -> home / ".opam"

    let cur_switch =
      let rex = Re_pcre.regexp "switch: \"([^\"]*)\"" in
      let config_lines = File.lines_of_file @@ root / "config" in
      List.fold_left
        (fun a l ->
           try
             let substrings = Re_pcre.exec ~rex l in
             Re_pcre.get_substring substrings 1
           with _ -> a
        )
        "" config_lines

    let switches =
      let aliases = File.lines_of_file @@ root / "aliases" in
      List.map (fun s -> String.sub s 0 (String.index s ' ')) aliases

    let switches_matching glob =
      let re = Re_glob.globx ~anchored:() glob |> Re.compile in
      List.filter (fun s -> Re.execp re s) switches

    let share s = root / s / "share"
  end
end

module Topic = struct
  module Time = struct
    type t = Real | User | Sys with sexp
    let of_string = function
      | "real" -> Real
      | "user" -> User
      | "sys" -> Sys
      | _ -> invalid_arg "time_of_string"

    let to_string = function
      | Real -> "real"
      | User -> "user"
      | Sys -> "sys"

    let compare = compare
  end

  module TimeSet = Set.Make(Time)

  module Gc = struct
    type t =
      | Minor_words
      | Promoted_words
      | Major_words
      | Minor_collections
      | Major_collections
      | Heap_words
      | Heap_chunks
      | Top_heap_words
      | Live_words
      | Live_blocks
      | Free_words
      | Free_blocks
      | Largest_free
      | Fragments
      | Compactions
    with sexp

    let of_string_exn : string -> t = function
      | "minor_words"       -> Minor_words
      | "promoted_words"    -> Promoted_words
      | "major_words"       -> Major_words
      | "minor_collections" -> Minor_collections
      | "major_collections" -> Major_collections
      | "heap_words"        -> Heap_words
      | "heap_chunks"       -> Heap_chunks
      | "top_heap_words"    -> Top_heap_words
      | "live_words"        -> Live_words
      | "live_blocks"       -> Live_blocks
      | "free_words"        -> Free_words
      | "free_blocks"       -> Free_blocks
      | "largest_free"      -> Largest_free
      | "fragments"         -> Fragments
      | "compactions"       -> Compactions
      | _ -> invalid_arg "gc_of_string_exn"

    let of_string s = try Some (of_string_exn s) with _ -> None

    let to_string = function
      | Minor_words -> "minor_words"
      | Promoted_words -> "promoted_words"
      | Major_words -> "major_words"
      | Minor_collections -> "minor_collections"
      | Major_collections -> "major_collections"
      | Heap_words -> "heap_words"
      | Heap_chunks -> "heap_chunks"
      | Top_heap_words -> "top_heap_words"
      | Live_words -> "live_words"
      | Live_blocks -> "live_blocks"
      | Free_words -> "free_words"
      | Free_blocks -> "free_blocks"
      | Largest_free -> "largest_free"
      | Fragments -> "fragments"
      | Compactions -> "compactions"

    let compare = compare
  end

  module GcSet = Set.Make(Gc)

  type _ kind =
    (* Time related *)
    | Time : Time.t kind

    (* GC related *)
    | Gc : Gc.t kind

    (* Use the perf-stat(1) command or ocaml-libperf *)
    | Perf : string kind

  type t = Topic : 'a * 'a kind -> t

  let of_string s =
    try Topic (Gc.of_string_exn s, Gc)
    with _ ->
      try
        let s = Perf.Attr.Kind.(of_string s |> to_string) in
        Topic (s, Perf)
      with _ ->
        (match s with
         | "time_real" -> Topic (Time.Real, Time)
         | "time_sys" -> Topic (Time.Sys, Time)
         | "time_user" -> Topic (Time.User, Time)
         | _ -> invalid_arg "Topic.of_string"
        )

  let to_string t =
    match t with
    | Topic (t, Time) -> "time_" ^ Time.to_string t
    | Topic (gc, Gc) -> Gc.to_string gc
    | Topic (p, Perf) -> p

  let sexp_of_t t =
    let open Sexplib.Sexp in
    match t with
    | Topic (time, Time) -> List [Atom "Time"; Time.sexp_of_t time]
    | Topic (gc, Gc) -> List [Atom "Gc"; Gc.sexp_of_t gc]
    | Topic (perf, Perf) -> List [Atom "Perf"; sexp_of_string perf]

  let t_of_sexp s =
    let open Sexplib.Sexp in
    match s with
    | List [Atom "Time"; t] -> Topic (Time.t_of_sexp t, Time)
    | List [Atom "Gc"; t] -> Topic (Gc.t_of_sexp t, Gc)
    | List [Atom "Perf"; t] -> Topic (string_of_sexp t, Perf)
    | _ -> invalid_arg "t_of_sexp"

  let compare = Pervasives.compare
end


module TSet = struct
  include Set.Make(Topic)

  let t_of_sexp s =
    of_list @@ list_of_sexp Topic.t_of_sexp s

  let sexp_of_t t =
    elements t |> sexp_of_list Topic.sexp_of_t
end

module SMap = struct
  include Map.Make(String)

  let of_list l =
    List.fold_left (fun a (k,v) -> add k v a) empty l

  let filter_map f t =
    fold (fun k v a -> match f v with Some r -> add k r a | None -> a) t empty

  let filter_mapi f t =
    fold (fun k v a -> match f k v with Some r -> add k r a | None -> a) t empty

  type 'a bindings = (string * 'a) list with sexp

  let t_of_sexp sexp_of_elt s = bindings_of_sexp sexp_of_elt s |> of_list
  let sexp_of_t sexp_of_elt t = sexp_of_bindings sexp_of_elt @@ bindings t
end

module TMap = struct
  include Map.Make(Topic)

  let key_of_sexp = Topic.t_of_sexp
  let sexp_of_key = Topic.sexp_of_t

  let of_list l =
    List.fold_left (fun a (k,v) -> add k v a) empty l

  let filter_map f t =
    fold (fun k v a -> match f v with Some r -> add k r a | None -> a) t empty

  let filter_mapi f t =
    fold (fun k v a -> match f k v with Some r -> add k r a | None -> a) t empty

  type 'a bindings = (key * 'a) list with sexp

  let t_of_sexp sexp_of_elt s = bindings_of_sexp sexp_of_elt s |> of_list
  let sexp_of_t sexp_of_elt t = sexp_of_bindings sexp_of_elt @@ bindings t

  let lmerge ts =
    List.fold_left
      (fun a t -> merge
          (fun k v1 v2 -> match v1, v2 with
             | None, Some v -> Some [v]
             | Some aa, Some v -> Some (v::aa)
             | _ -> invalid_arg "lmerge"
          )
          a t)
      empty ts
end

module Measure = struct
  type t = [ `Int of int64 | `Float of float | `Error ] with sexp
  let of_string s =
    try `Int (Int64.of_string s) with _ ->
      try `Float (float_of_string s) with _ ->
        `Error

  let of_float f = `Float f
  let to_float = function
    | `Float f -> f
    | `Int i -> Int64.to_float i
    | _ -> invalid_arg "Measure.to_float: cannot convert `Error to float"
  let of_int64 i = `Int i
  let to_int64 = function
    | `Int i -> i
    | _ -> invalid_arg "Measure.to_int64"
end

module Execution = struct
  type process_status = Unix.process_status

  let sexp_of_process_status ps =
    let open Unix in
    let open Sexplib.Sexp in
    match ps with
    | WEXITED n -> List [Atom "WEXITED"; sexp_of_int n]
    | WSIGNALED n -> List [Atom "WSIGNALED"; sexp_of_int n]
    | WSTOPPED n -> List [Atom "WSTOPPED"; sexp_of_int n]

  let process_status_of_sexp s =
    let open Unix in
    let open Sexplib.Sexp in
    match s with
    | List [Atom "WEXITED"; n] -> WEXITED (int_of_sexp n)
    | List [Atom "WSIGNALED"; n] -> WSIGNALED (int_of_sexp n)
    | List [Atom "WSTOPPED"; n] -> WSTOPPED (int_of_sexp n)
    | _ -> invalid_arg "process_status_of_sexp"

  type exec = {
    process_status: process_status;
    stdout: string;
    stderr: string;
    data: Measure.t TMap.t;
    checked: bool option
  } with sexp

  type t = [ `Ok of exec | `Timeout | `Error of string ]
  with sexp
  (** Type representing the execution of a benchmark. *)

  let error exn = `Error Printexc.(to_string exn)

  let strip chan t = match t, chan with
    | `Timeout, _
    | `Error _, _ -> t
    | `Ok e, `Stdout -> `Ok { e with stdout="" }
    | `Ok e, `Stderr -> `Ok { e with stderr="" }

  let find topic exec =
    TMap.filter (fun t m -> t = topic) exec.data

  let duration = function
    | `Ok e ->
        TMap.find Topic.(Topic (Time.Real, Time)) e.data |> Measure.to_int64
    | _ -> 0L
end

module Benchmark = struct

  type speed = [`Fast | `Slow | `Slower] with sexp

  type t = {
    name: string;
    descr: string with default("");
    cmd: string list;
    cmd_check: string list with default([]);
    env: string list option with default(None);
    speed: speed with default(`Fast);
    timeout: int with default(600);
    weight: float with default(1.);
    discard: [`Stdout | `Stderr] list with default([]);
    topics: TSet.t with default(TSet.singleton (Topic.(Topic("cycles", Perf))));
  } with sexp

  let make ~name ?(descr="") ~cmd ?(cmd_check=[])
      ?env ~speed ?(timeout=600) ?(weight=1.) ?(discard=[]) ~topics () =
    { name; descr; cmd; cmd_check; env; speed; timeout; weight; discard;
      topics = TSet.of_list topics; }

  let load_conv fn =
    Sexplib.Sexp.load_sexp_conv fn t_of_sexp

  let load_conv_exn fn =
    Util.File.sexp_of_file_exn fn t_of_sexp

  let save_hum fn s =
    sexp_of_t s |> Sexplib.Sexp.save_hum fn

  let output_hum oc s =
    sexp_of_t s |> Sexplib.Sexp.output_hum oc

  let find_installed ?glob switch =
    let share = Util.Opam.share switch in
    Util.FS.ls share
    |> List.map (fun n -> Filename.concat share n)
    |> List.filter (fun n -> Unix.((stat n).st_kind = S_DIR))
    |> List.map
      (fun selector ->
         let bench_files =
           Util.FS.ls selector
           |> List.map (Filename.concat selector)
           |> List.filter (fun fn -> Filename.check_suffix fn ".bench")
         in
         let bench_names = List.map
             (fun fn -> let b = load_conv_exn fn in b.name)
             bench_files in
         List.combine bench_names bench_files
      )
    |> List.flatten
    |> fun l -> match glob with
    | None -> l
    | Some glob ->
        let re = Re_glob.globx ~anchored:() glob |> Re.compile in
        List.filter_map (fun (name, path) ->
            if Re.execp re name
            then Some (name, path) else None
          )
          l
end

module Result = struct
  type t = {
    bench: Benchmark.t;
    context_id: string;
    execs: Execution.t list;
  } with sexp

  let make ~bench ?(context_id="") ~execs () =
    let execs = List.map
        (fun e -> List.fold_left
            (fun a ch -> Execution.strip ch a)
            e bench.Benchmark.discard
        )
        execs in
    { bench; context_id; execs; }

  let strip chan t = match chan with
    | `Stdout ->
        { t with execs = List.map (Execution.strip `Stdout) t.execs }
    | `Stderr ->
        { t with execs = List.map (Execution.strip `Stderr) t.execs }

  let load_conv fn =
    Sexplib.Sexp.load_sexp_conv fn t_of_sexp

  let load_conv_exn fn =
    Util.File.sexp_of_file_exn fn t_of_sexp

  let save_hum fn s=
    sexp_of_t s |> Sexplib.Sexp.save_hum fn

  let output_hum oc s =
    sexp_of_t s |> Sexplib.Sexp.output_hum oc
end

module Summary = struct
  module Aggr = struct
    type t = {
      mean: float;
      stddev: float;
      mini: float;
      maxi: float;
    } with sexp

    let create mean stddev mini maxi = { mean; stddev; mini; maxi; }

    let compare t1 t2 = Pervasives.compare t1.mean t2.mean
    let min t1 t2 = if t1.mean <= t2.mean then t1 else t2
    let max t1 t2 = if t1.mean >= t2.mean then t1 else t2
    let of_measures m =
      let measures_float = List.map Measure.to_float m in
      let mean, variance = Statistics.mean_variance measures_float in
      let maxi, mini = List.fold_left
          (fun (ma, mi) v -> Pervasives.(max v ma, min v mi))
          (min_float, max_float) measures_float in
      { mean; stddev = sqrt variance; mini; maxi; }

    let normalize t =
      let m = t.mean in
      { mean=1.; stddev = t.stddev /. m; mini = t.mini /. m; maxi = t.maxi /. m }

    (* t1 / t2 *)
    let normalize2 t1 t2 =
      { mean = t1.mean /. t2.mean;
        stddev = t1.stddev /. t2.mean;
        mini = t1.mini /. t2.mean;
        maxi = t1.maxi /. t2.mean;
      }
  end

  type t = {
    name: string;
    context_id: string;
    weight: float;
    data: Aggr.t TMap.t;
  } with sexp

  let of_result r =
    let open Execution in
    let open Result in
    let data = List.fold_left
        (fun a e -> match e with | `Ok e -> e.data::a | _ -> a)
        [] r.execs in
    let data = data
               |> TMap.lmerge
               |> TMap.map @@ Aggr.of_measures in
    { name = r.bench.Benchmark.name;
      context_id = r.Result.context_id;
      weight = r.bench.Benchmark.weight;
      data;
    }

  let normalize s =
    { s with data = TMap.map Aggr.normalize s.data }

  let normalize2 s1 s2 =
    { s1 with data = TMap.mapi (fun k v ->
         let v2 = TMap.find k s2.data in
         Aggr.normalize2 v v2) s1.data
    }

  let load_from_result fn =
    let result = Util.File.sexp_of_file_exn fn Result.t_of_sexp in
    of_result result

  let load_conv fn =
    Sexplib.Sexp.load_sexp_conv fn t_of_sexp

  let load_conv_exn fn =
    Util.File.sexp_of_file_exn fn t_of_sexp

  let save_hum fn s=
    sexp_of_t s |> Sexplib.Sexp.save_hum fn

  let output_hum oc s =
    sexp_of_t s |> Sexplib.Sexp.output_hum oc

  let fold_dir f acc dn =
    let open Unix in
    Util.FS.fold
      (fun acc fn -> match (stat fn).st_kind with
         | S_REG when Filename.check_suffix fn ".summary" -> f acc fn
         | _ -> acc
      )
      acc dn

  let summarize_dir ?(update_only=true) dn =
    let open Unix in
    let summarize_dir_unsafe () =
      Util.FS.fold_files
        (fun _ fn ->
           if Filename.check_suffix fn ".result" then
             let summary_fn =
               Filename.chop_suffix fn ".result" ^ ".summary" in
             if not
                 (update_only
                  && Sys.file_exists summary_fn
                  && Unix.((stat summary_fn).st_mtime > (stat fn).st_mtime))
             then
               let s = load_from_result fn in
               save_hum summary_fn s
        )
        () dn
    in
    if Sys.file_exists dn then summarize_dir_unsafe ()
end

module DB = struct
  type 'a t = ('a SMap.t) SMap.t with sexp
  (** Indexed by benchmark, context_id, topic. *)

  let empty = SMap.empty

  let add k1 k2 v t =
    let map1 = try SMap.find k1 t with Not_found -> SMap.empty in
    let map1 = SMap.add k2 v map1 in
    SMap.add k1 map1 t

  let map f t =
    SMap.map (fun v -> SMap.map (fun v -> f v) v) t

  let fold f t a =
    SMap.fold (fun k1 v a ->
        SMap.fold (fun k2 v a ->
            f k1 k2 v a)
          v a)
      t a

  let fold_data f t a =
    SMap.fold (fun k1 v a ->
        SMap.fold (fun k2 v a ->
            TMap.fold (fun k3 v a ->
                f k1 k2 k3 v a)
              v.Summary.data a)
          v a)
      t a

  let of_dir ?(acc=SMap.empty) dn =
    let open Summary in
    let of_dir_unsafe () =
      fold_dir
        (fun db fn ->
           let s = load_conv_exn fn in
           add s.name s.context_id s db
        )
        acc dn
    in
    if Sys.file_exists dn then of_dir_unsafe () else acc

  let save_hum fn f s =
    sexp_of_t f s |> Sexplib.Sexp.save_hum fn

  let output_hum oc f s =
    sexp_of_t f s |> Sexplib.Sexp.output_hum oc
end

module DB2 = struct
  type 'a t = (('a SMap.t) SMap.t) TMap.t with sexp
  (** Indexed by topic, benchmark, context_id *)

  let empty = TMap.empty

  let add topic bench context_id measure t =
    let bench_map = try TMap.find topic t with Not_found -> SMap.empty in
    let cid_map = try SMap.find bench bench_map with Not_found -> SMap.empty in
    let cid_map = SMap.add context_id measure cid_map in
    let bench_map = SMap.add bench cid_map bench_map in
    TMap.add topic bench_map t

  let map f t =
    TMap.map (fun v -> SMap.map (fun v -> f v) v) t

  let fold f t a =
    TMap.fold (fun k1 v a ->
        SMap.fold (fun k2 v a ->
            SMap.fold (fun k3 v a ->
                f k1 k2 k3 v a)
              v a)
          v a)
      t a

  let normalize ?against t =
    let normalize_smap ?against smap =
      match against with
      | Some (`Ctx context_id) ->
          (try
            let normal_aggr = SMap.find context_id smap in
            Some (SMap.map (fun a -> Summary.Aggr.normalize2 a normal_aggr) smap)
          with Not_found -> None)
      | Some `Biggest ->
          (
            let biggest = SMap.fold
                (fun k v a -> Summary.Aggr.max v a)
                smap (snd @@ SMap.min_binding smap)
            in
            Some (SMap.map (fun a -> Summary.Aggr.normalize2 a biggest) smap)
          )
      | None -> Some (SMap.map Summary.Aggr.normalize smap)
    in
    TMap.map (fun v -> SMap.filter_map (fun v -> normalize_smap ?against v) v) t

  let context_ids db =
    fold (fun _ _ ctx _ a -> SSet.add ctx a) db SSet.empty

  let add_missing_ctx db =
    let ctx_ids = context_ids db in
    let nb_ctxs = SSet.cardinal ctx_ids in
    let db = map (fun ctxmap -> SMap.map (fun aggr -> Some aggr) ctxmap) db in
    map (fun ctxmap ->
        SSet.fold (fun ctx ctxmap ->
            SMap.add ctx
              (try SMap.find ctx ctxmap with Not_found -> None)
              ctxmap)
          ctx_ids ctxmap)
      db, nb_ctxs

  let to_csv ?(escape_uscore=false) ?(sep=",") oc ?topic db =
    let print_table topic = function db when db = SMap.empty -> () | db ->
      let min_binding = snd @@ SMap.min_binding db in
      let context_ids =
        List.map (fun (ctxid,_) -> ctxid ^ "," ) @@ SMap.bindings min_binding in
      output_string oc @@ topic ^ sep;
      output_string oc @@ String.concat sep context_ids ^ "\n";
      SMap.iter (fun bench ctx_map ->
          let bench = if escape_uscore
            then Re_pcre.(substitute ~rex:(regexp "_") ~subst:(fun _ -> "\\\\_") bench)
            else bench in
          output_string oc @@ bench ^ sep;
          SMap.bindings ctx_map
          |> List.map (fun (_, sopt) -> match sopt with
              | None -> ","
              | Some aggr ->
                  string_of_float Summary.(aggr.Aggr.mean) ^ "," ^
                  string_of_float Summary.(aggr.Aggr.stddev)
            )
          |> String.concat sep
          |> output_string oc;
          output_string oc "\n"
        ) db
    in
    let db, nb_ctxs = add_missing_ctx db in
    match topic with
    | None ->
        TMap.iter
          (fun t db -> print_table (Topic.to_string t) db; print_endline "")
          db; nb_ctxs
    | Some t ->
        try print_table (Topic.to_string t) (TMap.find t db); nb_ctxs
        with Not_found -> nb_ctxs

  let save_hum fn f s =
    sexp_of_t f s |> Sexplib.Sexp.save_hum fn

  let output_hum oc f s =
    sexp_of_t f s |> Sexplib.Sexp.output_hum oc
end

module Process = struct

  type speed_characterization = {
    max_duration: int64;
    probability: float;
    confidence: float;
  }

  let fast = {
    max_duration = 100_000_000L;
    probability = 0.99;
    confidence = 0.05;
  }
  let slow = {
    max_duration = 1_000_000_000L;
    probability = 0.99;
    confidence = 0.05;
  }
  let slower = {
    max_duration = 300_000_000_000L;
    probability = 0.99;
    confidence = 0.05;
  }

  let run ?(fast=fast) ?(slow=slow) ?(slower=slower) ~interactive (f : unit -> Execution.t) =

    let run_until ~probability ~confidence (init_acc : Execution.t list) =
      let rec run_until (nb_iter, (acc : Execution.t list)) =
        let durations =
          List.map (fun e -> Execution.duration e |> Int64.to_float) acc in
        match Statistics.enough_samples ~probability ~confidence durations with
        | true ->
            if interactive then
              Printf.printf "%d times.\n" nb_iter;
            acc
        | false ->
            let exec = f () in
            run_until (succ nb_iter, (exec::acc))
      in
      run_until (1, init_acc)
    in
    let exec = f () in
    (* Remove the OCAML_GC_STATS env variable. *)
    Unix.putenv "OCAML_GC_STATS" "";
    match exec with
    | `Ok _ as e ->
        let duration = Execution.duration e in
        (match duration with
        | t when t < fast.max_duration -> (* Fast *)
            run_until
              ~probability:fast.probability
              ~confidence:fast.confidence []
        | t when t < slow.max_duration -> (* Slow *)
            run_until
              ~probability:slow.probability
              ~confidence:slow.confidence []
        | t ->                            (* Slower: keep the first execution *)
            run_until
              ~probability:slower.probability
              ~confidence:slower.confidence [exec])
    | other -> [other]

  let data_of_gc_stats () =
    let lines = Util.File.lines_of_file "gc_stats" in
    List.map
      (fun s ->
         let i = String.index s ':' in
         let gc = Topic.Gc.of_string_exn @@ String.sub s 0 i in
         let v = Int64.of_string @@ String.sub s (i+2) (String.length s - i - 2) in
         (Topic.(Topic (gc, Gc), Measure.of_int64 v))
      )
      lines
end

module Perf_wrapper = struct
  include Process

  let run_once ?env ?timeout ?chk_cmd cmd evts =
    let evts = SSet.elements evts in
    let perf_cmdline = ["perf"; "stat"; "-x,"; ] in
    let perf_cmdline = match evts with
      | [] -> perf_cmdline
      | _ -> perf_cmdline @ ["-e"; String.concat "," evts] in
    let cmd = perf_cmdline @ cmd in
    let env = match env with
      | None -> [|"LANG=C"|]
      | Some env -> Array.of_list @@ "LANG=C"::env in
    let cmd_string = String.concat " " cmd in
    let time_start = Oclock.(gettime monotonic) in
    let p_stdout, p_stdin, p_stderr = Unix.open_process_full cmd_string env in
    try
      let stdout_string = Util.File.string_of_ic p_stdout in
      Util.File.write_string_to_file "stdout" stdout_string;
      let stderr_lines = Util.File.lines_of_ic p_stderr in
      (* Setup an alarm that will make Unix.close_process_full raise
         EINTR if its process is not terminated by then *)
      let (_:int) = match timeout with None -> 0 | Some t -> Unix.alarm t in
      Sys.(set_signal sigalrm (Signal_handle (fun _ -> ())));
      let process_status =  Unix.close_process_full (p_stdout, p_stdin, p_stderr) in
      let time_end = Oclock.(gettime monotonic) in
      let rex = Re.(str "," |> compile) in
      let stderr_lines = List.map (Re_pcre.split ~rex) stderr_lines in
      let gc_topics = (match Sys.file_exists "gc_stats" with
          | false -> []
          | true -> data_of_gc_stats ()) in
      let time_topics = [Topic.(Topic (Time.Real, Time),
                                `Int Int64.(rem time_end time_start))] in
      let data = List.fold_left
          (fun acc l -> match l with
             | [v; ""; event; ]
             | [v; ""; event; _] ->
                 TMap.add Topic.(Topic (event, Perf)) (Measure.of_string v) acc
             | l ->
                 Printf.eprintf
                   "Ignoring perf result line [%s]" (String.concat "," l);
                 acc
          ) TMap.empty stderr_lines in
      let data = List.fold_left (fun a (k, v) -> TMap.add k v a) data gc_topics in
      let data = List.fold_left (fun a (k, v) -> TMap.add k v a) data time_topics in

      `Ok Execution.{
          process_status;
          stdout=stdout_string;
          stderr=""; (* Perf writes its result on stderr... *)
          data;
          checked=(match chk_cmd with
              | None -> None
              | Some chk -> (match Sys.command (String.concat " " chk)
                             with 0 -> Some true | _ -> Some false))
        }
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> `Timeout
    | exn ->
        ignore @@ Unix.close_process_full (p_stdout, p_stdin, p_stderr);
        Execution.error exn

  let run ?env ?timeout ?chk_cmd cmd evts =
    (* if evts = SSet.empty then [] *)
    (* else *)
      run (fun () -> run_once ?env ?timeout ?chk_cmd cmd evts)
end

module Libperf_wrapper = struct
  include Process

  let run_once ?env ?timeout ?chk_cmd cmd evts =
    let open Perf in
    let attrs = SSet.elements evts |> List.map Attr.Kind.of_string in
    let attrs = List.map Perf.Attr.make attrs in
    (* /!\ Perf.with_process <> Process.with_process, but similar /!\ *)
    with_process
      ?env ?timeout ~stdout:"stdout" ~stderr:"stderr" cmd attrs |> function
    | `Ok {process_status; stdout; stderr; duration; data;} ->
        let data = KindMap.fold
            (fun k v a -> TMap.add Topic.(Topic (Attr.Kind.to_string k, Perf)) (`Int v) a)
            data TMap.empty in
        let data = TMap.add Topic.(Topic ((Time.Real, Time))) (`Int duration) data in
        let data = List.fold_left (fun a (k, v) -> TMap.add k v a)
            data
            (match Sys.file_exists "gc_stats" with
             | false -> []
             | true -> data_of_gc_stats ())
        in
        let checked = match chk_cmd with
          | None -> None
          | Some chk -> (match Sys.command (String.concat " " chk) with
              | 0 -> Some true | _ -> Some false) in
        `Ok Execution.{ process_status; stdout; stderr; data; checked; }
    | `Timeout -> `Timeout
    | `Exn e -> Execution.error e

  let run ?env ?timeout cmd evts =
    run (fun () -> run_once ?env ?timeout cmd evts)
end

module Runner = struct
  type execs = {
    time: Topic.TimeSet.t;
    gc: Topic.GcSet.t;
    perf: SSet.t;
  }

  let run_exn ?(use_perf=false) ?(context_id=Util.Opam.cur_switch) ~interactive b =
    let open Benchmark in

    (* We run benchmarks in a temporary directory that we create now. *)
    let temp_dir = Filename.temp_file "macrorun" "" in
    Unix.unlink temp_dir;
    Unix.(try
       mkdir temp_dir 0o755
     with Unix_error (EEXIST, _, _) -> ());
    Unix.chdir temp_dir;

    let env = match b.env with
      | None -> ["OCAML_GC_STATS=gc_stats"] @
                Array.to_list @@ Unix.environment ()
      | Some e -> "OCAML_GC_STATS=gc_stats"::e
    in

    (* Transform individial topics into a list of executions *)
    let execs =
      let open Topic in
      TSet.fold
        (fun t a -> match t with
           | Topic (t, Time) -> { a with time = TimeSet.add t a.time }
           | Topic (t, Gc) -> { a with gc = GcSet.add t a.gc }
           | Topic (t, Perf) -> { a with perf= SSet.add t a.perf }
        )
        b.topics
        { time = TimeSet.empty;
          gc = GcSet.empty;
          perf = SSet.empty;
        }
    in

    let run_execs { time; gc; perf; } b =
      if use_perf then
        Perf_wrapper.(run ~interactive ~env b.cmd perf)
      else
        Libperf_wrapper.(run ~interactive ~env b.cmd perf)
    in

    if interactive then
      Printf.printf "Running benchmark %s (compiled with OCaml %s)... %!" b.name context_id;
    let execs = run_execs execs b in

    (* Cleanup temporary directory *)
    Util.FS.rm_r [temp_dir];
    Result.make ~context_id ~bench:b ~execs ()
end
