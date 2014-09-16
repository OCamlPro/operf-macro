open Sexplib.Std

module Topic = struct
  type t =
    (* Time related *)
    | Time_real
    | Time_user
    | Time_sys

    (* GC related *)
    | Allocs_major
    | Allocs_minor
    | Compactions

    (* PERF-STAT(1) related (linux only) *)
    | Perf of string with sexp

  let compare = Pervasives.compare
end

module TSet = struct
  include Set.Make(Topic)

  let t_of_sexp sexp = list_of_sexp Topic.t_of_sexp sexp |> of_list
  let sexp_of_t tset = elements tset |> (sexp_of_list Topic.sexp_of_t)
end

module Benchmark = struct

  type speed = [`Fast | `Slow | `Slower] with sexp

  type t = {
    name: string;
    descr: string option;
    deps: string list;
    cmd: string list;
    env: string list option;
    nb_iter: int;
    speed: speed;
    measures: TSet.t;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~name ?descr ?(deps=[]) ~cmd ?env ?(nb_iter=1)  ~speed ~measures () =
    {
      name; descr; deps; cmd; env; nb_iter; speed;
      measures=TSet.of_list measures;
    }
end

module Result = struct
  type measure = [ `Int of int | `Float of float | `Error ] with sexp

  module Unix = struct
    include Unix
    let tm_of_sexp sexp = sexp |> float_of_sexp |> Unix.gmtime
    let sexp_of_tm tm = Unix.mktime tm |> fst |> sexp_of_float
  end

  type t = {
    src: Benchmark.t;
    date: Unix.tm option;
    switch: string;
    data: (Topic.t * measure) list;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~src ?date ?switch ~data () =
    let switch = match switch with
      | None ->
          (match !OpamGlobals.switch with
           | `Command_line s
           | `Env s -> s
           | `Not_set ->
               let root = OpamFilename.Dir.of_string
                   OpamGlobals.default_opam_dir in
               let config = OpamPath.config root in
               OpamFile.Config.switch (OpamFile.Config.read config) |>
               OpamSwitch.to_string
          )
      | Some s -> s in
    { src; date; data; switch; }
end
