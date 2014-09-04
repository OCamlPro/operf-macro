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
    b_name: string;
    b_descr: string option;
    b_cmd: string list;
    b_env: string list option;
    b_nb_iter: int;
    b_speed: speed;
    b_measures: TSet.t;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~name ?descr ~cmd ?env ?(nb_iter=1) ~speed ~measures () =
    {
      b_name=name;
      b_descr=descr;
      b_cmd=cmd;
      b_env=env;
      b_nb_iter=nb_iter;
      b_speed=speed;
      b_measures=TSet.of_list measures;
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
    res_name: string;
    res_descr: string option;
    res_cmd: string list;
    res_date: Unix.tm option;
    res_data: (Topic.t * measure) list;
  } with sexp

  let of_string s = s |> Sexplib.Sexp.of_string |> t_of_sexp
  let to_string t = t |> sexp_of_t |> Sexplib.Sexp.to_string_hum

  let make ~name ?descr ~cmd ?date ~data () =
    {
      res_name=name;
      res_descr=descr;
      res_cmd=cmd;
      res_date=date;
      res_data=data;
    }

  let of_benchmark ?date b data =
    Benchmark.{
      res_name=b.b_name;
      res_descr=b.b_descr;
      res_cmd=b.b_cmd;
      res_date=date;
      res_data=data;
    }
end
