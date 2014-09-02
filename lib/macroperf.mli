module Topic : sig
  type t =
    (* Time related *)
    | Time_real
    | Time_user
    | Time_sys

    (* GC related *)
    | Allocs_major
    | Allocs_minor
    | Compactions

    (* PERF(1) related (linux only) *)
    | Perf of string list
end

module TSet : Set.S with type elt = Topic.t

module Benchmark : sig
  type speed = [`Fast | `Slow | `Slower]

  type t = private {
    b_name: string;
    b_descr: string option;
    b_cmd: string * string array;
    b_env: string array option;
    b_speed: speed;
    b_measures: TSet.t;
  }

  val make :
    name:string ->
    ?descr:string ->
    cmd:(string * string array) ->
    ?env:string array ->
    speed:speed ->
    measures:Topic.t list ->
    unit ->
    t

  val of_string : string -> t
  val to_string : t -> string
end
