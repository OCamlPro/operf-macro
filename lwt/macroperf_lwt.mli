open Macroperf

module Perf_wrapper : sig
  type measure = [ `Int of int | `Float of float | `Error]
  (** A measure can be expressed as an int or a float, or failed to be
      measured *)

  type result = (string * measure) list
  (** A result is a list of perf topics and associated measure *)

  val run :
    ?env:string array ->
    ?evts:string list ->
    ?nb_iter:int ->
    Lwt_process.command -> result Lwt.t
    (** [run ?topics ~cmd] is a thread that will launch [cmd] in perf --
        asking perf to run it [nb_iter] times (default: 1) -- and
        eventually return the result. [?topics] correspond to perf's
        "predefined events". *)
end

(* module Runner : sig *)
(*   val run : *)
(*     ?speed:Macroperf.Benchmark.speed -> *)
(* end *)
