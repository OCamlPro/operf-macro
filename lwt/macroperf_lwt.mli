open Macroperf

module Perf_wrapper : sig
  (** Wrapper for the PERF-STAT(1) command. *)

  type event = string
  (** Symbolic event (or group of event separated by ",", without
      space) that PERF-STAT(1) can measure. See PERF-LIST(1) for
      additional information. *)

  type measure = [ `Int of int | `Float of float | `Error ]
  (** A measure can be expressed as an int or a float, or failed to be
      measured *)

  type result = private {
    return_value: int;
    stdout: string;
    res: (Topic.t * measure) list
  }
  (** A result is a list of perf topics and associated measure *)

  val run :
    ?env:string list ->
    ?evt:event ->
    ?nb_iter:int ->
    string list -> result Lwt.t
    (** [run ?env ?evts ?nb_iter cmd] is a thread that will launch
        [cmd] in perf -- asking perf to run it [nb_iter] times
        (default: 1) -- and eventually return the perf result as well
        as the benchmark's return value *)
end

module Runner : sig
  exception Not_implemented
  (** Raised when the benchmark for a requested topic is not
      available *)

  val run_exn :
    ?nb_iter:int ->
    ?topics:Topic.t list ->
    Benchmark.t ->
    Result.t Lwt.t
  (** [run_exn ?nb_iter ?topics b] is a thread that will run [b]
      [nb_iter] times (default: 1) and return the a result value. If
      [?topics] is set, the benchmark will be performed on [topics],
      otherwise the list of topics to benchmark is taken from [b]. *)

  val run :
    ?nb_iter:int ->
    ?topics:Topic.t list ->
    Benchmark.t ->
    Result.t option Lwt.t
    (** Like the above but never throw an exception. *)
end
