open Macroperf

module Runner : sig
  val run_exn :
    ?nb_iter:int ->
    ?topics:Topic.t list ->
    Benchmark.t ->
    Result.t
  (** [run_exn ?nb_iter ?topics b] is a thread that will run [b]
      [nb_iter] times (default: 1) and return the a result value. If
      [?topics] is set, the benchmark will be performed on [topics],
      otherwise the list of topics to benchmark is taken from [b]. *)

  val run :
    ?nb_iter:int ->
    ?topics:Topic.t list ->
    Benchmark.t ->
    Result.t option
    (** Like the above but never throw an exception. *)
end
