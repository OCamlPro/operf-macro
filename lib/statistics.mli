val mean_variance : float list -> float * float

val mean_and_confidence_interval :
  probability:float -> float list ->
  float * float
(** [mean_and_confidence_interval ~probability samples =
     (empirical_mean, confidence_interval_width)] given a set of
     sample of a random variable (X) following a normal law, the mean
     of X is within [mean - width, mean + width] with the provided
     probability *)

val enough_samples :
  ?probability:float -> ?confidence:float -> float list -> bool
(** Given a random variable X following a normal law, [enought_samples
    ~probability ~confidence samples] is true if there is a
    probability higher than [probability] that the difference between
    the mean of X and the empirical mean of the samples is less than
    [confidence * empirical_mean] *)

val geometric_mean : float list -> float
(** [geometric_mean fs] is <insert tex formula here>. *)

val geometric_mean_w : (float * float) list -> float
(** [geometric_mean_w fs] is <insert tex formula here>. *)
