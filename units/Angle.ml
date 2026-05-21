module type Angle = sig
  type t

  val normalize : t -> t
  (** 0 <= t <= 2π *)

  val plus_minus_pi : t -> t
  (** -π <= t <= +π *)

  val plus_minus_half_pi : t -> t option
  (** -π/2 <= t <= +π/2 *)

  val rotate : t -> t -> t
  (** rotation from an initial angle *)
end
