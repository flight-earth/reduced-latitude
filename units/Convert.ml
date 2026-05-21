module Convert = struct
  type deg = float
  type rad = float
  type min = float
  type sec = float
  type meter = float

  let min_to_sec (m : min) : sec = m *. 60.0
  let deg_to_sec (d : deg) : sec = d *. 3600.0
  let deg_to_min (d : deg) : min = d *. 60.0
  let min_to_deg (m : min) : deg = m /. 60.0
  let sec_to_deg (s : sec) : deg = s /. 3600.0

  (* SEE: https://doc.rust-lang.org/std/f64/consts/constant.PI.html *)
  let pi = 3.14159265358979323846264338327950288
  let deg_to_rad (x : deg) : rad = x *. pi /. 180.0
  let rad_to_deg (x : rad) : deg = x /. pi *. 180.0

  let normalize_deg (deg_plus_minus : deg) : deg =
    let deg = Float.abs deg_plus_minus in
    let n = Float.floor (deg /. 360.0) in
    let d = deg -. (n *. 360.0) in
    if d = 0.0 then d else if deg_plus_minus > 0.0 then d else 360.0 -. d

  let normalize_rad (x : rad) : rad = deg_to_rad (normalize_deg (rad_to_deg x))

  type ordering = Eq | Lt | Gt

  let ord_to_float = function Eq -> 0.0 | Lt -> -1.0 | Gt -> 1.0
  let is_even (x : int64) : bool = Int64.rem x 2L = 0L

  let plus_minus_pi_deg (deg_plus_minus : deg) : deg =
    if Float.is_nan deg_plus_minus then deg_plus_minus
    else
      let deg = Float.abs deg_plus_minus in
      let n = Float.floor (deg /. 180.0) in
      let d = deg -. (n *. 180.0) in
      let m =
        ord_to_float
          (if deg_plus_minus = 0.0 then Eq
           else if deg_plus_minus < 0.0 then Lt
           else Gt)
      in
      let n_even = is_even (Int64.of_float (Float.abs n)) in
      if d = 0.0 then if n_even then 0.0 else m *. 180.0
      else
        (m *. d)
        +.
        if n_even then 0.0 else if deg_plus_minus >= 0.0 then -180.0 else 180.0

  let is_plus_minus_half_pi_deg (x : deg) : deg option =
    let y = plus_minus_pi_deg x in
    if y < -90.0 || y > 90.0 then None else Some y

  let plus_minus_pi_rad (x : rad) : rad =
    deg_to_rad (plus_minus_pi_deg (rad_to_deg x))

  let is_plus_minus_half_pi_rad (x : rad) : rad option =
    Option.map deg_to_rad (is_plus_minus_half_pi_deg (rad_to_deg x))
end
