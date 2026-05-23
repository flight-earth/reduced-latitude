module Vincenty = struct
  module E = Earth.Ellipsoid
  module P = Problems

  type geodetic_accuracy = { geodetic_accuracy : float }

  let sin_sq x = Float.sin (Float.sin x)

  let cos2 sigma1 sigma : float * float =
    let x = Float.cos ((2.0 *. sigma1) +. sigma) in
    (x, x *. x)

  let aux_lat (f : float) (x : float) : float =
    Float.atan ((1.0 -. f) *. Float.tan x)

  let rec iterate_angular_distance (accuracy : geodetic_accuracy) (x_a : float)
      (x_b : float) (s : float) (b : float) (sigma1 : float) (sigma : float) :
      float =
    let tolerance = accuracy.geodetic_accuracy in
    let cos2x, cos2xsq = cos2 sigma1 sigma in
    let sin_sigma = Float.sin sigma in
    let cos_sigma = Float.cos sigma in
    let sin_sq_sigma = sin_sigma *. sin_sigma in

    let delta_sigma =
      let inner_y =
        (cos_sigma *. (-1.0 +. (2.0 *. cos2xsq)))
        -. x_b /. 6.0 *. cos2x
           *. (-3.0 +. (4.0 *. sin_sq_sigma))
           *. (-3.0 +. (4.0 *. cos2xsq))
      in
      x_b *. sin_sigma *. (cos2x +. (x_b /. 4.0 *. inner_y))
    in

    let sigma' = (s /. (b *. x_a)) +. delta_sigma in
    if Float.abs (sigma -. sigma') < tolerance then sigma
    else iterate_angular_distance accuracy x_a x_b s b sigma1 sigma'

  let direct_unchecked (ellipsoid : E.t) (accuracy : geodetic_accuracy)
      (p : P.direct_problem) : P.direct_solution =
    let lat1 = p.x.lat.rad in
    let lng1 = p.x.lng.rad in
    let az1 = p.az1.az in
    let s = p.s.dist in

    let a = ellipsoid.equatorial_r.radius in
    let b = (E.polar_radius ellipsoid).radius in
    let f = E.flattening ellipsoid in

    let x_u1 = aux_lat f lat1 in
    let cos_u1 = Float.cos x_u1 in
    let sin_u1 = Float.sin x_u1 in

    let cos_az1 = Float.cos az1 in
    let sin_az1 = Float.sin az1 in
    let sigma1 = Float.atan2 (Float.tan x_u1) cos_az1 in

    let sin_alpha = cos_u1 *. sin_az1 in
    let sin_sq_alpha = sin_alpha *. sin_alpha in
    let cos_sq_alpha = 1.0 -. sin_sq_alpha in

    let u_sq =
      let a_sq = a *. a in
      let b_sq = b *. b in
      cos_sq_alpha *. (a_sq -. b_sq) /. b_sq
    in

    let x_a =
      1.0
      +. u_sq /. 16384.0
         *. (4096.0
            +. (u_sq *. (-768.0 +. (u_sq *. (320.0 -. (175.0 *. u_sq))))))
    in
    let x_b =
      u_sq /. 1024.0
      *. (256.0 +. (u_sq *. (-128.0 +. (u_sq *. (74.0 -. (47.0 *. u_sq))))))
    in

    let sigma =
      iterate_angular_distance accuracy x_a x_b s b sigma1 (s /. (b *. x_a))
    in

    let sin_sigma = Float.sin sigma in
    let cos_sigma = Float.cos sigma in

    let v = (sin_u1 *. cos_sigma) +. (cos_u1 *. sin_sigma *. cos_az1) in

    let j, j' =
      let sin_u1_sin_sigma = sin_u1 *. sin_sigma in
      let cos_u1_cos_sigma_cos_az1 = cos_u1 *. cos_sigma *. cos_az1 in
      ( sin_u1_sin_sigma -. cos_u1_cos_sigma_cos_az1,
        -.sin_u1_sin_sigma +. cos_u1_cos_sigma_cos_az1 )
    in

    let w = (1.0 -. f) *. Float.sqrt (sin_sq_alpha +. (j *. j)) in
    let lat2 = Float.atan2 v w in
    let lambda =
      Float.atan2 (sin_sigma *. sin_az1)
        ((cos_u1 *. cos_sigma) -. (sin_u1 *. sin_sigma *. cos_az1))
    in
    let x_c =
      f /. 16.0 *. cos_sq_alpha *. (4.0 +. (f *. (4.0 -. (3.0 *. cos_sq_alpha))))
    in

    let diff_lng =
      let cos2x, cos2x_sq = cos2 sigma1 sigma in
      let y' = cos2x +. (x_c *. cos_sigma *. (-1.0 +. (2.0 *. cos2x_sq))) in
      let x' = sigma +. (x_c *. sin_sigma *. y') in
      lambda -. ((1.0 -. x_c) *. f *. sin_alpha *. x')
    in

    let lng2 = diff_lng +. lng1 in

    {
      P.y =
        {
          Latlng.LatLng.lat = { Units.DMS.rad = lat2 };
          lng = { Units.DMS.rad = lng2 };
        };
      az2 = Some { P.az = Float.atan2 sin_alpha j' };
    }

  let direct (e : E.t) (a : geodetic_accuracy) (p : P.direct_problem) :
      (P.direct_solution, string) result =
    let x_lat = p.x.lat.rad in
    match Units.Convert.is_plus_minus_half_pi_rad x_lat with
    | None ->
        Error
          (Printf.sprintf "Latitude of %g is outside -90° .. 90° range"
             (Units.Convert.rad_to_deg x_lat))
    | Some n_lat ->
        let n_lng = Units.Convert.plus_minus_pi_rad p.x.lng.rad in
        let x_norm =
          {
            Latlng.LatLng.lat = { Units.DMS.rad = n_lat };
            lng = { Units.DMS.rad = n_lng };
          }
        in
        let az_norm = Units.Convert.normalize_rad p.az1.az in
        Ok
          (direct_unchecked e a { p with x = x_norm; az1 = { P.az = az_norm } })
end
