module Vincenty = struct
  module E = Earth.Ellipsoid
  module P = Problems

  let normalize_lng (lng : float) : float = Float.rem lng (2.0 *. Float.pi)
  let too_far : P.dist = { dist = 2000000.0 }
  let min_lat_bound = Units.Convert.deg_to_rad (-90.0)
  let max_lat_bound = Units.Convert.deg_to_rad 90.0
  let min_lng_bound = Units.Convert.deg_to_rad (-180.0)
  let max_lng_bound = Units.Convert.deg_to_rad 180.0

  type inverse_step = {
    tolerance : P.geodetic_accuracy;
    a : float;
    b : float;
    f : float;
    l : float;
    sin_u1 : float;
    sin_u2 : float;
    cos_u1 : float;
    cos_u2 : float;
    sin_u1_sin_u2 : float;
    cos_u1_cos_u2 : float;
  }

  let rec iloop (step : inverse_step) (lambda : float) : P.geodetic_inverse =
    if Float.abs lambda > Float.pi then P.Geodetic_inverse_antipodal
    else
      let sin_lambda = Float.sin lambda in
      let cos_lambda = Float.cos lambda in

      let i' = step.cos_u1 *. sin_lambda in
      let j' =
        (-.step.sin_u1 *. step.cos_u2)
        +. (step.cos_u1 *. step.sin_u2 *. cos_lambda)
      in

      let i = step.cos_u2 *. sin_lambda in
      let j =
        (step.cos_u1 *. step.sin_u2)
        -. (step.sin_u1 *. step.cos_u2 *. cos_lambda)
      in

      let sin2_sigma = (i *. i) +. (j *. j) in
      let sin_sigma = Float.sqrt sin2_sigma in
      let cos_sigma =
        step.sin_u1_sin_u2 +. (step.cos_u1_cos_u2 *. cos_lambda)
      in

      let sigma = Float.atan2 sin_sigma cos_sigma in

      let sin_alpha = step.cos_u1_cos_u2 *. sin_lambda /. sin_sigma in
      let cos2_alpha = 1.0 -. (sin_alpha *. sin_alpha) in
      let c =
        step.f /. 16.0 *. cos2_alpha
        *. (4.0 +. (step.f *. (4.0 -. (3.0 *. cos2_alpha))))
      in
      let u2 =
        cos2_alpha
        *. ((step.a *. step.a) -. (step.b *. step.b))
        /. (step.b *. step.b)
      in

      let cos2_sigma_m =
        if cos2_alpha = 0.0 then 0.0
        else cos_sigma -. (2.0 *. step.sin_u1_sin_u2 /. cos2_alpha)
      in
      let cos2_2_sigma_m = cos2_sigma_m *. cos2_sigma_m in

      let a =
        1.0
        +. u2 /. 16384.0
           *. (4096.0 +. (u2 *. (-768.0 +. (u2 *. (320.0 -. (175.0 *. u2))))))
      in
      let b =
        u2 /. 1024.0
        *. (256.0 +. (u2 *. (-128.0 +. (u2 *. (74.0 -. (47.0 *. u2))))))
      in

      let y =
        (cos_sigma *. (-1.0 +. (2.0 *. cos2_2_sigma_m)))
        -. b /. 6.0 *. cos2_sigma_m
           *. (-3.0 +. (4.0 *. sin2_sigma))
           *. (-3.0 +. (4.0 *. cos2_2_sigma_m))
      in

      let delta_sigma = b *. sin_sigma *. (cos2_sigma_m +. (b /. 4.0 *. y)) in

      let x =
        cos2_sigma_m +. (c *. cos_sigma *. (-1.0 +. (2.0 *. cos2_2_sigma_m)))
      in
      let lambda' =
        step.l
        +. (1.0 -. c) *. step.f *. sin_alpha
           *. (sigma +. (c *. sin_sigma *. x))
      in

      let s = step.b *. a *. (sigma -. delta_sigma) in
      let alpha1 = Float.atan2 i j in
      let alpha2 = Float.atan2 i' j' in

      if Float.abs (lambda -. lambda') >= step.tolerance.accuracy then
        iloop step lambda'
      else
        P.Geodetic_inverse
          {
            P.s = { P.dist = s };
            az1 = { P.az = alpha1 };
            az2 = Some { P.az = alpha2 };
          }

  let inverse (ellipsoid : E.t) (tolerance : P.geodetic_accuracy)
      (p : P.inverse_problem) : P.geodetic_inverse =
    let phi1 = p.x.lat.rad in
    let l1 = p.x.lng.rad in
    let phi2 = p.y.lat.rad in
    let l2 = p.y.lng.rad in

    let a = ellipsoid.equatorial_r.radius in
    let b = (E.polar_radius ellipsoid).radius in
    let f = E.flattening ellipsoid in

    let aux_lat phi = Float.atan ((1.0 -. f) *. Float.tan phi) in
    let u1 = aux_lat phi1 in
    let u2 = aux_lat phi2 in

    let l =
      let l' = l2 -. l1 in
      if Float.abs l' <= Float.pi then l'
      else normalize_lng l2 -. normalize_lng l1
    in

    let sin_u1 = Float.sin u1 in
    let sin_u2 = Float.sin u2 in
    let cos_u1 = Float.cos u1 in
    let cos_u2 = Float.cos u2 in

    let step =
      {
        tolerance;
        a;
        b;
        f;
        l;
        sin_u1;
        sin_u2;
        cos_u1;
        cos_u2;
        sin_u1_sin_u2 = sin_u1 *. sin_u2;
        cos_u1_cos_u2 = cos_u1 *. cos_u2;
      }
    in
    iloop step l

  let distance_unchecked (ellipsoid : E.t) (prob : P.inverse_problem) :
      P.geodetic_inverse =
    if prob.x = prob.y then
      P.Geodetic_inverse
        {
          P.s = { dist = 0.0 };
          az1 = { az = 0.0 };
          az2 = Some { az = Float.pi };
        }
    else if prob.x.lat.rad < min_lat_bound then
      P.Geodetic_inverse_abnormal P.Lat_under
    else if prob.x.lat.rad > max_lat_bound then
      P.Geodetic_inverse_abnormal P.Lat_over
    else if prob.x.lng.rad < min_lng_bound then
      P.Geodetic_inverse_abnormal P.Lng_under
    else if prob.x.lng.rad > max_lng_bound then
      P.Geodetic_inverse_abnormal P.Lng_over
    else if prob.y.lat.rad < min_lat_bound then
      P.Geodetic_inverse_abnormal P.Lat_under
    else if prob.y.lat.rad > max_lat_bound then
      P.Geodetic_inverse_abnormal P.Lat_over
    else if prob.y.lng.rad < min_lng_bound then
      P.Geodetic_inverse_abnormal P.Lng_under
    else if prob.y.lng.rad > max_lng_bound then
      P.Geodetic_inverse_abnormal P.Lng_over
    else inverse ellipsoid P.default_geodetic_accuracy prob

  let distance (e : E.t) (x : Latlng.LatLng.t) (y : Latlng.LatLng.t) :
      (P.dist, string) result =
    let qx = x.lat in
    let qy = y.lat in
    let sx = Units.DMS.(display_dms (normalize_dms (from_deg (from_rad qx)))) in
    let sy = Units.DMS.(display_dms (normalize_dms (from_deg (from_rad qy)))) in
    let msg =
      Printf.sprintf "Latitude of %s or %s is outside -90° .. 90° range" sx sy
    in

    let prob_opt =
      let open Units.DMS in
      let x_lat = from_deg (from_rad x.lat) in
      let y_lat = from_deg (from_rad y.lat) in
      let x_lng = from_deg (from_rad x.lng) in
      let y_lng = from_deg (from_rad y.lng) in
      match (dms_plus_minus_half_pi x_lat, dms_plus_minus_half_pi y_lat) with
      | Some x_lat', Some y_lat' ->
          let x_lng' = dms_plus_minus_pi x_lng in
          let y_lng' = dms_plus_minus_pi y_lng in
          Some
            {
              P.x = { Latlng.LatLng.lat = to_rad x_lat'; lng = to_rad x_lng' };
              y = { Latlng.LatLng.lat = to_rad y_lat'; lng = to_rad y_lng' };
            }
      | _ -> None
    in

    match prob_opt with
    | None -> Error msg
    | Some prob -> (
        match distance_unchecked e prob with
        | P.Geodetic_inverse_antipodal -> Ok too_far
        | P.Geodetic_inverse_abnormal _ -> Ok too_far
        | P.Geodetic_inverse i -> Ok i.s)
end
