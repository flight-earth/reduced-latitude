open Problems

let haversine (x : Units.Convert.rad) : Units.Convert.rad =
  let y = Float.sin (x /. 2.0) in
  y *. y

let a_of_haversine (x : Latlng.LatLng.t) (y : Latlng.LatLng.t) :
    Units.Convert.rad =
  let x_lat = x.lat.rad in
  let x_lng = x.lng.rad in
  let y_lat = y.lat.rad in
  let y_lng = y.lng.rad in
  let d_lat = y_lat -. x_lat in
  let d_lng = y_lng -. x_lng in
  let h_lat_f = haversine d_lat in
  let h_lng_f = haversine d_lng in
  h_lat_f +. (Float.cos x_lat *. Float.cos y_lat *. h_lng_f)

let distance (x : Latlng.LatLng.t) (y : Latlng.LatLng.t) : dist =
  let r_earth = Earth.Sphere.earth_radius.radius in
  let rad_dist = 2.0 *. Float.asin (Float.sqrt (a_of_haversine x y)) in
  { dist = rad_dist *. r_earth }

let azimuth_fwd' (x : Latlng.LatLng.t) (y : Latlng.LatLng.t) : Units.Convert.rad
    =
  let x_lat = x.lat.rad in
  let x_lng = x.lng.rad in
  let y_lat = y.lat.rad in
  let y_lng = y.lng.rad in
  let delta_lng = y_lng -. x_lng in
  let x' = Float.sin delta_lng *. Float.cos y_lat in
  let y' =
    (Float.cos x_lat *. Float.sin y_lat)
    -. (Float.sin x_lat *. Float.cos y_lat *. Float.cos delta_lng)
  in
  Float.atan2 x' y'

let azimuth_fwd (x : Latlng.LatLng.t) (y : Latlng.LatLng.t) :
    Units.Convert.rad option =
  Some (azimuth_fwd' x y)

let rotate (by : Units.DMS.rad) (x : Units.DMS.rad) : Units.DMS.rad =
  { rad = x.rad +. by.rad }

let azimuth_rev (x : Latlng.LatLng.t) (y : Latlng.LatLng.t) :
    Units.Convert.rad option =
  azimuth_fwd y x
  |> Option.map (fun az ->
      let az' = rotate { rad = Units.Convert.pi } { rad = az } in
      az'.rad)

let direct (prob : direct_problem) : direct_solution =
  let lat1 = prob.x.lat.rad in
  let lng1 = prob.x.lng.rad in
  let az1 = prob.az1.az in
  let earth_r = Earth.Sphere.earth_radius.radius in
  let d = prob.s.dist in
  let d_r = d /. earth_r in

  (* SEE: https://www.movable-type.co.uk/scripts/latlong.html *)
  let lat2 =
    Float.asin
      ((Float.sin lat1 *. Float.cos d_r)
      +. (Float.cos lat1 *. Float.sin d_r *. Float.cos az1))
  in
  let lng2 =
    lng1
    +. Float.atan2
         (Float.sin az1 *. Float.sin d_r *. Float.cos lat1)
         (Float.cos d_r -. (Float.sin lat1 *. Float.sin lat2))
  in
  let y = { Latlng.LatLng.lat = { rad = lat2 }; lng = { rad = lng2 } } in

  let az2 =
    azimuth_fwd y { Latlng.LatLng.lat = { rad = lat1 }; lng = { rad = lng1 } }
    |> Option.map (fun az ->
        let rotated =
          rotate { Units.DMS.rad = Units.Convert.pi } { rad = az }
        in
        let d = Units.DMS.from_rad rotated in
        let dms = Units.DMS.from_deg d in
        let ndms = Units.DMS.normalize_dms dms in
        let normalized_rad =
          Units.Convert.deg_to_rad (Units.DMS.to_deg ndms).deg
        in
        { az = normalized_rad })
  in
  { y; az2 }

let inverse (p : inverse_problem) : inverse_solution =
  let az1 = { az = azimuth_fwd' p.x p.y } in
  let az2 = Option.map (fun az -> { az }) (azimuth_rev p.x p.y) in
  { s = distance p.x p.y; az1; az2 }
