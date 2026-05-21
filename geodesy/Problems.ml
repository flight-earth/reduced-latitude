type az = { az : float }
type dist = { dist : float }
type direct_problem = { x : Latlng.LatLng.t; az1 : az; s : dist }
type inverse_problem = { x : Latlng.LatLng.t; y : Latlng.LatLng.t }
type direct_solution = { y : Latlng.LatLng.t; az2 : az option }
type inverse_solution = { s : dist; az1 : az; az2 : az option }

let string_of_dist (d : dist) : string = Printf.sprintf "%.17g" d.dist
let string_of_az (a : az) : string = Printf.sprintf "%.17g" a.az

let string_of_direct_problem (p : direct_problem) : string =
  Printf.sprintf "(x=%s, az1=%s, s=%s)"
    (Latlng.LatLng.to_string p.x)
    (string_of_az p.az1) (string_of_dist p.s)

let string_of_inverse_problem (p : inverse_problem) : string =
  Printf.sprintf "(x=%s, y=%s)"
    (Latlng.LatLng.to_string p.x)
    (Latlng.LatLng.to_string p.y)

let string_of_direct_solution (s : direct_solution) : string =
  let az2 =
    match s.az2 with None -> "None" | Some a -> "Some " ^ string_of_az a
  in
  Printf.sprintf "(y=%s, az2=%s)" (Latlng.LatLng.to_string s.y) az2

let string_of_inverse_solution (s : inverse_solution) : string =
  let az2 =
    match s.az2 with None -> "None" | Some a -> "Some " ^ string_of_az a
  in
  Printf.sprintf "(s=%s, az1=%s, az2=%s)" (string_of_dist s.s)
    (string_of_az s.az1) az2

type geodetic_accuracy = { accuracy : float }

let default_geodetic_accuracy : geodetic_accuracy =
  { accuracy = 1.0 /. 1_000_000_000_000.0 }

type abnormal_lat_lng = Lat_under | Lat_over | Lng_under | Lng_over

type geodetic_inverse =
  | Geodetic_inverse_abnormal of abnormal_lat_lng
  | Geodetic_inverse_antipodal
  | Geodetic_inverse of inverse_solution
