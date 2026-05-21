type t = { equatorial_r : Units.Radius.t; recip_f : float }

let to_string (e : t) : string =
  let r = e.equatorial_r.radius in
  Printf.sprintf "R=%gm, 1/ƒ=%g" r e.recip_f

let flattening (e : t) : float = 1.0 /. e.recip_f

let polar_radius (e : t) : Units.Radius.t =
  { radius = e.equatorial_r.radius *. (1.0 -. flattening e) }

(* SEE: https://en.wikipedia.org/wiki/World_Geodetic_System
   https://en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS_84 *)
let wgs84 : t =
  { equatorial_r = { radius = 6378137.0 }; recip_f = 298.257223563 }

(* As used by the National Geodetic Survey tool inverse when selecting the
   ellipsoid 1) GRS80 / WGS84 (NAD83) SEE:
   https://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/ *)
let nad83 : t = { wgs84 with recip_f = 298.25722210088 }

(* The Bessel ellipsoid from Vincenty 1975. Note that the flattening from
   Wikipedia for the Bessel ellipsoid is 299.1528153513233 not 299.1528128. SEE:
   https://en.wikipedia.org/wiki/Bessel_ellipsoid *)
let bessel : t =
  { equatorial_r = { radius = 6377397.155 }; recip_f = 299.1528128 }

(* The International ellipsoid 1924 also known as the Hayford ellipsoid from
   Vincenty 1975. SEE: https://en.wikipedia.org/wiki/Hayford_ellipsoid *)
let hayford : t = { equatorial_r = { radius = 6378388.0 }; recip_f = 297.0 }

(* Clarke's 1866 ellipsoid approximated in metres. "Clarke actually defined his
   1866 spheroid as a = 20,926,062 British feet, b = 20,855,121 British feet"
   SEE: https://en.wikipedia.org/wiki/North_American_Datum *)
let clarke : t =
  { equatorial_r = { radius = 6378206.4 }; recip_f = 294.978698214 }

(* The ellipsoid used in Evaluation Direct and Inverse Geodetic Algorithms, by
   Paul Delorme, Bedford Institute of Oceanography, Dartmouth, Nova Scotia,
   Canada, 1978. *)
let bedford_clarke : t = { clarke with recip_f = 294.9786986 }
