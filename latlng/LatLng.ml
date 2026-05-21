open Units.DMS

type t = { lat : rad; lng : rad }

let to_string (x : t) : string =
  let lat_dms = from_deg (from_rad x.lat) in
  let lng_dms = from_deg (from_rad x.lng) in
  Printf.sprintf "(%s, %s)" (display_dms lat_dms) (display_dms lng_dms)
