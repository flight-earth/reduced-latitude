type t = { lat : Units.DMS.rad; lng : Units.DMS.rad }

let to_string (x : t) : string =
  let lat_dms = Units.DMS.from_deg (Units.DMS.from_rad x.lat) in
  let lng_dms = Units.DMS.from_deg (Units.DMS.from_rad x.lng) in
  Printf.sprintf "(%s, %s)"
    (Units.DMS.display_dms lat_dms)
    (Units.DMS.display_dms lng_dms)
