type t = { radius : Convert.meter }

let make (radius : Convert.meter) : t = { radius }
let to_string (r : t) : string = Printf.sprintf "%gm" r.radius
