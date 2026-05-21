open Units.Convert

let () =
  let x = 42.0 in
  let y = deg_to_rad x in
  Printf.sprintf "deg_to_rad %f = %f" x y |> print_endline
