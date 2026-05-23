open Units.Convert
open Units.DMS
module H = Geodesy.Haversines
open Latlng
open Geodesy.Problems

let london : LatLng.t =
  { lat = { rad = deg_to_rad 51.5007 }; lng = { rad = deg_to_rad (-0.1246) } }

let newyork : LatLng.t =
  { lat = { rad = deg_to_rad 40.6892 }; lng = { rad = deg_to_rad (-74.0445) } }

let%expect_test "haversines london newyork distance" =
  let d = H.distance london newyork in
  Printf.printf "%.12f\n" d.dist;
  [%expect {|5574840.456848554313|}]

let%expect_test "haversines london newyork inverse" =
  let inv : inverse_solution = H.inverse { x = london; y = newyork } in
  Printf.printf "s=%.12f\n" inv.s.dist;
  Printf.printf "az1=%.15f\n" inv.az1.az;
  (match inv.az2 with
  | None -> print_endline "az2=None"
  | Some az2 -> Printf.printf "az2=%.15f\n" az2.az);
  [%expect
    {|
    s=5574840.456848554313
    az1=-1.250757751225103
    az2=4.035112115037735
    |}]
