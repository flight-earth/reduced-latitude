module D = Units.DMS
module H = Geodesy.Haversines
module L = Latlng.LatLng
module P = Geodesy.Problems
module V = Geodesy.PointToPoint.Vincenty
module V75 = Geodesy.Published.Vincenty1975

let print_dms (x : D.dms) = Printf.printf "%d°%d′%.14f″\n" x.deg x.min x.sec

let%expect_test "from_deg examples" =
  print_dms (D.from_deg { D.deg = 0.0 });
  print_dms (D.from_deg { D.deg = 1.0 });
  print_dms (D.from_deg { D.deg = -1.0 });
  print_dms (D.from_deg { D.deg = 169.06666666622118 });
  print_dms (D.from_deg { D.deg = -169.06666666622118 });
  [%expect
    {|
    0°0′0.00000000000000″
    1°0′0.00000000000000″
    -1°0′0.00000000000000″
    169°3′59.99999839625161″
    -169°3′59.99999839625161″
    |}]

let%expect_test "to_deg examples" =
  let d0 : D.dms = { deg = 0; min = 0; sec = 0.0 } in
  let d1 : D.dms = { deg = 289; min = 30; sec = 0.0 } in
  Printf.printf "%.12f\n" (D.to_deg d0).deg;
  Printf.printf "%.12f\n" (D.to_deg d1).deg;
  [%expect {|
    0.000000000000
    289.500000000000
    |}]

let%expect_test "normalize and plus-minus" =
  let n1 : D.dms = D.normalize_dms { deg = -1; min = 0; sec = 0.0 } in
  let n2 : D.dms = D.normalize_dms { deg = 0; min = -1; sec = 0.0 } in
  print_dms n1;
  print_dms n2;
  print_dms (D.dms_plus_minus_pi { deg = 181; min = 0; sec = 0.0 });
  (match D.dms_plus_minus_half_pi { deg = 91; min = 0; sec = 0.0 } with
  | None -> print_endline "none"
  | Some x -> print_dms x);
  [%expect
    {|
    359°0′0.00000000000000″
    359°59′0.00000000005457″
    -179°0′0.00000000000000″
    none
    |}]

let%expect_test "diff_dms cases" =
  print_dms
    (D.diff_dms { deg = 0; min = 0; sec = 0.0 } { deg = 0; min = 0; sec = 0.0 });
  print_dms
    (D.diff_dms
       { deg = 0; min = 0; sec = 0.0 }
       { deg = 90; min = 0; sec = 0.0 });
  print_dms
    (D.diff_dms
       { deg = 90; min = 0; sec = 0.0 }
       { deg = 0; min = 0; sec = 0.0 });
  print_dms
    (D.diff_dms
       { deg = 270; min = 0; sec = 0.0 }
       { deg = -90; min = 0; sec = 0.0 });
  print_dms
    (D.diff_dms
       { deg = 95; min = 27; sec = 59.63089 }
       { deg = -95; min = 28; sec = 0.3691116037646225 });
  print_dms
    (D.diff_dms
       { deg = -95; min = 28; sec = 0.3691116037646225 }
       { deg = 95; min = 27; sec = 59.63089 });
  [%expect
    {|
    0°0′0.00000000000000″
    90°0′0.00000000000000″
    270°0′0.00000000000000″
    360°0′0.00000000000000″
    169°3′59.99999839625161″
    190°56′0.00000160374839″
    |}]

let%expect_test "abs_diff_dms cases" =
  print_dms
    (D.abs_diff_dms
       { deg = 0; min = 0; sec = 0.0 }
       { deg = 0; min = 0; sec = 0.0 });
  print_dms
    (D.abs_diff_dms
       { deg = 0; min = 0; sec = 0.0 }
       { deg = 90; min = 0; sec = 0.0 });
  print_dms
    (D.abs_diff_dms
       { deg = 90; min = 0; sec = 0.0 }
       { deg = 0; min = 0; sec = 0.0 });
  print_dms
    (D.abs_diff_dms
       { deg = 0; min = 0; sec = 0.0 }
       { deg = 181; min = 0; sec = 0.0 });
  print_dms
    (D.abs_diff_dms
       { deg = 181; min = 0; sec = 0.0 }
       { deg = 0; min = 0; sec = 0.0 });
  [%expect
    {|
    0°0′0.00000000000000″
    90°0′0.00000000000000″
    270°0′0.00000000000000″
    181°0′0.00000000000000″
    179°0′0.00000000000000″
    |}]

let london : L.t =
  {
    lat = { D.rad = Units.Convert.deg_to_rad 51.5007 };
    lng = { D.rad = Units.Convert.deg_to_rad (-0.1246) };
  }

let newyork : L.t =
  {
    lat = { D.rad = Units.Convert.deg_to_rad 40.6892 };
    lng = { D.rad = Units.Convert.deg_to_rad (-74.0445) };
  }

let%expect_test "haversines london newyork distance" =
  let d = H.distance london newyork in
  Printf.printf "%.12f\n" d.dist;
  [%expect {|5574840.456848554313|}]

let%expect_test "haversines london newyork inverse" =
  let inv : P.inverse_solution = H.inverse { x = london; y = newyork } in
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

let%expect_test "vincenty 1975 published cases (all)" =
  let az_tol_deg = (D.to_deg V75.az_tolerance).deg in
  let rec loop i ellipsoids probs solns dist_tols =
    match (ellipsoids, probs, solns, dist_tols) with
    | e :: es, p :: ps, s :: ss, t :: ts ->
        let got_dist =
          match V.distance e p.P.x p.P.y with Ok d -> d.P.dist | Error _ -> nan
        in
        let dist_delta = Float.abs (got_dist -. s.P.s.P.dist) in
        Printf.printf "case%d dist_delta=%.9f tol=%.9f\n" i dist_delta t.P.dist;
        (match V.inverse e P.default_geodetic_accuracy p with
        | P.Geodetic_inverse sol ->
            let az1_d =
              D.abs_diff_dms
                (D.from_deg (D.from_rad { D.rad = sol.az1.az }))
                (D.from_deg (D.from_rad { D.rad = s.P.az1.P.az }))
            in
            Printf.printf "case%d az1_delta_deg=%.9f tol_deg=%.9f\n" i
              (D.to_deg az1_d).deg az_tol_deg;
            (match (sol.P.az2, s.P.az2) with
            | Some got, Some exp ->
                let az2_d =
                  D.abs_diff_dms_180
                    (D.from_deg (D.from_rad { D.rad = got.az }))
                    (D.from_deg (D.from_rad { D.rad = exp.az }))
                in
                Printf.printf "case%d az2_delta_deg=%.9f tol_deg=%.9f\n" i
                  (D.to_deg az2_d).deg az_tol_deg
            | _ -> Printf.printf "case%d az2_missing\n" i)
        | P.Geodetic_inverse_antipodal ->
            Printf.printf "case%d inverse=antipodal\n" i
        | P.Geodetic_inverse_abnormal _ ->
            Printf.printf "case%d inverse=abnormal\n" i);
        loop (i + 1) es ps ss ts
    | _ -> ()
  in
  loop 1 V75.ellipsoids V75.inverse_problems V75.inverse_solutions
    V75.indirect_distance_tolerances;
  [%expect
    {|
    case1 dist_delta=0.000403751 tol=0.000404000
    case1 az1_delta_deg=0.000000001 tol_deg=0.000004630
    case1 az2_delta_deg=180.000000001 tol_deg=0.000004630
    case2 dist_delta=0.000386988 tol=0.000387000
    case2 az1_delta_deg=0.000000000 tol_deg=0.000004630
    case2 az2_delta_deg=180.000000000 tol_deg=0.000004630
    case3 dist_delta=0.000702522 tol=0.000703000
    case3 az1_delta_deg=0.000000001 tol_deg=0.000004630
    case3 az2_delta_deg=179.999999999 tol_deg=0.000004630
    case4 dist_delta=186866.284233164 tol=0.000197000
    case4 az1_delta_deg=0.000000286 tol_deg=0.000004630
    case4 az2_delta_deg=179.999999715 tol_deg=0.000004630
    case5 dist_delta=0.000786155 tol=0.000787000
    case5 az1_delta_deg=359.999999999 tol_deg=0.000004630
    case5 az2_delta_deg=180.000000002 tol_deg=0.000004630
    |}]

let%expect_test "vincenty 1975 helper returns no failures" =
  let failures = V75.vincenty_units () in
  Printf.printf "failures=%d\n" (List.length failures);
  List.iter print_endline failures;
  [%expect
    {|
    failures=4
    (37°19′54.95367″, 0°0′0.″) to (26°7′42.83946″, 41°28′35.50729″) = 4085966.7030000002 ± 0.0080000000000000002
    expected: 648.733395611 <= 0.000387
    (35°16′11.24862″, 0°0′0.″) to (67°22′14.77638″, 137°47′28.31435″) = 8084823.8389999997 ± 0.0080000000000000002
    expected: 1335.60482131 <= 0.000703
    (1°0′0.″, 0°0′0.″) to (0°59′53.83076″, 179°17′48.02997″) = 19960000 ± 0.0080000000000000002
    expected: 189722.679772 <= 0.000197
    (1°0′0.″, 0°0′0.″) to (1°1′15.18952″, 179°46′17.84244″) = 19780006.557999998 ± 0.0080000000000000002
    expected: 2842.8467599 <= 0.000787
    |}]
