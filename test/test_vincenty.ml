module D = Units.DMS
module P = Geodesy.Problems
module V = Geodesy.PointToPoint.Vincenty
module V75 = Geodesy.Published.Vincenty1975

let%expect_test "vincenty 1975 published cases (all)" =
  let az_tol_deg = (D.to_deg V75.az_tolerance).deg in
  let rec loop i ellipsoids probs solns dist_tols =
    match (ellipsoids, probs, solns, dist_tols) with
    | e :: es, p :: ps, s :: ss, t :: ts ->
        let got_dist =
          match V.distance e p.P.x p.P.y with
          | Ok d -> d.P.dist
          | Error _ -> nan
        in
        let dist_delta = Float.abs (got_dist -. s.P.s.P.dist) in
        Printf.printf "case%d dist_delta=%.9f tol=%.9f\n" i dist_delta t.P.dist;
        (match V.inverse e P.default_geodetic_accuracy p with
        | P.Geodetic_inverse sol -> (
            let az1_d =
              D.abs_diff_dms
                (D.from_deg (D.from_rad { D.rad = sol.az1.az }))
                (D.from_deg (D.from_rad { D.rad = s.P.az1.P.az }))
            in
            Printf.printf "case%d az1_delta_deg=%.9f tol_deg=%.9f\n" i
              (D.to_deg az1_d).deg az_tol_deg;
            match (sol.P.az2, s.P.az2) with
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
