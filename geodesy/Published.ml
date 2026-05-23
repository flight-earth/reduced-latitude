module Vincenty1975 = struct
  module E = Earth.Ellipsoid
  module P = Problems
  module PtP = PointToPoint.Vincenty
  module D = Units.DMS

  let ellipsoids : E.t list =
    [ E.bessel; E.hayford; E.hayford; E.hayford; E.hayford ]

  let distances : float list =
    [ 14110526.170; 4085966.703; 8084823.839; 19960000.000; 19780006.558 ]

  let x_azimuths : D.dms list =
    [
      { deg = 96; min = 36; sec = 8.79960 };
      { deg = 95; min = 27; sec = 59.63089 };
      { deg = 15; min = 44; sec = 23.74850 };
      { deg = 89; min = 0; sec = 0.0 };
      { deg = 4; min = 59; sec = 59.99995 };
    ]

  let y_azimuths : D.dms list =
    [
      { deg = 137; min = 52; sec = 22.01454 };
      { deg = 118; min = 5; sec = 58.96161 };
      { deg = 144; min = 55; sec = 39.92147 };
      { deg = 91; min = 0; sec = 6.11733 };
      { deg = 174; min = 59; sec = 59.88481 };
    ]

  let inverse_problem_data : ((D.dms * D.dms) * (D.dms * D.dms)) list =
    [
      ( ({ deg = 55; min = 45; sec = 0.0 }, { deg = 0; min = 0; sec = 0.0 }),
        ({ deg = -33; min = 26; sec = 0.0 }, { deg = 108; min = 13; sec = 0.0 })
      );
      ( ({ deg = 37; min = 19; sec = 54.95367 }, { deg = 0; min = 0; sec = 0.0 }),
        ( { deg = 26; min = 7; sec = 42.83946 },
          { deg = 41; min = 28; sec = 35.50729 } ) );
      ( ({ deg = 35; min = 16; sec = 11.24862 }, { deg = 0; min = 0; sec = 0.0 }),
        ( { deg = 67; min = 22; sec = 14.77638 },
          { deg = 137; min = 47; sec = 28.31435 } ) );
      ( ({ deg = 1; min = 0; sec = 0.0 }, { deg = 0; min = 0; sec = 0.0 }),
        ( { deg = 0; min = -59; sec = 53.83076 },
          { deg = 179; min = 17; sec = 48.02997 } ) );
      ( ({ deg = 1; min = 0; sec = 0.0 }, { deg = 0; min = 0; sec = 0.0 }),
        ( { deg = 1; min = 1; sec = 15.18952 },
          { deg = 179; min = 46; sec = 17.84244 } ) );
    ]

  let to_inverse_problem (x : D.dms * D.dms) (y : D.dms * D.dms) :
      P.inverse_problem =
    let x_lat, x_lng = x in
    let y_lat, y_lng = y in
    {
      P.x = { Latlng.LatLng.lat = D.to_rad x_lat; lng = D.to_rad x_lng };
      y = { Latlng.LatLng.lat = D.to_rad y_lat; lng = D.to_rad y_lng };
    }

  let inverse_problems : P.inverse_problem list =
    List.map (fun (x, y) -> to_inverse_problem x y) inverse_problem_data

  let inverse_solutions : P.inverse_solution list =
    List.map2
      (fun d (x, y) ->
        {
          P.s = { P.dist = d };
          az1 = { P.az = (D.to_rad x).rad };
          az2 = Some { P.az = (D.to_rad y).rad };
        })
      distances
      (List.combine x_azimuths y_azimuths)

  let direct_pairs : (P.inverse_problem * P.inverse_solution) list =
    List.combine inverse_problems inverse_solutions

  let direct_problems : P.inverse_problem list = List.map fst direct_pairs
  let direct_solutions : P.inverse_solution list = List.map snd direct_pairs

  type test_tolerance = P.dist

  let tolerance : P.dist = { P.dist = 0.008 }

  let indirect_distance_tolerances : test_tolerance list =
    List.map
      (fun x -> { P.dist = x })
      [ 0.000404; 0.000387; 0.000703; 0.000197; 0.000787 ]

  let az_tolerance : D.dms = { D.deg = 0; min = 0; sec = 0.016667 }

  type diff_dms = D.dms -> D.dms -> D.dms
  type az_tolerance_t = D.dms
  type span_lat_lng = Latlng.LatLng.t -> Latlng.LatLng.t -> P.dist
  type azimuth_fwd = Latlng.LatLng.t -> Latlng.LatLng.t -> Units.DMS.rad option
  type azimuth_bwd = Latlng.LatLng.t -> Latlng.LatLng.t -> Units.DMS.rad option

  let describe_inverse_distance x y s_expected tol =
    Latlng.LatLng.to_string x ^ " to " ^ Latlng.LatLng.to_string y ^ " = "
    ^ P.string_of_dist s_expected
    ^ " ± " ^ P.string_of_dist tol

  let describe_azimuth_fwd x y az_actual az_expected tol =
    let show_rad_opt = function
      | None -> "None"
      | Some r -> D.display_dms (D.normalize_dms (D.from_deg (D.from_rad r)))
    in
    Latlng.LatLng.to_string x ^ " to " ^ Latlng.LatLng.to_string y ^ " -> "
    ^ D.display_dms (D.from_deg (D.from_rad { D.rad = az_expected.P.az }))
    ^ " ± " ^ D.display_dms tol ^ " (" ^ show_rad_opt az_actual ^ ")"

  let describe_azimuth_rev x y az_actual az_expected tol =
    let show_rad_opt = function
      | None -> "None"
      | Some r -> D.display_dms (D.normalize_dms (D.from_deg (D.from_rad r)))
    in
    Latlng.LatLng.to_string x ^ " to " ^ Latlng.LatLng.to_string y ^ " <- "
    ^ show_rad_opt az_expected ^ " ± " ^ D.display_dms tol ^ " ("
    ^ show_rad_opt az_actual ^ ")"

  let diff x y = Float.abs (x -. y)
  let az_to_dms (az : P.az) : D.dms = D.from_deg (D.from_rad { D.rad = az.az })
  let rad_to_dms (r : Units.DMS.rad) : D.dms = D.from_deg (D.from_rad r)

  let inverse_checks (diff_az_fwd : diff_dms) (diff_az_rev : diff_dms)
      (dist_tols : test_tolerance list) (az_tol : az_tolerance_t)
      (spans : span_lat_lng list) (az_fwds : azimuth_fwd list)
      (az_revs : azimuth_bwd list) (solns : P.inverse_solution list)
      (probs : P.inverse_problem list) : string list =
    let rec loop dist_tols spans az_fwds az_revs solns probs acc =
      match (dist_tols, spans, az_fwds, az_revs, solns, probs) with
      | ( dist_tol :: dt,
          span :: sp,
          az_fwd :: af,
          az_rev :: ar,
          soln :: ss,
          prob :: pp ) ->
          let s' = span prob.P.x prob.P.y in
          let az1' = az_fwd prob.P.x prob.P.y in
          let az2' = az_rev prob.P.x prob.P.y in
          let fails = ref acc in
          let actual_dist = diff s'.P.dist soln.P.s.P.dist in
          if actual_dist > dist_tol.P.dist then
            fails :=
              (describe_inverse_distance prob.P.x prob.P.y soln.P.s tolerance
              ^ "\nexpected: "
              ^ string_of_float actual_dist
              ^ " <= "
              ^ string_of_float dist_tol.P.dist)
              :: !fails;
          (match az1' with
          | None -> ()
          | Some r ->
              let actual = diff_az_fwd (az_to_dms soln.P.az1) (rad_to_dms r) in
              if (D.to_deg actual).deg > (D.to_deg az_tol).deg then
                fails :=
                  describe_azimuth_fwd prob.P.x prob.P.y az1' soln.P.az1 az_tol
                  :: !fails);
          (match (az2', soln.P.az2) with
          | Some r, Some rev ->
              let actual = diff_az_rev (rad_to_dms r) (az_to_dms rev) in
              if (D.to_deg actual).deg > (D.to_deg az_tol).deg then
                fails :=
                  describe_azimuth_rev prob.P.x prob.P.y az2'
                    (Some { D.rad = rev.P.az })
                    az_tol
                  :: !fails
          | _ -> ());
          loop dt sp af ar ss pp !fails
      | _ -> List.rev acc
    in
    loop dist_tols spans az_fwds az_revs solns probs []

  let vincenty_units () : string list =
    let diff_az_fwd : diff_dms = D.abs_diff_dms in
    let diff_az_rev : diff_dms = D.abs_diff_dms_180 in
    let span_lat_lng x y =
      match PtP.distance E.bessel x y with
      | Ok d -> d
      | Error _ -> { P.dist = 0.0 }
    in
    inverse_checks diff_az_fwd diff_az_rev indirect_distance_tolerances
      az_tolerance
      (List.init 5 (fun _ -> span_lat_lng))
      (List.init 5 (fun _ -> fun _ _ -> None))
      (List.init 5 (fun _ -> fun _ _ -> None))
      inverse_solutions inverse_problems
end
