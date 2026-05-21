module D = Units.DMS.DMS

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
  [%expect
    {|
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
  print_dms (D.diff_dms { deg = 0; min = 0; sec = 0.0 } { deg = 0; min = 0; sec = 0.0 });
  print_dms (D.diff_dms { deg = 0; min = 0; sec = 0.0 } { deg = 90; min = 0; sec = 0.0 });
  print_dms (D.diff_dms { deg = 90; min = 0; sec = 0.0 } { deg = 0; min = 0; sec = 0.0 });
  print_dms (D.diff_dms { deg = 270; min = 0; sec = 0.0 } { deg = -90; min = 0; sec = 0.0 });
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
  print_dms (D.abs_diff_dms { deg = 0; min = 0; sec = 0.0 } { deg = 0; min = 0; sec = 0.0 });
  print_dms (D.abs_diff_dms { deg = 0; min = 0; sec = 0.0 } { deg = 90; min = 0; sec = 0.0 });
  print_dms (D.abs_diff_dms { deg = 90; min = 0; sec = 0.0 } { deg = 0; min = 0; sec = 0.0 });
  print_dms (D.abs_diff_dms { deg = 0; min = 0; sec = 0.0 } { deg = 181; min = 0; sec = 0.0 });
  print_dms (D.abs_diff_dms { deg = 181; min = 0; sec = 0.0 } { deg = 0; min = 0; sec = 0.0 });
  [%expect
    {|
    0°0′0.00000000000000″
    90°0′0.00000000000000″
    270°0′0.00000000000000″
    181°0′0.00000000000000″
    179°0′0.00000000000000″
    |}]
