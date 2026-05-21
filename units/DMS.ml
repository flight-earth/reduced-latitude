module DMS = struct
  type deg = { deg : Convert.Convert.deg }
  type rad = { rad : Convert.Convert.rad }
  type dms = { deg : int; min : int; sec : float }

  let from_rad (r : rad) : deg = { deg = Convert.Convert.rad_to_deg r.rad }

  let signum (x : float) : Convert.Convert.ordering =
    if x < 0.0 then Convert.Convert.Lt
    else if x > 0.0 then Convert.Convert.Gt
    else Convert.Convert.Eq

  let sign_dms (x : dms) : Convert.Convert.ordering * dms =
    let sign =
      if x.deg < 0 || x.min < 0 || x.sec < 0.0 then Convert.Convert.Lt
      else if x.deg = 0 && x.min = 0 && x.sec = 0.0 then Convert.Convert.Eq
      else Convert.Convert.Gt
    in
    (sign, { deg = abs x.deg; min = abs x.min; sec = Float.abs x.sec })

  let div' (n : float) (d : float) : float =
    let n' = Float.abs n in
    let d' = Float.abs d in
    let q = Float.floor (n' /. d') in
    if signum n = signum d' then q else -.q

  let div (n : float) (d : int) : int =
    let d' = Float.of_int d in
    let q = int_of_float (Float.abs (div' n d')) in
    if signum n = signum d' then q else -q

  let mod_ (n : float) (d : int) : float = n -. Float.of_int (div n d * d)

  let div_mod (n : float) (d : int) : int * float =
    let q = div n d in
    (q, n -. Float.of_int (q * d))

  let from_deg (alpha : deg) : dms =
    let d_abs = Float.abs alpha.deg in
    let d_frac = d_abs -. Float.floor d_abs in
    let mm, m_frac = div_mod (d_frac *. 60.0) 1 in
    let dd = int_of_float (Float.floor d_abs) in
    {
      deg =
        (match signum alpha.deg with
        | Convert.Convert.Eq -> 0
        | Convert.Convert.Lt -> -dd
        | Convert.Convert.Gt -> dd);
      min = mm;
      sec = m_frac *. 60.0;
    }

  let to_deg (x : dms) : deg =
    let pn, x' = sign_dms x in
    let d = Float.of_int x'.deg in
    let m = Float.of_int x'.min in
    let dd = ((d *. 3600.0) +. (m *. 60.0) +. x'.sec) /. 3600.0 in
    {
      deg =
        (match pn with
        | Convert.Convert.Eq -> 0.0
        | Convert.Convert.Gt -> dd
        | Convert.Convert.Lt -> -.dd);
    }

  let to_rad (x : dms) : rad =
    let d = to_deg x in
    { rad = Convert.Convert.deg_to_rad d.deg }

  let normalize_deg (x : deg) : deg =
    let y = mod_ (Float.abs x.deg) 360 in
    if x.deg < 0.0 then { deg = 360.0 -. y } else { deg = y }

  let normalize_dms (x : dms) : dms = from_deg (normalize_deg (to_deg x))

  let display_dms (x : dms) : string =
    string_of_int x.deg ^ "°" ^ string_of_int x.min ^ "′"
    ^ string_of_float x.sec ^ "″"

  let diff_dms (x : dms) (y : dms) : dms =
    from_deg (normalize_deg { deg = (to_deg y).deg -. (to_deg x).deg })

  let abs_diff_dms (y : dms) (x : dms) : dms =
    let d = diff_dms y x in
    if (to_deg d).deg > (to_deg { deg = 180; min = 0; sec = 0.0 }).deg then
      diff_dms { deg = 360; min = 0; sec = 0.0 } d
    else d

  let dms_plus_minus_pi (x : dms) : dms =
    let d = to_deg x in
    let a, b = div_mod d.deg 180 in
    from_deg
      {
        deg =
          (if b = 0.0 then
             if Convert.Convert.is_even (Int64.of_int (abs a)) then 0.0
             else (if a < 0 then -1.0 else 1.0) *. 180.0
           else if Convert.Convert.is_even (Int64.of_int (abs a)) then b
           else b -. 180.0);
      }

  let dms_plus_minus_half_pi (x : dms) : dms option =
    let y = to_deg (dms_plus_minus_pi x) in
    if y.deg < -90.0 || y.deg > 90.0 then None else Some (from_deg y)

  let rotate (rotation : dms) (x : dms) : dms =
    from_deg { deg = (to_deg x).deg +. (to_deg rotation).deg }

  let diff_dms_180 (y : dms) : dms -> dms =
    diff_dms (rotate { deg = 180; min = 0; sec = 0.0 } y)

  let abs_diff_dms_180 (y : dms) : dms -> dms =
    abs_diff_dms (rotate { deg = 180; min = 0; sec = 0.0 } y)

  let print_dms_expect (x : dms) =
    Printf.printf "%d°%d′%.14f″\n" x.deg x.min x.sec

  let print_deg_expect (x : deg) = Printf.printf "%.30f°\n" x.deg

  let%expect_test _ =
    let x = from_deg { deg = -169.06666666622118 } in
    print_dms_expect x;
    [%expect {|-169°3′59.99999839625161″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 1; min = 0; sec = 0.0 });
    [%expect {|1°0′0.00000000000000″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 0; min = 1; sec = 0.0 });
    [%expect {|0°1′0.00000000000000″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 0; min = 0; sec = 1.0 });
    [%expect {|0°0′1.00000000000000″|}]

  let%expect_test _ =
    print_deg_expect (normalize_deg { deg = 361.0 });
    [%expect {|1.000000000000000000000000000000°|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 361; min = 0; sec = 0.0 });
    [%expect {|1°0′0.00000000000000″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms (from_deg { deg = 361.0 }));
    [%expect {|1°0′0.00000000000000″|}]

  let%expect_test _ =
    print_deg_expect (normalize_deg { deg = -1.0 });
    [%expect {|359.000000000000000000000000000000°|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = -1; min = 0; sec = 0.0 });
    [%expect {|359°0′0.00000000000000″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms (from_deg { deg = -1.0 }));
    [%expect {|359°0′0.00000000000000″|}]

  let%expect_test _ =
    print_dms_expect (from_deg (normalize_deg { deg = -1.0 /. 60.0 }));
    [%expect {|359°59′0.00000000005457″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 0; min = -1; sec = 0.0 });
    [%expect {|359°59′0.00000000005457″|}]

  let%expect_test _ =
    print_dms_expect (from_deg (normalize_deg { deg = -1.0 /. 3600.0 }));
    [%expect {|359°59′58.99999999993270″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 0; min = 0; sec = -1.0 });
    [%expect {|359°59′58.99999999993270″|}]

  let%expect_test _ =
    print_dms_expect
      (from_deg (normalize_deg { deg = 360.0 -. (1.0 /. 3600.0) }));
    [%expect {|359°59′58.99999999993270″|}]

  let%expect_test _ =
    let d = to_deg { deg = 0; min = 0; sec = -1.0 } in
    print_dms_expect (from_deg (normalize_deg d));
    [%expect {|359°59′58.99999999993270″|}]

  let%expect_test _ =
    print_deg_expect { deg = -1.0 /. 3600.0 };
    [%expect {|-0.000277777777777777777536843962°|}]

  let%expect_test _ =
    print_deg_expect (to_deg { deg = 0; min = 0; sec = -1.0 });
    [%expect {|-0.000277777777777777777536843962°|}]

  let%expect_test _ =
    print_deg_expect (normalize_deg { deg = -1.0 /. 3600.0 });
    [%expect {|359.999722222222203527053352445364°|}]

  let%expect_test _ =
    print_deg_expect (normalize_deg { deg = 360.0 -. (1.0 /. 3600.0) });
    [%expect {|359.999722222222203527053352445364°|}]

  let%expect_test _ =
    let d = to_deg { deg = 0; min = 0; sec = 60.0 } in
    Printf.printf "%.30f\n" (d.deg *. 3600.0);
    [%expect {|60.000000000000000000000000000000|}]

  let%expect_test _ =
    let d = to_deg { deg = 0; min = 0; sec = 61.0 } in
    Printf.printf "%.30f\n" (d.deg *. 3600.0);
    [%expect {|61.000000000000007105427357601002|}]

  let%expect_test _ =
    let d = to_deg { deg = 0; min = 1; sec = 1.0 } in
    Printf.printf "%.30f\n" (d.deg *. 3600.0);
    [%expect {|61.000000000000007105427357601002|}]

  let%expect_test _ =
    let d = to_deg { deg = 0; min = 0; sec = 1.0 } in
    Printf.printf "%.30f\n" (d.deg *. 3600.0);
    [%expect {|1.000000000000000000000000000000|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 0; min = 0; sec = 60.0 });
    [%expect {|0°1′0.00000000000000″|}]

  let%expect_test _ =
    print_dms_expect (normalize_dms { deg = 0; min = 60; sec = 0.0 });
    [%expect {|1°0′0.00000000000000″|}]
end
