open! Base

let%test_unit _ =
  List.iter Ppx_fields_conv.Selector.all ~f:(fun selector ->
    let sexp = Ppx_fields_conv.Selector.sexp_of_t selector in
    let round_trip = Ppx_fields_conv.Selector.t_of_sexp sexp in
    if not (Ppx_fields_conv.Selector.equal selector round_trip)
    then
      raise_s
        (Sexp.message
           "round-trip error"
           [ "selector", sexp
           ; "round_trip", Ppx_fields_conv.Selector.sexp_of_t round_trip
           ]))
;;
