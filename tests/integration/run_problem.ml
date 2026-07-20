open Alcotest
open Prover_lib
open Prover

let here = Sys.getcwd ()

let path name =
  let candidates =
    [
      Filename.concat here name;
      Filename.concat here ("tests/integration/" ^ name);
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> Filename.concat here name

let config : Prover.config =
  {
    (* default_config with *)
    time_limit_s = Some 2.0;
    max_generated_clauses = Some 20_000;
    print_derivation = false;
    inference_mode = Resolution.Ordered_with_fallback;
    tptp_dir = None;
    use_sos = true;
    portfolio_mode = Prover.Legacy_then_modern;
  }


let expect_unsat file () =
  match run_file ~config:config (path file) with
  | { status = Unsatisfiable; empty_clause = Some _; _ }
  | { status = Theorem; empty_clause = Some _; _ } ->
      ()
  | { status; _ } ->
      fail
        (Printf.sprintf
           "expected UNSAT/THEOREM with empty clause for %s, got %s"
           file
           (string_of_szs_status status))

let expect_not_refuted file () =
  match run_file ~config:config (path file) with
  | { status = Unsatisfiable; _ }
  | { status = Theorem; _ } ->
      fail ("expected non-refutation for " ^ file)
  | { status = InputError; _ } ->
      fail ("unexpected InputError for " ^ file)
  | { status = GaveUp; _ }
  | { status = Timeout; _ }
  | { status = ResourceOut; _ }
  | { status = Satisfiable; _ }
  | { status = CounterSatisfiable; _ } ->
      ()

let expect_input_rejected file () =
  match run_file ~config:config (path file) with
  | exception Tptp_frontend.Error _ -> ()
  | { status = InputError; _ } -> ()
  | { status; _ } ->
      fail
        (Printf.sprintf
           "expected InputError for %s, got %s"
           file (string_of_szs_status status))

let with_env name value f =
  let previous = Sys.getenv_opt name in
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | None -> Unix.putenv name "0"
      | Some v -> Unix.putenv name v)
    (fun () ->
      Unix.putenv name value;
      f ())

let expect_proofless_prefilter_disabled file () =
  with_env "VIP_GROUND_SAT_PREFILTER" "1" (fun () ->
      match run_file ~config:config (path file) with
      | { status = (Unsatisfiable | Theorem); empty_clause = Some root; _ } ->
          check bool
            "normal clause proof root"
            true
            (root.rule <> "ground_sat_prefilter")
      | { status; _ } ->
          fail
            (Printf.sprintf
               "expected ordinary refutation for %s, got %s"
               file
               (string_of_szs_status status)))

let expect_set_bridge_unsat file () =
  with_env "VIP_SET_BRIDGE_SELECTION" "1" (fun () ->
      let config =
        {
          config with
          portfolio_mode = Prover.Legacy_only;
          max_generated_clauses = Some 100_000;
        }
      in
      match run_file ~config (path file) with
      | { status = Unsatisfiable; empty_clause = Some _; _ }
      | { status = Theorem; empty_clause = Some _; _ } ->
          ()
      | { status; _ } ->
          fail
            (Printf.sprintf
               "expected set-bridge proof for %s, got %s"
               file
               (string_of_szs_status status)))

let check_stats_present file () =
  let result = run_file ~config:config (path file) in
  match result.resolution_stats with
  | None ->
      fail ("expected stats for " ^ file)
  | Some s ->
      check bool "generated >= 0" true (s.generated_clauses >= 0);
      check bool "processed >= 0" true (s.processed_clauses >= 0);
      check bool "time >= 0" true (s.wall_clock_s >= 0.0)

let check_derivation_present file () =
  let result = run_file ~config:config (path file) in
  match result.status with
  | Unsatisfiable | Theorem ->
      check bool "derivation non-empty" true (List.length result.derivation > 0);
      check bool "empty clause present" true (result.empty_clause <> None)
  | _ ->
      fail
        (Printf.sprintf
           "expected proof-producing result for %s, got %s"
           file
           (string_of_szs_status result.status))

let contains_substring haystack needle =
  let h_len = String.length haystack in
  let n_len = String.length needle in
  let rec loop i =
    if i + n_len > h_len then false
    else if String.sub haystack i n_len = needle then true
    else loop (i + 1)
  in
  n_len = 0 || loop 0

let check_tstp_derivation file () =
  let result = run_file ~config:config (path file) in
  match result.status with
  | Unsatisfiable | Theorem ->
      let proof = Resolution.tstp_derivation result.derivation in
      check bool "tstp start marker" true
        (contains_substring proof "% SZS output start CNFRefutation");
      check bool "contains cnf" true
        (contains_substring proof "cnf(");
      check bool "contains inference" true
        (contains_substring proof "inference(")
  | _ ->
      fail
        (Printf.sprintf
           "expected proof-producing result for %s, got %s"
           file
           (string_of_szs_status result.status))

let check_legacy_demodulation_provenance file () =
  let legacy_config = { config with portfolio_mode = Prover.Legacy_only } in
  let result = run_file ~config:legacy_config (path file) in
  match result.status, result.tstp_prelude with
  | Theorem, Some prelude ->
      let proof =
        Resolution.tstp_derivation
          ~prelude
          ~skip_initial:true
          result.derivation
      in
      check bool "records demodulation" true
        (contains_substring proof "inference(demodulation");
      check bool "keeps raw preprocessing parent" true
        (contains_substring proof "vip_m");
      check bool "does not invent axiom leaves" false
        (contains_substring proof "cnf(vip_1,axiom")
  | status, _ ->
      fail
        (Printf.sprintf
           "expected legacy theorem with TSTP prelude for %s, got %s"
           file
           (string_of_szs_status status))

let () =
  run "integration"
    [
      ( "problems",
        [
          test_case "cnf unsat" `Quick (expect_unsat "unsat_01.p");
          test_case "fof unsat" `Quick (expect_unsat "fof_unsat_01.p");
          test_case "sat or unknown" `Quick (expect_not_refuted "sat_01.p");
          test_case "finite equality model stays satisfiable" `Quick
            (expect_not_refuted "sat_equality_finite_model.p");
          test_case "subsumption variable capture stays satisfiable" `Quick
            (expect_not_refuted "sat_subsumption_variable_capture.p");
          test_case "subsumption resolution keeps one substitution" `Quick
            (expect_not_refuted "sat_subsumption_resolution_substitution.p");
          test_case "implicit universal parameterizes Skolem witness" `Quick
            (expect_not_refuted "sat_free_variable_skolem_dependency.p");
          test_case "multiple conjectures are rejected" `Quick
            (expect_input_rejected "unsupported_multiple_conjectures.p");
          test_case "proofless ground prefilter is disabled" `Quick
            (expect_proofless_prefilter_disabled "unsat_01.p");
          test_case "set bridge SEU140 shape" `Quick (expect_set_bridge_unsat "set_bridge_seu140_shape.p");
          test_case "stats present" `Quick (check_stats_present "unsat_01.p");
          test_case "derivation present" `Quick (check_derivation_present "unsat_01.p");
          test_case "tstp derivation present" `Quick (check_tstp_derivation "unsat_01.p");
          test_case "legacy demodulation proof provenance" `Quick
            (check_legacy_demodulation_provenance "legacy_demodulation_proof.p");
        ] );
    ]
