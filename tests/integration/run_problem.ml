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

let () =
  run "integration"
    [
      ( "problems",
        [
          test_case "cnf unsat" `Quick (expect_unsat "unsat_01.p");
          test_case "fof unsat" `Quick (expect_unsat "fof_unsat_01.p");
          test_case "sat or unknown" `Quick (expect_not_refuted "sat_01.p");
          test_case "stats present" `Quick (check_stats_present "unsat_01.p");
          test_case "derivation present" `Quick (check_derivation_present "unsat_01.p");
        ] );
    ]
