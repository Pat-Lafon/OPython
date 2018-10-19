open OUnit2
open Parser

parse_assignment "abc"

let suite =
  "test suite for A2"  >::: List.flatten [
  ]

let _ = run_test_tt_main suite
