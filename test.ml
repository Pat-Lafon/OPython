open OUnit2
open Parser
open Evaluate
open State

let make_line_test
    (name : string)
    (line : string)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (evaluate (parse_line line) []))

(**** TESTING *****)

let line_tests = [
  make_line_test "Plus" "x=4+5" ["x",Int(9)]
]

let suite =
  "test suite for A2"  >::: List.flatten [
  ]

let _ = run_test_tt_main suite
