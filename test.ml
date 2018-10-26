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
  make_line_test "Plus" "x=4+5" ["x",Int(9)];
  make_line_test "Subtract" "y=4-5" ["y",Int(-1)];
  make_line_test "Div" "x=55/5" ["x",Float(11.)];
  make_line_test "FloorDiv" "x=55//5" ["x",Int(11)];
  make_line_test "Mult" "x=9*5" ["x",Int(45)];
  make_line_test "Modular" "x=53%5" ["x",Int(3)];
  make_line_test "Exp" "x=2**5" ["x",Int(32)];
  make_line_test "Div" "x=55//5" ["x",Int(11)];
  make_line_test "Equal" "x=(1==True)" ["x",Bool(true)];
  make_line_test "notEqual" "x=(4!=3)" ["x",Bool(false)];
  make_line_test "GreaterThan" "x=55>5" ["x",Bool(true)];
  make_line_test "LessThan" "x=55<5" ["x",Bool(false)];
  make_line_test "GreaterEqual" "x=5>=5" ["x",Bool(true)];
  make_line_test "LessEqual" "x=5<=5" ["x",Bool(true)];
  make_line_test "And" "x=True and True" ["x",Bool(true)];
  make_line_test "Or" "x=False or True" ["x",Bool(true)];
  make_line_test "Not" "x=(not False)" ["x",Bool(true)];
  make_line_test "LessThan" "x=~5" ["x",Int(-6)];
]

let suite =
  "test suite for A2"  >::: List.flatten [
  ]

let _ = run_test_tt_main suite
