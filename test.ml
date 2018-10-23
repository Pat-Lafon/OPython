open OUnit2
open Parser

let t = line_type "if 5:"
let () = match t with
| If -> print_endline "If"
| _ -> print_endline "Not if"

let suite =
  "test suite for A2"  >::: List.flatten [
  ]

let _ = run_test_tt_main suite
