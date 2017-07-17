open Lamia_ast;;
open Lamia_heap;;
open Lamia_interpreter;;

let parse_and_run lexbuf =
  let block = Lamia_parser.parse_from_lexbuf lexbuf in
  let result, heap = Lamia_interpreter.evaluate block in
  begin
    match result with
    | Evaluated_successfully ->
      print_endline "Evaluation successful."
    | Evaluated_to_exception(Memory_variable(y)) ->
      Printf.printf "Raised exception: %s\n" y
    | Evaluation_error s ->
      Printf.printf "Evaluation error: %s\n" s
  end;
  print_endline "Current heap:";
  print_endline @@ Heap.to_string heap;
  print_endline "";
;;

let main () =
  let buf = Lexing.from_channel stdin in
  let rec loop () =
    parse_and_run buf;
    loop ()
  in
  loop ()
;;

main ();;
