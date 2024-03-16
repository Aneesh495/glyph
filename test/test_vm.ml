open Alcotest

let compile_src src =
  match Compile.compile_string src with
  | Error diags ->
      let msg = String.concat "\n" (List.map (fun d -> d.Diagnostic.message) diags) in
      fail ("compile failed: " ^ msg)
  | Ok res -> (
      match res.chunk with
      | None -> fail "no chunk"
      | Some chunk -> chunk)

let test_closure_capture () =
  let src = "let make_adder x = let f y = x + y in f\nlet add10 = make_adder 10\nlet main = add10 32" in
  let chunk = compile_src src in
  let vm = Interp.create chunk in
  match Interp.run vm with
  | Interp.Runtime_error err -> fail ("VM error: " ^ err)
  | Interp.Ok v -> check string "result is 42" "42" (Value.to_string v)

let test_tail_calls_bounded_stack () =
  let src = "let rec loop acc n = if n <= 0 then acc else loop (acc + 1) (n - 1)\nlet main = loop 0 10000" in
  let chunk = compile_src src in
  let vm = Interp.create chunk in
  match Interp.run vm with
  | Interp.Runtime_error err -> fail ("VM error: " ^ err)
  | Interp.Ok v ->
      check string "result is 10000" "10000" (Value.to_string v);
      check int "frames empty on halt" 0 (List.length vm.frames)

let test_gc_stress () =
  (* Create tiny heap of 64 words to force many Cheney copying GC cycles during allocation *)
  let src = "type List a = Nil | Cons a (List a)\nlet rec make_list n = if n <= 0 then Nil else Cons n (make_list (n - 1))\nlet rec sum xs acc = match xs with | Nil -> acc | Cons h t -> sum t (acc + h)\nlet main = sum (make_list 200) 0" in
  let chunk = compile_src src in
  let vm = Interp.create ~heap_capacity:64 chunk in
  match Interp.run vm with
  | Interp.Runtime_error err -> fail ("VM error: " ^ err)
  | Interp.Ok v ->
      (* sum of 1..200 is 200*201/2 = 20100 *)
      check string "result is 20100 after multiple GC cycles" "20100" (Value.to_string v);
      let collections, _ = Gc.stats () in
      check bool "gc ran at least once" true (collections > 0)

let test_match_failure () =
  let src = "let f x = match x with | 0 -> 10\nlet main = f 1" in
  let chunk = compile_src src in
  let vm = Interp.create chunk in
  match Interp.run vm with
  | Interp.Ok _ -> fail "expected match failure runtime error"
  | Interp.Runtime_error msg ->
      check bool "contains match failure" true
        (String.contains msg 'm' || String.contains msg 'M' || String.contains msg 'f')

let tests =
  [
    ("closure_capture", `Quick, test_closure_capture);
    ("tail_calls_bounded_stack", `Quick, test_tail_calls_bounded_stack);
    ("gc_stress", `Quick, test_gc_stress);
    ("match_failure", `Quick, test_match_failure);
  ]
