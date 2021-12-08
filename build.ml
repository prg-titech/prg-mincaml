#!/usr/bin/env ocaml

open Printf
module F = Filename

let byte_flg = ref false

let s = sprintf
let cmd = Sys.command
let with_error_code f = if f () = 0 then () else raise Exit
let emit_x86 f = cmd @@ s "/usr/local/bin/min-caml -inline 0 %s" f
let emit_bytecode f = cmd @@ s "dune exec bac-caml -- %s" f

let compile_with_gcc f =
  cmd
  @@ s
       "gcc -m32 -g -O2 -Wall src/stub.c src/lib.c src/libmincaml.S %s -lm -o %s"
       (f ^ ".s")
       (f ^ ".exe")
;;

let usage = Sys.argv.(0) ^ " [-byte] file"

let () =
  let file = ref "" in
  Arg.parse
    [ ("-byte", Arg.Unit (fun _ -> byte_flg := true), "") ]
    (fun f -> file := f)
    usage;
  let name, ext = F.chop_extension !file, F.extension !file in
  if not (!byte_flg)
  then (
    with_error_code (fun () -> emit_x86 name);
    with_error_code (fun () -> compile_with_gcc name))
  else (
    match ext with
    | ".mcml" ->
      with_error_code (fun () -> emit_x86 !file);
      with_error_code (fun () -> compile_with_gcc name)
    | ".ml" -> with_error_code (fun _ -> emit_bytecode !file)
    | _ -> print_endline usage)
;;
