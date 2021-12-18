type bytecode =
  | CONST_INT
  | CONST_FLOAT
  | DUP
  | DUPN
  | POP
  | LT
  | GT
  | EQ
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | EXIT
  | JUMP
  | JUMP_IF
  | CALL
  | CALL_JIT
  | CALL_NORMAL
  | RET
  | NEWSTR
  | PRINT
  | Literal of int
  | Lref of string
  | Ldef of string
[@@deriving show]

let bytecodes =
  [| CONST_INT
   ; CONST_FLOAT
   ; DUP
   ; DUPN
   ; POP
   ; LT
   ; GT
   ; EQ
   ; ADD
   ; SUB
   ; MUL
   ; DIV
   ; MOD
   ; EXIT
   ; JUMP
   ; JUMP_IF
   ; CALL
   ; CALL_JIT
   ; CALL_NORMAL
   ; RET
   ; NEWSTR
   ; PRINT
  |]
;;

let pp_bytecode_counter = ref 0

let pp_pc () =
  print_int !pp_bytecode_counter;
  print_string "\t";
  incr pp_bytecode_counter
;;

let rec pp_bytecode ?(i = 0) insts =
  let rec pp_bytecode' ?(i = 0) insts =
    match insts with
    | [] -> ()
    | hd :: tl ->
      (match hd with
      | CONST_INT | DUP | JUMP | JUMP_IF | RET | CALL ->
        pp_pc ();
        print_string (show_bytecode hd);
        print_string " ";
        pp_bytecode' ~i:0 tl
      | Literal n ->
        print_string "\t";
        print_string (show_bytecode hd);
        if i = 0 then print_newline () else print_string "\t";
        incr pp_bytecode_counter;
        pp_bytecode' ~i:(i - 1) tl
      | _ ->
        pp_pc ();
        print_string (show_bytecode hd);
        print_newline ();
        pp_bytecode' ~i:0 tl)
  in
  pp_bytecode' ~i (Array.to_list insts)
;;

let rec string_of_codes ?(i = 0) codes =
  if i > Array.length codes - 1
  then ""
  else (
    let code = codes.(i) in
    let opcode, hasarg = string_of_code code in
    if hasarg
    then (
      let arg, _ = string_of_code codes.(i + 1) in
      "tla." ^ opcode ^ ", " ^ arg ^ ",\n" ^ string_of_codes ~i:(i + 2) codes)
    else "tla." ^ opcode ^ ",\n" ^ string_of_codes ~i:(i + 1) codes)

and string_of_code = function
  | CONST_INT -> "CONST_INT", true
  | CONST_FLOAT -> "CONST_FLOAT", true
  | DUP -> "DUP", false
  | DUPN -> "DUPN", true
  | POP -> "POP", false
  | LT -> "LT", false
  | GT -> "GT", false
  | EQ -> "EQ", false
  | ADD -> "ADD", false
  | SUB -> "SUB", false
  | MUL -> "MUL", false
  | DIV -> "DIV", false
  | MOD -> "MOD", false
  | EXIT -> "EXIT", false
  | JUMP -> "JUMP", true
  | JUMP_IF -> "JUMP_IF", true
  | CALL -> "CALL", true
  | CALL_JIT -> "CALL_JIT", true
  | CALL_NORMAL -> "CALL_NORMAL", true
  | RET -> "RET", true
  | NEWSTR -> "NEWSTR", true
  | PRINT -> "PRINT", false
  | Literal n -> string_of_int n, false
  | Lref _ | Ldef _ -> failwith "Lref and Ldef is not supported here"
;;

let pp_tla_bytecode codes =
  let str = string_of_codes codes in
  Printf.sprintf "code = [\n%s]" str |> print_endline
;;
