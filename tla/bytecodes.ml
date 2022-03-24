type bytecode =
  | CONST_INT
  | CONST_FLOAT
  | DUP
  | DUPN
  | POP
  | POP1
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
  | FRAME_RESET
  | BUILD_LIST
  | LOAD
  | STORE
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
   ; POP1
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
   ; FRAME_RESET
   ; BUILD_LIST
   ; LOAD
   ; STORE
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
      | CONST_INT | DUPN | JUMP | JUMP_IF | RET | CALL ->
        pp_pc ();
        print_string (show_bytecode hd);
        print_string " ";
        pp_bytecode' ~i:0 tl
      | Literal n ->
        print_string "    ";
        print_string (show_bytecode hd);
        if i = 0 then print_newline () else print_string "    ";
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
    let opcode, argnum = string_of_code code in
    let str = ref "" in
    for j = i + 1 to i + argnum do
      let opcode, argnum = string_of_code codes.(j) in
      str := !str ^ opcode ^ ", "
    done;
    (Printf.sprintf "    tla.%s, %s\n" opcode !str) ^ string_of_codes ~i:(i + argnum + 1) codes)

and string_of_code = function
  | CONST_INT -> "CONST_INT", 1
  | CONST_FLOAT -> "CONST_FLOAT", 1
  | DUP -> "DUP", 0
  | DUPN -> "DUPN", 1
  | POP -> "POP", 0
  | POP1 -> "POP1", 0
  | LT -> "LT", 0
  | GT -> "GT", 0
  | EQ -> "EQ", 0
  | ADD -> "ADD", 0
  | SUB -> "SUB", 0
  | MUL -> "MUL", 0
  | DIV -> "DIV", 0
  | MOD -> "MOD", 0
  | EXIT -> "EXIT", 0
  | JUMP -> "JUMP", 1
  | JUMP_IF -> "JUMP_IF", 1
  | CALL -> "CALL", 2
  | CALL_JIT -> "CALL_JIT", 2
  | CALL_NORMAL -> "CALL_NORMAL", 2
  | RET -> "RET", 1
  | NEWSTR -> "NEWSTR", 1
  | PRINT -> "PRINT", 0
  | Literal n -> string_of_int n, 0
  | FRAME_RESET -> "FRAME_RESET", 3
  | Lref _ | Ldef _ -> failwith "Lref and Ldef is not supported here"
;;

let pp_tla_bytecode codes =
  let str = string_of_codes codes in
  Printf.sprintf "code = [\n%s]" str |> print_endline
;;
