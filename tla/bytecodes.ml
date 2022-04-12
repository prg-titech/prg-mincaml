open Printf

let sp = sprintf

(*
('NOP', False),
('CONST_INT', True),
('CONST_NEG_INT', True),
('CONST_FLOAT', True),
('CONST_NEG_FLOAT', True),
('CONST_N', True),
('DUP', False),
('DUPN', True),
('POP', False),
('POP1', False),
('LT', False),
('GT', False),
('EQ', False),
('ADD', False),
('SUB', False),
('MUL', False),
('DIV', False),
('MOD', False),
('EXIT', False),
('JUMP', True),
('JUMP_N', True),
('JUMP_IF', True),
('JUMP_IF_N', True),
('CALL', True),
('CALL_ASSEMBLER', True),
('CALL_TIER2', True),
('CALL_TIER0', True),
('RET', True),
('NEWSTR', True),
('FRAME_RESET', True),
('PRINT', False),
('LOAD', True),
('STORE', True),
('BUILD_LIST', True),
('RAND_INT', False),
('FLOAT_TO_INT', False),
('INT_TO_FLOAT', False),
('ABS_FLOAT', False),
('SIN', False),
('COS', False),
('SQRT', False)
*)

type bytecode =
  | NOP
  | CONST_INT
  | CONST_NEG_INT
  | CONST_FLOAT
  | CONST_NEG_FLOAT
  | CONST_N
  | CONST_NEG_N
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
  | JUMP_N
  | JUMP_IF
  | JUMP_IF_N
  | CALL
  | CALL_N
  | CALL_ASSEMBLER
  | CALL_N_ASSEMBLER
  | CALL_TIER2
  | CALL_TIER0
  | RET
  | NEWSTR
  | FRAME_RESET
  | PRINT
  | BUILD_LIST
  | LOAD
  | STORE
  | RAND_INT
  | FLOAT_TO_INT
  | INT_TO_FLOAT
  | ABS_FLOAT
  | SIN
  | COS
  | SQRT
  | Literal of int
  | Lref of string
  | Ldef of string
[@@deriving show]

let bytecodes =
  [|NOP
   ;CONST_INT
   ;CONST_NEG_INT
   ;CONST_FLOAT
   ;CONST_NEG_FLOAT
   ;CONST_N
   ;CONST_NEG_N
   ;DUP
   ;DUPN
   ;POP
   ;POP1
   ;LT
   ;GT
   ;EQ
   ;ADD
   ;SUB
   ;MUL
   ;DIV
   ;MOD
   ;EXIT
   ;JUMP
   ;JUMP_N
   ;JUMP_IF
   ;JUMP_IF_N
   ;CALL
   ;CALL_N
   ;CALL_ASSEMBLER
   ;CALL_N_ASSEMBLER
   ;CALL_TIER2
   ;CALL_TIER0
   ;RET
   ;NEWSTR
   ;PRINT
   ;RAND_INT
   ;FLOAT_TO_INT
   ;INT_TO_FLOAT
   ;ABS_FLOAT
   ;SIN
   ;COS
   ;SQRT
   ;FRAME_RESET
   ;BUILD_LIST
   ;LOAD
   ;STORE
  |]
;;

let string_of_code = function
  | NOP -> "NOP", 0
  | CONST_INT -> "CONST_INT", 1
  | CONST_NEG_INT -> "CONST_NEG_INT", 1
  | CONST_FLOAT -> "CONST_FLOAT", 9
  | CONST_NEG_FLOAT -> "CONST_NEG_FLOAT", 9
  | CONST_N (* a, b, c, d *) -> "CONST_N", 4
  | CONST_NEG_N (* a, b, c, d *) -> "CONST_NEG_N", 4
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
  | JUMP_N -> "JUMP_N", 4
  | JUMP_IF -> "JUMP_IF", 1
  | JUMP_IF_N -> "JUMP_IF_N", 4
  | CALL -> "CALL", 2
  | CALL_N -> "CALL_N", 5
  | CALL_ASSEMBLER -> "CALL_ASSEMBLER", 2
  | CALL_N_ASSEMBLER -> "CALL_N_ASSEMBLER", 5
  | CALL_TIER2 -> "CALL_TIER2", 2
  | CALL_TIER0 -> "CALL_TIER0", 2
  | RET -> "RET", 1
  | NEWSTR -> "NEWSTR", 1
  | PRINT -> "PRINT", 0
  | RAND_INT -> "RAND_INT", 4
  | Literal n -> string_of_int n, 0
  | FRAME_RESET -> "FRAME_RESET", 3
  | FLOAT_TO_INT -> "FLOAT_TO_INT", 0
  | INT_TO_FLOAT -> "INT_TO_FLOAT", 0
  | ABS_FLOAT -> "ABS_FLOAT", 0
  | SIN -> "SIN", 0
  | COS -> "COS", 0
  | SQRT -> "SQRT", 0
  | LOAD -> "LOAD", 0
  | STORE -> "STORE", 0
  | BUILD_LIST -> "BUILD_LIST", 0
  | Lref _ -> "Lref", 1
  | Ldef _ ->  "Ldef", 1
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


let pp_bytecode_counter = ref 0

let pp_pc () =
  print_int !pp_bytecode_counter;
  print_string "\t";
  incr pp_bytecode_counter
;;

let rec pp_bytecode ?(i = 0) codes =
  let rec pp_bytecode' ?(i = 0) codes =
    match codes with
    | [] -> ()
    | hd :: tl ->
      (match hd with
      | CONST_INT | CONST_FLOAT | DUPN | JUMP | JUMP_IF | RET ->
        pp_pc ();
        print_string (show_bytecode hd);
        print_string "\t";
        pp_bytecode' ~i:0 tl
      | CALL ->
        pp_pc ();
        print_string (show_bytecode hd);
        print_string "\t";
        pp_bytecode' ~i:1 tl
      | FRAME_RESET ->
        pp_pc ();
        print_string (show_bytecode hd);
        print_string "\t";
        pp_bytecode' ~i:2 tl
      | CONST_N ->
        pp_pc ();
        print_string (show_bytecode hd);
        print_string "\t";
        pp_bytecode' ~i:3 tl
      | Literal n ->
        print_string "\t";
        print_string (show_bytecode hd);
        if i = 0 then print_newline () else print_string "\t";
        incr pp_bytecode_counter;
        pp_bytecode' ~i:(i - 1) tl
      | Lref x ->
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
  pp_bytecode' ~i (Array.to_list codes)
;;


let pp_tla_bytecode codes =
  let str = string_of_codes codes in
  Printf.sprintf "code = [\n%s]" str |> print_endline
;;
