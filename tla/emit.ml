open MinCaml
open Asm
open Bytecodes

exception NoImplementedError of string

module Debug = struct
  let print_env env =
    prerr_string "env: ";
    prerr_string "[";
    List.iter
      (fun x ->
        prerr_string x;
        prerr_string ", ")
      env;
    prerr_string "]\n";
    flush stderr
  ;;
end

let compare_id v1 v2 =
  let get_id id = List.nth (String.split_on_char '.' id) 1 |> int_of_string in
  let v1_id, v2_id = get_id v1, get_id v2 in
  v1_id - v2_id
;;

let sort_by_id = List.sort (fun a b -> compare_id a b)

let%test "sort_by_compare_id" =
  let v1, v2, v3 = "l.39", "l.44", "l.50" in
  let args = [v3; v2; v1] in
  let res = sort_by_id args in
  res = [v1; v2; v3]
;;

type number = int * int * int * int

type float_with_digits = Float_with_digits of number * number * int

(* mask the lower rhs bits of lhs *)
let ( |%| ) lhs rhs = lhs land ((1 lsl rhs) - 1)

let devide_large_num n =
  let n0, n1, n2, n3 = n, n |%| 24, n |%| 16, n |%| 8 in
  let a = n0 lsr 24 in
  let b = n1 lsr 16 in
  let c = n2 lsr 8 in
  let d = n3 in
  assert ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor n3 == n);
  a, b, c, d
;;

let devide_large_num_with_digits f =
  let devide_into_to lst = List.nth lst 0, List.nth lst 1 in
  try
    Printf.eprintf "deviding %f\n" f;
    let int_n, decimal_n =
      Float.to_string f |> String.split_on_char '.' |> devide_into_to
    in
    let int = int_n |> int_of_string |> devide_large_num in
    let decimal, digits =
      if decimal_n = ""
      then (0, 0, 0, 0), 0
      else
        ( decimal_n |> int_of_string |> devide_large_num
        , (decimal_n |> float_of_string |> Float.log10 |> int_of_float) + 1 )
    in
    Float_with_digits (int, decimal, digits)
  with
  | e -> raise e
;;

let%test "devide_float_num" =
  let f1 = 1.2345 in
  let (Float_with_digits (i1, d1, d_num1)) = devide_large_num_with_digits f1 in
  i1 = (0, 0, 0, 1) && d1 = (0, 0, 9, 41) && 4 = d_num1
;;

let opcode_of_binop e =
  match e with
  | Add _ | FAddD _ -> ADD
  | Sub _ | FSubD _ -> SUB
  | Mul _ | FMulD _ -> MUL
  | Div _ | FDivD _ -> DIV
  | Mod _ -> MOD
  | _ -> failwith @@ Printf.sprintf "unsupported pattern %s" (show_exp e)
;;

let literals_of_large_num (a, b, c, d) =
  [ Literal a; Literal b; Literal c; Literal d ]
;;


(* generate a unique label id *)
let gen_label, reset =
  let counter = ref 0 in
  ( (fun () ->
      let l = !counter in
      counter := !counter + 1;
      "$" ^ string_of_int l)
  , fun () -> counter := 0 )
;;

(* compilation environment maps local variable names to local variable
   numbers *)
let lookup env var =
  try
    List.mapi (fun idx v -> idx, v) env
    |> List.find (fun (_, v) -> var = v)
    |> fst
  with
  | Not_found ->
    Printf.eprintf "var %s is not found\n" var;
    raise Not_found
;;

let extend_env env var = var :: env
let shift_env env = extend_env env "*dummy*"
let downshift_env env = List.tl env
let return_address_marker = "$ret_addr"
let return_value_marker = "$ret_val"
let build_arg_env args = return_address_marker :: List.rev args

(* computes the number of arguments to this frame. The stack has a shape like
   [...local vars...][ret addr][..args...], the return address position from the
   top indicates the number of local variables on top of the return address. *)
let arity_of_env env =
  let num_local_vars = lookup env return_address_marker in
  List.length env - num_local_vars - 1, num_local_vars
;;

let compile_id_or_imm env = function
  | C n when n >= 1 lsl 8 ->
    let a, b, c, d = devide_large_num n in
    [ CONST_N; Literal a; Literal b; Literal c; Literal d ]
  | C n when n < 0 -> [ CONST_NEG_INT; Literal (abs n) ]
  | C n -> [ CONST_INT; Literal n ]
  | V x ->
    let n = lookup env x in
    prerr_endline @@ Printf.sprintf "Get %s, DUP %d" x n;
    if n = 0 then [ DUP ] else [ DUPN; Literal n ]
;;

let rec compile_t data fname env =
  Debug.print_env env;
  let open Asm in
  function
  | Ans (CallDir (Id.L fname', args, fargs) as e) ->
    if not @@ !Config.flg_tail_opt
    then compile_exp data fname env e
    else if fname' = fname
    then
      (if !Config.flg_frame_reset
      then (
        let old_arity, local_size = arity_of_env env in
        let new_arity = List.length args in
        Printf.eprintf
          "FRAME_RESET old_arity: %d local_size: %d new_arity: %d\n"
          old_arity
          local_size
          new_arity;
        (List.fold_left
           (fun (rev_code_list, env) v ->
             compile_id_or_imm env (V v) :: rev_code_list, shift_env env)
           ([], env)
           args
        |> fst
        |> List.rev
        |> List.flatten)
        @ [ FRAME_RESET
          ; Literal old_arity
          ; Literal local_size
          ; Literal new_arity
          ])
      else [])
      @ [ JUMP; Lref fname ]
    else compile_exp data fname env e
  | Ans e -> compile_exp data fname env e
  | Let ((x, _), exp, t) ->
    let ex_env = extend_env env x in
    compile_exp data fname env exp @ compile_t data fname ex_env t @ [ POP1 ]

and compile_exp data fname env = function
  | Nop -> []
  | Set i -> compile_id_or_imm env (C i)
  | SetL (Id.L l) ->
    (try
       let fval = List.find (fun (Id.L l', _) -> l = l') data |> snd in
       let Float_with_digits (i_num, f_num, digits) = devide_large_num_with_digits (abs_float fval) in
       Printf.eprintf "SetL %s, fval %f\n" l fval;
       (if fval < 0.0 then [ CONST_NEG_FLOAT ]
        else [ CONST_FLOAT ])
       @ (literals_of_large_num i_num) @ (literals_of_large_num f_num) @ [ Literal digits ]
     with
    | e ->
      Printf.eprintf "%s is not found in data\n" l;
      raise e)
  | Mov var -> compile_id_or_imm env (V var)
  | FMovD var -> compile_id_or_imm env (V var)
  | (Add (x, y) | Sub (x, y) | Mul (x, y) | Div (x, y) | Mod (x, y)) as e ->
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) y
    @ [ opcode_of_binop e ]
  | (FAddD (x, y) | FSubD (x, y) | FMulD (x, y) | FDivD (x, y)) as e ->
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) (V y)
    @ [ opcode_of_binop e ]
  | Ld (x, y, n) ->
    (* target ary, index, offset *)
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ LOAD ]
  | LdDF (x, y, n) -> compile_id_or_imm env (V x)
  | St (x, y, z, n) | StDF (x, y, z, n) ->
    (* value, taget ary, index, offset *)
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) (V y)
    @ compile_id_or_imm (shift_env (shift_env env)) z
    @ [ STORE ]
  | IfEq (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) y
    @ [ EQ ]
    @ [ JUMP_IF; Lref l1 ]
    @ compile_t data fname env then_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t data fname env else_exp
    @ [ Ldef l2 ]
  | IfFEq (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) (V y)
    @ [ EQ ]
    @ [ JUMP_IF; Lref l1 ]
    @ compile_t data fname env then_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t data fname env else_exp
    @ [ Ldef l2 ]
  | IfLE (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) y
    @ [ LT ]
    @ [ JUMP_IF; Lref l1 ]
    @ compile_t data fname env else_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t data fname env then_exp
    @ [ Ldef l2 ]
  | IfFLE (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) (V y)
    @ [ LT ]
    @ [ JUMP_IF; Lref l1 ]
    @ compile_t data fname env else_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t data fname env then_exp
    @ [ Ldef l2 ]
  | IfGE (x, y, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    compile_id_or_imm env (V x)
    @ compile_id_or_imm (shift_env env) y
    @ [ GT ]
    @ [ JUMP_IF; Lref l1 ]
    @ compile_t data fname env else_exp
    @ [ JUMP; Lref l2 ]
    @ [ Ldef l1 ]
    @ compile_t data fname env then_exp
    @ [ Ldef l2 ]
  | CallDir (Id.L "min_caml_create_array", args, fargs) ->
    let size, init = List.nth args 0, List.nth args 1 in
    compile_id_or_imm env (V init)
    @ compile_id_or_imm (shift_env env) (V size)
    @ [ BUILD_LIST ]
  | CallDir (Id.L "min_caml_create_float_array", args, fargs) ->
    let size, init = List.nth args 0, List.nth fargs 0 in
    compile_id_or_imm env (V init)
    @ compile_id_or_imm (shift_env env) (V size)
    @ [ BUILD_LIST ]
  | CallDir (Id.L "min_caml_sin", [x], _) ->
    compile_id_or_imm env (V x) @ [ SIN ]
  | CallDir (Id.L "min_caml_cos", [x], _) ->
    compile_id_or_imm env (V x) @ [ COS ]
  | CallDir (Id.L "min_caml_abs_float", _, [x]) ->
    compile_id_or_imm env (V x) @ [ ABS_FLOAT ]
  | CallDir (Id.L "min_caml_sqrt", [x], _) ->
    compile_id_or_imm env (V x) @ [ SQRT ]
  | CallDir (Id.L "min_caml_int_of_float", _, [x]) ->
    compile_id_or_imm env (V x) @ [ FLOAT_TO_INT ]
  | CallDir (Id.L "min_caml_float_of_int", [x], _) ->
    compile_id_or_imm env (V x) @ [ INT_TO_FLOAT ]
  | CallDir (Id.L "min_caml_print_int", [x], _) ->
    compile_id_or_imm env (V x) @ [ PRINT ]
  | CallDir (Id.L "min_caml_print_float", _, [x]) ->
    compile_id_or_imm env (V x) @ [ PRINT ]
  | CallDir (Id.L var, args, fargs) ->
    Printf.eprintf "CALL %s %d\n" var (List.length fargs);
    (args @ fargs
    |> sort_by_id
    |> List.fold_left
         (fun (rev_code_list, env) v ->
           Debug.print_env env;
           compile_id_or_imm env (V v) :: rev_code_list, shift_env env)
         ([], env)
    |> fst
    |> List.rev
    |> List.flatten)
    @
    (if !Config.flg_call_assembler
     then [ CALL_ASSEMBLER; Lref var; Literal (List.length (args @ fargs)) ]
     else [ CALL; Lref var; Literal (List.length (args @ fargs)) ])
  | CallCls (var, args, fargs) ->
    (args @ fargs
    |> sort_by_id
    |> List.fold_left
         (fun (rev_code_list, env) v ->
           Debug.print_env env;
           compile_id_or_imm env (V v) :: rev_code_list, shift_env env)
         ([], env)
    |> fst
    |> List.rev
    |> List.flatten)
    @
    if !Config.flg_call_assembler
    then [ CALL_ASSEMBLER; Lref var; Literal (List.length args) ]
    else [ CALL; Lref var; Literal (List.length args) ]
  | exp ->
    raise
      (NoImplementedError
         (Printf.sprintf "un matched pattern: %s" (Asm.show_exp exp)))
;;

(* [...;Ldef a;...] -> [...;a,i;...] where i is the index of the next
   instruction of Ldef a in the list all Ldefs are removed e.g., [_;Ldef
   8;_;Ldef 7;_] ==> [8,1; 7,2] *)
let make_label_env instrs =
  snd
    (List.fold_left
       (fun (addr, env) -> function
         | Ldef n -> addr, (Lref n, Literal addr) :: env
         | _ -> addr + 1, env)
       (0, [])
       instrs)
;;

let resolve_n_opt inst =
  match inst with
  | Literal n ->
    if n > 255
    then (
      let a, b, c, d = devide_large_num n in
      Some (Literal a, Literal b, Literal c, Literal d))
    else None
  | _ -> None
;;

let change_to_n_inst inst =
  match inst with
  | CALL -> CALL_N
  | CALL_ASSEMBLER -> CALL_N_ASSEMBLER
  | JUMP -> JUMP_N
  | JUMP_IF -> JUMP_IF_N
  | _ -> inst
;;

let rec resolve_n_insts insts =
  match insts with
  | [] -> []
  | [ hd ] -> [ hd ]
  | hd1 :: hd2 :: tl ->
    (match hd1 with
    | JUMP | JUMP_IF | CALL ->
      (match hd2 with
      | Literal n when n > 255 ->
        let a, b, c, d = devide_large_num n in
        [ change_to_n_inst hd1 ]
        @ [ Literal a; Literal b; Literal c; Literal d ]
        @ resolve_n_insts tl
      | _ -> hd1 :: hd2 :: resolve_n_insts tl)
    | _ -> hd1 :: resolve_n_insts (hd2 :: tl))
;;

let assoc_if subst elm = try List.assoc elm subst with Not_found -> elm

let resolve_labels instrs =
  let lenv = make_label_env instrs in
  instrs
  |> List.map (fun instr -> assoc_if lenv instr)
  |> List.filter (function Ldef _ -> false | _ -> true)
  |> resolve_n_insts
;;

let compile_fun_body data fenv name arity annot exp env =
  [ Ldef name ]
  @ compile_t data name env exp
  @ if name = "main" then [ EXIT ] else [ RET; Literal arity ]
;;

let compile_fun
    (data : (Id.l * float) list)
    (fenv : Id.l -> Asm.fundef)
    { name = Id.L name; args; fargs; body; annot }
  =
  compile_fun_body
    data
    fenv
    name
    (List.length (args @ fargs))
    annot
    body
    (build_arg_env (args @ fargs))
;;

let compile_funs data fundefs =
  (* let fenv name = fst(List.find (fun (_,{name=n}) -> name=n)
   *                       (List.mapi (fun idx fdef -> (idx,fdef))
   *                          fundefs)) in *)
  let fenv name = List.find (fun Asm.{ name = n } -> n = name) fundefs in
  Array.of_list
    (resolve_labels (List.flatten (List.map (compile_fun data fenv) fundefs)))
;;

let f (Asm.Prog (data, fundefs, main)) =
  let main =
    { name = Id.L "main"
    ; args = []
    ; fargs = []
    ; ret = Type.Int
    ; body = main
    ; annot = None
    }
  in
  compile_funs data (main :: fundefs)
;;
