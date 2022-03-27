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

let opcode_of_binop e =
  match e with
  | Add _ | FAddD _ -> ADD
  | Sub _ | FSubD _ -> SUB
  | Mul _ | FMulD _ -> MUL
  | Div _ | FDivD _ -> DIV
  | Mod _ -> MOD
  | _ -> failwith @@ Printf.sprintf "unsupported pattern %s" (show_exp e)
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
    Printf.eprintf "var %s is not found" var;
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
       let f = List.find (fun (Id.L l', _) -> l = l') data |> snd in
       [ CONST_FLOAT; FLiteral f ]
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
  | Ld (x, y, n) | LdDF (x, y, n) ->
    (* target ary, index, offset *)
    compile_id_or_imm env (V x) @ compile_id_or_imm (shift_env env) y @ [ LOAD ]
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
  | CallDir (Id.L "min_caml_print_int", args, fargs) -> [ PRINT ]
  | CallDir (Id.L "min_caml_create_array", args, fargs) ->
    let size, init = List.nth args 0, List.nth args 1 in
    compile_id_or_imm env (V init)
    @ compile_id_or_imm (shift_env env) (V size)
    @ [ BUILD_LIST ]
  | CallDir (Id.L var, args, _) ->
    Printf.eprintf "CALL %s %d\n" var (List.length args);
    (args
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
  | CallCls (var, args, _) ->
    (args
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

let rec resolve_largenum_insts insts =
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
         @ [ Literal a; Literal b; Literal c; Literal d]
         @ resolve_largenum_insts tl
      | _ -> hd1 :: hd2 :: resolve_largenum_insts tl)
    | _ -> hd1 :: resolve_largenum_insts (hd2 :: tl))
;;

let assoc_if subst elm =
  try
    List.assoc elm subst
  with Not_found -> elm
;;

let resolve_labels instrs =
  let lenv = make_label_env instrs in
  instrs
  |> List.map (fun instr -> assoc_if lenv instr)
  |> List.filter (function Ldef _ -> false | _ -> true)
;;

let compile_fun_body data fenv name arity annot exp env =
  [ Ldef name ]
  @ compile_t data name env exp
  @ if name = "main" then [ EXIT ] else [ RET; Literal arity ]
;;

let compile_fun
    (data : (Id.l * float) list)
    (fenv : Id.l -> Asm.fundef)
    { name = Id.L name; args; body; annot }
  =
  compile_fun_body
    data
    fenv
    name
    (List.length args)
    annot
    body
    (build_arg_env args)
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
