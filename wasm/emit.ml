open MinCaml
open Asm

exception Error of string

let global_hp = "@hp"
let global_cp = "@cp"

let rec localvs_exp = function
  | IfEq (_, _, e1, e2)
  | IfLE (_, _, e1, e2)
  | IfGE (_, _, e1, e2)
  | IfFEq (_, _, e1, e2)
  | IfFLE (_, _, e1, e2) ->
    localvs e1 @ localvs e2
  | _ -> []

and localvs = function
  | Ans exp -> localvs_exp exp
  | Let (xt, exp, e) -> (xt :: localvs_exp exp) @ localvs e
;;

(* super-tenuki global variables *)
(* let fentries = ref [] *)

let rec findi f = findi' f 0

and findi' f i = function
  | [] -> None
  | x :: xs -> if f x then Some i else findi' f (i + 1) xs
;;

(* WAT style of function name. *)
let without_suffix x = String.split_on_char '.' x |> List.hd
let without_suffix_idl (Id.L x) = String.split_on_char '.' x |> List.hd
let local_name x = "$" ^ without_suffix x
let func_name (Id.L x) = local_name x

let local_name_or_imm = function
  | V x -> Printf.sprintf "get_local %s" (local_name x)
  | C i -> Printf.sprintf "i32.const %d" i
;;

let wat_type = function Type.Float -> "f32" | _ -> "i32"

let func_sig oc = function
  | Type.Fun (tys, ret) ->
    List.iter
      (fun ty ->
        if ty <> Type.Unit then Printf.fprintf oc "(param %s)" (wat_type ty))
      tys;
    if ret <> Type.Unit then Printf.fprintf oc " (result %s)" (wat_type ret)
  | _ -> assert false
;;

let rec g oc = function
  | Ans e ->
    gexp oc e;
    Printf.fprintf oc "  return\n"
  | Let ((x, t), e, n) ->
    (* Calculation of `e` should leave a variable when t is not unit. *)
    gexp oc e;
    if t <> Type.Unit then Printf.fprintf oc "    set_local %s\n" (local_name x);
    g oc n

and gexp oc = function
  | Nop -> ()
  | Mov x -> Printf.fprintf oc "    get_local %s\n" (local_name x)
  | Set i -> Printf.fprintf oc "    i32.const %d\n" i
  | Add (x, y) ->
    gv oc x;
    gv_imm oc y;
    Printf.fprintf oc "    i32.add\n"
  | Sub (x, y) ->
    gv oc x;
    gv_imm oc y;
    Printf.fprintf oc "    i32.sub\n"
  | FAddD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.add\n"
  | FSubD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.sub\n"
  | FMulD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.mul\n"
  | FDivD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.div_s\n"
  | Ld (x, y, z) ->
    Printf.fprintf oc "    get_local %s\n" (local_name x);
    (match y with
    | V x ->
      Printf.fprintf oc "    get_local %s\n" (local_name x);
      Printf.fprintf oc "    i32.const %d\n" z;
      Printf.fprintf oc "    i32.mul\n";
      Printf.fprintf oc "    i32.load\n"
    | C i -> Printf.fprintf oc "    i32.load offset=%d\n" (i * z))
  | St (w, x, y, z) ->
    Printf.fprintf oc "    get_local %s\n" (local_name x);
    (match y with
    | V x ->
      Printf.fprintf oc "    get_local %s\n" (local_name x);
      Printf.fprintf oc "    i32.const %d\n" z;
      Printf.fprintf oc "    i32.mul\n";
      Printf.fprintf oc "    get_local %s\n" (local_name w);
      Printf.fprintf oc "    i32.store\n"
    | C i ->
      Printf.fprintf oc "    get_local %s\n" (local_name w);
      Printf.fprintf oc "    i32.store offset=%d\n" (i * z))
  | IfEq (x, y, t1, t2) ->
    Printf.fprintf
      oc
      "  (if (result i32) (i32.eq (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc "  (then\n";
    g oc t1;
    Printf.fprintf oc "  )\n";
    Printf.fprintf oc "  (else\n";
    g oc t2;
    Printf.fprintf oc "  ))\n"
  | IfGE (x, y, t1, t2) ->
    Printf.fprintf
      oc
      "  (if (result i32) (i32.ge_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc "  (then\n";
    g oc t1;
    Printf.fprintf oc "  )\n";
    Printf.fprintf oc "  (else\n";
    g oc t2;
    Printf.fprintf oc "  ))\n"
  | IfLE (x, y, t1, t2) ->
    Printf.fprintf
      oc
      "  (if (result i32) (i32.le_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc "  (then\n";
    g oc t1;
    Printf.fprintf oc "  )\n";
    Printf.fprintf oc "  (else\n";
    g oc t2;
    Printf.fprintf oc "  ))\n"
  | IfFEq (x, y, t1, t2) ->
    Printf.fprintf
      oc
      "  (if (result f32) (f32.eq (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name y);
    Printf.fprintf oc "  (then\n";
    g oc t1;
    Printf.fprintf oc "  )\n";
    Printf.fprintf oc "  (else\n";
    g oc t2;
    Printf.fprintf oc "  ))\n"
  | IfFLE (x, y, t1, t2) ->
    Printf.fprintf
      oc
      "  (if (result f32) (f32.le_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name y);
    Printf.fprintf oc "  (then\n";
    g oc t1;
    Printf.fprintf oc "  )\n";
    Printf.fprintf oc "  (else\n";
    g oc t2;
    Printf.fprintf oc "  ))\n"
  | CallDir (f, args, fargs) ->
    List.iter (gv oc) args;
    List.iter (gvf oc) fargs;
    Printf.fprintf oc "    call %s\n" (func_name f)
  | CallCls (f, args, fargs) ->
    Printf.fprintf oc "    TODO  ;; CallCls is not implemented"
  | Comment _ -> ()
  | _ -> raise (Error "unimplemented instruction")

and gv oc x = Printf.fprintf oc "    get_local %s\n" (local_name x)
and gvf oc x = Printf.fprintf oc "    get_local %s\n" (local_name x)

and gv_imm oc = function
  | V x -> Printf.fprintf oc "    get_local %s\n" (local_name x)
  | C i -> Printf.fprintf oc "    i32.const %d\n" i
;;

let h oc { name; args; fargs; body = e; ret } =
  (* Emit a function definition. *)
  Printf.fprintf oc "  (func %s" (func_name name);
  Printf.fprintf oc "  (export \"%s\")" (without_suffix_idl name);
  (* Declare function signature. *)
  List.iter
    (fun x ->
      Printf.fprintf oc " (param %s %s)" (local_name x) (wat_type Type.Int))
    args;
  List.iter
    (fun x ->
      Printf.fprintf oc " (param %s %s) " (local_name x) (wat_type Type.Float))
    fargs;
  if ret <> Type.Unit then Printf.fprintf oc " (result %s)" (wat_type ret);
  (* Declare local variables. *)
  Printf.fprintf oc "\n   ";
  List.iter
    (fun (x, t) ->
      Printf.fprintf oc " (local %s %s)" (local_name x) (wat_type t))
    (localvs e);
  (* Body of function. *)
  Printf.fprintf oc "\n";
  g oc e;
  Printf.fprintf oc ")\n"
;;

let f oc (Prog (ftable, fundefs, main)) =
  Format.eprintf "generating assembly...@.";
  (* start module *)
  Printf.fprintf oc "(module\n";
  Printf.fprintf
    oc
    "  (func $min_caml_print_int (import \"imports\" \"log\") (param i32) \
     (result i32))\n";
  Printf.fprintf
    oc
    "  (func $min_caml_print_newline (import \"imports\" \"newline\") (param \
     i32) (result i32))\n";
  (* main *)
  let mainfun =
    { name = Id.L "main"
    ; args = []
    ; fargs = []
    ; body = main
    ; ret = Type.Unit
    ; annot = None
    }
  in
  Printf.fprintf
    oc
    "%s\n"
    (String.concat
       "\n"
       [ "  (func $min_caml_create_array (param $num i32) (param $value i32) \
          (result i32)"
       ; "    (local $i i32) (local $res i32)"
       ; Printf.sprintf "    get_global %s" (local_name global_hp)
       ; "    set_local $res"
       ; "    i32.const 0"
       ; "    set_local $i"
       ; "    get_local $num"
       ; "    i32.const 2"
       ; "    i32.shl"
       ; "    set_local $num"
       ; "    loop $loop"
       ; "      get_local $i"
       ; "      get_local $num"
       ; "      i32.lt_u"
       ; "      if"
       ; Printf.sprintf "        get_global %s" (local_name global_hp)
       ; "        get_local $i"
       ; "        i32.add"
       ; "        get_local $value"
       ; "        i32.store offset=0 align=4"
       ; "        get_local $i"
       ; "        i32.const 4"
       ; "        i32.add"
       ; "        set_local $i"
       ; "        br $loop"
       ; "      end"
       ; "    end"
       ; Printf.sprintf "    get_global %s" (local_name global_hp)
       ; "    get_local $num"
       ; "    i32.add"
       ; Printf.sprintf "    set_global %s" (local_name global_hp)
       ; "    get_local $res)"
       ]);
  List.iter (h oc) (fundefs @ [ mainfun ]);
  (* Require 192KiB of memory. *)
  Printf.fprintf oc "  (memory %d)\n" 3;
  (* Declare global variables. *)
  Printf.fprintf
    oc
    "  (global %s (mut i32) i32.const 0)\n"
    (local_name global_hp);
  Printf.fprintf
    oc
    "  (global %s (mut i32) i32.const 0)\n"
    (local_name global_cp);
  (* Declare start function. *)
  Printf.fprintf oc "  (start $main)\n";
  (* end module *)
  Printf.fprintf oc ")"
;;
