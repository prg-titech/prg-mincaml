open MinCaml
open TLA

type backend =
  | Bytecode
  | PPBytecode
  | Virtual

let backend_type = ref Bytecode

let rec lexbuf oc l =
  let open TLA in
  Id.counter := 0;
  Typing.extenv := M.empty;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> Alpha.f
  |> Util.(iter !limit)
  |> Closure.f
  |> Virtual.f
  |> Simm.f
  |> function
  | p ->
    if !Config.flg_emit_virtual then (
      Asm.show_prog p |> prerr_string; prerr_newline ());
    (match !backend_type with
    | PPBytecode -> p |> Emit.f |> Bytecodes.pp_bytecode
    | Bytecode -> p |> Emit.f |> Bytecodes.pp_tla_bytecode
    | Virtual -> Asm.show_prog p |> print_string)
;;

let main f =
  let ic = open_in f in
  let oc = stdout in
  try
    let input = Lexing.from_channel ic in
    lexbuf oc input;
    close_in ic;
    close_out oc
  with
  | e ->
    close_in ic;
    close_out oc;
    raise e
;;

let () =
  let files = ref [] in
  Arg.parse
    [ ( "-inline"
      , Arg.Int (fun i -> MinCaml.Inline.threshold := i)
      , "set a threshold for inlining" )
    ; ( "-iter"
      , Arg.Int (fun i -> MinCaml.Util.limit := i)
      , "set a threshold for iterating" )
    ; ( "-pp"
      , Arg.Unit (fun _ -> backend_type := PPBytecode)
      , "emit bytecode for BacCaml" )
    ; ( "-virt"
      , Arg.Unit (fun _ -> Config.flg_emit_virtual := true)
      , "emit a MinCaml IR")
    ; "-v", Arg.Unit (fun _ -> backend_type := Virtual), "emit MinCaml IR"
    ]
    (fun s -> files := !files @ [ s ])
    (Sys.argv.(0) ^ " [-options] filename.ml");
  List.iter main !files
;;
