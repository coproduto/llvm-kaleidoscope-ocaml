(* binop_precedence - This holds the precedence for each binary operator that is
 * defined *)
let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10

(* precedence - Get the precedence of the pending binary operator token. *)
let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

let rec parse_primary = parser
  | [< 'Token.Number n >] -> Ast.Number n
  | [< 'Token.Kwd '('; e = parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e
  | [< 'Token.Ident id; stream >] ->
     let rec parse_args acc = parser
                            | [< e0 = parse_expr; stream >] ->
                               begin parser
                                   | [< 'Token.Kwd ','; e1 = parse_args (e0 :: acc) >] -> e1
                                   | [< >] -> e0 :: acc
                               end stream
                            | [< >] -> acc
     in
     let rec parse_ident id = parser
                            | [< 'Token.Kwd '(';
                               args = parse_args [];
                               'Token.Kwd ')' ?? "expected ')'" >] -> Ast.Call (id, Array.of_list (List.rev args))
                            | [< >] -> Ast.Variable id
                                     
     in parse_ident id stream
  | [< >] -> raise (Stream.Error "unknown token when expecting an expression")

and parse_bin_rhs expr_prec lhs stream =
  match Stream.peek stream with
  | Some (Token.Kwd c0) when Hashtbl.mem binop_precedence c0 ->
     let token_prec = precedence c0 in
     if token_prec < expr_prec then lhs else begin
         Stream.junk stream;
         let rhs0 = parse_primary stream in
         let rhs1 = match Stream.peek stream with
           | Some (Token.Kwd c1) ->
              let next_prec = precedence c1 in
              if token_prec < next_prec
              then parse_bin_rhs (token_prec + 1) rhs0 stream
              else rhs0
           | _ -> rhs0
         in
         let lhs = Ast.Binary (c0, lhs, rhs1) in
         parse_bin_rhs expr_prec lhs stream
       end
  | _ -> lhs

and parse_expr = parser
               | [< lhs = parse_primary; stream >] -> parse_bin_rhs 0 lhs stream

let parse_prototype =
  let rec parse_args accumulator = parser
    | [< 'Token.Ident id; e=parse_args (id::accumulator) >] -> e
    | [< >] -> accumulator
  in parser
    | [< 'Token.Ident id;
         'Token.Kwd '(' ?? "expected '(' in prototype";
         args=parse_args [];
         'Token.Kwd ')' ?? "expected ')' in prototype" >] ->
       Ast.Prototype (id, Array.of_list (List.rev args))
    | [< >] ->
       raise (Stream.Error "expected function name in prototype")

(* definition ::= 'def' prototype expression *)
let parse_definition = parser
  | [< 'Token.Def; p=parse_prototype; e=parse_expr >] ->
      Ast.Function (p, e)

(* toplevelexpr ::= expression *)
let parse_toplevel = parser
  | [< e=parse_expr >] ->
      (* Make an anonymous proto. *)
      Ast.Function (Ast.Prototype ("", [||]), e)

(*  external ::= 'extern' prototype *)
let parse_extern = parser
  | [< 'Token.Extern; e=parse_prototype >] -> e
