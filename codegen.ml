open Llvm

exception Error of string
   
let the_context = global_context ()
let the_module = create_module the_context "KaleidoJIT"
let the_builder = builder the_context
let the_double_type = double_type the_context                

let named_values: (string, llvalue) Hashtbl.t = Hashtbl.create 10

let rec codegen_expr = function
  | Ast.Number n -> const_float the_double_type n
  | Ast.Variable name ->
     (try Hashtbl.find named_values name with
      | Not_found -> raise (Error ("unknown variable name: " ^ name)))
  | Ast.Binary (op, lhs, rhs) ->
     let lhs_val = codegen_expr lhs in
     let rhs_val = codegen_expr rhs in
     begin
       match op with
       | '+' -> build_add lhs_val rhs_val "addtmp" the_builder
       | '-' -> build_sub lhs_val rhs_val "subtmp" the_builder
       | '*' -> build_mul lhs_val rhs_val "multmp" the_builder
       | '<' ->
          let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmpulttmp" the_builder in
          build_uitofp i the_double_type "booltmp" the_builder
       | _ ->
          raise (Error ("invalid binary operator " ^ (Printf.sprintf "%c" op)))
     end
  | Ast.Call (callee, args) ->
     let callee_fun =
       match lookup_function callee the_module with
       | Some callee_fun -> callee_fun
       | None -> raise (Error ("unknown function referenced: " ^ callee))
     in
     let params = params callee_fun in
     (if Array.length params == Array.length args
      then ()
      else
        let expected_args = Array.length params in
        let actual_args = Array.length args in
        raise
          (Error (Printf.sprintf
                    {|incorrect number of args to function %s.
                     expected: %d, found: %d|}
                    callee expected_args actual_args
          )));
       let args = Array.map codegen_expr args in
       build_call callee_fun args "calltmp" the_builder

let codegen_proto = function
  | Ast.Prototype (name, args) ->
     let doubles = Array.make (Array.length args) the_double_type in
     let ft = function_type the_double_type doubles in
     let f =
       match lookup_function name the_module with
       | None -> declare_function name ft the_module

       (* If 'f' conflicted, there was already something named 'name'. If it
        * has a body, don't allow redefinition or reextern. *)
       | Some f ->
          (* If 'f' already has a body, reject this. *)
          if block_begin f <> At_end f then
            raise (Error "redefinition of function");

          (* If 'f' took a different number of arguments, reject. *)
          if element_type (type_of f) <> ft then
            raise (Error "redefinition of function with different # args");
          f
     in

     (* Set names for all arguments. *)
     Array.iteri (fun i a ->
         let n = args.(i) in
         set_value_name n a;
         Hashtbl.add named_values n a;
       ) (params f);
     f

let codegen_func = function
  | Ast.Function (proto, body) ->
      Hashtbl.clear named_values;
      let the_function = codegen_proto proto in

      (* Create a new basic block to start insertion into. *)
      let bb = append_block the_context "entry" the_function in
      position_at_end bb the_builder;

      try
        let ret_val = codegen_expr body in

        (* Finish off the function. *)
        let _ = build_ret ret_val the_builder in

        (* Validate the generated code, checking for consistency. *)
        Llvm_analysis.assert_valid_function the_function;

        the_function
      with e ->
        delete_function the_function;
        raise e
