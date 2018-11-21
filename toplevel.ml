let rec main_loop prompt stream =
  match Stream.peek stream with
  | None -> ()

  | Some (Token.Kwd ';') ->
      Stream.junk stream;
      main_loop prompt stream

  | Some token ->
     begin
       try match token with
           | Token.Def ->
              ignore(Parser.parse_definition stream);
              print_endline "parsed a function definition.";
           | Token.Extern ->
              ignore(Parser.parse_extern stream);
              print_endline "parsed an extern.";
           | _ ->
              ignore(Parser.parse_toplevel stream);
              print_endline "parsed a top-level expr.";
       with Stream.Error s ->
         Stream.junk stream;
         print_endline s;
     end;
     print_string prompt;
     flush stdout;
     main_loop prompt stream
                         
