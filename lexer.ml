let rec lex = parser
            | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream

            | [< ' ('A' .. 'Z' | 'a' .. 'z' as c); stream >] ->
               let buffer = Buffer.create 1 in
               Buffer.add_char buffer c;
               lex_ident buffer stream

            | [< ' ('0' .. '9' as c); stream >] ->
               let buffer = Buffer.create 1 in
               Buffer.add_char buffer c;
               lex_number buffer stream

            | [< ' ('#'); stream >] ->
               lex_comment stream

            | [< 'c; stream >] ->
               [< 'Token.Kwd c; lex stream >]

            | [< >] -> [< >]

and lex_ident buffer = parser
                     | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c); stream >] ->
                        Buffer.add_char buffer c;
                        lex_ident buffer stream
                     | [< stream = lex >] ->
                        match Buffer.contents buffer with
                        | "def" -> [< 'Token.Def; stream >]
                        | "extern" -> [< 'Token.Extern; stream >]
                        | id -> [< 'Token.Ident id; stream >]

and lex_number buffer = parser
                      | [< ' ('0' .. '9' as c); stream >] ->
                         Buffer.add_char buffer c;
                         lex_number buffer stream
                      | [< stream = lex >] -> [< 'Token.Number (float_of_string (Buffer.contents buffer)); stream >]

and lex_comment = parser
                | [< ' ('\n'); stream = lex >] -> stream
                | [< 'c; e = lex_comment >] -> e
                | [< >] -> [< >]
                                  
