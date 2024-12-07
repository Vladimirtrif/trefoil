include Ast_types
open Errors

(* TO DO: make sure patterns cannot reuse same variables *)
let rec pattern_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
      match int_of_string_opt sym with
      | Some n -> IntPattern n
      | None ->
         match sym with
         | "_" -> WildcardPattern
         | "true" -> BoolPattern true
         | "false" -> BoolPattern false
         | "nil" -> NilPattern
         | _ ->
            if String.get sym 0 = '\'' (* if the string starts with an apostrophe *)
            then let sym_without_apostrophe = String.sub sym 1 (String.length sym - 1)
                 in SymbolPattern sym_without_apostrophe
            else VarPattern sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
     | Pst.Symbol s, ps -> begin
        let rec pattern_of_pstList ps acc =
          match ps with
          | [] -> List.rev acc
          | p :: tl -> pattern_of_pstList tl (pattern_of_pst p :: acc)
        in
        StructPattern (s, pattern_of_pstList ps [])
      end
     | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))

let pattern_of_string s =
  s
  |> Pstparser.pst_of_string
  |> pattern_of_pst



(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
     try
       Int (int_of_string sym)
     with
       Failure _ ->
       match sym with
       | "true" -> Bool true
       | "false" -> Bool false
       | "nil" -> Nil
       | _ -> if String.get sym 0 = '\''
              then Symbol (String.sub sym 1 (String.length sym - 1))
              else Var sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "+", [left; right] -> Add (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "+", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "if", [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
     | Pst.Symbol "if", _ -> raise (AbstractSyntaxError ("'if' special form expects 3 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "-", [left; right] -> Sub (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "-", _ -> raise (AbstractSyntaxError ("operator - expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "*", [left; right] -> Mul (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "*", _ -> raise (AbstractSyntaxError ("operator * expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "=", [left; right] -> Eq (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "=", _ -> raise (AbstractSyntaxError ("operator = expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons", [left; right] -> Cons (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "cons", _ -> raise (AbstractSyntaxError ("operator cons expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "let", [Pst.Node dl; body] -> begin 
        let rec processLetDefList l acc =
          match l with 
          | [] -> acc (* don't reverse acc list since order doesn't matter for let expressions*)
          | (Pst.Node [Pst.Symbol x; e]) :: tl -> if not (List.mem_assoc x acc)
                                                  then processLetDefList tl ((x, expr_of_pst e):: acc)
                                                  else raise (AbstractSyntaxError ("Cannot define same variable twice in one let binding " ^ x))
          | _ -> raise (AbstractSyntaxError ("Operator let expects 2 args with first being (defs) with defs being a"
                                              ^ " sequence of psts in the form (x, e) where x is an arbitrary symbol and"
                                              ^ " e is an arbitrary expression.  However, got " ^ Pst.string_of_pst p))
        in Let (processLetDefList dl [], expr_of_pst body)
       end
     | Pst.Symbol "let", _ -> raise (AbstractSyntaxError ("operator let expects 2 args with first being of the form '((x, expr))' but got " 
                                                          ^ Pst.string_of_pst p))
     | Pst.Symbol "nil?", [e] -> IsNil (expr_of_pst e)
     | Pst.Symbol "nil?", _ -> raise (AbstractSyntaxError ("operator nil? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons?", [e] -> IsCons (expr_of_pst e)
     | Pst.Symbol "cons?", _ -> raise (AbstractSyntaxError ("operator cons? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "car", [e] -> Car (expr_of_pst e)
     | Pst.Symbol "car", _ -> raise (AbstractSyntaxError ("operator car expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cdr", [e] -> Cdr (expr_of_pst e)
     | Pst.Symbol "cdr", _ -> raise (AbstractSyntaxError ("operator cdr expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cond", cl -> begin 
        let rec parseClauseList l acc =
          match l with
          | [] -> List.rev acc (* reverse since clause order matters *)
          | (Pst.Node [e1; e2]) :: tl -> parseClauseList tl ((expr_of_pst e1, expr_of_pst e2):: acc)
          | _ -> raise (AbstractSyntaxError ("operator cond expects arguements of the form (e1 e2) but got " ^ Pst.string_of_pst p))
        in Cond (parseClauseList cl [])
       end
     | Pst.Symbol "print", [e] -> Print (expr_of_pst e)
     | Pst.Symbol "print", _ -> raise (AbstractSyntaxError ("operator print expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "lambda", [Node params; body] -> begin 
        let rec checkSignature l acc =
          match l with
          | [] -> List.rev acc
          | (Pst.Symbol x) :: tl -> if not (List.mem x acc) 
                                    then checkSignature tl (x :: acc)
                                    else raise (AbstractSyntaxError ("Lambda cannot have same name for multiple params " ^ Pst.string_of_pst p))
          | _ -> raise (AbstractSyntaxError ("Lambda parameter must be a symbol" ^ Pst.string_of_pst p))
        in Lambda {rec_name = None; lambda_param_names = checkSignature params []; lambda_body = expr_of_pst body}
      end
     | Pst.Symbol "lambda", _ -> raise (AbstractSyntaxError ("operator lambda expects 2 args with first being a node but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "match", scr :: cl -> begin
        let rec vars_of_pattern p =
          match p with
           | VarPattern x -> [x]
           | ConsPattern (p1, p2) -> vars_of_pattern p1 @ vars_of_pattern p2
           | StructPattern (_, hd :: tl) -> vars_of_pattern hd @ vars_of_pattern (StructPattern ("dummy", tl))
           | _ -> []
        in
        let rec checkNoRepeats l acc = (* returns true if list doesn't have repeats*)
          match l with
          | [] -> true
          | hd :: tl -> if List.mem hd acc then false else checkNoRepeats tl (hd :: acc)
        in
        let rec parseClauseList l acc =
          match l with
          | [] -> List.rev acc (* reverse since clause order matters *)
          | (Pst.Node [ptrn; e]) :: tl -> let ptrn = pattern_of_pst ptrn in
                                        if checkNoRepeats (vars_of_pattern ptrn) [] then parseClauseList tl ((ptrn, expr_of_pst e):: acc)
                                        else raise (AbstractSyntaxError ("Pattern cannot have repeat variable names. In " ^ Pst.string_of_pst p))
          | _ -> raise (AbstractSyntaxError ("operator match expects arguments of the form (p e) with p being a pattern and e an expression but got " ^ Pst.string_of_pst p))
        in
        Match (expr_of_pst scr, parseClauseList cl [])
      end
     | Pst.Symbol "match", _ -> raise (AbstractSyntaxError ("operator match expects at least 1 arg but got " ^ Pst.string_of_pst p))
     | f, args -> let rec processArgs l acc = 
                    (match l with
                     | []       -> List.rev acc
                     | hd :: tl -> processArgs tl (expr_of_pst hd :: acc))
                  in Call (expr_of_pst f, processArgs args [])

let expr_of_string s =
  s
  |> Pstparser.pst_of_string
  |> expr_of_pst


let binding_of_pst p =
  match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "define", [Pst.Symbol lhs_var; rhs] -> VarBinding (lhs_var, expr_of_pst rhs)
     | Pst.Symbol "define", [Pst.Node (Pst.Symbol nm :: params); body] -> begin 
        let rec checkSignature l acc =
          match l with
          | [] -> List.rev acc
          | (Pst.Symbol x) :: tl -> if not (List.mem x acc || x = nm) 
                                    then checkSignature tl (x :: acc)
                                    else raise (AbstractSyntaxError ("Function cannot have same name for multiple params "
                                                ^ "or param with same name as function " ^ Pst.string_of_pst p))
          | _ -> raise (AbstractSyntaxError ("Function parameter must be a symbol" ^ Pst.string_of_pst p))
        in FunctionBinding {func_name = nm; param_names = checkSignature params []; body = expr_of_pst body}
       end
     | Pst.Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ Pst.string_of_pst p))
     | Pst.Symbol "test", [e] -> TestBinding (expr_of_pst e)
     | Pst.Symbol "test", _  -> raise (AbstractSyntaxError("This test is malformed " ^ Pst.string_of_pst p))
     | Pst.Symbol "struct", (Pst.Symbol name) :: fns -> begin 
      let rec checkFieldNames l acc =
        match l with
        | [] -> List.rev acc
        | (Pst.Symbol x) :: tl -> if not (List.mem x acc)
                                  then checkFieldNames tl (x :: acc)
                                  else raise (AbstractSyntaxError ("Struct cannot have same name for multiple fields " ^ Pst.string_of_pst p))
        | _ -> raise (AbstractSyntaxError ("Struct field name must be a symbol" ^ Pst.string_of_pst p))
      in StructBinding {struct_name = name; field_names = checkFieldNames fns []}
     end
     | Pst.Symbol "struct", _ -> raise (AbstractSyntaxError("This struct is malformed " ^ Pst.string_of_pst p))
     | Pst.Node _, _ -> raise (AbstractSyntaxError("Expected binding to start with a symbol but got " ^ Pst.string_of_pst p))
     | _ -> TopLevelExpr (expr_of_pst p)

let binding_of_string s =
  s
  |> Pstparser.pst_of_string
  |> binding_of_pst

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst ->
       binding_of_pst pst :: parse_binding_list ()
  in
  parse_binding_list ()
