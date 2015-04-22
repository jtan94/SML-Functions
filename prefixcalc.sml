(*Jessica Tan*)

exception eofException;
datatype
  AST = add' of AST * AST
      | sub' of AST * AST
      | prod' of AST * AST
      | div' of AST * AST
      | negate' of AST
      | integer' of int
      | store' of AST
      | recall';


val memory = ref 0;
fun constants(add'(t1, t2)) = constants (t1) @ (constants t2)
  | constants(sub'(t1, t2)) = (constants t1) @ (constants t2)
  | constants(prod'(t1, t2)) = (constants t1) @ (constants t2)
  | constants(div'(t1, t2)) = (constants t1) @ (constants t2)
  | constants(negate'(t)) = constants t
  | constants(store'(t)) = constants t
  | constants(recall') = []
  | constants(integer'(x)) = [x];


fun helper (E, l) = 
  let
    val constants = l
    fun indexhelper (x, []) = raise Subscript
      | indexhelper (x, h::t) =
        if x = h then 0 else 1 + indexhelper (x, t)
    fun codegen(add'(t1,t2)) =
        (codegen t1) ^ (codegen t2) ^ "BINARY_ADD\n"
    | codegen(sub'(t1,t2)) =
        (codegen t1) ^ (codegen t2) ^ "BINARY_SUBTRACT\n"
    | codegen(prod'(t1,t2)) =
        (codegen t1) ^ (codegen t2) ^ "BINARY_MULTIPLY\n"
    | codegen(div'(t1,t2)) =
        (codegen t1) ^ (codegen t2) ^ "BINARY_TRUE_DIVIDE\n"
    | codegen(negate'(t)) =
       "LOAD_CONST 1\n" ^ codegen(t) ^ "BINARY_SUBTRACT\n"
    | codegen(store'(t)) = 
      (codegen(t))^ "STORE_FAST 0\n" ^ "LOAD_FAST 0\n"
    | codegen(recall') = "LOAD_FAST 0\n"
    | codegen(integer'(x)) = 
    let
      val index = indexhelper(x, constants) + 2
    in
      "LOAD_CONST " ^ Int.toString(index) ^ "\n"
    end
  in
    codegen(E)
  end
 
 fun E ("+"::rest) =
  let val (ast1,rest1) = E(rest)
    val (ast2,rest2) = E(rest1)
  in
    (add'(ast1,ast2),rest2)
  end
  | E ("-"::rest) =
  let val (ast1,rest1) = E(rest)
    val (ast2,rest2) = E(rest1)
  in
    (sub'(ast1,ast2),rest2)
  end
  | E ("*"::rest) =
  let val (ast1,rest1) = E(rest)
    val (ast2,rest2) = E(rest1)
  in
    (prod'(ast1,ast2),rest2)
  end
  | E ("/"::rest) =
  let val (ast1,rest1) = E(rest)
    val (ast2,rest2) = E(rest1)
  in
    (div'(ast1,ast2),rest2)
  end
  | E ("~"::rest) =
  let val (ast,rest1) = E(rest)
  in
    (negate'(ast),rest1)
  end
   | E ("S"::rest) =
  let val (ast,rest1) = E(rest)
  in
    (store'(ast),rest1)
  end
  | E ("R"::rest) = (recall',rest)
  | E (x::rest) =
    let val i = valOf(Int.fromString(x))
    in
      (integer'(i),rest)
    end
  | E nil = raise eofException;
  val memory = ref 0;


fun delimiter #" " = true
  | delimiter #"\t" = true
  | delimiter #"\n" = true
  | delimiter _ = false


fun run() =
  (TextIO.output(TextIO.stdOut,"Please enter a prefix calculator expression: ");
   TextIO.flushOut(TextIO.stdOut);
   let val line = TextIO.inputLine(TextIO.stdIn)
       val tokens = String.tokens delimiter (valOf line)
       val (ast,remainingTokens) = E(tokens)
       val constantL = constants ast;
       val outs = helper(ast, constantL); 
       val casmfile = TextIO.openOut("a.casm")
   in
    if length(remainingTokens) <> 0 then
       raise(eofException)
     else ();
     TextIO.output(casmfile, "Function: main/0\n");
     TextIO.output(casmfile, "Constants: 'The answer is:', 0" ^ List.foldr (fn (x, y) => ", " ^ (Int.toString(x) ^ y)) "\n" (constantL));
     TextIO.output(casmfile, "Locals: x\n");
     TextIO.output(casmfile, "Globals: print\n");
     TextIO.output(casmfile, "BEGIN\n");
     TextIO.output(casmfile, "LOAD_GLOBAL 0\n");
     TextIO.output(casmfile, "LOAD_CONST 0\n");
     TextIO.output(casmfile, outs); 
     TextIO.output(casmfile, "CALL_FUNCTION 2\n");
     TextIO.output(casmfile, "RETURN_VALUE\n");
     TextIO.output(casmfile, "END\n")
     

   end 
   handle eofException =>
             TextIO.output(TextIO.stdOut,
               "You entered an invalid prefix expression.\n")
          | Option =>
             TextIO.output(TextIO.stdOut,
               "You entered invalid characters in the prefix expression.\n"))

