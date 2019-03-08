open GT
open Language
open List  

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
                         
let eval _ = failwith "Not yet implemented" *)

	let rec eval_bl config prg = 
		let (stack, prime) = config in
		let (state, input, output) = prime in
		match prg with
	
		| BINOP op -> (Language.Expr.operator op (List.hd (List.tl stack)) (List.hd stack) :: (List.tl (List.tl stack)), prime)
		| CONST const -> (const :: stack, prime)
		| READ -> (List.hd input :: stack, (state, List.tl input, output))
		| WRITE -> (List.tl stack, (state, input, output @ [List.hd stack]))
		| LD var -> (state var :: stack, prime)
		| ST var -> (List.tl stack, (Language.Expr.update var (List.hd stack) state, input, output))
		
		let eval config prg = List.fold_left eval_bl config prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine

let compile _ = failwith "Not yet implemented" *)

	let rec compile_expr c_e = match c_e with
		| Language.Expr.Const const -> [CONST const]
		| Language.Expr.Var var -> [LD var]
		| Language.Expr.Binop (op, left, right) -> (compile_expr left)@(compile_expr right)@[BINOP op]

	let rec compile stmt = match stmt with
		| Language.Stmt.Read var -> [READ; ST var]
		| Language.Stmt.Write expr -> (compile_expr expr) @ [WRITE]
		| Language.Stmt.Assign (var, expr)   -> (compile_expr expr) @ [ST var]
		| Language.Stmt.Seq (s1, s2) -> (compile s1) @ (compile s2);;
