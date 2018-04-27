module Compiler where

import Clike.Language as Clike
import Clike.Text as Clike
import LL.Language as LL
import LL.Text as LL
import Text

import Data.List

import Debug.Trace

{-------------------------------------------------------------------------------

Entry point: compile Clike programs into LLVM programs.

Note: there is significant overlap of constructor names between the LLVM and
Clike syntax trees.  You should prefix constructors with LL. or Clike. to
distinguish them.

-------------------------------------------------------------------------------}

compileProgram :: Clike.Prog -> LL.Prog
compileProgram fdecls = P { types = [], globals = [], functions = map compileFunction fdecls, externs = [] }

{-------------------------------------------------------------------------------

Compiles Clike functions into LLVM functions.

This is the starting point of your implementation.  So long as this bit
corresponds to the results of the interpreter, you'll receive credit.

-------------------------------------------------------------------------------}

-- compileFunction (FunDecl fty name [(aty, arg)] [Stmt]) = (name, ([(aty, arg)], fty, (Block, [(String, Block)])))
compileFunction :: Clike.TopDecl -> (String, LL.Function)
compileFunction = error "unimplemented"
-- compileFunction (FunDecl fty name [(aty, arg)] [Stmt]) = (name, ([(aty, arg)], fty, (Block, [(String, Block)])))
--                     where stmts = compileStatements ...


compileStatements :: [Clike.Stmt] -> String -> String -> String -> Int -> (([(String, LL.Block)], String), Int)
compileStatements stmts continueLabel breakLabel nextBlockLabel i =
    foldr (\stmt ((blocks, nextBlockLabel), i) ->
               let ((blocks', entry), i') = compileStatement stmt continueLabel breakLabel nextBlockLabel i in
               ((blocks' ++ blocks, entry), i'))
          (([], nextBlockLabel), i)
          stmts

{-------------------------------------------------------------------------------

My compileStatement function has the following interpretation:

    compileStatement stmt continueLabel breakLabel nextBlockLabel i

`stmt` is the statement to be compiled.  `continueLabel` is the label to jump to
in case of a continue.  `breakLabel` is the label to jump to in case of a break.
`nextBlockLabel` is the label to jump to should control exit normally.  Finally,
`i` is an integer used to generate fresh names.

My compileStatement returns three things: a list of blocks generated, the label
to use to enter those blocks, and finally the updated integer after generating
fresh names.

-------------------------------------------------------------------------------}
-- clike.Stmt = ExpS Expr | Return Expr | Break | Continue
--           | If Expr Stmt Stmt | While Expr Stmt
--           | Block [Stmt] | Decl Type String
-- LL.Terminator
-- = Ret Type (Maybe Operand)
-- | Bra String
-- | CBr Operand String String
-- i = 5;
compileStatement :: Clike.Stmt -> String -> String -> String -> Int -> (([(String, LL.Block)], String), Int)
-- compileStatement = error "unimplemented"
-- compileStatement (ExpS exp) continueLabel breakLabel nextBlockLabel i = (([(instrs,blk)], nextBlockLabel), updatedi)
--                             where blk = (instrs, term)
--                                   instrs = ...
compileStatement (Return exp) continueLabel breakLabel nextBlockLabel i = (([(nextBlockLabel,(opInstrs, Ret I64 (Just opLlop)))], nextBlockLabel), i')
                                where ((opInstrs, opLlop), i') = compileExpression exp i
-- compileStatement Break continueLabel breakLabel nextBlockLabel i = (([(instrs,blk)], s), updatedi)
-- compileStatement Continue continueLabel breakLabel nextBlockLabel i = (([(instrs,blk)], s), updatedi)
-- compileStatement (If exp stmt1 stmt2) continueLabel breakLabel nextBlockLabel i = (([(instrs,blk)], s), updatedi)
-- compileStatement (While exp stmt) continueLabel breakLabel nextBlockLabel i = (([(instrs,blk)], s), updatedi)
-- compileStatement (Block stmts) continueLabel breakLabel nextBlockLabel i = (([(instrs,blk)], s), updatedi)
-- compileStatement (Decl ty strngs) continueLabel breakLabel nextBlockLabel i = (([(instrs,blk)], s), updatedi)

{-------------------------------------------------------------------------------

compileExpression is simpler than compileStatement, because you don't have to
worry about flow of control.  You do, however, have to worry about un-nesting
nested expressions.  In the type below, the Int argument is for generating fresh
names.  The result includes the sequence of instructions used to compute the
expression, the LLVM operand containing the result of the expression, and the
updated integer after generating fresh names.  For example, suppose you had:

    compileExpression (Bin Plus (Bin Times (OpE (Const 3))
                                           (OpE (Const 5)))
                                (OpE (Const 6))
                      4

I would expect this to produce results similar to:

    (([LL.Bin "__x4" LL.Times (LL.Const 3) (LL.Const 5),
       LL.Bin "__x5" LL.Times (LL.Uid "__x4") (LL.Const 6)],
      LL.Uid "__x5"),
     6)

-------------------------------------------------------------------------------}

compileExpression :: Clike.Expr -> Int -> (([LL.Instruction], LL.Operand), Int)
compileExpression (OpE op) i = compileOperand op (i+1)
compileExpression (Clike.Bin binOptr exp1 exp2) i = ((op1Instrs ++ op2Instrs ++ [instrs], operand), i2+1)
                where ((op1Instrs, op1Llop), i1) = compileExpression exp1 i
                      ((op2Instrs, op2Llop), i2) = compileExpression exp2 i1
                      (operand, instrs) = instr binOptr
                      instr Plus         = (Uid (show i2), LL.Bin (show i2) Add I64 op1Llop op2Llop)
                      instr Minus        = (Uid (show i2), LL.Bin (show i2) Sub I64 op1Llop op2Llop)
                      instr Times        = (Uid (show i2), LL.Bin (show i2) Mul I64 op1Llop op2Llop)
                      instr Clike.And    = (Uid (show i2), LL.Bin (show i2) LL.And I64 op1Llop op2Llop)
                      instr Clike.Or     = (Uid (show i2), LL.Bin (show i2) LL.Or I64 op1Llop op2Llop)
                      instr Clike.Xor    = (Uid (show i2), LL.Bin (show i2) LL.Xor I64 op1Llop op2Llop)
                      instr Clike.Shl    = (Uid (show i2), LL.Bin (show i2) LL.Shl I64 op1Llop op2Llop)
                      instr Clike.Ashr   = (Uid (show i2), LL.Bin (show i2) LL.Ashr I64 op1Llop op2Llop)
                      instr Clike.Lshr   = (Uid (show i2), LL.Bin (show i2) LL.Lshr I64 op1Llop op2Llop)
                      instr Clike.Eq     = (Uid (show i2), LL.Icmp (show i2) LL.Eq I64 op1Llop op2Llop)
                      instr Clike.Neq    = (Uid (show i2), LL.Icmp (show i2) LL.Neq I64 op1Llop op2Llop)
                      instr Clike.Lt     = (Uid (show i2), LL.Icmp (show i2) LL.Lt I64 op1Llop op2Llop)
                      instr Clike.Lte    = (Uid (show i2), LL.Icmp (show i2) LL.Le I64 op1Llop op2Llop)
                      instr Clike.Gt     = (Uid (show i2), LL.Icmp (show i2) LL.Gt I64 op1Llop op2Llop)
                      instr Clike.Gte    = (Uid (show i2), LL.Icmp (show i2) LL.Ge I64 op1Llop op2Llop)
                      instr Clike.Assign = (op2Llop, LL.Store I64 op1Llop op2Llop)
compileExpression (Unary Negate exp ) i = (([ LL.Bin (show i1)  Mul I64 opLlop (LL.Const (-1))], (Uid (show i1))), i1+1) -- Mult by -1
                    where ((opInstrs, opLlop), i1) = compileExpression exp i
compileExpression (Unary Complement exp ) i = (([ LL.Bin (show i1)  LL.Xor I64 opLlop (LL.Const (-1))], (Uid (show i1))), i1+1) -- Xor by -1
                    where ((opInstrs, opLlop), i1) = compileExpression exp i
compileExpression (Clike.Call s exps) i = ((instrs ++ [LL.Call (show i) I64 s (map (\ops -> (I64, ops)) argOps)], Uid (show i)), i+1)
                    where (instrs, argOps, i') = compileArgs exps i
                          compileArgs (arg:args) i = (instrs ++ instrs', op : ops, i'')
                                     where ((instrs, op), i') = compileExpression arg i 
                                           (instrs', ops, i'') = compileArgs args i'

{-------------------------------------------------------------------------------

compileOperand has identical behavior to compileExpression.  Note that, as Clike
variables support assignment, unlike LLVM temporaries, you will need to store
your Clike variables on the stack and use Store/Load to get to them.

-------------------------------------------------------------------------------}

compileOperand :: Clike.Operand -> Int -> (([LL.Instruction], LL.Operand), Int)
compileOperand (Var s) i = (([ (Load (show i) I64 (Uid s)) ], Uid (show i)), i+1)
compileOperand (Clike.Const i64) i = (([ (Load (show i) I64 (Uid (show i))) ], Uid (show i)), i+1)
-- compileOperand (Dot cOp s) i = (([  (Load (show i) I64 (Uid s)) ], Uid (show i)), i+1)
                
