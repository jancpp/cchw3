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

compileFunction :: Clike.TopDecl -> (String, LL.Function)
compileFunction = error "unimplemented"



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


compileStatement :: Clike.Stmt -> String -> String -> String -> Int -> (([(String, LL.Block)], String), Int)
compileStatement = error "unimplemented"

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
compileExpression = error "unimplemented"

{-------------------------------------------------------------------------------

compileOperand has identical behavior to compileExpression.  Note that, as Clike
variables support assignment, unlike LLVM temporaries, you will need to store
your Clike variables on the stack and use Store/Load to get to them.

-------------------------------------------------------------------------------}


compileOperand :: Clike.Operand -> Int -> (([LL.Instruction], LL.Operand), Int)
compileOperand = error "unimplemented"
