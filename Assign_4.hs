{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2021
-}
module Assign_4 where

import Test.QuickCheck

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "kim370"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Add (MathExpr a) (MathExpr a)
  | Mult (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -  Finds the value of the equation at value v
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   X        | MathExpr Input                           
 - |   v        | Float Input                             
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   a        | Float Output                               
 - -----------------------------------------------------------------
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval (Add x y) v = eval x v + eval y v
eval (Mult x y) v = eval x v * eval y v
eval (Power x y) v = eval x v ^^ y
eval (Cos x) v = cos (eval x v)
eval (Sin x) v = sin (eval x v)
eval (Abs x) v = abs (eval x v)
eval (Coef x) v = x
eval X v = v

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -  Converts regular math expressions to MathExpr
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   F        | Num Input                           
 - |   v        | Float Input                             
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   a        | MathExpr Output                               
 - -----------------------------------------------------------------
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Add x y
  x * y         = Mult x y
  negate x      = Mult (Coef(-1)) x
  abs x         = Abs x
  fromInteger i = Coef(fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -  Converts Fractional to Frational MathExpr
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   F        | Fractional Input                   
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   a        | Fractional MathExpr Output                         
 - -----------------------------------------------------------------
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -  Converts Float to Floating MathExpr
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   F        | Float Input                   
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   a        | Floating MathExpr Output                         
 - -----------------------------------------------------------------
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin x    = Sin x
  cos x   = Cos x
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -  Differentiates MathExpr
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   F        | MathExpr Input                   
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   a        | MathExpr Output                         
 - -----------------------------------------------------------------
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff (Add x y) = diff x + diff y
diff (Mult x y) = Mult (diff x) y + Mult x (diff y)
diff (Power x y) = Mult x (Power x (y-1)) * diff x
diff (Cos x) = (Coef(-1) * Sin x) * diff x
diff (Sin x) = Cos x * diff x
diff (Abs x) = (x * recip(Abs x)) * diff x
diff (Coef x) = Coef 0
diff x = Coef 1

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 - Converts MathExpr equation to string
 - -----------------------------------------------------------------
 - |   Input    |                                              
 - |   F        | MathExpr Input                   
 - -----------------------------------------------------------------
 - |   Output   |                                              
 - |   a        | String Output                         
 - -----------------------------------------------------------------
 -}
-- NOTE: you'll have to test pretty yourself
pretty :: (Show a) => MathExpr a -> String
pretty (Add u0 u1) = "(" ++ pretty u0 ++ " + " ++ pretty u1 ++ ")"
pretty (Mult u0 u1) = "(" ++ pretty u0 ++ " * " ++ pretty u1 ++ ")"
pretty (Power u0 d) = "(" ++ pretty u0 ++ " ^^ (" ++ show d ++ "))"
pretty (Cos u0) = "cos(" ++ pretty u0 ++ ")"
pretty (Sin u0) = "sin(" ++ pretty u0 ++ ")"
pretty (Abs u0) = "abs(" ++ pretty u0 ++ ")"
pretty (Coef c) = "(" ++ show c ++ ")"
pretty c = show c


{-
----------------------------------------------------------
Function: eval
Test Case Number: 1
Input: (Add X 5) 5.0
Expected Output: 7.0
Actual Output: 7.0

Function: eval
Test Case Number: 2
Input: (Mult X 9) 7.0
Expected Output: 63.0
Actual Output: 63.0

Function: eval
Test Case Number: 3
Input: (Power X 2) 2.0
Expected Output: 4.0
Actual Output: 4.0


----------------------------------------------------------
Function: diff
Test Case Number: 1
Input: Sin 5
Expected Output: Mult (Cos X) (Coef 1.0)
Actual Output: Mult (Cos X) (Coef 1.0)

Function: diff
Test Case Number: 2
Input: Mult X 7
Expected Output: Add (Mult (Coef 0.0) X) (Mult (Coef 5.0) (Coef 1.0))
Actual Output: Add (Mult (Coef 0.0) X) (Mult (Coef 5.0) (Coef 1.0))

Function: diff
Test Case Number: 3
Input: Abs X
Expected Output: Mult (Mult X (Power (Abs X) (-1))) (Coef 1.0)
Actual Output: Mult (Mult X (Power (Abs X) (-1))) (Coef 1.0)


----------------------------------------------------------
Function: pretty
Test Case Number: 1
Input: Mult (Mult (Coef (-1.0)) (Sin X)) (Coef 1.0)
Expected Output: "(((-1.0) * sin(X)) * (1.0))"
Actual Output: "(((-1.0) * sin(X)) * (1.0))"

Function: pretty
Test Case Number: 2
Input: Add X 5
Expected Output: "(X + (5))"
Actual Output: "(X + (5))"

Function: pretty
Test Case Number: 3
Input: Mult (Cos (Coef X)) (Coef (Coef 0.0))
Expected Output: "(cos((X)) * (Coef 0.0))"
Actual Output: "(cos((X)) * (Coef 0.0))"
-}





{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
- Function: eval
- Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
- Actual Test Result: Pass
-}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0

{-
- Function: eval
- Property: eval (Func3 Mult (Coef x) X) y is correct for all x,y
- Actual Test Result: Pass
-}
evalProp1 :: (Float,Float) -> Bool
evalProp1 (x,y) = (x * y) =~ eval (Mult (Coef x) X) y

runEvalProp1 :: IO ()
runEvalProp1 = quickCheck  evalProp1


{-
- Function: diff
- Property: diff (Func5 (Sin x)) is correct for all x,y
- Actual Test Result: Pass
-}
diffProp0 :: (Float, Float) -> Bool
diffProp0 (x,y) = eval (Cos X) y =~ eval (diff (Sin X)) y

runDiffProp0 :: IO ()
runDiffProp0 = quickCheck  diffProp0