module LambdaCalc exposing (..)

import String.Interpolate exposing(interpolate)


      


-- Core Lambda stuff

type Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  | Err String



substitute : String -> Expr -> Expr -> Expr
substitute varname body rand =
  case body of
    Var localname ->
      if localname == varname
        then rand
        else Var(localname)
    Lam v1 expr ->
      Lam v1 (substitute varname expr rand)
    App localraptor localrand ->
      (App (substitute varname localraptor rand) (substitute varname localrand rand ) )
    Err string ->
      Err(string)





apply : Expr -> Expr
apply expr =
  case expr of
    Var _
      -> Err "Cannot apply Var"
    Lam _ _
      -> Err "Cannot apply Lam"
    App raptor rand ->
      case raptor of
         Var _ -> Err "raptor cannot be a Var, it has to be a Lambda"
         App _ _ -> Err "raptor cannot be an App, it has to be a Lambda"
         Err string   -> Err(string)
         Lam s raptorBody
          -> substitute s raptorBody rand
    Err string
      -> Err(string)
 



{-
Evaluation strategy:
to preform an application, we need to replace every unbound
instance of the argument name in the argument with a copy
of the rand.  We can keep a list of bound variable names
as we go down the tree and alpha substitute when we
needed to.
-}

printExpr : Expr -> String
printExpr expr =
  case expr of
    Var string -> string
    Lam var body -> interpolate "(λ{0}.{1})" [var, printExpr body]
    App raptor rand -> interpolate "({0} {1})" [printExpr raptor, printExpr rand]
    Err e -> e



store : String -> Expr
store key =
  case key of
    "id"
      -> Lam "z" (Var "z")
    "1"
      -> Lam "f" (Lam "x" (App (Var "f") (Var "x")))
    "2"
      -> Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (Var "x"))))
    "3"
      -> Lam "f" (Lam "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))
    "Y"
      -> Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "y" (App (Var "f") (App (Var "y") (Var "y")))))
    "Omega"
      -> App (Lam "x" (App (Var "x") (Var "x"))) (Lam "x" (App (Var "x") (Var "x")))
    _
      -> Err "Empty"