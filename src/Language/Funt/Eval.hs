module Language.Funt.Eval where

import Language.Funt.Syntax ( Term(..), Literal(LitBool, LitNat) ) 
import Text.Megaparsec (runParser)
import Language.Funt.Parser (pTerm)
import Data.Text (Text)
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void (Void)

isVal (Value _) = True
isVal _ = False

valOf :: Term -> Maybe Literal
valOf (Value l) = Just l
valOf _ = Nothing

boolOf :: Term -> Maybe Bool
boolOf t = do
  l <- valOf t
  case l of (LitBool b) -> pure b; _ -> Nothing

intOf :: Term -> Maybe Integer
intOf t = do
  l <- valOf t
  case l of (LitNat i) -> pure i; _ -> Nothing

-- | Single step evaluation of a term @(t -> t')@
eval :: Term -> Term
eval v@(Value _) = v
eval (Succ (Value (LitNat n))) = Value (LitNat (n+1))
eval (Succ (Value _))          = error "Invalid literal for succ"
eval (Succ t)                  = eval (Succ (eval t))

eval (Pred (Value (LitNat 0))) = Value (LitNat 0)
eval (Pred (Value (LitNat n))) = Value (LitNat (n-1))
eval (Pred (Value _))          = error "Invalid literal type for pred"
eval (Pred t)                  = eval (Pred (eval t))

eval (IsZero (Value (LitNat 0))) = Value (LitBool True)
eval (IsZero (Value (LitNat _))) = Value (LitBool False)
eval (IsZero (Value _))          = Value (LitBool False)
eval (IsZero t)                  = eval (IsZero (eval t))

eval (IfThenElse (Value (LitBool True))  tExp _   ) = eval tExp
eval (IfThenElse (Value (LitBool False)) _    fExp) = eval fExp
eval (IfThenElse (Value _)               _    fExp) = 
  error "Invalid literal type for condition"
eval (IfThenElse cond tExp fExp) =
  eval $ IfThenElse (eval cond) tExp fExp

eval (Abs v t)   = error "abs not yet implemented"
eval (App t1 t2) = error "app not yet implemented"

exec :: Text -> Either (ParseErrorBundle Text Void) Term
exec s = do
  ast <- runParser pTerm "" s
  pure $ eval ast