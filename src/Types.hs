{-# LANGUAGE PatternSynonyms, GADTs #-}


module Types where


-- values. we interpret Exp into. "model of untyped CBV lambda calculus"

data U  = UF (U -> U) | UP (U,U) | UI Int | US String | UUnit | UB Bool


--- object language
data Exp =
  EId String                      -- (* identifier    *)
  | EI Int                        -- (* integer const *)
  | ES String                     -- (* string const  *)
  | EApp Exp Exp                  -- (* application   *)
  | EPair Exp Exp                 -- (* pairing       *)
  | ELet String Exp Exp           -- (* let binding   *)
  | EIf Exp Exp Exp               -- (* conditional   *)
  | ELam String Exp               -- (* abstraction   *)
  | ELetfun String String Exp Exp -- (* recursive fn  *)


