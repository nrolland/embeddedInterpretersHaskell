module Lib where

import Data.List
import Data.Maybe
import Data.Function

import Prelude hiding ((**))
import Types
import EP


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Dans le meta language
iter :: (Eq t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
iter n f x = if n == 0 then x else f (iter (n-1) f x)


builtins = [ ("true", embed bool True),
             ("*"   , embed ((int ** int) -->int) (uncurry (*) :: (Int,Int) -> Int)),
             ("iter", embed (int --> (int --> int) --> int --> int) ( iter :: Int -> (Int -> Int) -> Int -> Int)), -- accessible au language object
             ("suc", embed (int --> int) ( (\n -> n + 1 ):: Int -> Int)) -- idem : meta-language vers objet
           ]

lookupb :: String -> U
lookupb s = lookup s builtins & fromJust

u = undefined

--  Figure 1; this is fairly standard, though note the binding- time separation: rather than a single environment of type (string*U) list, the interpreter takes a static environment of type string list and produces a function consuming a matching dynamic environment of type U list. The case for identifiers calls Builtins.lookup, which we will define shortly.


type Staticenv = [String]
type Dynamicenv = [U]


indexof :: Staticenv -> String -> Maybe Int
indexof names x = findIndex ((==) x) names

--(* val interpret : Exp*staticenv -> dynamicenv -> U *)    -- evaluateur du language objet vers U
interpret :: (Exp, Staticenv) -> Dynamicenv -> U
interpret (e,static) = case e of
   EId s ->  let on = indexof static s -- static ne cherche qu'a recuperer la position
             in on & maybe (\dynamic -> lookupb s)
                           (\n -> \dynamic -> dynamic !! n) -- qui sera celle de la valeur
   EI n -> (\dynamic -> UI n)
   EB b -> (\dynamic -> UB b)
   ES s -> (\dynamic -> US s)
   EApp e1 e2  -> let s1 = interpret (e1,static)
                      s2 = interpret (e2,static)
                   in \dynamic -> let UF(f) = s1 dynamic
                                      a = s2 dynamic
                                  in f a
   EPair e1 e2 -> \dynamic -> UP(interpret (e1, static) dynamic, interpret (e2, static) dynamic )
   ELet x b n   ->  let s1 = interpret(b, static)
                        s2 = interpret(n, x : static)
                    in \dynamic -> let v = s1 dynamic
                                   in s2 (v:dynamic)
   EIf eb e1 e2   -> let sb = interpret (eb, static)
                         s1 = interpret (e1, static)
                         s2 = interpret (e2, static)
                     in \dynamic -> let UB(b) = sb dynamic
                                    in if b then s1 dynamic else s2 dynamic
   ELam x ebody -> let sebody = interpret (ebody, x:static)
                     in \dynamic -> UF $ \v -> sebody (v:dynamic) -- la valeur de x sera en 1e position sur la pile
                                                                  -- la fonction construite depend elle meme d'autres
                                                                  -- variables
   ELetFun f x e1 e2 -> -- letrec f x = e1 in e2
                    let s1 = interpret (e1, x:f:static)
                        s2 = interpret (e2,f:static)
                    in \dynamic -> let g v = s1 (v:UF(g):dynamic) -- g a acces a g
                                   in s2 (UF(g):dynamic)          -- la suite a acces a g

interpretclosed :: Exp -> U
interpretclosed e = interpret (e,[]) []
