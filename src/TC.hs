module TC where

import Data.List
import Data.Maybe

import Types


class EmbedProject a where
    embed :: a -> U
    project :: U -> a

instance EmbedProject Int where
    embed = UI
    project (UI i) = i

instance EmbedProject Bool where
    embed = UB
    project (UB b) = b

instance EmbedProject String where
    embed = US
    project (US s) = s

instance EmbedProject U where
    embed = id
    project = id

instance (EmbedProject a, EmbedProject b) => EmbedProject (a,b) where
    embed (a,b) = UP (embed a, embed b)
    project (UP (a,b)) = (project a,project b)

instance (EmbedProject a, EmbedProject b) => EmbedProject (a -> b) where
    embed f = UF (embed . f . project)
    project (UF f) = project . f . embed

iter :: U -> (Int -> U -> U) -> Int -> U
iter m f 0 = m
iter m f n = f n (iter m f (n - 1))

builtins = [("*", embed ((*) :: Int -> Int -> Int)),
            ("true",embed True),
            (">",embed ((>) :: Int -> Int -> Bool)),
            --("toString",embed (show :: U -> String)),
            ("iter",embed iter)]
            

--simplified using type classes: only need to call at type
eval (EId v) env = fromJust (lookup v env) -- position non fixee
eval (EI i) env = embed i
eval (ES s) env = embed s
eval (EB b) env = embed b
eval (EApp f x) env = project (eval f env) (eval x env)
eval (EPair a b) env = embed (eval a env,eval b env)
eval (ELet v e b) env = eval b ((v,eval e env):env)
eval (EIf c t e) env = if project (eval c env) then eval t env else eval e env
eval (ELam v b) env = embed (\x -> eval b ((v,x):env))
eval (ELetFun f v e b) env = eval b ((f,f'):env)
    where env' = (f,f'):env
          f' = embed (\x -> eval e ((v,x):env'))