{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Types
import Lib
import EP

main :: IO ()
main = someFunc

u = undefined


-- We can lift ML values to our language

true = project bool (interpretclosed (EId "true"))
-- true == True
p = project int (interpretclosed (EApp (EId "*") (EPair (EI 5)(EI 5))))
-- p == 25

ierr = project int (interpretclosed (EApp (EApp (EId "iter") (EId "suc")) (EI 5)))
i = project int (interpretclosed (EApp (EApp (EApp (EId "iter") (EI 2)) (EId "suc")) (EI 5)))



ep1 :: EP a -> EP b -> EP ( a ->  b)
ep1 = \a b -> a --> b
ep2 = \a b -> ((a --> b) --> a --> b) --> a --> b


--Yfx=fYfx
embY0 :: Exp
embY0 = ELam "f" (EApp
                   (ELam "g"  (EApp (EId "f") (ELam "a" (EApp (EApp (EId "g")(EId "g"))(EId "a")))))
                   (ELam "g"  (EApp (EId "f") (ELam "a" (EApp (EApp (EId "g")(EId "g"))(EId "a"))))))

--Yf=fYf
embY :: Exp
embY = ELam "f" (EApp (ELam "x" (EApp (EId "f") (EApp(EId "x")(EId "x"))))
                      (ELam "x" (EApp (EId "f") (EApp(EId "x")(EId "x")))))

polyY :: EP a -> EP b -> ((a  -> b)  -> a  -> b)  -> (a  -> b)
polyY = \a b -> project (((a --> b) --> a --> b) --> (a --> b)) (interpretclosed embY)

polyY0 :: EP a -> EP b -> ((a  -> b)  -> a  -> b)  -> (a  -> b)
polyY0 = \a b -> project (((a --> b) --> a --> b) --> (a --> b)) (interpretclosed embY0)

factorial :: Int -> Int
factorial = polyY int int (\r n -> if n == 0 then 1 else n * r (n - 1) )

factorial0 :: Int -> Int
factorial0 = polyY0 int int (\r n -> if n == 0 then 1 else n * r (n - 1) )

thisis = factorial 5
thisis0 = factorial0 5

