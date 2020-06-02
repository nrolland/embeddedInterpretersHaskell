module EP(EP, unit, bool, int, string, (**), (-->), embed, project) where


import Prelude hiding ((**))
import Types

data EP a = EP { embed :: a -> U, project :: U -> a }

unit :: EP ()
unit = EP (\() -> UUnit)(\(UUnit) -> ())

bool :: EP Bool
bool = EP UB (\(UB b) -> b)

int :: EP Int
int = EP UI (\(UI i) -> i)

string :: EP String
string = EP US (\(US s) -> s)


(**)   :: EP a -> EP b -> EP  (a, b)
(**) ea eb = EP (\(a,b)-> UP(embed ea a, embed eb b)) 
                (\(UP (ua, ub)) -> (project ea ua, project eb ub) )

infixr 8 -->
(-->)  :: EP a -> EP b -> EP (a -> b)
(-->) ea eb = EP (\f -> UF (embed eb . f . project ea)) (\(UF uf) -> project eb . uf . embed ea )



-- data Exp a where
--   Eunit :: Exp ()
--   Ebool :: Exp Bool
--   Eint :: Exp Int
--   Estring :: Exp String
--   Pair   :: Exp a -> Exp b -> Exp  (a, b)
--   Arr  :: Exp a -> Exp b -> Exp (a -> b)
