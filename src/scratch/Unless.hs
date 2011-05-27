module Unless where


data Unless a = Yes a | No deriving Show

unless :: Unless a -> (a -> Unless a) -> Unless a
unless No _ = No
unless input@(Yes a) f =
    let r = f a in
    case r of
      No -> input
      otherwise -> r
      
class MonadR d where
  returnR :: a -> d a
  (>>=<)  :: d a -> (a -> d a) -> d a

instance MonadR Unless where
  returnR = Yes
  (>>=<) = unless


instance MonadR Maybe where
  returnR = Just
  (>>=<) Nothing _ = Nothing
  (>>=<) (Just a) f =
      let r = f a in
      case r of 
        Nothing   -> Just a
        otherwise -> r                   

      
less' :: Integer -> Integer -> Unless Integer
less' x y
    | x  < y    = Yes x
    | otherwise = No                     
                                    
less :: Integer -> Integer -> Maybe Integer
less x y
    | x < y     = Just x
    | otherwise = Nothing
