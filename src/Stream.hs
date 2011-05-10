{-# LANGUAGE TypeFamilies #-}

module Stream where

stream :: (p -> (e,p)) -> (e -> Bool) -> p -> [(e, p)]                                               
stream stepper continue p = next p
  where next p = 
          let (e, p') = stepper p
          in  (e, p') : if continue e then next p' else []
            
