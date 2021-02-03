{-# LANGUAGE ScopedTypeVariables #-}
module ProcessingTree where

import Control.Monad.Writer
import Control.Monad.Except
import FRP.Rhine
import Vocoder
import Vocoder.Filter

type STFTMSF m = MSF m [[STFTFrame]] [STFTFrame]

data ProcessingTree m = PTSource Int
                      | PTNamed String
                      | PTBind String (ProcessingTree m)
                      | PTMSF (MSF m [STFTFrame] [STFTFrame]) (ProcessingTree m)
                      | PTFilter (Filter m) (ProcessingTree m)
                      | PTBinary (STFTFrame -> STFTFrame -> STFTFrame) (ProcessingTree m) (ProcessingTree m)

elaboratePT :: forall m. Monad m
            => m FreqStep
            -> ProcessingTree m
            -> Maybe (STFTMSF m)
elaboratePT mfs t0 = either (const Nothing) Just r where
    (r, e0) = runWriter $ runExceptT $ g e0 t0
    g :: [(String, STFTMSF m)]
      -> ProcessingTree m
      -> ExceptT () (Writer [(String, STFTMSF m)]) (STFTMSF m)
    g _ (PTSource k) = return $ arr (!! k)
    g e (PTNamed n) | Just v <- lookup n e = return v
                    | otherwise = throwError ()
    g e (PTBind n t) = g e t >>= (\v -> tell [(n, v)] >> return v)
    g e (PTMSF f t) = (>>> f) <$> g e t
    g e (PTFilter f t) = (>>> arrM (\x -> mfs >>= forM x . f)) <$> g e t
    g e (PTBinary f t1 t2) = (\m1 m2 -> zipWith f <$> m1 <*> m2) <$> g e t1 <*> g e t2

numSourcesPT :: (ProcessingTree m) -> Int
numSourcesPT (PTSource k) = k+1
numSourcesPT (PTNamed _)  = 0
numSourcesPT (PTBind _ t) = numSourcesPT t
numSourcesPT (PTMSF _ t) = numSourcesPT t
numSourcesPT (PTFilter _ t) = numSourcesPT t
numSourcesPT (PTBinary _ t1 t2) = numSourcesPT t1 `max` numSourcesPT t2

