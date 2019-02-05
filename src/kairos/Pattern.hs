{-# LANGUAGE DeriveFunctor #-}

module Kairos.Pattern where

import Kairos.IOI
import Kairos.Clock
import Kairos.Instrument
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Bifunctor
import Data.Maybe

data Eventf t a = Event { wholE :: t
                        , partE :: t
                        , action :: a
                        } deriving (Functor, Ord, Eq, Show)

type Event a = Eventf (IOIf Double) a

toEv :: (Beats,Beats) -> (Beats,Beats) -> a -> Event a
toEv (wo, wt) (po, pt) a = Event (IOI wo po) (IOI wt pt) a

instance Bifunctor Eventf where
  bimap f g (Event w p e) = Event (f w) (f p) (g e)

data Pattern a = Pattern {query :: (Status -> [Event a])}

data Status = Status { ioi :: IOI
                     , act :: PfMap --make data type for this
                     }

type PfMap = M.Map Int Pfield

instance Functor Pattern where
 fmap f p = p {query = (fmap(fmap f)) . query p}

instance Applicative Pattern where
 pure val = Pattern $ \(Status a _) -> 
   map (\a' -> Event a' (sect a a') val) $ barsIOIinIOI a

 (<*>) po pt = Pattern q 
   where q stat = catMaybes $ concat $ map match $ query po stat where 
             match (Event oW oP oA) = map (\(Event tW tP tA) -> do w' <- timeSpanIOI tW oW
                                                                   p' <- timeSpanIOI oP tP
                                                                   return $ Event w' p' (oA tA) 
                                          ) (query pt $ stat {ioi = pure (st oP)})

instance Monad Pattern where
  return = pure
  p >>= f = unwrap $ f <$> p

unwrap :: Pattern (Pattern a) -> Pattern a
unwrap pp = pp {query = q}
  where q stat = concatMap (\(Event w p val) -> catMaybes $ map (newSubE w p) $ query val stat { ioi = p }) (query pp stat) 
        newSubE w p (Event t pa a) = do
            nw <- timeSpanIOI w t
            np <- timeSpanIOI p pa
            return $ Event nw np a

fromJustPat :: Pattern (Maybe a) -> Pattern a
fromJustPat p = fromJust <$> (filterEvPat (isJust) p)

filterEvPat :: (a -> Bool) -> Pattern a -> Pattern a
filterEvPat f p = p {query = (filter (f . action)) . query p} 
