{-# LANGUAGE DeriveFunctor #-}

module Kairos.Pattern where

import Kairos.Base
import Kairos.TimePoint
import Kairos.Clock
import Kairos.Instrument
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Bifunctor
import Data.Maybe

data Eventf t a = Event { time :: t
                        , action :: a
                        } deriving (Functor, Ord, Eq, Show)

type Event a = Eventf (TimePointf Beats) a

--toEv :: (Beats,Beats) -> a -> Event a
--toEv (wo, wt) (po, pt) a = Event (IOI wo po) (IOI wt pt) a

instance Bifunctor Eventf where
  bimap f g (Event t e) = Event (f t) (g e)
{--
data Pattern a = Pattern {query :: (Status -> [Event a])}

data Status = Status { ioi :: TimePoint
                     , act :: PfMap --make data type for this
                     }

instance Functor Pattern where
 fmap f p = p {query = (fmap(fmap f)) . query p}

instance Applicative Pattern where
 pure val = Pattern $ \(Status a _) ->
   map (\a' -> Event a' (sect a a') val) $ barsTPinTP a

 (<*>) po pt = Pattern q
   where q stat = catMaybes $ concat $ map match $ query po stat where
             match (Event oT oA) = map (\(Event tT tA) -> do w' <- timeSpanInters tT oT
                                                            return $ Event w'  (oA tA)
                                          ) (query pt $ stat {ioi = pure (ioi oT)})

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
--}
