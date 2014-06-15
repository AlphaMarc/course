{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.Bind(
  Bind(..)
, (>>=)
, join
, (<=<)
) where

import Course.Core
import Course.Functor
import Course.Apply(Apply)
import Course.Id
import Course.List
import Course.Optional
import qualified Prelude as P

class Apply f => Bind f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
-- >>> Id (+10) <*> Id 8
-- Id 18
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
(<*>) ::
  Bind f =>
  f (a -> b)
  -> f a
  -> f b
-- f a   :: f (a -> b)
-- a     :: a -> b
-- (a-> b):: a -> b -> b
-- (<$>) :: (a -> b)   -> f a -> f b
-- (=<<) :: (a -> f b) -> f a -> f b
--             
(<*>) g func = (\f -> f <$> func) =<< g

infixl 4 <*>

-- | Binds a function on the Id monad.
--
-- >>> (\x -> Id(x+1)) =<< Id 2
-- Id 3
instance Bind Id where
  (=<<) f (Id a) = f a

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Bind List where
  (=<<) _ Nil     = Nil
  (=<<) g (x:.xs) = (g x) ++ (g =<< xs)
    -- foldRight (map f l) (:.) Nil

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Bind Optional where
  (=<<) _ Empty  = Empty
  (=<<) g (Full a) = (g a)

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Bind ((->) t) where
--  (a -> (t -> b))
--    -> (t -> a)
--    -> (t -> b)
  (=<<) g a = (\t -> g (a t) t) 

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Bind f =>
-- f (f a) -> f a
-- f a -> f b 
-- (<$>) :: (f a -> a)   -> f (f a) -> f a impossible
-- (=<<) :: (a -> f b) -> f a -> f b
-- (=<<) :: (f a -> f a) -> f (f a) -> f a
-- (<*>) :: f (f a -> a) -> f (f a) -> f a impossible
  f (f a)
  -> f a
join ffa = (\fa -> fa) =<< ffa

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
-- >>> ((+10) >>= (*)) 7
-- 119
(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) x f = join $ f <$> x

infixl 1 >>=

-- | Implement composition within the @Bind@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
(<=<) ::
  Bind f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
(<=<) f1 f2 a = join $ f1 <$> f2 a 

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Bind IO where
  (=<<) =
    (P.=<<)

instance Bind [] where
  (=<<) =
    (P.=<<)

instance Bind P.Maybe where
  (=<<) =
    (P.=<<)
