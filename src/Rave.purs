module Rave where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (class VariantShows, Variant, inj)
import Data.Variant.Internal (class VariantTags)
import Effect.Aff (Aff, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Prim.Row as R
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (get)
import Type.Row (RProxy)

-- | Short for "Reader, Aff, Variant."
newtype Rave r v a = Rave (ReaderT r (ExceptV v Aff) a)

derive newtype instance raveMonadAff :: MonadAff (Rave r v)
derive newtype instance raveMonadEffect :: MonadEffect (Rave r v)
derive newtype instance raveMonad :: Monad (Rave r v)
derive newtype instance raveApplicative :: Applicative (Rave r v)
derive newtype instance raveApply :: Apply (Rave r v)
derive newtype instance raveFunctor :: Functor (Rave r v)
derive newtype instance raveBind :: Bind (Rave r v)
derive newtype instance raveMonadError :: MonadThrow (Variant v) (Rave r v)

class VariantInjTagged a b | a -> b where
  injTagged :: Record a -> Variant b

instance variantInjTagged ::
  ( RowToList r1 (RL.Cons sym a RL.Nil)
  , R.Cons sym a () r1
  , R.Cons sym a rx r2
  , IsSymbol sym
  ) =>
  VariantInjTagged r1 r2 where
    injTagged = inj (SProxy :: SProxy sym) <<< get (SProxy :: SProxy sym)

throw :: forall m r1 r2 a.
  VariantInjTagged r1 r2 =>
  MonadThrow (Variant r2) m =>
  Record r1 ->
  m a
throw = throwError <<< injTagged

runRave :: forall v r rl a.
  RowToList v rl =>
  VariantTags rl =>
  VariantShows rl =>
  RProxy v ->
  Rave r v a ->
  r ->
  Aff a
runRave _ (Rave rave) r = do
  ran <- runExceptT $ runReaderT rave r
  case ran of
    Right res -> pure res
    Left l -> throwError $ error $ show l

liftRave :: forall m a r. MonadError Error m => m a -> ExceptV (liftedError :: Error | r) m a
liftRave e = do
  run <- lift $ try e
  case run of
    Right r -> pure r
    Left l -> throw { liftedError: l }

liftAffV :: forall r m a. MonadAff m => Aff a -> ExceptV (liftedError :: Error | r) m a
liftAffV e = do
  run <- liftAff $ try e
  case run of
    Right r -> pure r
    Left l -> throw { liftedError: l }

-- itV :: forall r.
--   RProxy r
--   String ->
--   ExceptV r Aff Unit ->
--   Spec Unit
-- itV name toRun = it name $ runAffV RProxy toRun
