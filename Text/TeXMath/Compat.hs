{-# LANGUAGE CPP #-}
module Text.TeXMath.Compat ( ExceptT
                           , Except
                           , MonadError
                           , runExceptT
                           , runExcept
                           , throwError
                           , catchError )
       where

#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except



#else
import Control.Monad.Error
import Control.Monad.Identity
type ExceptT = ErrorT
type Except e = ErrorT e Identity

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = runErrorT

runExcept :: Except e a -> Either e a
runExcept = runIdentity . runErrorT

#endif


