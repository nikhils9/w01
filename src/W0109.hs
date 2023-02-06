{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module W0109 where

import Control.Monad
import Data.Map

-- Reader Impl

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    m >>= cont =
        Reader (\r ->
            runReader (cont $ runReader m r) r
        )

data Config =
    Config {prec :: Int}

showDouble :: Double -> Int -> Double
showDouble d p =
    (fromIntegral (floor (d * (10^p :: Double))) :: Double) / 10^p

showDoubleReader :: Double -> Reader Config Double
showDoubleReader d =
    Reader (\cfg -> 
        let
            p = prec cfg
        in
            (fromIntegral (floor (d * (10^p :: Double))) :: Double) / 10^p
    )

showDouble' :: MonadReader Config m => Double -> m Double 
showDouble' d = do
    cfg <- ask
    let
        p = 10 ^ prec cfg 
    return $ ((fromIntegral (floor (d * p))) :: Double) / p

div' :: MonadError String m => Double -> Double -> m Double
div' m n =
    case n of
        0 -> throwError "denominator cannot be zero"
        _ -> return $ m / n

divDef :: MonadError String m => Double -> Double -> Double -> m Double
divDef m n d =
    catchError (div' m n) (\_ -> return d)
    
doublePrec :: Config -> Config
doublePrec c = Config $ prec c * 2

-- MonadReader Impl

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance MonadReader r (Reader r) where
    ask :: Reader r r
    ask = Reader id

    local :: (r -> r) -> Reader r a -> Reader r a
    local f c = Reader $ runReader c . f 

instance MonadReader r ((->) r) where
    ask :: r -> r
    ask = id

    local :: (r -> r) -> (r -> a) -> (r -> a)
    local f c = c . f

-- MonadError Impl

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
    throwError :: e -> Either e a
    throwError = Left

    catchError :: Either e a -> (e -> Either e a) -> Either e a
    catchError c h =
        case c of
            Left e -> h e
            Right v -> Right v

-- ReaderT Impl

type Env = Map String Int

data EnvErr e a = EnvErr {runEnvErr :: Env -> Either e a}

-- EnvErr e a = ReaderT Env (Either e) a
-- Reader r a = ReaderT r Identity a

newtype ReaderT r m a = 
    ReaderT {runReaderT :: r -> m a} 

instance Monad m => Functor (ReaderT r m) where
    fmap = liftM

instance Monad m => Applicative (ReaderT r m) where
    pure = return 
    (<*>) = ap

instance Monad m => Monad (ReaderT r m) where
    return :: a -> ReaderT r m a
    return a = ReaderT (\_ -> return a)

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    m >>= cont =
        ReaderT (\r ->
            runReaderT m r >>= \a -> runReaderT (cont a) r)

instance Monad m => MonadReader r (ReaderT r m) where
    ask :: ReaderT r m r
    ask = ReaderT (\r -> return r)

    local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
    local f c = 
        ReaderT (\r ->
            runReaderT c $ f r)

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError :: e -> ReaderT r m a
    throwError e = ReaderT (\_ -> throwError e)

    -- throwError' e = lift $ throwError e Using MonadTrans

    catchError :: ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
    catchError c h =
        ReaderT (\r ->
            catchError (runReaderT c r) (\e -> runReaderT (h e) r))

instance MonadState s m => MonadState s (ReaderT r m) where
    get :: ReaderT r m s
    get = lift get

    put :: s -> ReaderT r m ()
    put = lift . put

-- ExceptT Impl

newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}

instance Monad m => Functor (ExceptT e m) where
    fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
    fmap = liftM

instance Monad m => Applicative (ExceptT e m) where
    pure :: a -> ExceptT e m a
    pure = return

    (<*>) :: ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
    (<*>) = ap

instance Monad m => Monad (ExceptT e m) where
    return :: a -> ExceptT e m a
    return a = ExceptT (return $ Right a)

    (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
    c >>= cont =
        ExceptT (
            runExceptT c >>= 
            (\e -> 
                case e of
                    Right a -> runExceptT $ cont a
                    Left err -> return $ Left err
            )
        )
        
instance Monad m => MonadError e (ExceptT e m) where
    throwError :: e -> ExceptT e m a
    throwError e = ExceptT (return $ Left e)

    catchError :: ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
    catchError c h =
        ExceptT (
            runExceptT c >>= \v ->
                case v of
                    Right _ -> return v
                    Left e -> runExceptT $ h e
        )
    
instance MonadReader r m => MonadReader r (ExceptT e m) where
    ask :: ExceptT e m r
    ask = ExceptT (liftM Right ask)

    local :: (r -> r) -> ExceptT e m a -> ExceptT e m a
    local f c =
        ExceptT (
            runExceptT c >>= 
                \v ->
                    case v of
                        Right a -> liftM Right $ local f $ return a
                        Left e -> return $ Left e
        )

instance MonadState s m => MonadState s (ExceptT e m) where
    get :: ExceptT e m s
    get = lift get

    put :: s -> ExceptT e m ()
    put s = lift $ put s

-- MonadIO Impl

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO :: IO a -> ReaderT r m a
    liftIO a =
        ReaderT (\_ -> liftIO a)

ex :: ReaderT Config IO Double
ex = do
    m <- liftIO (readLn :: IO Double)
    n <- liftIO (readLn :: IO Double)
    r <- showDouble' $ m + n
    liftIO $ putStrLn $ "The result is: " ++ show r
    return r

-- MonadState Impl

class Monad m => MonadState s m | m -> s where
    get :: m s

    put :: s -> m ()

-- StateT Impl

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Monad m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap = liftM

instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure = return

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return a = StateT (\s -> return (a, s) )

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    c >>= cont =
        StateT (\s ->
            runStateT c s >>= \(a, s') ->
                runStateT (cont a) s')

instance Monad m => MonadState s (StateT s m) where
    get :: StateT s m s
    get = StateT (\s -> return (s, s))

    put :: s -> StateT s m ()
    put s = StateT (\_ -> return ((), s))

instance MonadReader r m => MonadReader r (StateT s m) where
    ask :: StateT s m r
    ask = lift ask

    local :: (r -> r) -> StateT s m a -> StateT s m a
    local f c = StateT (\s ->
        local f (runStateT c s))

instance MonadError e m => MonadError e (StateT s m) where
    throwError :: e -> StateT s m a
    throwError e = lift $ throwError e

    catchError :: StateT s m a -> (e -> StateT s m a) -> StateT s m a
    catchError c h =
        StateT (\s ->
            catchError (runStateT c s) (\e -> runStateT (h e) s)
        )

-- MonadTrans Impl

class MonadTrans t where
    lift ::Monad m => m a -> t m a

instance MonadTrans (ReaderT r) where
    lift :: Monad m => m a -> ReaderT r m a
    lift m = ReaderT (\_ -> m)

instance MonadTrans (ExceptT e) where
    lift :: Monad m => m a -> ExceptT e m a
    lift m = ExceptT (liftM Right m)

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    lift m = StateT (\s -> liftM (, s) m)

-- Compose Impl

newtype Compose f g a = Compose (f (g a)) deriving (Show)

decompose :: Compose f g a -> f (g a)
decompose (Compose v) = v

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f c =
        Compose ((f <$>) <$> (decompose c))

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ pure $ pure a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    x <*> y =
        Compose $ (pure (<*>)) <*> (decompose x) <*> (decompose y)