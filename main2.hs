import Control.Exception (try, SomeException)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (replicateM_)


-- Define a transactional variable (TVar)
data TVar a = TVar (MVar a)

-- Function to create a new TVar
newTVar :: a -> IO (TVar a)
newTVar val = TVar <$> newMVar val

-- Function to read from a TVar
readTVar :: TVar a -> IO a
readTVar (TVar mvar) = do 
                val <- readMVar mvar
                threadDelay 15000000 -- Add a delay of 5 second
                return val

-- Function to write to a TVar
writeTVar :: TVar a -> a -> IO ()
writeTVar (TVar mvar) val = modifyMVar_ mvar (\_ -> return val)

-- Stage 2 update
-- Define a simple STM monad
newtype STM a = STM { runSTM :: IO a }

-- The atomically function to execute STM actions
atomically :: STM a -> IO a
atomically (STM action) = do
    -- Wrap action in exception handling to simulate a retry mechanism
    result <- try action
    case result of
      Right val -> return val
      Left (_ :: SomeException) -> atomically (STM action)  -- Retry on failure

-- Functions to lift TVar operations into STM
readTVarSTM :: TVar a -> STM a
readTVarSTM tvar = STM $ readTVar tvar

writeTVarSTM :: TVar a -> a -> STM ()
writeTVarSTM tvar val = STM $ writeTVar tvar val

-- Test function 
testSTM :: IO () 
testSTM = do 
    tvar <- newTVar 0 
    putStrLn "Initial value (with delay):" 
    atomically (readTVarSTM tvar) >>= print 
    
    putStrLn "Writing 42 to TVar..." 
    atomically (writeTVarSTM tvar 42) 
    putStrLn "Value after writing 42 (with delay):" 
    atomically (readTVarSTM tvar) >>= print 

    putStrLn "Writing 100 to TVar..." 
    atomically (writeTVarSTM tvar 100) 
    putStrLn "Value after writing 100 (with delay):" 
    atomically (readTVarSTM tvar) >>= print 
    
    -- Simulating concurrent writes 
    putStrLn "Testing concurrent writes..." 
    replicateM_ 5 $ forkIO $ atomically (writeTVarSTM tvar 200) 
    putStrLn "Value after concurrent writes (with delay):" 
    atomically (readTVarSTM tvar) >>= print
