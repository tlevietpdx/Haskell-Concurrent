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
                threadDelay 5000000 -- Add a delay of 5 second
                return val

-- Function to write to a TVar
writeTVar :: TVar a -> a -> IO ()
writeTVar (TVar mvar) val = modifyMVar_ mvar (\_ -> return val)

-- Test function 
testTVar :: IO () 
testTVar = do 
    tvar <- newTVar 0 
    putStrLn "Initial value (with delay):" 
    readTVar tvar >>= print 
    
    putStrLn "Writing 42 to TVar..." 
    writeTVar tvar 42 
    putStrLn "Value after writing 42 (with delay):" 
    readTVar tvar >>= print 
    
    putStrLn "Writing 100 to TVar..." 
    writeTVar tvar 100 
    putStrLn "Value after writing 100 (with delay):" 
    readTVar tvar >>= print 
    
    -- Simulating concurrent writes 
    putStrLn "Testing concurrent writes..." 
    replicateM_ 5 $ forkIO $ writeTVar tvar 200 
    putStrLn "Value after concurrent writes (with delay):" 
    readTVar tvar >>= print
