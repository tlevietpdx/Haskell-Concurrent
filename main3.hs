import Control.Exception (try, SomeException)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (replicateM_, forever, when)
import Data.List (nub)

-- Define a transactional variable (TVar)
data TVar a = TVar (MVar a)

-- Function to create a new TVar
newTVar :: a -> IO (TVar a)
newTVar val = TVar <$> newMVar val

-- Function to read from a TVar
readTVar :: TVar a -> IO a
readTVar (TVar mvar) = readMVar mvar

-- Function to write to a TVar
writeTVar :: TVar a -> a -> IO ()
writeTVar (TVar mvar) val = modifyMVar_ mvar (\_ -> return val)

-- Stage 2
-- Define a simple STM monad
newtype STM a = STM { runSTM :: IO a }

-- The atomically function to execute STM actions
atomically :: STM a -> IO a
atomically (STM action) = do
    result <- try action
    case result of
      Right val -> return val
      Left (_ :: SomeException) -> atomically (STM action)  -- Retry on failure

-- Functions to lift TVar operations into STM
readTVarSTM :: TVar a -> STM a
readTVarSTM tvar = STM $ readTVar tvar

writeTVarSTM :: TVar a -> a -> STM ()
writeTVarSTM tvar val = STM $ writeTVar tvar val

-- Make STM an instance of Functor, Applicative, and Monad
instance Functor STM where
    fmap f (STM action) = STM (fmap f action)

instance Applicative STM where
    pure x = STM (pure x)
    (STM f) <*> (STM x) = STM (f <*> x)

instance Monad STM where
    return x = STM (return x)
    (STM action) >>= f = STM $ do
        result <- action
        runSTM (f result)

-- Stage 3
-- Define a job queue using TVar
type JobQueue = TVar [String]

-- Submit a job to the queue
submitJobSTM :: JobQueue -> String -> STM ()
submitJobSTM queue job = do
    jobs <- readTVarSTM queue
    writeTVarSTM queue (jobs ++ [job])

-- Process a job from the queue
processJobSTM :: JobQueue -> STM (Maybe String)
processJobSTM queue = do
    jobs <- readTVarSTM queue
    case jobs of
      [] -> return Nothing
      (job:rest) -> do
        writeTVarSTM queue rest
        return (Just job)

-- Concurrently process jobs using custom STM
processJobsConcurrentlySTM :: JobQueue -> IO ()
processJobsConcurrentlySTM queue = atomically $ do
    mJob <- processJobSTM queue
    case mJob of
      Just job -> STM $ putStrLn ("Processing job: " ++ job)
      Nothing  -> return ()

-- Test case to test concurrency in the job queue
testConcurrentJobProcessing :: IO ()
testConcurrentJobProcessing = do
    -- Step 1: Create an empty job queue
    queue <- newTVar []

    -- Step 2: Spawn multiple threads to concurrently submit jobs
    let numJobs = 10  -- Total number of jobs to submit
    putStrLn "Submitting jobs concurrently..."
    replicateM_ 10 $ forkIO $ replicateM_ (numJobs `div` 10) $ do
        atomically $ submitJobSTM queue "TestJob"

    -- Step 3: Spawn multiple threads to process jobs concurrently
    putStrLn "Processing jobs concurrently..."
    replicateM_ 5 $ forkIO $ replicateM_ (numJobs `div` 5) $ processJobsConcurrentlySTM queue

    -- Step 4: Allow time for job submission and processing
    threadDelay 2000000  -- Wait for 2 seconds to allow all jobs to be processed

    -- Step 5: Check if all jobs were processed
    jobsRemaining <- readTVar queue
    putStrLn $ "Remaining jobs: " ++ show jobsRemaining

    -- Since we've submitted `numJobs` jobs, the queue should be empty after all are processed
    when (null jobsRemaining) $
        putStrLn "All jobs processed. PASS"
    when (not (null jobsRemaining)) $
        putStrLn "Some jobs were not processed. FAIL"
