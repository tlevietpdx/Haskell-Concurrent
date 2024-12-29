import Control.Exception (SomeException, try)
import Control.Concurrent
import Control.Monad (replicateM_, forever, when)
import Control.Concurrent.MVar
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Define a transactional variable (TVar) for custom STM
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
-- Define STM monad for custom STM
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

-- Submit a job to the queue in STM
submitJobSTM :: JobQueue -> String -> STM ()
submitJobSTM queue job = do
    jobs <- readTVarSTM queue
    writeTVarSTM queue (jobs ++ [job])

-- Process a job from the queue in STM
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
processJobsConcurrentlySTM queue = forever $ do
    mJob <- atomically $ processJobSTM queue
    case mJob of
        Just job -> putStrLn ("Processing job: " ++ job)
        Nothing -> return ()  -- No more jobs to process

-- Stage 4
-- Test case to test concurrency in the custom STM job queue with timing
testCustomSTMJobProcessing :: IO ()
testCustomSTMJobProcessing = do
    -- Step 1: Create an empty job queue
    queue <- newTVar []

    -- Step 2: Record the start time of the execution
    startTime <- getCurrentTime

    -- Step 3: Spawn multiple threads to concurrently submit jobs
    let numJobs = 10  -- Total number of jobs to submit
    putStrLn "Submitting jobs concurrently..."
    replicateM_ 10 $ forkIO $ replicateM_ (numJobs `div` 10) $ do
        atomically $ submitJobSTM queue "TestJob"

    -- Step 4: Spawn multiple threads to process jobs concurrently
    -- putStrLn "Processing jobs concurrently..."
    replicateM_ 5 $ forkIO $ processJobsConcurrentlySTM queue

    -- Step 5: Continuously check the queue until it is empty
    -- putStrLn "Waiting for jobs to finish..."
    let checkJobs = do
          jobsRemaining <- readTVar queue
          if null jobsRemaining then
            putStrLn "All jobs processed. PASS"
          else do
            putStrLn $ "Remaining jobs: " ++ show jobsRemaining
            threadDelay 1000  -- Check every 1ms
            checkJobs

    -- Start checking the jobs until they're processed
    checkJobs

    -- Step 6: Record the end time of the execution
    endTime <- getCurrentTime

    -- Step 7: Calculate and print the elapsed time
    let elapsedTime = diffUTCTime endTime startTime
    putStrLn $ "Elapsed time: " ++ show elapsedTime
