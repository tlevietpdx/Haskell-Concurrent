import Control.Concurrent
import Control.Monad (replicateM_, forever, when)
import Control.Concurrent.MVar
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- Define a job queue using MVar
type JobQueue = MVar [String]

-- Submit a job to the queue
submitJob :: JobQueue -> String -> IO ()
submitJob queue job = do
    jobs <- takeMVar queue  -- Take the current queue
    putMVar queue (jobs ++ [job])  -- Add job and put it back

-- Process a job from the queue
processJob :: JobQueue -> IO (Maybe String)
processJob queue = do
    jobs <- takeMVar queue  -- Take the current queue
    case jobs of
        [] -> do
            putMVar queue []  -- Empty the queue
            return Nothing
        (job:rest) -> do
            putMVar queue rest  -- Put back the remaining jobs
            return (Just job)

-- Concurrently process jobs using MVar
processJobsConcurrently :: JobQueue -> IO ()
processJobsConcurrently queue = forever $ do
    mJob <- processJob queue
    case mJob of
        Just job -> putStrLn ("Processing job: " ++ job)
        Nothing -> return ()  -- No more jobs to process

-- Test case to test concurrency in the job queue with timing
testConcurrentJobProcessing :: IO ()
testConcurrentJobProcessing = do
    -- Step 1: Create an empty job queue
    queue <- newMVar []

    -- Step 2: Record the start time of the execution
    startTime <- getCurrentTime

    -- Step 3: Spawn multiple threads to concurrently submit jobs
    let numJobs = 10  -- Total number of jobs to submit
    putStrLn "Submitting jobs concurrently..."
    replicateM_ 10 $ forkIO $ replicateM_ (numJobs `div` 10) $ do
        submitJob queue "TestJob"

    -- Step 4: Spawn multiple threads to process jobs concurrently
    -- putStrLn "Processing jobs concurrently..."
    replicateM_ 5 $ forkIO $ processJobsConcurrently queue

    -- Step 5: Continuously check the queue until it is empty
    -- putStrLn "Waiting for jobs to finish..."
    let checkJobs = do
          jobsRemaining <- readMVar queue
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
