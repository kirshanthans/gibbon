{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module TestRunner where

import           Control.Monad
import           Data.Maybe ( catMaybes )
import           Data.Foldable
import           Data.List
import           Data.Scientific
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Time.LocalTime
import           Data.Yaml as Y
import           Options.Applicative as OA hiding (empty, str)
import           Options.Applicative.Types (readerAsk)
import           System.Clock
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Printf

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

{-

TestRunner
~~~~~~~~~~

This is a simple script which compiles and tests the Gibbon examples.

(1) Read the config file to build a TestRun.

(2) Run all tests specified by the config.

(3) If it compiles and produces a answer, we use "diff" to check if the output
    matches the answer generated by Racket.

    (TestRunner uses 'make answers' to generate expected answers.)

(4) Depending on (3), put the result in the appropriate bucket:
    - Expected pass
    - Unexpected pass
    - Expected failure
    - Unexpected failure

(5) Print out a summary at the end (and write the results to gibbon-test-summary.txt).


Comparing answers
~~~~~~~~~~~~~~~~~

If we're running a benchmark i.e anything that produces a BATCHTIME or SELFTIMED,
the Gibbon output may not match the answer generated by Racket.
We ignore such failures for now. Later, we could add a "bench" mode, which ensures that
the delta is within some reasonable range (like nofib).

TODOs:
(1) Could we somehow use tasty-golden to compare the output with the answer files ?
(2) ...


Running performance tests
~~~~~~~~~~~~~~~~~~~~~~~~~

Some of the tests are actually benchmarks, and we should treat them as such.
If a test is actually a benchmark, we run it with a larger SIZE parameter (eg. 25),
and measure the median SELFTIMED across N trials. Then, we compare this with the expected
time, modulo a reasonable delta, and fail if it doesn't match.

TODO: check more things here, #allocs, size of the buffers etc.

-}

--------------------------------------------------------------------------------
-- A single test

data Test = Test
    { name :: String
    , dir  :: FilePath
    , expectedResults :: M.Map Mode Result
    , skip :: Bool
    , runModes :: [Mode] -- ^ If non-empty, run this test only in the specified modes.
    , isBenchmark :: Bool
    , numTrials :: Int
    , sizeParam :: Int
    , moreIters :: [Mode] -- ^ In these modes, this test would require lots of iterations to
                          -- keep it running for measurable amount of time.

    , test_flags :: [String]   -- ^ We don't parse the flags here but instead pass them onto
                               -- Gibbon as is.

    -- Temporary hack until we can generate output in both Racket and Haskell
    -- syntax. #t => True etc.
    , mb_anspath :: Maybe String -- ^ Override the default answer file.

    -- Used in BenchWithDataset mode
    , isMegaBench :: Bool
    , benchFun    :: String
    , benchInput  :: FilePath
    }
  deriving (Show, Eq, Read)

defaultTest :: Test
defaultTest = Test
    { name = ""
    , dir = "examples"
    , expectedResults = M.fromList (zip allModes (repeat Pass))
    , skip = False
    , runModes = []
    , isBenchmark = False
    , numTrials = 9
    , sizeParam = 25
    , moreIters = []
    , test_flags = []
    , mb_anspath = Nothing
    , isMegaBench = False
    , benchFun   = "bench"
    , benchInput = ""
    }

instance Ord Test where
    compare t1 t2 = compare (name t1) (name t2)

instance FromJSON Test where
    parseJSON (Y.Object o) = do
        name <- o .: "name"
        dir  <- o .:? "dir" .!= (dir defaultTest)
        skip <- o .:? "skip" .!= (skip defaultTest)
        failing  <- o .:? "failing" .!= []
        runmodes <- o .:? "run-modes" .!= (runModes defaultTest)
        isbenchmark <- o .:? "bench" .!= (isBenchmark defaultTest)
        trials      <- o .:? "trials" .!= (numTrials defaultTest)
        sizeparam   <- o .:? "size-param" .!= (sizeParam defaultTest)
        moreiters   <- o .:? "more-iters" .!= (moreIters defaultTest)
        test_flags  <- o .:? "test-flags" .!= (test_flags defaultTest)
        mbanspath   <- o .:? "answer-file" .!= (mb_anspath defaultTest)
        megabench   <- o .:? "mega-bench" .!= (isMegaBench defaultTest)
        benchfun    <- o .:? "bench-fun" .!= (benchFun defaultTest)
        benchinput  <- o .:? "bench-input" .!= (benchInput defaultTest)
        let expectedFailures = M.fromList [(mode, Fail) | mode <- failing]
            -- Overlay the expected failures on top of the defaults.
            expected = M.union expectedFailures (expectedResults defaultTest)
        return $ Test name dir expected skip runmodes isbenchmark trials sizeparam moreiters test_flags mbanspath megabench benchfun benchinput
    parseJSON oth = error $ "Cannot parse Test: " ++ show oth

data Result = Pass | Fail
  deriving (Show, Eq, Read, Ord)

-- Not used atm.
-- | Gibbon mode to run programs in
data Mode = Gibbon3 | Gibbon2 | Pointer | Interp1 | Gibbon1
  deriving (Show, Eq, Read, Ord, Bounded, Enum)

instance FromJSON Mode where
    parseJSON (Y.String s) = return $ readMode s
    parseJSON oth = error $ "Cannot parse Mode: " ++ show oth

allModes :: [Mode]
allModes = [minBound ..]

readMode :: T.Text -> Mode
readMode s =
    case T.toLower s of
        "gibbon3" -> Gibbon3
        "gibbon2" -> Gibbon2
        "pointer" -> Pointer
        "interp1" -> Interp1
        "gibbon1" -> Gibbon1
        _ -> error $ "readMode: " ++ show s

-- Must match the flag expected by Gibbon.
modeRunFlags :: Mode -> [String]
modeRunFlags Gibbon3  = ["--run", "--packed"]
modeRunFlags Gibbon2  = ["--run", "--packed", "--nongen-gc"]
modeRunFlags Pointer = ["--run", "--pointer"]
modeRunFlags Interp1 = ["--interp1"]
modeRunFlags Gibbon1 = ["--run", "--packed", "--gibbon1"]

-- Must match the flag expected by Gibbon.
modeExeFlags :: Mode -> [String]
modeExeFlags Gibbon3 = ["--to-exe", "--packed"]
modeExeFlags Gibbon2 = ["--to-exe", "--packed", "--nongen-gc"]
modeExeFlags Pointer = ["--to-exe", "--pointer"]
modeExeFlags Interp1 = error "Cannot compile in Interp1 mode."
modeExeFlags Gibbon1 = ["--to-exe", "--packed", "--gibbon1"]

modeFileSuffix :: Mode -> String
modeFileSuffix Gibbon3  = "_gibbon3"
modeFileSuffix Gibbon2  = "_gibbon2"
modeFileSuffix Pointer = "_ptr"
modeFileSuffix Interp1 = "_interp1"
modeFileSuffix Gibbon1 = "_gibbon1"

-- Couldn't figure out how to write a parser which accepts multiple arguments.
-- The 'many' thing cannot be used with an option. I suppose that just
-- having modes trailing at the end, or as flags is also acceptable.
-- This needs to go away soon.
-- > readM_modes gibbon2 = [Gibbon2]
-- > readM_modes "gibbon2, pointer" = [Gibbon2, Pointer]
-- > readM_modes "gibbon2, gibbon2" = [Gibbon2]
readM_modes :: ReadM [Mode]
readM_modes = do
    str <- readerAsk
    let txt = T.pack str
        split = T.splitOn "," txt
        clean = map T.strip split
    mapM (return . readMode) (nub clean)

-- This doesn't have to be a new datatype. But it makes parsing the YAML file easier.
-- We peel off this layer later.
data Tests = Tests [Test]
  deriving (Show, Eq, Read, Ord)

instance FromJSON Tests where
    parseJSON (Y.Object o) = do
        tests <- o .:? "tests" .!= []
        return $ Tests tests
    parseJSON oth = error $ "Cannot parse Tests: " ++ show oth

-- | The things we care about when running benchmarks.
newtype BenchResult = BenchResult Double
    deriving (Show, Eq, Read, Ord)

-- Ugh.. just parse it by hand for now
readBenchResult :: String -> BenchResult
readBenchResult str =
    if isInfixOf "BATCHTIME" str
    then readIterate
    else readTime
  where
    readIterate =
        let (_iters:_size:_batchtime:selftimed:_oth) = lines str
            timing_info = selftimed \\ "SELFTIMED: "
        in BenchResult (toRealFloat $ read timing_info)

    -- Read the output of Gibbon (time).
    readTime =
        let (_size:selftimed:_oth) = lines str
            timing_info = selftimed \\ "SELFTIMED: "
        in BenchResult (toRealFloat $ read timing_info)

{-

This is a lame way to parse files. But good enough for now.
A perf file cotanins some results for each "mode". And each stanza is separated by a newline.

    Gibbon1:
    --------
    SIZE: 25
    SELFTIMED: 0.218558
-}
readPerfFile :: FilePath -> IO (M.Map Mode BenchResult)
readPerfFile fp = do
    str <- readFile fp
    let stanza_indices = findIndices (== 'X') str
        pairs = partition' 2 1 (stanza_indices ++ [length str])
        stanzas = map (\[a,b] -> drop a $ take b str) pairs
        perf_res = map (\(h:_:br) -> (readHeader h, readBenchResult (intercalate "\n" br))) $
                   map lines stanzas
    return $ M.fromList perf_res

  where
    readHeader :: String -> Mode
    readHeader = readMode . T.pack . drop 2 . init

    -- https://stackoverflow.com/a/16762033
    -- Like Clojure partition: https://clojuredocs.org/clojure.core/partition
    partition' :: Int -> Int -> [a] -> [[a]]
    partition' size offset
      | size <= 0   = error "partition': size must be positive"
      | offset <= 0 = error "partition': offset must be positive"
      | otherwise   = loop
      where
        loop :: [a] -> [[a]]
        loop xs = case splitAt size xs of
                    -- If the second part is empty, we're at the end. But we might
                    -- have gotten less than we asked for, hence the check.
                    (ys, []) -> if length ys == size then [ys] else []
                    (ys, _ ) -> ys : loop (drop offset xs)

medianBenchResult :: Int -> [BenchResult] -> BenchResult
medianBenchResult numTrials ls =
    (sortOn (\(BenchResult time) -> time) ls) !! (quot numTrials 2)

-- t1: actual, t2: expected
-- | Compare results, and return an error if they differ (modulo a reasonable delta)
diffBenchResult :: BenchResult -> BenchResult -> Maybe String
diffBenchResult (BenchResult t1) (BenchResult t2) =
    if delta > acceptableTimeDelta
    then Just $ printf "%.2f%% %s. ACTUAL/EXPECTED: %e/%e" (100 * delta) speed t1 t2
    else Nothing
  where
    delta = abs $ (t1 - t2) / t2

    speed :: String
    speed = if t1 > t2 then "slower" else "faster"

-- | A 5% error is acceptable when comparing timing info.
acceptableTimeDelta :: Double
acceptableTimeDelta = 0.05

--------------------------------------------------------------------------------
-- Test configuration

data TestConfig = TestConfig
    { skipFailing :: Bool     -- ^ Don't run the expected failures.
    , verbosity   :: Int      -- ^ Ranges from [0..5], and is passed on to Gibbon
    , testSummaryFile :: FilePath -- ^ File in which to store the test summary
    , tempdir     :: FilePath -- ^ Temporary directory to store the build artifacts
    , gRunModes   :: [Mode]   -- ^ When not empty, only run the tests in these modes.
                              --   It's a global parameter i.e it affects ALL tests.
                              --   However, if a corresponding parameter is specified
                              --   for a particular test, that has higher precedence.
    , checkPerf   :: Bool     -- ^ Should we also run the performance tests ?
    , onlyPerf    :: Bool     -- ^ If true, only run the benchmarks.
    , recordBenchmarks :: Bool -- ^ If true, record the results of running fresh benchmarks. Used via BenchRunner.
    , benchSummaryFile :: FilePath -- ^ A CSV file in which to store the benchmarks.
    }
  deriving (Show, Eq, Read, Ord)

defaultTestConfig :: TestConfig
defaultTestConfig = TestConfig
    { skipFailing = False
    , verbosity   = 1
    , testSummaryFile = "gibbon-test-summary.txt"
    , tempdir     = "examples/build_tmp"
    , gRunModes   = []
    , checkPerf   = False
    , onlyPerf    = False
    , recordBenchmarks = False
    , benchSummaryFile = "gibbon-benchmarks-summary.csv"
    }

instance FromJSON TestConfig where
    parseJSON (Y.Object o) = TestConfig <$>
                                 o .:? "skip-failing" .!= (skipFailing defaultTestConfig) <*>
                                 o .:? "verbosity"    .!= (verbosity defaultTestConfig)   <*>
                                 o .:? "test-summary-file" .!= (testSummaryFile defaultTestConfig) <*>
                                 o .:? "tempdir"      .!= (tempdir defaultTestConfig)     <*>
                                 o .:? "run-modes"    .!= (gRunModes defaultTestConfig)   <*>
                                 o .:? "check-perf"   .!= (checkPerf defaultTestConfig)   <*>
                                 o .:? "only-perf"    .!= (onlyPerf defaultTestConfig)    <*>
                                 o .:? "run-benchmarks"   .!= (recordBenchmarks defaultTestConfig) <*>
                                 o .:? "bench-summary-file" .!= (benchSummaryFile defaultTestConfig)
    parseJSON oth = error $ "Cannot parse TestConfig: " ++ show oth

-- Accept a default test config as a fallback, either 'DefaultTestConfig',
-- or read from the config file.
configParser :: TestConfig -> OA.Parser TestConfig
configParser dtc = TestConfig
                   <$> switch (long "skip-failing" <>
                               help "Skip tests in the error/ directory." <>
                               showDefault)
                   <*> option auto (short 'v' <>
                                    help "Verbosity level." <>
                                    showDefault <>
                                    value (verbosity dtc))
                   <*> strOption (long "test-summary-file" <>
                                  help "File in which to store the test summary" <>
                                  showDefault <>
                                  value (testSummaryFile dtc))
                   <*> strOption (long "tempdir" <>
                                  help "Temporary directory to store the build artifacts" <>
                                  showDefault <>
                                  value (tempdir dtc))
                   <*> option readM_modes (long "run-modes" <>
                                           help "Only run the tests in these modes" <>
                                           value (gRunModes dtc))
                   <*> switch (long "check-perf" <>
                               help "Run performance tests." <>
                               showDefault)
                   <*> switch (long "only-perf" <>
                               help "*Only* run performance tests." <>
                               showDefault)
                   <*> switch (long "run-benchmarks" <>
                               help "Record the results of running fresh benchmarks" <>
                               showDefault)
                   <*> strOption (long "bench-summary-file" <>
                                  help "A CSV file in which to store the benchmarks." <>
                                  showDefault <>
                                  value (benchSummaryFile dtc))

--------------------------------------------------------------------------------

-- | Details related to a single run of the testsuite.
data TestRun = TestRun
    { tests :: [Test]
    , startTime :: TimeSpec
    , expectedPasses :: M.Map String [Mode]
    , unexpectedPasses :: M.Map String [Mode]
    , expectedFailures :: M.Map String [(Mode, String)]
    , unexpectedFailures :: M.Map String [(Mode, String)]
    , skipped :: [String]
    }
  deriving (Show, Eq, Read, Ord)

clk :: Clock
clk = Realtime

getTestRun :: Tests -> IO TestRun
getTestRun (Tests tests) = do
    time <- getTime clk
    return $ TestRun
        { tests = tests
        , startTime = time
        , expectedPasses = M.empty
        , unexpectedPasses = M.empty
        , expectedFailures = M.empty
        , unexpectedFailures = M.empty
        , skipped = []
        }

--------------------------------------------------------------------------------
-- The main event

data TestVerdict
    = EP -- ^ Expected pass
    | UP -- ^ Unexpected pass
    | EF String -- ^ Expected failure
    | UF String -- ^ Unexpected failure
  deriving (Eq, Read, Ord)

instance Show TestVerdict where
    show EP = "Expected pass"
    show UP = "Unexpected pass"
    show (EF s) = "Expected failure\n" ++ s
    show (UF s) = "Unexpected failure\n" ++ s

runTests :: TestConfig -> TestRun -> IO TestRun
runTests tc tr = do
    ls <- if (onlyPerf tc)
          then do
              putStrLn "Only running performance tests."
              return $ filter isBenchmark (tests tr)
          else return $ filter (not . isMegaBench) (tests tr)
    putStrLn "Running tests...\n"
    foldlM (\acc t -> go t acc) tr (sort ls)
  where
    go test acc =
        if skip test
        then return (acc { skipped = (name test):(skipped acc) })
        else do
            -- Check if the global gRunModes or the test specific runModes was modified
            let test' = case (runModes test, gRunModes tc) of
                            -- Nothing was globally modified
                            (_,[])  -> test { runModes = allModes }
                            -- The tests doesn't specify an override, but there's a global override
                            ([],ms) -> test { runModes = ms }
                            -- There's a global override, but the one specified for a test
                            -- has higher precedence
                            _ -> test { runModes = allModes }
            results <- if isBenchmark test' && (checkPerf tc || onlyPerf tc)
                       then runBenchmark tc test'
                       else runTest tc test'
            let extend = M.insertWith (++) (name test')
            foldrM
                (\(mode,res) acc2 ->
                     return $ case res of
                        EP -> acc2 { expectedPasses = extend [mode] (expectedPasses acc2) }
                        UP -> acc2 { unexpectedPasses = extend [mode] (unexpectedPasses acc2) }
                        EF err -> acc2 { expectedFailures = extend [(mode,err)] (expectedFailures acc2) }
                        UF err -> acc2 { unexpectedFailures = extend [(mode,err)] (unexpectedFailures acc2) })
                acc results

runTest :: TestConfig -> Test -> IO [(Mode,TestVerdict)]
runTest tc Test{name,dir,expectedResults,runModes,mb_anspath,test_flags} = do
    dbgFlushIt' tc 2 (name ++ "...") (".")
    ls <- mapM (\(m,e) -> go m e) (M.toList $ M.restrictKeys expectedResults (S.fromList runModes))
    let msgs = catMaybes $ map error_msg ls
    if (msgs == [])
    then dbgFlushItDoc tc 2 (ok_doc <> "\n")
    else dbgFlushItDoc tc 2 (vcat [fail_doc <> "\n" , hang 2 (docErrors msgs)])
    pure ls
  where

    error_msg :: (Mode, TestVerdict) -> Maybe (Mode, String)
    error_msg (m, v) =
      case v of
        UF msg -> Just (m,msg)
        _      -> Nothing


    go :: Mode -> Result -> IO (Mode, TestVerdict)
    go mode expected = do
        compiler_dir <- getCompilerDir
        let tmppath  = compiler_dir </> tempdir tc </> name
            basename = compiler_dir </> replaceBaseName tmppath (takeBaseName tmppath ++ modeFileSuffix mode)
            outpath  = replaceExtension basename ".out"
            anspath  = case mb_anspath of
                         Nothing -> tmppath ++ ".ans"
                         Just p  -> compiler_dir </> p
            cpath    = replaceExtension basename ".c"
            exepath  = replaceExtension basename ".exe"
            cmd = "gibbon"
            -- The order of (++) is important. The PATH to the test file must always be at the end.
            cmd_flags = modeRunFlags mode ++ test_flags ++
                        [ "--cfile=" ++ cpath ,
                          "--exefile=" ++ exepath ,
                          compiler_dir </> dir </> name ]
        (_, Just hout, Just herr, phandle) <-
            createProcess (proc cmd cmd_flags) { std_out = CreatePipe
                                               , std_err = CreatePipe }
        dbgFlushIt tc 3 ("CMD: " ++ cmd ++ " " ++ (intercalate " " cmd_flags) ++ "\n")
        exitCode <- waitForProcess phandle
        case exitCode of
            ExitSuccess -> do
                -- Write the output to a file
                out <- hGetContents hout
                writeFile outpath out
                -- Diff the output and the answer
                answer_exists <- doesFileExist anspath
                diff_res <- if answer_exists
                            then diff outpath anspath
                            else return $ Just ("File does not exist: " ++ anspath)
                case (diff_res, expected) of
                    -- Nothing == No difference between the expected and actual answers
                    (Nothing, Pass) -> return (mode, EP)
                    (Nothing, Fail) -> return (mode, UP)
                    (Just err , Fail) -> return (mode, EF err)
                    (Just err , Pass) -> return (mode, UF err)
            ExitFailure _ -> do
                case expected of
                    Fail -> (mode,) <$> EF <$> hGetContents herr
                    Pass -> (mode,) <$> UF <$> hGetContents herr

runBenchmark :: TestConfig -> Test -> IO [(Mode,TestVerdict)]
runBenchmark tc t@Test{name,dir,expectedResults,runModes,numTrials} = do
    putStrLn $ "Benchmarking " ++ show name ++ "..."
    compiler_dir <- getCompilerDir
    let perfpath = compiler_dir </> replaceExtension (dir </> "perf" </> takeBaseName name) ".perf"
    perf_exists <- doesFileExist perfpath
    -- Cannot really check the performance in Interp1 mode.
    let modesToRun = S.fromList $ delete Gibbon1 $ delete Interp1 runModes
    if perf_exists
    then do expected_perf_res <- readPerfFile perfpath
            mapM (\(m,e) -> go expected_perf_res m e)
                 (M.toList $ M.restrictKeys expectedResults modesToRun)
    else mapM (\m -> return (m, UF ("File does not exist: " ++ perfpath))) (S.toList modesToRun)
  where
    go :: M.Map Mode BenchResult -> Mode -> Result -> IO (Mode, TestVerdict)
    go expected_perf_res_mp mode expected = do
      trials <- doNTrials tc mode t
      case trials of
        Right bench_results -> do
            let median_res = medianBenchResult numTrials bench_results
                diff_res = diffBenchResult median_res (expected_perf_res_mp M.! mode)
            -- Nothing == No difference between the expected and actual answers
            case (diff_res, expected) of
                (Nothing, Pass) -> return (mode, EP)
                (Nothing, Fail) -> return (mode, UP)
                (Just err , Fail) -> return (mode, EF err)
                (Just err , Pass) -> return (mode, UF err)
        Left err ->
            case expected of
                Fail -> return (mode, EF err)
                Pass -> return (mode, UF err)

--
doNTrials :: TestConfig -> Mode -> Test -> IO (Either String [BenchResult])
doNTrials tc mode t@Test{name,dir,numTrials,sizeParam,moreIters,isMegaBench,benchFun,test_flags} = do
    compiler_dir <- getCompilerDir
    let tmppath  = compiler_dir </> tempdir tc </> name
        basename = compiler_dir </> replaceBaseName tmppath (takeBaseName tmppath ++ modeFileSuffix mode)
        cpath    = replaceExtension basename ".c"
        exepath  = replaceExtension basename ".exe"


        -- The order of (++) is important. The PATH to the test file must always be at the end.
        cmd_flags = modeExeFlags mode ++ test_flags ++
                      [ "--cfile=" ++ cpath ,
                        "--exefile=" ++ exepath ,
                        compiler_dir </> dir </> name ]
        cmd = "gibbon"
        bench_cmd_flags = if isMegaBench
                          then ["--bench-print","--bench-fun=" ++ benchFun] ++ cmd_flags
                          else cmd_flags

    (_, Just _hout, Just herr, phandle) <-
      createProcess (proc cmd bench_cmd_flags) { std_out = CreatePipe
                                               , std_err = CreatePipe }
    dbgFlushIt tc 2 ("CMD: " ++ cmd ++ " " ++ (intercalate " " bench_cmd_flags) ++ "\n")
    toexeExitCode <- waitForProcess phandle

    -- Number of iterations that should get us beyond the 1s threshold.
    let more_iters = (10 ^ (8 :: Int))

    case toexeExitCode of
        ExitSuccess -> do
            let iters = if mode `elem` moreIters
                        then more_iters
                        else 1
            first_trial <- if isMegaBench
                           then doMegaBenchmark tc cpath exepath t
                           else doTrial tc exepath sizeParam iters
            case first_trial of
                Right{} -> do
                    -- Run it N times, and record the median time.
                    -- ASSUMPTION: If one trial ran without an error, all the others should do too.
                    bench_results <- mapM (\_ -> if isMegaBench
                                                 then fromRight_ <$> doMegaBenchmark tc cpath exepath t
                                                 else fromRight_ <$> doTrial tc exepath sizeParam iters)
                                          [0..numTrials]
                    return (Right bench_results)
                Left err -> return (Left err)
        ExitFailure _ -> Left <$> hGetContents herr

doTrial :: TestConfig -> FilePath -> Int -> Int -> IO (Either String BenchResult)
doTrial tc exepath sizeParam iters = do
    let cmd_options = [show sizeParam, show iters]
    when (verbosity tc > 1) $
        putStrLn $ "Executing: " ++ exepath ++ (intercalate " " cmd_options)
    (_, Just hout, Just herr, phandle) <-
        createProcess (proc exepath cmd_options)
            { std_out = CreatePipe
            , std_err = CreatePipe }
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess -> do
            out <- hGetContents hout
            return $ Right (readBenchResult out)
        ExitFailure _ -> Left <$> hGetContents herr

-- a: actual, b: expected
diff :: FilePath -> FilePath -> IO (Maybe String)
diff a b = do
    (_, Just hout, _, phandle) <-
        -- Ignore whitespace
        createProcess (proc "diff" ["-B", "-w", a, b])
            { std_out = CreatePipe
            , std_err = CreatePipe }
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess -> return Nothing
        ExitFailure _ -> do
            d <- hGetContents hout
            -- See Note [Comparing answers]
            if isBenchOutput d
            then return Nothing
            else return (Just d)

isBenchOutput :: String -> Bool
isBenchOutput s = isInfixOf "BATCHTIME" s || isInfixOf "SELFTIMED" s

doMegaBenchmark :: TestConfig -> FilePath -> FilePath -> Test -> IO (Either String BenchResult)
doMegaBenchmark _tc _cpath exepath Test{benchInput} = do
    compiler_dir <- getCompilerDir
    bench_input_exists <- doesFileExist benchInput
    if not bench_input_exists
    then return $ Left ("File does not exist: " ++ benchInput)
    else do
        let cmd_options = ["--bench-input", benchInput]
        -- when (verbosity tc > 1) $
        putStrLn (show (proc exepath cmd_options))
        (_, Just hout, Just herr, phandle) <-
            createProcess (proc exepath cmd_options)
                { std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = Just compiler_dir }
        exitCode <- waitForProcess phandle
        case exitCode of
            ExitSuccess -> do
                out <- hGetContents hout
                return $ Right (readBenchResult out)
            ExitFailure _ -> Left <$> hGetContents herr

--------------------------------------------------------------------------------

docErrors :: [(Mode, String)] -> Doc ann
docErrors m_errors =
  vcat (map (\(m,err) -> vcat [(sdoc m <> colon), hang 2 (pretty err)]) m_errors)

summary :: TestConfig -> TestRun -> IO String
summary tc tr = do
    endTime <- getTime clk
    day <- getZonedTime
    let timeTaken = quot (toNanoSecs (diffTimeSpec endTime (startTime tr))) (10 ^ (9 :: Int))
    return $ show (go timeTaken day)
  where
    hline x = vcat ["--------------------------------------------------------------------------------", x]

    fixedWidthText :: String -> Doc ann
    fixedWidthText str = pretty ((printf "%-25s" str) :: String)

    docNameModes name modes = fixedWidthText name <+> colon <+> (hsep $ punctuate comma (map sdoc modes))

    docNameErrors :: [(String, [(Mode, String)])] -> Doc ann
    docNameErrors ls =
        vcat (map (\(name,m_errors) ->
                       (if (verbosity tc) >= 2
                        then vcat [ (fixedWidthText name <> colon) , hang 2 (docErrors m_errors)]
                        else docNameModes name (map fst m_errors)))
                  ls)

    go :: Integer -> ZonedTime -> Doc ann
    go timeTaken day = vcat $
        [
          "Gibbon testsuite summary: " <+> parens (sdoc day)
        , "--------------------------------------------------------------------------------"
        , "Time taken:" <+> (pretty $ show timeTaken) <> "s"
        , emptyDoc
        , (pretty $ length $ expectedPasses tr) <+> "expected passes"
        , (pretty $ length $ unexpectedPasses tr) <+>"unexpected passes"
        , (pretty $ length $ expectedFailures tr) <+> "expected failures"
        , (pretty $ length $ unexpectedFailures tr) <+> "unexpected failures"
        , (pretty $ length $ skipped tr) <+> "skipped"
        , (case skipped tr of
               [] -> emptyDoc
               ls -> if (verbosity tc) >= 2
                     then vcat ["\nSkipped: ", hline (hsep $ punctuate comma (map pretty ls))]
                     else emptyDoc)
        , (case M.toList (unexpectedPasses tr) of
               [] -> emptyDoc
               ls -> vcat [ "\nUnexpected passes:" ,
                            hline (vcat (map (\(name,modes) -> docNameModes name modes) ls)) ])
        , (case M.toList (unexpectedFailures tr) of
              [] -> emptyDoc
              ls -> vcat ["\nUnexpected failures:" , hline (docNameErrors ls)])
        , (case M.toList (expectedFailures tr) of
               [] -> if skipFailing tc
                     then "Expected failures: skipped."
                     else emptyDoc
               ls -> if (verbosity tc) >= 3
                     then vcat ["\nExpected failures:" , hline (docNameErrors ls)]
                     else emptyDoc)
        ]

sdoc :: Show a => a -> Doc ann
sdoc = viaShow

fromRight_ :: (Show a, Show b) => Either a b -> b
fromRight_ (Right b) = b
fromRight_ oth  = error $ "fromRight_: Unexpected value " ++ show oth

configFile :: String
configFile = "tests/test-gibbon-examples.yaml"

-- | Flush @msg1@ if the versosity level is >= @n@. Otherwise, flush @msg2@.
-- The caller is responsible for controlling newlines.
dbgFlushIt' :: TestConfig -> Int -> String -> String -> IO ()
dbgFlushIt' tc n msg1 msg2 =
  (if (verbosity tc >= n) then putStr msg1 else putStr msg2) >> hFlush stdout

-- | Flush @msg1@ if the versosity level is >= @n@.
dbgFlushIt :: TestConfig -> Int -> String -> IO ()
dbgFlushIt tc n msg = dbgFlushIt' tc n msg ""

-- | Like 'dbgFlushIt'', but works with 'Doc' values.
dbgFlushItDoc' :: TestConfig -> Int -> Doc AnsiStyle -> Doc AnsiStyle -> IO ()
dbgFlushItDoc' tc n msg1 msg2 =
  (if (verbosity tc >= n) then putDoc msg1 else putDoc msg2) >> hFlush stdout

-- | Like 'dbgFlushIt', but works with 'Doc' values.
dbgFlushItDoc :: TestConfig -> Int -> Doc AnsiStyle -> IO ()
dbgFlushItDoc tc n msg = dbgFlushItDoc' tc n msg emptyDoc

ok_doc :: Doc AnsiStyle
ok_doc = annotate sty "OK"
  where sty = color Green <> bold

fail_doc :: Doc AnsiStyle
fail_doc = annotate sty "FAILED"
  where sty = color Red <> bold

getCompilerDir :: IO String
getCompilerDir = do
    env <- getEnvironment
    case lookup "GIBBONDIR" env of
        Just p  -> return (p </> "gibbon-compiler")
        Nothing -> return ("") -- Assume that we're running from the compiler DIR.

-- It might be handy to configure some things via environment variables. We might want to have a Jenkins job
-- to just run the benchmarks.
mergeTestConfigWithEnv :: TestConfig -> IO TestConfig
mergeTestConfigWithEnv tc = do
    only_perf <- lookupEnv "ONLY_PERF"
    case only_perf of
        Just "1" -> return $ tc { onlyPerf = True }
        _ -> return tc

test_main :: TestConfig -> Tests -> IO ()
test_main tc tests = do
    putStrLn "Executing TestRunner... \n"

    compiler_dir <- getCompilerDir
    dbgFlushIt tc 1 ("Generating answers...")
    (_, Just _hout, Just herr, phandle) <-
        createProcess (proc "make" ["answers"])
        { std_out = CreatePipe
        , std_err = CreatePipe
        , cwd = Just compiler_dir
        }
    exitCode <- waitForProcess phandle
    case exitCode of
      ExitSuccess -> putStrLn "Done."
      ExitFailure _ -> error <$> hGetContents herr

    test_run  <- getTestRun tests
    test_run' <- runTests tc test_run
    report <- summary tc test_run'
    writeFile (testSummaryFile tc) report
    putStrLn $ "\nWrote " ++ (testSummaryFile tc) ++ "."
    putStrLn $ "\n\n" ++ report
    unless (M.null (unexpectedFailures test_run') && M.null (unexpectedPasses test_run'))
        exitFailure
