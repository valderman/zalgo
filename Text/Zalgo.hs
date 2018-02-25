-- | Provides facilities for generating a wide range of glitched/creepy
--   text through horrifying abuse of diacritics.
module Text.Zalgo
  ( -- * Pure interface
    zalgo, zalgoWith, gradualZalgo, unZalgo

    -- * Effectful interface
  , zalgoIO, zalgoIOWith, gradualZalgoIOWith

    -- * Printing functions
  , printZalgo, printZalgoWith, printGradualZalgo

    -- * Configuration
  , ZalgoSettings
  , maxHeightAt, varianceAt, overlayProbabilityAt
  , defaultZalgoSettings
  ) where
import Data.Char (chr)
import Data.List (foldl')
import System.Random (RandomGen, StdGen, newStdGen, randomR, randomRs, split)

-- TODO: sporadically zalgo a text using Perlin noise

data ZalgoSettings = ZalgoSettings
  { -- | Maximum number of diacritics above or below a character at the
    --   given position of the input string.
    --
    --   Default: const 10
    maxHeightAt :: Int -> Int

    -- | Maximum random variance in height, as a fraction of 'maxHeight', at
    --   the given position of the input string.
    --
    --   Default: const 1
  , varianceAt :: Int -> Double

    -- | Probability of generating an overlay character at the given position
    --   of the input string.
    --
    --   Default: const 0.4
  , overlayProbabilityAt :: Int -> Double
  }

-- | The default zalgo settings. Creepy yet readable.
defaultZalgoSettings :: ZalgoSettings
defaultZalgoSettings = ZalgoSettings
  { maxHeightAt          = const 10
  , varianceAt           = const 1
  , overlayProbabilityAt = const 0.4
  }

minHeightAt :: ZalgoSettings -> Int -> Int
minHeightAt cfg n = floor $ maxH - maxH * varianceAt cfg n
  where
    maxH = fromIntegral (maxHeightAt cfg n)

-- | Combining diacritics above.
over :: [Char]
over = map chr $ concat
  [ [768 .. 789]
  , [829 .. 836]
  , [842 .. 844]
  , [848 .. 850]
  , [867 .. 879] -- latin letters
  , [794, 795, 836, 838, 855, 856, 859, 861, 862, 864, 865]
  ]

-- | Overlaid diacritics.
overlay :: [Char]
overlay = map chr [820 .. 824]

-- | Combining diacritics below.
under :: [Char]
under = map chr $ concat
  [ [x | x <- [790 .. 819], not $ x `elem` [794, 795]]
  , [825 .. 828]
  , [839 .. 841]
  , [851 .. 854]
  , [837, 845, 846, 857, 858, 860, 863]
  ]

-- | Choose n characters from the given list, where n is chosen at random
--   in the given interval.
combiners :: RandomGen g => [Char] -> (Int, Int) -> g -> (g, [Char])
combiners source numRange g =
    (g1, take numMarks $ map (source !!) indices)
  where
    (g0, g1) = split g
    (numMarks, g0') = randomR numRange g0
    indices = randomRs (0, length source-1) g0'

-- | Combine a character with over, under and overlay characters.
--   At most one overlay character is chosen, with probability @overlayProb@.
--   The numbers of top and bottom characters are drawn from the given interval.
combineAll :: RandomGen g => Double -> (Int, Int) -> Char -> g -> (g, String)
combineAll overlayProb numRange c gen
  | o <= overlayProb =
    case marks of
      (g, marks') -> fmap ((c:marks')++) (combiners overlay (1, 1) g)
  | otherwise =
    fmap (c:) marks
  where
    (o, gen') = randomR (0, 1) gen
    marks = foldl' f (gen', "") [over, under]
    f (g, s') src = fmap (s'++) (combiners src numRange g)


-- | Exorcise Zalgo from the given string.
unZalgo :: String -> String
unZalgo = filter (not . (`elem` concat [over, under, overlay]))

-- | Zalgo the given text, using the given algorithm settings and generator.
zalgoWith :: RandomGen g => ZalgoSettings -> String -> g -> (g, String)
zalgoWith cfg s g0 = fmap (concat . reverse) $ snd $ foldl' f (0, (g0, [])) s
  where
    f (n, (g, s')) c = (n+1, fmap (:s') (combineAll o (lo, hi) c g))
      where
        hi = maxHeightAt cfg n
        lo = minHeightAt cfg n
        o = overlayProbabilityAt cfg n

-- | Zalgo the given text using the default zalgo settings and the given
--   generator.
zalgo :: RandomGen g => String -> g -> (g, String)
zalgo = zalgoWith defaultZalgoSettings

-- | Zalgo the given text with the given settings, but scale the maximum
--   height and overlay probability
--   according to the fraction of the string processed so far.
--
--   For example, to only zalgo the last 25% of the string @s@, using settings
--   @cfg@ and generator @g@, one would do:
--
--   > gradualZalgoWith cfg (\f -> if f >= 0.75 then 1 else 0)
--
--   To start scaling the string after 75%, and then linearly increase the
--   output's zalgo level until the final character is zalgo'd using the
--   base settings:
--
--   > gradualZalgoWith cfg (\f -> if f >= 0.75 then (f-0.75)*4 else 0)
gradualZalgoWith :: ZalgoSettings      -- ^ Base settings for this run.
                 -> (Double -> Double) -- ^ Scale variance and overlay
                                       --   probability by this function, whose
                                       --   input is the fraction of the input
                                       --   string processed so far.
                 -> String
                 -> StdGen
                 -> (StdGen, String)
gradualZalgoWith cfg f s g = zalgoWith cfg' s g
  where
    len = fromIntegral $ length s
    scale g n = f (fromIntegral n/len) * g n
    cfg' = cfg
      { maxHeightAt = round . scale (fromIntegral . maxHeightAt cfg)
      , overlayProbabilityAt = scale (overlayProbabilityAt cfg)
      }

-- | Gradually zalgo the given string, starting from the given threshold and
--   linearly scaling towards the default zalgo settings.
gradualZalgo :: Double -> String -> StdGen -> (StdGen, String)
gradualZalgo from = gradualZalgoWith defaultZalgoSettings f
  where
    f x | x >= from = (x-from)*(1/(1-from))
        | otherwise = 0


-- | Zalgo the given text with the given settings, using a fresh
--   standard generator.
zalgoIOWith :: ZalgoSettings -> String -> IO String
zalgoIOWith cfg s = do
  g <- newStdGen
  return $ snd $ zalgoWith cfg s g

-- | Zalgo the given text using the standard settings and a fresh generator.
zalgoIO :: String -> IO String
zalgoIO = zalgoIOWith defaultZalgoSettings

-- | Zalgo the given text using a fresh random generator,
--   starting after the given fraction of the input string, from there on
--   scaling the zalgo factor linearly towards the given settings.
gradualZalgoIOWith :: ZalgoSettings -> Double -> String -> IO String
gradualZalgoIOWith cfg from s = do
  g <- newStdGen
  return $ snd $ gradualZalgoWith cfg f s g
  where
    f x | x >= from = (x-from)*(1/(1-from))
        | otherwise = 0

-- | Zalgo the given text using a fresh random generator,
--   starting after the given fraction of the input string, from there on
--   scaling the zalgo factor linearly towards the default settings.
gradualZalgoIO :: Double -> String -> IO String
gradualZalgoIO = gradualZalgoIOWith defaultZalgoSettings


-- | Print zalgo'd text using the given settings and a fresh
--   random generator.
printZalgoWith :: ZalgoSettings -> String -> IO ()
printZalgoWith cfg s = zalgoIOWith cfg s >>= putStrLn

-- | Print zalgo'd text using the default settings and a fresh default generator.
printZalgo :: String -> IO ()
printZalgo = printZalgoWith defaultZalgoSettings

-- | Gradually zalgo and print the given text starting at the given threshold.
--   Uses default settings and a fresh system default generator.
printGradualZalgo :: Double -> String -> IO ()
printGradualZalgo from s = gradualZalgoIO from s >>= putStrLn
