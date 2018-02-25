-- | Provides facilities for generating a wide range of glitched/creepy
--   text through horrifying abuse of diacritics.
module Text.Zalgo
  ( -- * Pure interface
    zalgo, zalgoWith, gradualZalgo, unZalgoWith, redact

    -- * Effectful interface
  , zalgoIO, zalgoIOWith, gradualZalgoIO, gradualZalgoIOWith, redactIO

    -- * Printing functions
  , printZalgo, printZalgoWith, printGradualZalgo, printRedacted

    -- * Configuration
  , ZalgoSettings
  , maxHeightAt, varianceAt, overlayProbabilityAt
  , numOverlayCharsAt, overlayCharset
  , defaultZalgoSettings, unreadableZalgoSettings
  , defaultOverlayCharset
  ) where
import Data.Array (Array, listArray, bounds, elems, (!))
import Data.Char (ord, chr)
import Data.List (foldl', isSuffixOf, isPrefixOf)
import System.Random (RandomGen, StdGen, newStdGen, randomR, randomRs, split)

-- TODO: sporadically zalgo a text using Perlin noise

data ZalgoSettings = ZalgoSettings
  { -- | Maximum number of diacritics above or below a character at the
    --   given position of the input string.
    --
    --   Default: const 5
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

    -- | Number of characters to use for overlay at the given position of the
    --   input string. The number of overlays for any character will always be
    --   this number or zero.
    --
    --   Default: const 1
  , numOverlayCharsAt :: Int -> Int

    -- | Charset from which to pick overlay characters.
    --
    --   Default: 'defaultOverlayCharset'
  , overlayCharset :: Array Int Char
  }

-- | The default zalgo settings. Creepy yet readable.
defaultZalgoSettings :: ZalgoSettings
defaultZalgoSettings = ZalgoSettings
  { maxHeightAt          = const 5
  , varianceAt           = const 1
  , overlayProbabilityAt = const 0.4
  , numOverlayCharsAt    = const 1
  , overlayCharset       = defaultOverlayCharset
  }

-- | Settings to make text completely unreadable.
unreadableZalgoSettings :: ZalgoSettings
unreadableZalgoSettings = defaultZalgoSettings
  { overlayProbabilityAt = const 1
  , numOverlayCharsAt    = const 7
  }

minHeightAt :: ZalgoSettings -> Int -> Int
minHeightAt cfg n = floor $ maxH - maxH * varianceAt cfg n
  where
    maxH = fromIntegral (maxHeightAt cfg n)

-- | Combining diacritics above.
over :: Array Int Char
over = listArray (0, length list-1) list
  where
    list = map chr $ concat
      [ [768 .. 789]
      , [829 .. 836]
      , [842 .. 844]
      , [848 .. 850]
      , [867 .. 879] -- latin letters
      , [794, 795, 836, 838, 855, 856, 859, 861, 862, 864, 865]
      ]

-- | Overlaid diacritics.
defaultOverlayCharset :: Array Int Char
defaultOverlayCharset = listArray (0, length list-1) list
  where
    list = map chr $ [820 .. 824]

-- | Combining diacritics below.
under :: Array Int Char
under = listArray (0, length list-1) list
  where
    list = map chr $ concat
      [ [x | x <- [790 .. 819], not $ x `elem` [794, 795]]
      , [825 .. 828]
      , [839 .. 841]
      , [851 .. 854]
      , [837, 845, 846, 857, 858, 860, 863]
      ]

-- | Choose n characters from the given list, where n is chosen at random
--   in the given interval.
combiners :: RandomGen g => Array Int Char -> (Int, Int) -> g -> (g, [Char])
combiners source numRange g =
    (g1, take numMarks $ map (source !) indices)
  where
    (g0, g1) = split g
    (numMarks, g0') = randomR numRange g0
    indices = randomRs (bounds source) g0'

-- | Combine a character with over, under and overlay characters.
--   At most one overlay character is chosen, with probability @overlayProb@.
--   The numbers of top and bottom characters are drawn from the given interval.
combineAll :: RandomGen g => Double -> (Int, Int) -> Int -> Array Int Char -> Char -> g -> (g, String)
combineAll overlayProb numRange ovrs overlay c gen
  | o <= overlayProb =
    case marks of
      (g, marks') -> fmap ((c:marks')++) (combiners overlay (ovrs, ovrs) g)
  | otherwise =
    fmap (c:) marks
  where
    (o, gen') = randomR (0, 1) gen
    marks = foldl' f (gen', "") [over, under]
    f (g, s') src = fmap (s'++) (combiners src numRange g)

-- | Break the second given string on the first occurrence of the first
--   (the "needle").
--   The first element of the returned pair is the string up until the needle,
--   and the second is the string after it.
--   The needle itself is not included in either.
--   If the needle is not found, the first element will be the input string and
--   the second will be empty.
break1 :: Eq a => [a] -> [a] -> ([a], [a])
break1 [] xs     = (xs, [])
break1 needle xs = go [] xs
  where
    go pre [] =
        (reverse pre, [])
    go pre xs
      | needle `isPrefixOf` xs =
        (reverse pre, drop (length needle) xs)
      | otherwise =
        go (head xs:pre) (tail xs)

-- | Break the second string on each occurrence of first (the "needle").
--   The returned list will contain the needle interspersed between non-needle
--   segments so that `forall needle xs. concat (breakAll needle xs) == id`.
breakAll :: Eq a => [a] -> [a] -> [[a]]
breakAll needle xs =
  case break1 needle xs of
    (_, []) | not (needle `isSuffixOf` xs) ->
      [xs]
    ([], xs') ->
      needle : breakAll needle xs'
    (pre, xs') ->
      pre : needle : breakAll needle xs'

-- | Blot out any occurrence of the given needles in the given string using
--   extreme zalgo.
redact :: RandomGen g => [String] -> String -> g -> (g, String)
redact needles haystack gen = foldl' f (gen, haystack) needles
  where
    f (g, xs) needle = redact' needle g (breakAll needle xs)
    rot13 c = chr $ (ord c + 13 - 97) `mod` 26 + 97

    redact' needle g (x:xs)
      | needle == x =
        case zalgoWith unreadableZalgoSettings (map rot13 x) g of
          (g', x') -> fmap (x'++) (redact' needle g' xs)
      | otherwise =
        fmap (x++) (redact' needle g xs)
    redact' _ g _ =
      (g, "")

-- | Exorcise Zalgo from the given string using the given settings.
unZalgoWith :: ZalgoSettings -> String -> String
unZalgoWith cfg =
  filter (not . (`elem` concat [elems over, elems under, elems (overlayCharset cfg)]))

-- | Exorcise Zalgo from the given string using the default settings.
unZalgo :: String -> String
unZalgo = unZalgoWith defaultZalgoSettings

-- | Zalgo the given text, using the given algorithm settings and generator.
zalgoWith :: RandomGen g => ZalgoSettings -> String -> g -> (g, String)
zalgoWith cfg s g0 = fmap (concat . reverse) $ snd $ foldl' f (0, (g0, [])) s
  where
    f (n, (g, s')) c = (n+1, fmap (:s') (combineAll o (lo, hi) novrs ovrs c g))
      where
        hi = maxHeightAt cfg n
        lo = minHeightAt cfg n
        o = overlayProbabilityAt cfg n
        novrs = numOverlayCharsAt cfg n
        ovrs = overlayCharset cfg

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

-- | Like 'redact', but with a fresh random generator.
redactIO :: [String] -> String -> IO String
redactIO needles s = do
  g <- newStdGen
  return $ snd $ redact needles s g

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

-- | 'redact' and print the given needles and haystack.
printRedacted :: [String] -> String -> IO ()
printRedacted needles s = redactIO needles s >>= putStrLn
