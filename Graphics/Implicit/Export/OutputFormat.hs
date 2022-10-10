{- ORMOLU_DISABLE -}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Graphics.Implicit.Export.OutputFormat
  ( OutputFormat (SVG, SCAD, PNG, GCode, ASCIISTL, STL, THREEJS, OBJ, DXF),
    guessOutputFormat,
    formatExtensions,
    formatExtension,
    formats2D,
    formatIs2D,
    def2D,
    formats3D,
    formatIs3D,
    def3D,
  )
where

import Prelude (Bool, Eq, FilePath, Maybe, Read (readsPrec), String, drop, flip, length, tail, take, ($), (==), pure, (>>=), maxBound, (<$>), Show, Enum, Bounded, minBound, (.))
-- For making the format guesser case insensitive when looking at file extensions.
import Data.Char (toLower)
import Data.Default.Class (Default(def))
import Data.List (lookup, elem)
-- For handling input/output files.
import System.FilePath (takeExtensions)
import Data.List.NonEmpty (NonEmpty ((:|)), toList, head)

-- | A type serving to enumerate our output formats.
data OutputFormat
  = SVG
  | SCAD
  | PNG
  | GCode
  | ASCIISTL
  | STL
  | THREEJS
  | OBJ
  | DXF
--  | 3MF
  deriving (Show, Eq, Enum, Bounded)

instance Default OutputFormat where
  def = STL

-- | Default 2D output format
def2D :: OutputFormat
def2D = SVG

-- | Default 3D output format
def3D :: OutputFormat
def3D = def

-- | All supported 2D formats
formats2D :: [OutputFormat]
formats2D = [GCode, DXF, PNG, SCAD, SVG]

-- | True for 2D capable `OutputFormat`s
formatIs2D :: OutputFormat -> Bool
formatIs2D  = flip elem formats2D

-- | All supported 3D formats
formats3D :: [OutputFormat]
formats3D = [ASCIISTL, OBJ, STL, SCAD, THREEJS]

-- | True for 3D capable `OutputFormat`s
formatIs3D :: OutputFormat -> Bool
formatIs3D = flip elem formats3D

-- | A list mapping file extensions to output formats.
-- Allow GHC to do the heavy lifting for us, rather than
-- doing it by hand.
formatExtensions :: [(String, OutputFormat)]
formatExtensions = [minBound .. maxBound] >>= toList . go
  where
    go e = (, e) <$> getFormatExtensions e

-- | A list mapping file extensions to output formats.
-- Formats must have at least one extension, so non-empty it is!
getFormatExtensions :: OutputFormat -> NonEmpty String
getFormatExtensions = \case
  SVG      -> pure "svg"
  SCAD     -> pure "scad"
  PNG      -> pure "png"
  GCode    -> pure "ngc"
  ASCIISTL -> "ascii.stl" :| pure "asciistl"
  STL      -> pure "stl"
  THREEJS  -> "three.js" :| pure "threejs"
  OBJ      -> pure "obj"
  DXF      -> pure "dxf"
  -- 3MF      -> pure "3mf"

-- | Lookup an output format for a given output file.
guessOutputFormat :: FilePath -> Maybe OutputFormat
guessOutputFormat fileName = readOutputFormat . tail $ takeExtensions fileName

-- | Try to look up an output format from a supplied extension.
readOutputFormat :: String -> Maybe OutputFormat
readOutputFormat ext = lookup (toLower <$> ext) formatExtensions

-- | A Read instance for our output format. Used by 'auto' in our command line parser.
--   Reads a string, and evaluates to the appropriate OutputFormat.
instance Read OutputFormat where
  readsPrec _ myvalue =
    tryParse formatExtensions
    where
      tryParse :: [(String, OutputFormat)] -> [(OutputFormat, String)]
      tryParse [] = [] -- If there is nothing left to try, fail
      tryParse ((attempt, result) : xs) =
        if take (length attempt) myvalue == attempt
          then [(result, drop (length attempt) myvalue)]
          else tryParse xs

-- | Get filename extension for `OutputFormat`
formatExtension :: OutputFormat -> String
formatExtension = head . getFormatExtensions