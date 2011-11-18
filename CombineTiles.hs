{-# LANGUAGE TemplateHaskell #-}

module Main
( Tile (..), tileParents, tileQuad, tilePath
, Quad (..)
, Place (..), placePos, placeSize, placePath
, main
) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.Foldable as F
import Data.Lens
import Data.Lens.Template
import Data.List
import Data.NumInstances ()
import qualified Data.Sequence as Seq
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.XML.Generator

data Tile = Tile { _tileParents :: Seq.Seq Quad
                 , _tileQuad    :: Quad
                 , _tilePath    :: String
                 }
  deriving (Eq, Ord, Read, Show)

data Quad = Q0 | Q1 | Q2 | Q3
  deriving (Eq, Ord, Enum, Read, Show)

data Place = Place { _placePos  :: (Integer, Integer)
                   , _placeSize :: (Integer, Integer)
                   , _placePath :: String
                   }
  deriving (Eq, Ord, Read, Show)

makeLenses [''Tile, ''Place]

tileSize :: (Integer, Integer)
tileSize = (384, 384)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [indir, outfile] -> combineTiles indir outfile
       _ -> usage

usage :: IO ()
usage =
  do p <- getProgName
     hPutStrLn stderr ("USAGE: " ++ p ++ " input-directory output-file")
     exitFailure

combineTiles :: String -> String -> IO ()
combineTiles indir outfile =
  do places <- placesForTiles <$> tilesFromDirectory indir

     BS.putStr . xrender . genMSL outfile $ places

tilesFromDirectory :: String -> IO ([Tile])
tilesFromDirectory =
  fmap concat . mapM (\(quad, dir) -> go' Seq.empty quad dir) <=< children
  where
    go' :: Seq.Seq Quad -> Quad -> String -> IO ([Tile])
    go' parents quad dir =
      do cs <- children dir
         case cs of
           [] -> return [Tile parents quad (dir ++ ".png")]
           _  -> fmap concat
              .  mapM (\(subq, subd) -> go' (parents Seq.|> quad) subq subd)
              $  cs

    children :: String -> IO [(Quad, String)]
    children = filterM (\(_, d) -> (||) <$> doesDirectoryExist d
                                        <*> doesFileExist (d ++ ".png"))
             . potentialChildren
      where
        potentialChildren dir =
          map (second (dir </>))
              [(Q0, "0"), (Q1, "1"), (Q2, "2"), (Q3, "3")]

placesForTiles :: [Tile] -> [Place]
placesForTiles tiles =
  let maxDepth = (+1) . maximum
               . map (fromIntegral . Seq.length . getL tileParents)
               $ tiles

      places   = map (placeForTile maxDepth) tiles

      minX     = minimum . map (fst . getL placePos) $ places
      minY     = minimum . map (snd . getL placePos) $ places

  in  map (modL placePos (subtract (minX, minY))) places

placeForTile :: Integer -> Tile -> Place
placeForTile maxDepth tile =
  let quads = F.toList (getL tileParents tile Seq.|> getL tileQuad tile)
      depth = genericLength quads

      facs = map (2^) . zipWith const (iterate pred (maxDepth-1)) $ quads
      pos  = sum . zipWith (\fac quad -> fac * xy quad * tileSize) facs $ quads

      size = 2^(maxDepth - depth) * tileSize

  in Place pos size (getL tilePath tile)

xy :: Quad -> (Integer, Integer)
xy Q0 = (0, 0)
xy Q1 = (1, 0)
xy Q2 = (0, 1)
xy Q3 = (1, 1)

genMSL :: String -> [Place] -> Xml Doc
genMSL outfile places =
  doc defaultDocInfo $
    xelem "group" $
      xelems . concat $
        [ map placeImage placesIds
        , [ xelem "image"  $  xattr "size" (show width ++ "x" ++ show height)
                           <> xattr "background" "transparent"
                           <> xattr "color" "transparent"
                          <#> xelems (map compImage placesIds)
          ]
        , [ xelem "write" $ xattr "filename" outfile ]
        ]

  where
    placeImage (pid, place) =
      xelem "image"
         $  xattr "id" pid
        <#> xelem "read" (xattr "filename" (getL placePath place))
         <> xelem "resize" (xattr "geometry" (placeSizeGeo place))

    compImage (pid, place) =
      xelem "composite"
        $  xattr "image" pid
        <> xattr "geometry" (placePosGeo place)

    placesIds = zip (map (('i':) . show) [0 :: Integer ..]) places

    placePosGeo  (Place (x,y) _ _) = "+" ++ show x ++ "+" ++ show y
    placeSizeGeo (Place _ (w,h) _) = show w ++ "x" ++ show h

    width  = maximum . map (\(Place (x,_) (w,_) _) -> x + w) $ places
    height = maximum . map (\(Place (_,y) (_,h) _) -> y + h) $ places
