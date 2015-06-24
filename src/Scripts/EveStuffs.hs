{-# LANGUAGE OverloadedStrings #-}
module Parseeve where

import Data.List
import Data.Monoid
import Numeric
import qualified Data.Text as T

data PirateFaction = BloodRaider | Gurista | Sansha | Angel | Serpentis
                   | Drone
                   deriving Show
data WEffect = None | CataclysmicVariable | Margentar | BlackHole | Pulsar |
               WolfRayetStar | RegGiant
             deriving Show
data Region = Region { regionName :: T.Text , pirates :: PirateFaction }
            deriving Show

data System = WSystem { code :: Int
                      , wclass :: Int
                      , statics :: [Int]
                      , effect :: WEffect }
            | KSystem { region :: Region
                      , systemName :: T.Text
                      , security :: Float
                      }
            deriving Show


regionToLine :: Region -> String
regionToLine r = (T.unpack $ regionName r) ++ replicate nb ' ' ++
                 (show $ pirates r) ++ "\n"
  where nb = 20 - T.length (regionName r)

systemToLine :: System -> String
systemToLine (KSystem r n s) = (T.unpack n) ++ replicate nn ' ' ++
                               (T.unpack $ regionName r) ++
                               replicate nr ' ' ++
                               ((showFFloat (Just 2) s) "") ++ "\n"
  where nn = 20 - T.length n
        nr = 20 - T.length (regionName r)

writeRegions :: IO ()
writeRegions = do
  r <- parseRegions
  let rs = foldl (++) "" $ map regionToLine r
  writeFile "regions.txt" rs

writeSystems :: IO ()
writeSystems = do
  s <- parseAllSystems
  let ss = foldl (++) "" $ map systemToLine s
  writeFile "systems.txt" ss

parseAllSystems :: IO [System]
parseAllSystems = do
  r <- parseRegions
  parseSystems r

parseSystems :: [Region] -> IO [System]
parseSystems rs = do
  let msys = map parseRegionSystems rs
  sys <- sequence msys
  let fsys = foldl (++) [] sys
  return fsys

parseRegionSystems :: Region -> IO [System]
parseRegionSystems r = do
  file <- readFile $ T.unpack $ T.concat
          ["static/eve/", (regionName r), ".txt"]
  let k = map (parseLine . tabify) (T.lines $ T.pack file)
  return k
  where parseLine ws = KSystem r (T.drop 3 (ws !! 1))
                       (read $ T.unpack (ws !! 2))

parseRegions :: IO [Region]
parseRegions = do
  e <- parseRegionsE
  o <- parseRegionsO
  return (e ++ o)

parseRegionsE :: IO [Region]
parseRegionsE = do
  file <- readFile "static/eve/regionsE.txt"
  let k = map (parseLine . tabify) (T.lines $ T.pack file)
  return k
  where parseLine ws = Region (ws !! 0) (toPirate $ ws !! 2)

parseRegionsO :: IO [Region]
parseRegionsO = do
  file <- readFile "static/eve/regionsO.txt"
  let k = map (parseLine . tabify) (T.lines $ T.pack file)
  return k
  where parseLine ws = Region (ws !! 0) (toPirate $ ws !! 1)

tabify l = map rmSpaces ((T.splitOn "\t" . T.drop 4) l)
rmSpaces = T.filter (/= ' ')
toPirate "BloodRaiders" = BloodRaider
toPirate "Guristas" = Gurista
toPirate "Sanshas" = Sansha
toPirate "Angels" = Angel
toPirate "Serpentis" = Serpentis
toPirate "Drones" = Drone
toPirate _ = undefined
