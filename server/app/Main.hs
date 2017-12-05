{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Datas
import Web.Scotty hiding (delete)
import System.Directory
import Data.List
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Lazy.IO as I
import Data.Text.Lazy (Text)
import Control.Concurrent (forkIO)
import System.IO.Unsafe (unsafePerformIO)
-- import qualified Data.Foldable
import Data.Maybe
import Data.Monoid (mconcat)
import Data.Function (on)
import Summary
import System.Environment


fileLoc = "./trips"
getTrips :: IO [Maybe Trip]
getTrips = do
    files <- getDirectoryContents fileLoc
    let jsons =map ((fileLoc ++ "/") ++ ) $ filter (".json" `isSuffixOf`) files
     in do
      (mapM parseTrip jsons)


    --simpleHTTP nullConf $ ok "Hello World!"

serveIndex :: IO ()
serveIndex = scotty 80 $ do
    get "/" $ file "../index.html"
    get "/jsonloader.js" $ file "../jsonloader.js"

toSegments_ :: [Spot] -> Trip ->(Spot -> Bool) -> [Trip]
toSegments_ [] t _ = []
toSegments_ coords t f = let 
        (goods,rest) = span f coords 
        ready = dropWhile (not . f) rest in 
        case goods of
            -- if I took nothing, get the bads out of the way! 
            [] -> toSegments_ ready t f
            --otherwise, This chunk meets my needs! let's include it!
            goods -> (t {coords=goods}) : (toSegments_ ready t f)




--for all of these spot pairs, I will 
--remove the ones in the given trip.
removeSpots :: [(Spot,Trip)] -> Trip -> Trip 
removeSpots [] t = t 
removeSpots ((s,t):toremove) t' | t == t' = removeSpots toremove (removeSpot s t')
                                | otherwise = removeSpots toremove t' 

--removes all spots from all trips. 
removeFromTrips :: [(Spot,Trip)] -> [Trip] -> [Trip]
removeFromTrips toRemoves trips = map (removeSpots toRemoves) trips 

aveSpotRemoveMatchesFrom :: (Spot,Trip) -> Distance ->  [Trip] -> [(Spot,Trip)] -> IO (Spot,Trip,[Trip], [(Spot,Trip)])
aveSpotRemoveMatchesFrom (spot,trip) d restTrips dat | (coords trip) == [] = return (spot,trip,restTrips,dat) 
aveSpotRemoveMatchesFrom (spot,trip) d restTrips dat = do
    Prelude.putStrLn ("in aveSpotRemoveMatchesFrom. trip has coord length of : " ++ (show (length (coords trip))))
    let ((mAveQ),matches) =getAveSpotWithin' dat spot d
     in 
        case mAveQ of 
          Nothing -> return (spot,trip,restTrips, dat)
          (Just aveQ) -> 
             let (Just (newlat,newlng)) = aveLocation aveQ 
                 newSpot = spot{speed =(fromJust . aveSpeed) aveQ, 
                                lat = newlat, 
                                lng = newlng }
              in 
               return (newSpot, 
                       head $  removeFromTrips ((spot,trip):matches) [trip],
                       removeFromTrips matches restTrips, 
                       foldr (\pair acc -> delete pair acc ) dat matches )

aveTripRemoveMatchesFrom :: ([Spot],Trip) -> Distance ->  [Trip] -> [(Spot,Trip)] -> IO (([Spot],Trip),[Trip], [(Spot,Trip)])
aveTripRemoveMatchesFrom ([],t) _ rest dat = return (([],t),rest,dat)
aveTripRemoveMatchesFrom (c:cs,t) d rest dat = do 
     Prelude.putStrLn ("spot length: " ++ (show (length cs)) ++ "data size:" ++ (show (length dat)))
     (cnew,tnew,tsnew,datnew) <- aveSpotRemoveMatchesFrom (c,t) d rest dat
     ((as,bt),cts,dds) <- aveTripRemoveMatchesFrom ((coords tnew),tnew) d tsnew datnew
     
     return ((cnew:as,bt),cts,dds)
        
test :: IO [Trip]
test = do 
    trips <- getTrips
    let t = catMaybes trips
        spots = tripListToSpotList' t 0
     in do
      aveRemoving t 5 spots 

aveRemoving :: [Trip] -> Distance -> [(Spot,Trip)] -> IO [Trip]
aveRemoving [] _ _ = return [] 
aveRemoving (t:ts) d dat = do 
     print ("trips left: " ++ (show (length ts)))
     ((newspots,newTrip),newRestTrips,newDat) <- aveTripRemoveMatchesFrom (coords t,t) d ts dat
     rest <- (aveRemoving newRestTrips d newDat)
     return ((newTrip{coords=newspots}) : rest )
   
averageizeAllTrips :: [Trip] -> Distance -> [(Spot,Trip)] -> [Trip]
averageizeAllTrips trips d dat = 
    map (\t -> t{coords = (map (\spot -> 
            let (Just aveQ) = getAveSpotWithin dat spot d 
                (Just (newlat,newlng)) = aveLocation aveQ 
                in 
                 spot{speed =(fromJust . aveSpeed) aveQ, 
                      lat = newlat, 
                      lng = newlng }) (coords t))  } )  trips 



toSegments :: [Trip] ->(Spot -> Bool) -> [Trip]
toSegments [] _ = []
toSegments (t:ts) f = toSegments_ (coords t) t f ++ (toSegments ts f) 

fromEither :: Either a b -> Maybe b 
fromEither (Left _) = Nothing 
fromEither (Right v) = Just v
mkFilter :: Maybe Double -> Maybe Double -> (Spot -> Bool)
mkFilter minSpeed maxSpeed = case (minSpeed,maxSpeed) of 
    (Nothing,Nothing)   -> (\s -> let v = (speed s) in  True)
    (Just p, Nothing)   -> (\s -> let v = (speed s) in  v >= p )
    (Nothing, Just p2 ) -> (\s -> let v = (speed s) in  v <= p2 )
    (Just p, Just p2)   -> (\s -> let v = (speed s) in v >= p && v <= p2) 

getall :: IO ([(Spot,Trip)])
getall = do 
    t <- getTrips
    let t' = catMaybes t 
    return $ tripListToSpotList' t' 0

testAverage :: Distance ->  IO (Set.Set Trip)
testAverage d = do 
    spotTrips <- getall
    Prelude.putStrLn ("spot trip size: " ++ (show (length spotTrips)))
    let spotTripDic = Map.fromList spotTrips 
      in do
        Prelude.putStrLn ("spot trip size: " ++ (show (Map.size spotTripDic)))    
        averageOut  spotTripDic d (Set.fromList (Map.elems spotTripDic))   
        
writeOutAverage :: Distance -> FilePath ->  IO ()
writeOutAverage d f = do 
    tripSet <- testAverage d
    writeOut f tripSet 

main :: IO ()
main = do
 args <- getArgs
 if length args == 2 then 
    writeOutAverage (read (args !! 1) :: Double) (args !! 0) 
 else  do
    trips <- getTrips
    let t = catMaybes trips
        spots = tripListToSpotList' t 0
        absMaxSpeed = fst $ maximumBy (compare `on` (speed .fst) ) spots
        in do
          print (length spots)
          --print (length consoled)
          scotty 80 $ do
           get "/" $ file "../index.html"
           get "/maxspeed" $
             Web.Scotty.json absMaxSpeed
           get "/jsonloader.js" $ file "../jsonloader.js"
           get "/trips" $ do 
            Web.Scotty.json trips
           get "/filteredTrips" $ do 
              minspeedz <- (param "minSpeed") :: ActionM Double
              maxspeedz <- (param "maxSpeed") :: ActionM Double
              liftAndCatchIO  $ print (minspeedz,maxspeedz)
              let minSpeed = if minspeedz == (-1) then Nothing else Just minspeedz
                  maxSpeed = if maxspeedz == (-1) then Nothing else Just maxspeedz
                  answer = (toSegments t f)
                  f = mkFilter minSpeed maxSpeed
                  in do
                    --liftAndCatchIO $ print answer 
                    Web.Scotty.json answer

           get "/query" $ do
              latt <- param "lat"
              long <- param "lng"
              d    <- param "within"
              let spot = mkSpot (read latt) (read long)
               in  do
                let mqres = getAveSpotWithin spots spot d
                   -- response =  case mqres of 
                    --    Nothing ->  "no data"
                            --let movedSpot =  fst $ closestto spots spot  in 
                            --qres_setSpot (fromJust $ getAveSpotWithin spots movedSpot d) movedSpot 
                    --    Just res -> qres_setSpot res spot 
                 in
                  case mqres of 
                    Nothing -> Web.Scotty.json ("no data" :: String)
                    Just res -> Web.Scotty.json res
           --get "/consol" $ do 
             -- Web.Scotty.json consoled
    
           






-- spotsWithin :: Spot -> Distance -> Map.Map Spot Trip -> (Spot, Map.Map Spot Trip )
-- spotsWithin origin maxd spotMap = (origin, Map.filterWithKey (\spot trip -> (distanceSpot spot origin) < maxd) spotMap)


-- groupSet :: Distance -> Set.Set Spot -> Map.Map Spot (Double, Int)
-- groupSet d set = Set.foldr' (\spot newMap -> case find (\sp -> (distanceSpot sp spot) < d) (Map.keysSet newMap) of
--     Nothing -> Map.insert spot (speed spot,1) newMap   
--     Just p  -> Map.insertWith (\(newspeed,_) (cur,count) -> 
--         let more = (fromIntegral count) + 1 
--          in 
--             ( cur * ((fromIntegral count)/more) + (newspeed)*(1/more), 
--               count + 1) ) p (speed spot, 1) newMap   
--    ) (Map.empty :: Map.Map Spot (Double,Int)) set

-- groupSpots :: Distance -> Map.Map Spot Trip -> Map.Map Spot (Map.Map Spot Trip )
-- groupSpots d spotMap =
--         --we first look for an existing key within our specified distance. If there isn't one there already, we make one
--         Map.foldrWithKey' (\spot trip newMap -> case find (\sp -> (distanceSpot sp spot) < d) (Map.keysSet newMap) of
--              Nothing -> Map.insert spot (Map.fromList [(spot,trip)]) newMap   
--              Just p  -> Map.insertWith Map.union p (Map.fromList [(spot,trip)]) newMap   
--             ) Map.empty spotMap

-- groupSpots' :: Distance -> Map.Map Spot Trip -> Map.Map Spot (Map.Map Spot Trip )
-- groupSpots' d spotMap =
--         --we first look for an existing key within our specified distance. If there isn't one there already, we make one
--         Map.foldrWithKey' (\spot trip newMap -> case find (\sp -> (distanceSpot sp spot) < d) (Map.keysSet newMap) of
--                 Nothing -> Map.insert spot (Map.fromList [(spot,trip)]) newMap   
--                 Just p  -> Map.insertWith Map.union p (Map.fromList [(spot,trip)]) newMap   
--             ) Map.empty spotMap

tripListToSpotList' :: [Trip] -> Int ->  [(Spot,Trip)]
tripListToSpotList' [] _ = []
tripListToSpotList' (x:xs) i = 
    let sps = coords x 
        maxdist = dist (sps !! ((length sps) -1)) in
            (map (\g -> (g{tripInfo = Just (i,maxdist )  }, x)
    )  (coords x)) ++ (tripListToSpotList' xs (i+1) )

tripListToSpotList :: [Trip] -> [Spot]
tripListToSpotList [] = []
tripListToSpotList (x:xs) = (coords x) ++ tripListToSpotList xs 
--             main = do
--                   mtripLS <- getTrips
--                   let trips = catMaybes mtripLS
--                       spotMap = tripsToSpots trips
--                       sized = 30
--                       groups  = groupSpots sized  spotMap--100 meters
--                       groups' = tripsToSpotsSet trips
--                       grouped = groupSet sized groups'
--                    in do
--                     print "let us begin"
--                     print (Map.size spotMap)
--                     print (Map.size grouped)
--                     writeOut ("./groupSize_" ++ (show sized) ++ "totalSpots" ++ (show . Map.size $ spotMap) ++  ".json") (Map.toList grouped)
--                     return grouped
--                     --print groups
            