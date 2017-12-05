module Summary where

import Datas
import Lib

import qualified Data.Map.Strict as M 
-- import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable
import Data.List (delete)
import System.Random
type Spotdick = M.Map Spot Trip 

type SpotSet = S.Set Spot 

-- data Holder = Holder {
--     dict :: Spotdick
--     , spots :: SpotSet
-- }

allWithin :: Spotdick -> Spot -> Distance -> Spotdick
allWithin dic spot d = M.filterWithKey (\s _ -> distanceSpot s spot < d ) dic
-- allWithin' :: [(Spot,Trip)] -> Spot -> Distance -> [(Spot,Trip)]
-- allWithin' dic spot d = filter (\(s,_) -> distanceSpot s spot < d ) dic

-- averageBy :: (Fractional a, Foldable t) => (b -> a) -> t b  -> a 
-- averageBy f ls = (foldr' (\a b -> b + (f a)) 0 ls) / (genericLength ls) 

--takes a spot, distance a set of trips and dictionary of spot trips
--returns the average spot within that radius along with min and max speeds
--as well as the set of trips modified so that all the spots used to compute
--the average have been removed. I also need to return the spots used, so 
--I don't have to recompute them. an average for them has already been given.
ggg :: Spot -> Distance -> S.Set Trip -> Spotdick -> (Spot,Spotdick, S.Set Trip)
ggg sp d initTripSet dic = do 
    let closies = allWithin dic sp d 
        (aveSpot,tripsUsed) = computeAverageSpot closies 
        newTrips = M.foldrWithKey' (\matchingSpot matchingTrip newTripSet -> 
            case matchingSpot == sp of --if this is the one true spot, replace with the average!
                True  -> S.insert (replaceSpot sp aveSpot matchingTrip)  (S.delete matchingTrip newTripSet)
                False -> S.insert (removeSpot matchingSpot matchingTrip) (S.delete matchingTrip newTripSet)
            ) initTripSet closies
        in
            (aveSpot,closies, newTrips)
averageOut :: Spotdick -> Distance ->  S.Set Trip -> IO (S.Set Trip)
averageOut dic _ trips | M.null dic =  return trips
averageOut dic d trips =
 let maxSize = M.size dic  in do
  --rand <- randomIO :: IO Float 
  --let index = rand * (fromIntegral maxSize) in do
    putStrLn ("spotDick size: " ++ (show (M.size dic)))
    let (spot,_) =  M.findMax dic --M.elemAt (floor index) dic
        (aveSpot, usedSpotsDic ,reducedTrips) = ggg spot d trips dic in do
            --I can now remove aaaall elements used to create that average. 
            --putStrLn ("here is the spot I'm using: " ++ (show spot))
            --putStrLn ("here is how many results should be given back in the radius!!" ++ (show (M.size (allWithin dic spot d))))
            --putStrLn ("UsedSpots for average: " ++ (show (M.size usedSpotsDic)))
            averageOut (dic M.\\ usedSpotsDic) d reducedTrips 
            --given a spot and a trip, 
--I will remove it from the coords
removeSpot :: Spot -> Trip -> Trip 
removeSpot s t = t {coords=delete s (coords t)}

replaceSpot :: Spot -> Spot -> Trip -> Trip 
replaceSpot old new trip = trip{coords=replace old new (coords trip)}


computeAverageSpot :: Spotdick -> (Spot,S.Set Trip) 
computeAverageSpot m | null m = error "empty dic given"
computeAverageSpot dic = 
 let firstSpeed = speed entry
     (entry,ftrip) = head $ M.toList dic 
     dic' = M.delete entry dic 
  in
   M.foldrWithKey' (\sp trip (spave,trips)-> 
    let spotSpeed   = speed sp
        cAve        = speed spave
        cDist       = dist spave
        (Just (spCount,_ )) = sources spave
        --safe because I gave a just in init for fold
        (Just (cMin,cMax)) = extremes spave 
        spCount'    = fromIntegral spCount
        newavespeed = cAve * (spCount' / (spCount' + 1)) + spotSpeed * (1/(spCount' + 1))
        newDist = cDist * (spCount' / (spCount' + 1)) + (dist sp) * (1/(spCount' + 1))
        newminspeed = min cMin spotSpeed
        newmaxspeed = max cMax spotSpeed
        newTrips = S.insert trip trips
        cLat = lat sp
        cLng = lng sp
        newLat = cLat * (spCount' / (spCount' + 1)) + (lat sp) * (1/(spCount' + 1))
        newLng = cLng * (spCount' / (spCount' + 1)) + (lng sp) * (1/(spCount' + 1))
        in
            (spave {speed=cAve, lat=newLat,lng=newLng, dist = newDist
            ,  extremes = Just (newminspeed,newmaxspeed)
            ,  sources  = (Just (spCount + 1, S.size newTrips)) }
            ,newTrips) ) (entry{extremes= Just (firstSpeed,firstSpeed), sources = (Just (1,1))}, S.singleton ftrip ) dic'


emptySpot = Spot {speed=0, lat=0,lng=0,dist=0,index=0, extremes=Nothing, sources=Nothing, tripInfo=Nothing}
