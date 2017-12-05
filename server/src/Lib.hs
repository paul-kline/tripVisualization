module Lib
    
     where

import Datas
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable (foldr', maximumBy, minimumBy)
import qualified Data.ByteString.Lazy as B
import Data.List (genericLength)
import Data.Function (on)
import Data.Maybe
writeOut :: ToJSON a => FilePath -> a -> IO ()
writeOut file thing= I.writeFile file (encodeToLazyText thing)


getJSON :: String -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile

parseTrip :: String -> IO (Maybe Trip)
parseTrip file = do
    bs <- getJSON file
    return (decode bs) 


tripToSpotsSet ::  Trip -> Set.Set Spot
tripToSpotsSet trip = (Set.fromList (coords trip))


tripsToSpotsSet :: [Trip] -> Set.Set Spot
tripsToSpotsSet trips = let spots = map tripToSpotsSet trips
    in
        Set.unions spots

tripToSpots ::  Trip -> Map.Map Spot Trip
tripToSpots trip = Map.fromSet (\_ -> trip) (Set.fromList (coords trip) )
tripsToSpots :: [Trip] -> Map.Map Spot Trip
tripsToSpots travTrips =
    let maps = map tripToSpots travTrips
     in 
        Map.unions maps

distance :: Lat -> Long -> Lat -> Long -> Distance
distance lat1 lng1 lat2 lng2 = distanceSpot (mkSpot lat1 lng1) (mkSpot lat2 lng2)



distanceSpot :: Spot -> Spot -> Distance
distanceSpot x y =
  let (lat1,lon1) = getRadianPair x
      (lat2,lon2) = getRadianPair y
      deltaLat    = lat2 - lat1
      deltaLon    = lon2 - lon1
      a = (sin (deltaLat / 2))^(2::Int) + cos lat1 * cos lat2 * (sin (deltaLon / 2))^(2::Int)
      c = 2 * atan2 (a**0.5) ((1-a)**0.5)
  in radiusOfEarth * c

allWithinof :: [(Spot,Trip)] -> Spot -> Distance -> [(Spot,Trip)]
allWithinof [] _ _= []
allWithinof (s:xs) spot d | distanceSpot spot (fst s) < d = s : allWithinof xs spot d
                          | otherwise = allWithinof xs spot d 

averageBy :: (Fractional a) => (b -> a) -> [b] -> a 
averageBy f ls = (foldr' (\a b -> b + (f a)) 0 ls) / (genericLength ls) 

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a => a   -- ^ Value to look for
 -> a   -- ^ Value to replace it with
 -> [a] -- ^ Input list
 -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

closestto :: [(Spot,Trip)] -> Spot -> (Spot, Distance)
closestto ls tome = foldr' (\(sp,_) (curClose,distance) -> 
    let newDist =  (distanceSpot sp tome) in 
     if newDist < distance then (sp,newDist) else (curClose,distance)     ) (let h = (fst . head) ls in (h, distanceSpot h tome)) (tail ls)

averageSpot :: [(Spot,Trip)] -> Maybe QRes --(Double,Double,Double, Int)
averageSpot [] = Nothing
averageSpot ls = let aveSpeedp = averageBy (speed . fst) ls 
                     maxSpeedp = (speed . fst) (maximumBy (compare `on` (speed . fst) ) ls)
                     minSpeedp = (speed . fst) (minimumBy (compare `on` (speed . fst) ) ls)
                     aveLat = averageBy (lat . fst) ls 
                     aveLng = averageBy (lng . fst) ls 
                     in
                        Just $ mkQRes minSpeedp aveSpeedp maxSpeedp (Set.size (Set.fromList (map snd ls))) (length ls) (aveLat,aveLng)
                        
--QRes {aveSpeed = (Just aveSpeedp), maxSpeed = (Just maxSpeedp), minSpeed = (Just minSpeedp), trips = (Just (Set.size (Set.fromList (map snd ls))) ), dataPoints = (Just (length ls)) } --(minSpeed,aveSpeed,maxSpeed,length ls) 
                
-- groupSpotsByDist :: [Spot] -> [[Spots]] -> Distance -> [[Spot]]
-- groupSpotsByDist [] curState _ = curState
-- groupSpotsByDist (s:xs) curState d = case allWithinof 
getAveSpotWithin :: [(Spot,Trip)] -> Spot -> Distance -> Maybe QRes --(Double,Double,Double,Int)
getAveSpotWithin ls sp d = averageSpot (allWithinof ls sp d)

getAveSpotWithin' :: [(Spot,Trip)] -> Spot -> Distance -> (Maybe QRes,[(Spot,Trip)] )--(Double,Double,Double,Int)
getAveSpotWithin' ls sp d = let matches = (allWithinof ls sp d) in (averageSpot matches,matches) 


amendQRes :: QRes -> Set.Set Trip -> Spot -> Trip -> (QRes,Set.Set Trip)
amendQRes qres qTrips newSpot newTrip = let
    trpsInt = (fromJust . trips) qres
    trps = fromIntegral trpsInt
    cAve = (fromJust . aveSpeed) qres
    cMin = (fromJust . minSpeed) qres
    cMax = (fromJust . maxSpeed) qres
    dps = (fromIntegral . fromJust . dataPoints) qres
    cLat  = ( fst . fromJust . aveLocation) qres 
    cLng = (snd . fromJust . aveLocation) qres 
    spotSpeed = speed newSpot
    newavespeed = cAve * (trps / (trps + 1)) + spotSpeed * (1/(trps + 1))
    newminspeed = min cMin spotSpeed
    newmaxspeed = max cMax spotSpeed
    newTrips = Set.insert newTrip qTrips
    newNumTrips = if Set.member newTrip qTrips then trpsInt else trpsInt + 1
    newLat = cLat * (dps / (dps + 1)) + (lat newSpot) * (1/(dps + 1))
    newLng = cLng * (dps / (dps + 1)) + (lng newSpot) * (1/(dps + 1))
    newQRES = mkQRes newminspeed newavespeed newmaxspeed newNumTrips (((fromJust . dataPoints) qres) + 1) (newLat,newLng)
--QRes {aveSpeed=Just newavespeed,maxSpeed=Just newmaxspeed,minSpeed=Just newminspeed,trips=Just newNumTrips,dataPoints=Just (dps + 1)}
    in
        (newQRES,newTrips)

myinsert :: Spot -> Trip -> Distance -> Bool -> [(Spot,QRes,Set.Set Trip)] -> [(Spot,QRes,Set.Set Trip)]
myinsert _ _ _ True [] = []
myinsert spot trip d False [] = 
    let newspeed = speed spot
        qres = mkQRes newspeed newspeed newspeed 1 1 (lat spot, lng spot)
        --QRes {aveSpeed = dps + 1
-- (Just (speed spot)), maxSpeed = (Just (speed spot)), minSpeed = (Just (speed spot)), trips = (Just 1 ), dataPoints = (Just 1) }
        in
            [(spot,qres,Set.singleton trip)]
myinsert spot trip d inserted ((sp,qres,tripSet ):xs) | distanceSpot spot sp < d = let (newQ,newTrps) = amendQRes qres tripSet spot trip 
                                                 in 
                                                            (sp,newQ,newTrps) : (myinsert spot trip d True xs )
                                             | otherwise =  (sp,qres,tripSet) : (myinsert spot trip d inserted xs) 

consolidate :: [(Spot,Trip)] -> Distance -> [(Spot,QRes)]
consolidate ls d = let newLS = consolidate_ ls [] d in 
    map (\(x,y,_) -> (x,y)) newLS 

consolidate_ :: [(Spot,Trip)] -> [(Spot,QRes,Set.Set Trip)] -> Distance -> [(Spot,QRes,Set.Set Trip)]
consolidate_ [] st _ = st
consolidate_ ((spot,trip):xs) st d = consolidate_ xs (myinsert spot trip d False st) d 
        


getRadianPair :: Spot -> (Double,Double)
getRadianPair p = (toRadians (lat p), toRadians (lng p))

toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)

