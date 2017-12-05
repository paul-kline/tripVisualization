{-# LANGUAGE DeriveGeneric #-}
module Datas where
import Data.List
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import GHC.Generics
import Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy as B
type Lat = Double
type Long = Double
type Distance = Double

-- |radius of the earth in meters
radiusOfEarth :: Double
radiusOfEarth = 6378700

data QRes = QRes{
  maxSpeed :: Maybe Double,
  minSpeed :: Maybe Double,
  aveSpeed :: Maybe Double,
  trips    :: Maybe TripCount,
  dataPoints :: Maybe DataPoints,
  aveLocation :: Maybe (Double,Double)

} deriving (Show,Generic,Eq,Ord)
instance FromJSON QRes
instance ToJSON QRes
type TripCount = Int
type DataPoints = Int 

mkQRes :: Double -> Double -> Double -> TripCount -> DataPoints -> (Double,Double) -> QRes
mkQRes mn av mx tc dpc latlng = QRes {aveSpeed = (Just av), maxSpeed = (Just mx), minSpeed = (Just mn), trips = (Just tc ), dataPoints = (Just dpc), aveLocation= Just latlng } --(minSpeed,aveSpeed,maxSpeed,length ls) 


qres_setSpot :: QRes -> (Double,Double) -> QRes
qres_setSpot q sp = q {aveLocation = Just sp}

data Spot = Spot {
              lat   :: Double
            , speed :: Double
            , lng   :: Double
            , dist  :: Double
            , index :: Int  
            , extremes :: Maybe (Double,Double)
            , sources :: Maybe (Int, Int)
            , tripInfo :: Maybe (Int,Double)
} deriving (Show, Generic, Eq)


--so that when I call findMix, it will start
--with the longest trip and then by trip
--index, ensuring that spots in trips
--stay together when replacing!
--we start with longest to try to get rid
--of all shorties. 
instance Ord Spot where
  x `compare` y = case (tripInfo x, tripInfo y) of
    (Nothing, Nothing) -> case (index x) `compare` (index y) of 
      EQ -> case (lat x) `compare` (lat y) of 
        EQ ->  (lng x) `compare` (lng y)
        anyy -> anyy
      anyy -> anyy 
    (Just _, Nothing) -> GT 
    (Nothing, Just _) -> LT 
    (Just (i,maxD), Just (i2,maxD2)) -> case (maxD `compare` maxD2) of 
      EQ -> case i `compare` i2 of 
        EQ -> case (index x) `compare` (index y) of 
          EQ -> case (lat x) `compare` (lat y) of 
           EQ ->  (lng x) `compare` (lng y)
           anyy -> anyy 
          anyy -> anyy
        anyy -> anyy
      anyy -> anyy 

mkSpot :: Lat -> Long -> Spot
mkSpot lat1 lng1 = Spot {lat = lat1, lng = lng1, speed=0, index=0, dist=0, extremes=Nothing, sources = Nothing, tripInfo=Nothing}


instance FromJSON Spot
instance ToJSON Spot
data Trip = Trip {
              coords :: [Spot]
            , start_time :: String 
            , end_time   :: String   

} deriving (Show, Generic,Eq,Ord)
instance FromJSON Trip
instance ToJSON Trip