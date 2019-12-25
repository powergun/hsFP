{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Validation.Demo where

import           Data.Bool           (bool)
import           Data.List           (elem)
import           Data.Time.LocalTime
import           GHC.Generics        (Generic)
import           Text.Read           (readMaybe)

-- NOTE: this example skip the json parsing step since it is not relevant
-- to the validation concept; the test case will directly create
-- ReservationJson value using the data ctor

data ReservationJson = ReservationJson
  { jsonDate     :: String
  , jsonQuantity :: Double
  , jsonName     :: String
  , jsonEmail    :: String
  } deriving (Eq, Show, Read, Generic)

data Reservation = Reservation
  { reservationDate     :: ZonedTime
  , reservationQuantity :: Int
  , reservationName     :: String
  , reservationEmail    :: String
  } deriving (Show, Read)

newtype Validation e a = Validation (Either e a) deriving (Eq, Show, Functor)

hasError :: Validation [String] Reservation -> String -> Bool
hasError (Validation (Right _ )) _ = False
hasError (Validation (Left es)) s  = s `elem` es

validateDate :: String -> Validation [String] ZonedTime
validateDate =
  -- case readMaybe candidate of
  --   Just d -> Validation $ Right d
  --   _      -> Validation $ Left ["Invalid date"]
  Validation . maybe (Left ["Invalid date"]) Right . readMaybe

validateQuantity :: Double -> Validation [String] Int
validateQuantity candidate =
  let isInt x = x == fromInteger (round x)
      isPositiveInt x = isInt x && x > 0
  in if isPositiveInt candidate
       then Validation . Right . round $ candidate
       else Validation . Left $ ["Not a positive integer"]

validateEmail :: String -> Validation [String] String
validateEmail candidate =
  if '@' `elem` candidate
    then Validation . Right $ candidate
    else Validation . Left $ ["Not an email address"]

instance Monoid m => Applicative (Validation m) where
  pure = Validation . pure
  Validation (Left x) <*> Validation (Left y) = Validation (Left $ mappend x y)
  Validation f <*> Validation r = Validation (f <*> r)

validate :: ReservationJson -> Validation [String] Reservation
validate r =
  Reservation <$> validateDate (jsonDate r)
              <*> validateQuantity (jsonQuantity r)
              <*> pure (jsonName r)
              <*> validateEmail (jsonEmail r)
