#!/usr/bin/env stack runghc

-- module ColourRGB (Colour) where
-- data Colour = RGB Int Int Int deriving Show

-- using record syntax
-- each field is named
-- compiler will generate accessor functions for each field
module ColourRGBRecord (Colour) where
data Colour = RGB { 
    red   :: Int
    , green :: Int
    , blue  :: Int
    } deriving Show

-- real world haskell P/95
-- recap: for each of the fields that we name in our type 
-- definition, haskell creates an accessor function of that name
data Customer = Customer {
    customerID :: Int 
    , customerName :: String
    , customerAddress :: [String]
    } deriving (Show)

demoAutoAccessor :: IO ()
demoAutoAccessor = do
    let customer = Customer {
        customerID = 12
        , customerName = "doom"
        , customerAddress = ["e1", "m1"]
    }
    print $ customerName customer
    print $ customerAddress customer

-- real world haskell P/270
-- the Iterator type gives us a convenient alias for the function...
-- MY NOTE:
-- parameter seed is to tell the compiler,
-- for each of the value ctor, it will take some kind of value
-- the type of the value is the same as that of seed
-- therefore if a consumer of this data says: I require Iterate Int
-- then the only value ctor it accept is Done Int, Skip Int or Continue Int
-- in this regard, we really have infinite variation of Iterate
-- in contrast,
-- data Info = Info {
-- infoPath :: FilePath
-- , infoPerms :: Maybe Permissions
-- , infoSize :: Maybe Integer
-- , infoModTime :: Maybe UTCTime
-- } deriving (Eq, Ord, Show)
-- Info has one and only one representation; its fields have 
-- fixed types
data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Int -> Iterate seed

demoParameterizedData :: IO ()
demoParameterizedData = do
  print "//// demo parameterized data"
  print $ (Done 2)
  print $ (Skip "asd")
  print $ (process $ Skip "thereisacow")
  -- process does not take Iterate data type that collaborates 
  -- with Int/Num
  -- print $ (process $ Skip 1)
  where
    process :: Iterate String -> Int
    process it = length $ unwrap it 

x = RGB 10 20 30
-- this is NOT type update syntax
-- type is by default IMMUTABLE
y = x { green = 40 }

-- real world haskell P/281
-- use record to copy and partly change an existing value
-- we can set as many fields as we want inside the curly braces, 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- separating them using commas
data ParseState = ParseState {
  string :: String
, offset :: Int
} deriving (Show)

modifyOffset :: ParseState -> Int -> ParseState
modifyOffset state newOffset =
  state { offset = newOffset }

demoCopyToNewAndUpdateField :: IO ()
demoCopyToNewAndUpdateField = do
  print "//// demo copy to new value and update field"
  let s = ParseState "te" 13
  print s
  print $ modifyOffset s 2312
  print s  -- immutable

main :: IO ()
main = do
  print $ show x ++ show y
  demoAutoAccessor
  demoParameterizedData
  demoCopyToNewAndUpdateField
