{-# LANGUAGE TemplateHaskell #-}
module LensTut.Atom (demo) where

-- source: 
-- http://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html

import Control.Lens hiding (element)
import Control.Lens.TH

data Atom = Atom 
    { _element :: String
    , _point :: Point 
    } deriving (Show)
data Point = Point 
    { _x :: Double
    , _y :: Double 
    } deriving (Show)
data Molecule = Molecule 
    { _atoms :: [Atom]
    } deriving (Show)

$(makeLenses ''Atom)
$(makeLenses ''Point)
$(makeLenses ''Molecule)
-- equivalent to
-- element :: Lens' Atom String
-- point   :: Lens' Atom Point
-- x       :: Lens' Point Double
-- y       :: Lens' Point Double

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

demoUpdateDeeplyNestedRecords :: IO ()
demoUpdateDeeplyNestedRecords = do
    let atom = Atom { _element = "C"
                    , _point = Point { _x = 1.0, _y = 2.0 } 
                    }
        atom' = shiftAtomX $ atom
    print atom'
    print . shiftMoleculeX . Molecule $ [atom', atom]

demoRetrieveDeeplyNestedFields :: IO ()
demoRetrieveDeeplyNestedFields = do
    let atom = Atom { _element = "C"
                    , _point = Point { _x = 1.0, _y = 2.0 } 
                    }
        mol = Molecule [atom]
    print $ view (point . x) atom
    print $ fmap (view (point . x)) (view atoms mol)

demoAccessorNotation :: IO ()
demoAccessorNotation = do
    let atom = Atom { _element = "C"
                    , _point = Point { _x = 1.0, _y = 2.0 } 
                    }
    print (atom^.point.x) -- atom ^. (point . x) 
                          -- ^. atom (point . x)
                          -- (flip . view) (point . x) atom

demoFirstClassLens :: IO ()
demoFirstClassLens = do
    let atom = Atom { _element = "C"
                    , _point = Point { _x = 1.0, _y = 2.0 } 
                    } 
        mol = Molecule [atom, atom]
    print $ shift (point . x) atom
    print $ toListOf (atoms . traverse . point . x) mol
    where
        shift lens = over lens (+ 100)

demo :: IO ()
demo = do
    demoUpdateDeeplyNestedRecords
    demoRetrieveDeeplyNestedFields
    demoAccessorNotation
    demoFirstClassLens
