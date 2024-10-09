-- arthur lucas david alexis geatan
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Redundant id" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Algorithm.Search
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State.Lazy
import Criterion.Main
import Data.Biapplicative
import Data.Bifunctor
import Data.Char
import qualified Data.Foldable as F
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Data.List as L
import Data.Maybe
import Data.MemoTrie
import Data.Ord
import Data.Range
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tree
import Data.Tuple
import qualified Data.Vector as V
import Debug.Trace
import Lib
import Linear.V2
import Numeric
import Test.HUnit hiding (State)
import Text.Parsec hiding (State, (<|>))
import Text.Pretty.Simple

data MType = Broadcaster | FlipFlop {_active :: Bool} | Conjonction {_remb :: HashMap String Bool}
  deriving (Eq, Show)

data Module = Module {_mtype :: MType, _mid :: String, _moutput :: [String]}
  deriving (Eq, Show)

data Signal a = Signal {_input :: String, _low :: Bool, _outputId :: String, _output :: a}
  deriving (Eq, Show)

makeLenses ''MType
makeLenses ''Module
makeLenses ''Signal

parser :: Parsec String Int (HashMap String Module)
parser =
  M.fromList . ap (flip zip) (map (view mid))
    <$> ( ( do
              t <- try (char '%' $> FlipFlop False) <|> try (char '&' $> Conjonction M.empty) <|> return Broadcaster
              label <- many1 letter
              manyTill anyChar (lookAhead letter)
              output <- many1 letter `sepBy` (char ',' >> char ' ')
              return $ Module {_mtype = t, _mid = label, _moutput = output}
          )
            `sepBy1` newline
        )

tests =
  test
    [ "triv" ~: 1 + 1 ~?= 2
    ]

main :: IO ()
main = do
  runTestTT tests
  input <- readFile "input"
  case runParser parser 0 "input" input of
    Left err -> pPrint err
    Right r -> do
      pPrint $ run r
      defaultMain [bench "run" $ whnf run r]
      where
        run =
          evalState
            ( do
                fixState
                -- findMod "jm"
                m <- get
                let m' :: HashMap String [Module]
                    m' = M.map (map (m M.!)) $ minvert (M.map (view mid) m)
                pure $
                  unfoldTree
                    ( \mod@(Module {_mid, _moutput}) ->
                        ( mod,
                          concatMap
                            ( filter
                                ( \case
                                    (Module {_mtype = (Conjonction _)}) -> True
                                    _ -> False
                                )
                            )
                            (mapMaybe (m' M.!?) _moutput)
                        )
                    )
                    (m M.! "kj")
            )
          where
            fixState :: State (HashMap String Module) ()
            fixState = do
              mods <- get <&> M.elems
              forM_
                mods
                ( \(Module {_mtype, _mid, _moutput}) -> do
                    m <- get
                    let conjs = mapMaybe ((\x -> case x of (Module {_mtype = (Conjonction _)}) -> Just x; _ -> Nothing) <=< (m M.!?)) _moutput
                    forM_ conjs (modify . M.adjust (over (mtype . remb) (M.insert _mid True)) . view mid)
                )
            -- see (input, sig, mod) = input ++ " -" ++ (if sig then "low" else "high") ++ "-> " ++ maybe "output" (view mid) mod
            findMod = findMod' 0
            findMod' :: Int -> String -> State (HashMap String Module) Int
            findMod' presses mod =
              let n = succ presses
               in do
                    out <- concat <$> tree [Signal {_input = "button", _low = True, _outputId = "broadcaster", _output = r M.!? "broadcaster"}]
                    if any (\s -> view outputId s == mod && view low s) out then pure n else findMod' n mod
            tree :: [Signal (Maybe Module)] -> State (HashMap String Module) [[Signal (Maybe Module)]]
            tree [] = pure []
            tree sigmods = do
              sigmods' <- concat <$> mapM f (mapMaybe (\s -> do m <- view output s; pure (set output m s)) sigmods)
              (:) sigmods <$> tree sigmods'
            f :: Signal Module -> State (HashMap String Module) [Signal (Maybe Module)]
            f (Signal {_low, _output = (Module {_mtype = Broadcaster, _mid, _moutput})}) =
              sget _moutput <&> map (\(id, mod) -> Signal {_input = _mid, _low = _low, _outputId = id, _output = mod})
            f (Signal {_low = True, _output = (Module {_mtype = (FlipFlop a), _mid, _moutput})}) =
              do
                modify (M.adjust (over (mtype . active) not) _mid)
                sget _moutput <&> map (\(id, mod) -> Signal {_input = _mid, _low = a, _outputId = id, _output = mod})
            f (Signal {_low = False, _output = (Module {_mtype = (FlipFlop _), _mid, _moutput})}) = pure []
            f (Signal {_low, _input, _output = (Module {_mtype = (Conjonction _), _mid, _moutput})}) =
              do
                modify (M.adjust (over (mtype . remb) (M.insert input _low)) _mid)
                remb' <- get <&> view (mtype . remb) . flip (M.!) _mid
                sget _moutput <&> map (\(id, mod) -> Signal {_input = _mid, _low = not (or $ M.elems remb'), _outputId = id, _output = mod})
            sget :: [String] -> State (HashMap String Module) [(String, Maybe Module)]
            sget n = get <&> zip n . flip map n . (M.!?)