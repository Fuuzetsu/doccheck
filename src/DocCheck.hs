module Main where

import GHC (renamedSource, emptyRnGroup)
import Name (Name)
import HsDecls (HsGroup, LHsDecl)

import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import qualified Data.Traversable as T

import qualified Packages
import qualified Module
import qualified SrcLoc
import GHC
import HscTypes
import Name
import Bag
import RdrName
import TcRnTypes
import FastString (unpackFS, concatFS)


data Hole = Hole

analyseDocs tm = do
  group <- case renamedSource tm of
    Nothing -> do
      return emptyRnGroup
    Just (x, _, _, _) -> return x
  let (docs, _) = unzip $ topDecls group
  return (group :: HsGroup Name)

-- | The top-level declarations of a module that we care about,
-- ordered by source location, with documentation attached if it exists.
topDecls :: HsGroup Name -> [(LHsDecl Name, [HsDocString])]
topDecls = filterClasses . filterDecls . collectDocs . sortByLoc . ungroup

-- | Collect docs and attach them to the right declarations.
collectDocs :: [LHsDecl a] -> [(LHsDecl a, [HsDocString])]
collectDocs = go Nothing []
  where
    go Nothing _ [] = []
    go (Just prev) docs [] = finished prev docs []
    go prev docs (L _ (DocD (DocCommentNext str)) : ds)
      | Nothing <- prev = go Nothing (str:docs) ds
      | Just decl <- prev = finished decl docs (go Nothing [str] ds)
    go prev docs (L _ (DocD (DocCommentPrev str)) : ds) = go prev (str:docs) ds
    go Nothing docs (d:ds) = go (Just d) docs ds
    go (Just prev) docs (d:ds) = finished prev docs (go (Just d) [] ds)

    finished decl docs rest = (decl, reverse docs) : rest


-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)

-- | Go through all class declarations and filter their sub-declarations
filterClasses :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterClasses decls = [ if isClassD d then (L loc (filterClass d), doc) else x
                      | x@(L loc d, doc) <- decls ]
  where
    filterClass (TyClD c) =
      TyClD $ c { tcdSigs = filter isVanillaLSig $ tcdSigs c }
    filterClass _ = error "expected TyClD"

-- | Filter out declarations that we don't handle in Haddock
filterDecls :: [(LHsDecl a, doc)] -> [(LHsDecl a, doc)]
filterDecls = filter (isHandled . unL . fst)
  where
    isHandled (ForD (ForeignImport {})) = True
    isHandled (TyClD {}) = True
    isHandled (InstD {}) = True
    isHandled (SigD d) = isVanillaLSig (reL d)
    isHandled (ValD _) = True
    -- we keep doc declarations to be able to get at named docs
    isHandled (DocD _) = True
    isHandled _ = False


-- | Take all declarations except pragmas, infix decls, rules from an 'HsGroup'.
ungroup :: HsGroup Name -> [LHsDecl Name]
ungroup group_ =
  mkDecls (concat   . hs_tyclds) TyClD  group_ ++
  mkDecls hs_derivds             DerivD group_ ++
  mkDecls hs_defds               DefD   group_ ++
  mkDecls hs_fords               ForD   group_ ++
  mkDecls hs_docs                DocD   group_ ++
  mkDecls hs_instds              InstD  group_ ++
  mkDecls (typesigs . hs_valds)  SigD   group_ ++
  mkDecls (valbinds . hs_valds)  ValD   group_
  where
    typesigs (ValBindsOut _ sigs) = filter isVanillaLSig sigs
    typesigs _ = error "expected ValBindsOut"

    valbinds (ValBindsOut binds _) = concatMap bagToList . snd . unzip $ binds
    valbinds _ = error "expected ValBindsOut"


-- | Take a field of declarations from a data structure and create HsDecls
-- using the given constructor
mkDecls :: (a -> [Located b]) -> (b -> c) -> a -> [Located c]
mkDecls field con struct = [ L loc (con decl) | L loc decl <- field struct ]

unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined

isClassD :: HsDecl a -> Bool
isClassD (TyClD d) = isClassDecl d
isClassD _ = False



main :: IO ()
main = return ()
