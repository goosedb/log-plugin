{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Plug (plugin, Config (..)) where

import qualified Data.Generics.Uniplate.Data as Uniplate
import qualified GHC
import qualified GHC.Builtin.Names as GHC
import qualified GHC.Plugins as GHC
import qualified Language.Haskell.TH.Syntax as TH

data Config = Config
    { op :: GHC.FastString
    , toLog :: GHC.RdrName
    }

type WithConfig = (?config :: Config)

plugin :: GHC.FastString -> TH.Name -> GHC.Plugin
plugin op toLog =
    let ?config = Config{op, toLog = toGHCName toLog}
     in GHC.defaultPlugin
            { GHC.parsedResultAction = \_ _ -> pure . processModule
            , GHC.pluginRecompile = GHC.purePlugin
            }

processModule :: WithConfig => GHC.HsParsedModule -> GHC.HsParsedModule
processModule GHC.HsParsedModule{..} = GHC.HsParsedModule{hpm_module = Uniplate.transformBi findAndReplaceDecls hpm_module, ..}

findAndReplaceDecls :: WithConfig => [GHC.LHsDecl GHC.GhcPs] -> [GHC.LHsDecl GHC.GhcPs]
findAndReplaceDecls = do
    map \case
        GHC.L loc expr -> GHC.L loc $ Uniplate.transformBi findAndReplaceExpr expr

findAndReplaceExpr :: WithConfig => GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
findAndReplaceExpr (GHC.L sp (GHC.OpApp _ l (GHC.L _ (GHC.HsVar _ (GHC.L _ (GHC.Unqual (GHC.occNameFS -> op'))))) r))
    | op' == ?config.op =
        GHC.L sp (GHC.HsApp GHC.EpAnnNotUsed l (concatExprs $ reverse $ map wrapIntoBuilder $ toListOfExprs r))
findAndReplaceExpr a = a

toListOfExprs :: GHC.LHsExpr GHC.GhcPs -> [GHC.LHsExpr GHC.GhcPs]
toListOfExprs (GHC.L _ (GHC.HsApp _ l r)) = r : toListOfExprs l
toListOfExprs a = [a]

concatExprs :: [GHC.LHsExpr GHC.GhcPs] -> GHC.LHsExpr GHC.GhcPs
concatExprs [] = undefined
concatExprs [x] = x
concatExprs ((GHC.L sp x) : xs) =
    GHC.L
        sp
        ( GHC.OpApp
            GHC.EpAnnNotUsed
            (GHC.L sp x)
            (GHC.L sp (GHC.HsVar GHC.NoExtField (GHC.L (GHC.la2na sp) (GHC.Exact GHC.sappendName))))
            (concatExprs xs)
        )

wrapIntoBuilder :: WithConfig => GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
wrapIntoBuilder str@(GHC.L _ (GHC.HsLit _ (GHC.HsString _ _))) = str
wrapIntoBuilder x@(GHC.L sp _) =
    GHC.L
        sp
        ( GHC.HsApp
            GHC.EpAnnNotUsed
            (GHC.L sp (GHC.HsVar GHC.NoExtField (GHC.L (GHC.la2na sp) ?config.toLog)))
            x
        )

toGHCName :: TH.Name -> GHC.RdrName
toGHCName (TH.Name (TH.OccName n) nameFlavour) = case nameFlavour of
    TH.NameS -> GHC.Unqual (GHC.mkVarOcc n)
    TH.NameQ (TH.ModName str) -> GHC.Qual (GHC.mkModuleName str) (GHC.mkVarOcc n)
    TH.NameG TH.VarName _ (TH.ModName str) -> GHC.Qual (GHC.mkModuleName str) (GHC.mkVarOcc n)
    _ -> error "Unsupported"
