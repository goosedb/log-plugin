{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module ToLogPlug where

import Data.String (IsString (..))
import qualified Data.Text.Lazy.Builder as TL
import qualified GHC.Plugins as GHC
import qualified Plug
import Data.List (intersperse)
import Data.Foldable (fold)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

class ToLog a where
  toLog :: a -> TL.Builder

instance ToLog TL.Builder where
  toLog = id

instance ToLog T.Text where
  toLog = TL.fromLazyText . TL.fromStrict

instance ToLog TL.Text where
  toLog = TL.fromLazyText

instance ToLog Int where
  toLog = fromString . show

instance ToLog Float where
  toLog = fromString . show

instance ToLog Double where
  toLog = fromString . show

instance ToLog Bool where
  toLog = fromString . show

instance ToLog String where
  toLog = fromString

instance ToLog Char where
  toLog a = fromString ['\'', a, '\'']

instance ToLog a => ToLog [a] where
  toLog = fold . intersperse ", " . map toLog

instance ToLog a => ToLog (Maybe a) where
  toLog = maybe "Nothing" (("Just " <>) . toLog)

instance (ToLog a, ToLog b) => ToLog (Either a b) where
  toLog = either (("Left " <>) . toLog) (("Right " <>) . toLog)

instance ToLog () where
  toLog _ = "()"

instance (ToLog a, ToLog b) => ToLog (a, b) where
  toLog (a, b) = "(" <> toLog a <> ", " <> toLog b <>  ")"

instance (ToLog a, ToLog b, ToLog c) => ToLog (a, b, c) where
  toLog (a, b, c) = "(" <> toLog a <> ", " <> toLog b <> ", " <> toLog c <> ")"


plugin :: GHC.Plugin
plugin = Plug.plugin "#!" 'toLog
