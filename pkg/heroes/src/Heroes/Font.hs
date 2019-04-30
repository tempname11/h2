module Heroes.Font (
  Font(..),
  fontNameOf,
  fontSizeOf,
  charSet
) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *
import Heroes
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- * -- *

data Font
  = Font'FutilePro24
  | Font'CompassPro24
  | Font'EquipmentPro24
  | Font'ExpressionPro24
  | Font'MatchupPro24
  deriving (Eq, Ord, Generic)

-- XXX DeriveAnyClass
instance GEnum Font

fontSizeOf :: Font -> Int
fontSizeOf _ = 24

fontNameOf :: Font -> String
fontNameOf = \case
  Font'FutilePro24 -> "FutilePro"
  Font'CompassPro24 -> "CompassPro"
  Font'EquipmentPro24 -> "EquipmentPro"
  Font'ExpressionPro24 -> "ExpressionPro"
  Font'MatchupPro24 -> "MatchupPro"

charSet :: String
charSet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" <>
  "abcdefghijklmnopqrstuvwxyz" <>
  "0123456789.,;:?!-_~#\"'&()[]{}^|`/\\@°+=*%€$£¢<>©®" <>
  "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØŒÙÚÛÜÝÞ" <>
  "àáâãäåæçèéêëìíîïðñòóôõöøœùúûüýþßÿ¿¡"
