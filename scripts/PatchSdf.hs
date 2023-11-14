{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
import Data.ATerm.Lib
import Data.Data
import Data.Generics.Strafunski.StrategyLib.StrategyLib
import Data.Maybe (fromJust)
import Data.List

deriving instance Data ATerm

patchSdf :: MonadPlus m => ATerm -> m ATerm
patchSdf (AAppl "amb" [AList [AAppl "Layout" [], AAppl "Sort" [AAppl "\"LAYOUT\"" []]]]) = pure $ AAppl "Sort" [AAppl "\"LAYOUT\"" []]
patchSdf (AAppl "Present" [AAppl "Conc" xs]) = pure $ AAppl "Present1" [AAppl "Conc" xs]
patchSdf (AAppl "Present" [AAppl "Range" xs]) = pure $ AAppl "Present1" [AAppl "Range" xs]
patchSdf (AAppl "Lit" [AAppl x []]) | length x > 1 && head x == '"' && last x == '"'  = pure $ AAppl "Lit" [AAppl "Quoted" [AAppl x []]]
patchSdf (AAppl "Assoc" xs) = pure $ AAppl "Atr" xs
patchSdf (AAppl "Left" xs) = pure $ AAppl "LeftAssoc" xs
patchSdf (AAppl "Right" xs) = pure $ AAppl "RightAssoc" xs
patchSdf (AAppl "Unquoted" xs) = pure $ AAppl "Uqlit" xs
patchSdf _ = mzero

hoistMaybe (Just x) = pure x
hoistMaybe Nothing = mzero

resolveAmb (AAppl "amb" [AList (xxs@(AAppl "Atr" _) : _)]) = pure xxs
resolveAmb (AAppl "amb" [AList xs]) = hoistMaybe $ find (\case
    AAppl "AssocGroup" _ -> True
    _ -> False) xs
resolveAmb _ = mzero

patchPresent1 :: ATerm -> Bool -> ATerm
patchPresent1 (AAppl "Present1" xs) _ = AAppl "Present1" (fmap (`patchPresent1` True) xs)
-- patchPresent1 (AAppl "Range" xs) _ = AAppl "Range" (fmap (`patchPresent1` True) xs)
-- TODO: Add case for when it's not Conc, but CharRange
patchPresent1 (AAppl "Range" xs) True = AAppl "CharRange" [AAppl "Range" $ fmap (`patchPresent1` True) xs]
patchPresent1 xxs@(AAppl "Short" _) True = AAppl "CharRange" [AAppl "Character" [xxs]]
patchPresent1 (AAppl nm xs) p = AAppl nm $ fmap (`patchPresent1` p) xs
patchPresent1 x@(AInt _) _ = x
patchPresent1 (AList xs) p = AList $ fmap (`patchPresent1` p) xs

patchRange :: ATerm -> Bool -> ATerm
patchRange (AAppl "Range" xs) _ = AAppl "Range" (fmap (`patchRange` True) xs)
patchRange (AAppl "Short" xs) True = AAppl "SHORT" xs
patchRange (AAppl nm xs) p = AAppl nm $ fmap (`patchRange` p) xs
patchRange x@(AInt _) _ = x
patchRange (AList xs) p = AList $ fmap (`patchRange` p) xs

unpatchRange :: ATerm -> Bool -> ATerm
unpatchRange (AAppl "Range" xs) _ = AAppl "Range" (fmap (`unpatchRange` True) xs)
unpatchRange (AAppl "SHORT" xs) True = AAppl "Short" xs
unpatchRange (AAppl nm xs) p = AAppl nm $ fmap (`unpatchRange` p) xs
unpatchRange x@(AInt _) _ = x
unpatchRange (AList xs) p = AList $ fmap (`unpatchRange` p) xs

patchProd :: ATerm -> Bool -> ATerm
patchProd (AAppl "Prod" xs) _ = AAppl "Prod" (fmap (`patchProd` True) xs)
patchProd (AAppl "CharClass" xs) True = AAppl "CharClass1" xs
patchProd (AAppl nm xs) p = AAppl nm $ fmap (`patchProd` p) xs
patchProd x@(AInt _) _ = x
patchProd (AList xs) p = AList $ fmap (`patchProd` p) xs

patchAttrs :: ATerm -> Bool -> ATerm
patchAttrs (AAppl "Attrs" xs) _ = AAppl "Attrs" (fmap (`patchAttrs` True) xs)
patchAttrs (AAppl "Term" [AAppl "Default" xs]) True = AAppl "Cons1" $ fmap (`patchAttrs` True) xs
patchAttrs (AAppl "Uqlit" xs) True = AAppl "Literal" [AAppl "Uqlit" xs]
patchAttrs xxs@(AAppl "Quoted" _) True = AAppl "Literal" [xxs]
patchAttrs (AAppl nm xs) p = AAppl nm $ fmap (`patchAttrs` p) xs
patchAttrs x@(AInt _) _ = x
patchAttrs (AList xs) p = AList $ fmap (`patchAttrs` p) xs

patchImports :: ATerm -> Bool -> ATerm
patchImports (AAppl "Imports" xs) _ = AAppl "Imports" (fmap (`patchImports` True) xs)
patchImports (AAppl "Module" xs) True = AAppl "Module1" xs
patchImports (AAppl nm xs) p = AAppl nm $ fmap (`patchImports` p) xs
patchImports x@(AInt _) _ = x
patchImports (AList xs) p = AList $ fmap (`patchImports` p) xs

main :: IO ()
main = do
    x <- readFile "Expression.aterm"
    putStrLn . writeATerm
        . fromJust
        . applyTP (full_tdTP (tryTP $ monoTP resolveAmb))
        . (`patchImports` False)
        . (`patchAttrs` False)
        . (`patchProd` False)
        . (`unpatchRange` False)
        . (`patchPresent1` False)
        . (`patchRange` False)
        . fromJust
        . applyTP (full_tdTP (tryTP $ monoTP patchSdf))
        . readATerm $ x
