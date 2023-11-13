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
patchSdf (AAppl "Lit" [AAppl x []]) | length x > 1 && head x == '"' && last x == '"'  = pure $ AAppl "Lit" [AAppl "Quoted" [AAppl x []]]
patchSdf (AAppl "Assoc" xs) = pure $ AAppl "Atr" xs
patchSdf (AAppl "Unquoted" xs) = pure $ AAppl "Uqlit" xs
patchSdf _ = mzero

hoistMaybe (Just x) = pure x
hoistMaybe Nothing = mzero

resolveAmb (AAppl "amb" [AList (xxs@(AAppl "Atr" _) : _)]) = pure xxs
resolveAmb (AAppl "amb" [AList xs]) = hoistMaybe $ find (\case
    AAppl "AssocGroup" _ -> True
    _ -> False) xs
resolveAmb _ = mzero

patchConc :: ATerm -> Bool -> ATerm
patchConc (AAppl "Conc" xs) _ = AAppl "Conc" (fmap (`patchConc` True) xs)
patchConc (AAppl "Range" xs) True = AAppl "CharRange" [AAppl "Range" $ fmap (`patchConc` True) xs]
patchConc xxs@(AAppl "Short" _) True = AAppl "CharRange" [AAppl "Character" [xxs]]
patchConc (AAppl nm xs) p = AAppl nm $ fmap (`patchConc` p) xs
patchConc x@(AInt _) _ = x
patchConc (AList xs) p = AList $ fmap (`patchConc` p) xs

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

main :: IO ()
main = do
    x <- readFile "Operators.aterm"
    putStrLn . writeATerm
        . fromJust
        . applyTP (full_tdTP (tryTP $ monoTP resolveAmb))
        . (`patchAttrs` False)
        . (`patchProd` False)
        . (`unpatchRange` False)
        . (`patchConc` False)
        . (`patchRange` False)
        . fromJust
        . applyTP (full_tdTP (tryTP $ monoTP patchSdf))
        . readATerm $ x
