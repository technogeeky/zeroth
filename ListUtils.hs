module ListUtils where

import Data.List  ( intercalate, isPrefixOf, unfoldr )

-- | Replace a substring with a replacement string throughout a list
replaceAll :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAll []     newSub = intercalate newSub . map return
replaceAll oldSub newSub = concat . unfoldr replace
    where
        replace list = do
            (h:t) <- return list
            return $ if oldSub `isPrefixOf` list then (newSub, drop len list) else ([h], t)
        len = length oldSub
