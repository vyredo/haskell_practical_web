module Domain.Validation where

import ClassyPrelude
import Text.Regex.PCRE.Heavy

type Validation e a = a -> Maybe e

validate :: (a->b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
    case concatMap(\f -> maybeToList $ f val) validations of
        [] -> Right $ constructor val
        errs -> Left errs



lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLength maxLength msg val =
    rangeBetween minLength maxLength msg $ length val
    where 
        rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
        rangeBetween minRange maxRange msg val =
            if val >= minRange && val <= maxRange
                then Nothing
                else Just msg

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val =
    if val =~ regex
        then Nothing
        else Just msg
        