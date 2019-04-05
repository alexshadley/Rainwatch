module Week exposing (next, prev, days)

import Time exposing (Weekday(..))
import List exposing (head, reverse)
import List.Extra exposing (splitWhen)

days = [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

daysAt : Weekday -> List Weekday
daysAt d =
  case splitWhen (\a -> a == d) days of
    Just (a, b) -> 


nextInList : List a -> a -> Maybe a
nextInList xs i =
  case splitWhen (\a -> a == i) xs of
    Just (_,rest) -> head rest
    Nothing       -> Nothing

prevInList : List a -> a -> Maybe a
prevInList xs i =
  nextInList (reverse xs) i

next : Weekday -> Weekday
next d =
  case nextInList days d of
    Just n  -> n
    Nothing -> Mon

prev : Weekday -> Weekday
prev d =
  case prevInList days d of
    Just n  -> n
    Nothing -> Sun