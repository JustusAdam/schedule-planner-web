module Util where

import Html.Events exposing (on, keyCode)
import Html exposing (Attribute)
import Json.Decode as Decode
import Signal exposing (Signal, Address)


days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]


infixl 9 !!
(!!) : List a -> Int -> Maybe a
xs !! n  =
  if | n < 0     -> Nothing
     | otherwise -> case (xs,n) of
         ([],_)    -> Nothing
         (x::xs,0) -> Just x
         (_::xs,n) -> xs !! (n-1)


withDefault : b -> (a -> b) -> Result err a -> b
withDefault v f res =
  case res of
    Err _ -> v
    Ok n  -> f n


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Decode.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


const : a -> b -> a
const a1 _ = a1


isJust : Maybe a -> Bool
isJust m =
  case m of
    Nothing -> False
    Just _ -> True


groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy eq l =
  case l of
    [] -> []
    (x::xs) ->
      let
        (ys,zs) = span (eq x) xs
      in (x::ys) :: groupBy eq zs

span : (a -> Bool) -> List a -> (List a, List a)
span p xs =
  case xs of
    [] -> (xs, xs)
    (x::xs') ->
      if p x
        then let (ys,zs) = span p xs' in (x::ys,zs)
        else ([],xs)


bool : a -> a -> Bool -> a
bool f g b = if b then g else f
