module OAuth exposing (..)

import String exposing (..)
import Navigation exposing (Location)
import Task
import Redirect

type Msg
  = OAuthToken String
  | OAuthTokenFailure
  | OAuthRedirectFailure String
  | OAuthRedirectSuccess

type alias Model =
  { token : Maybe String
  }

isAuthorized : Model -> Bool
isAuthorized { token } =
  case token of
    Just _ ->
      True
    Nothing ->
      False


init : String -> Location -> (Model, Cmd msg)
init authUrl location =
  case tokenFromLocation location of
    Just token ->
      ({ token = Just token }, Navigation.modifyUrl (hashlessUrl location))
    Nothing ->
      ({ token = Nothing }, Redirect.setUrl authUrl) -- Task.perform OAuthRedirectFailure (\_ -> OAuthRedirectSuccess) <| Location.assign authUrl)

urlUpdate : Location -> Model -> (Model, Cmd msg)
urlUpdate location model =
  ({ model | token = tokenFromLocation location }, Cmd.none)

tokenFromLocation : Location -> Maybe String
tokenFromLocation location =
  let
      hash = String.slice 1 (String.length location.hash) location.hash
      pairs = String.split "&" hash |> List.map splitPair
      matchingPairs = List.filter (\pair -> fst pair |> (==) "access_token") pairs
  in
     case matchingPairs of
       (_, token) :: _ ->
         Just token
       _ ->
         Nothing


splitPair : String -> (String, String)
splitPair pair =
  case indexes "=" pair of
    index :: _ ->
      ( String.slice 0 index pair
      , String.slice (index + 1) (String.length pair) pair
      )
    _ ->
      (pair, "")

hashlessUrl : Location -> String
hashlessUrl location =
    location.origin ++ location.pathname ++ location.search
