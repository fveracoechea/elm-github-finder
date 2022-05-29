module Types exposing (..)

import Browser.Navigation exposing (Key)
import Http


type alias Profile =
    { id : Int
    , login : String
    , name : Maybe String
    , avatar_url : String
    , html_url : String
    , bio : Maybe String
    , location : String
    , email : Maybe String
    , public_repos : Int
    , blog : Maybe String
    , followers : Int
    , following : Int
    }


type Token
    = Token String


type ProfileState
    = Unknown
    | Fullfilled Profile
    | Failed String String


type alias Model =
    { navKey : Key
    , profile : ProfileState
    , query : String
    }


type Msg
    = Noop
    | OnSearch
    | SetQuery String
    | SearchProfile String
    | GotProfile (Result Http.Error Profile)
    | SendUserToExternalUrl String
