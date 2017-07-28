module ProgramFlags exposing (..)


type alias ApiRoot =
    String


type alias Flags =
    { apiRoot : ApiRoot
    , minimumOccurrencePoints : Int
    , completedPollingSeconds : Maybe Float
    }
