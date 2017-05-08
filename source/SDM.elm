module SDM exposing (..)

import Decoder exposing (ProjectionPOST(..))


-- Model


type alias Model =
    { projectionPOST : ProjectionPOST
    }


model : Model
model =
    { projectionPOST =
        ProjectionPOST
            { projectionScenarios = Nothing
            , modelScenario = Nothing
            , occurrenceSets = Nothing
            , algorithms = Nothing
            }
    }
