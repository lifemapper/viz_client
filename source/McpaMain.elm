module McpaMain exposing (..)

import Html
import McpaModel exposing (..)
import McpaView


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = McpaView.view
        , subscriptions = subscriptions
        }
