module McpaMain exposing (..)

import Html
import McpaModel exposing (..)
import LinearTreeView
import McpaView


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = LinearTreeView.view
        , subscriptions = subscriptions
        }
