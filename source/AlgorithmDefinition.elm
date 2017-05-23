module AlgorithmDefinition exposing (..)

import Json.Decode exposing (Decoder, field, string, list, succeed, fail, andThen, map, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


type alias Algorithm =
    { code : String
    , name : String
    , version : String
    , authors : String
    , link : String
    , software : String
    , description : String
    , parameters : List Parameter
    }


type alias Parameter =
    { name : String
    , displayName : String
    , min : Maybe String
    , max : Maybe String
    , default : Maybe String
    , dataType : ParameterType
    , doc : String
    , options : List ParameterOption
    }



type ParameterType
    = IntegerParam
    | FloatParam

typeFromString : String -> Decoder ParameterType
typeFromString s =
    case s of
        "Integer" -> succeed IntegerParam
        "Float" -> succeed FloatParam
        t -> fail ("Unknown type: " ++ t ++ ". Expected 'Integer' or 'Float'.")


type alias ParameterOption =
    { name : String
    , value : Int
    }

optionValueFromString : String -> Decoder Int
optionValueFromString s =
    case String.toInt s of
        Err msg -> fail msg
        Ok v -> succeed v



algorithms : List Algorithm
algorithms =
    case decodeString (list algorithm) json of
        Err msg ->
            Debug.crash msg

        Ok algs ->
            algs


algorithm : Decoder Algorithm
algorithm =
    decode Algorithm
        |> required "_code" string
        |> required "_name" string
        |> required "_version" string
        |> required "authors" string
        |> required "link" string
        |> required "software" string
        |> required "description" string
        |> required "parameters" (field "parameter" (list parameter))


parameter : Decoder Parameter
parameter =
    decode Parameter
        |> required "_name" string
        |> required "_displayName" string
        |> optional "_min" (map Just string) Nothing
        |> optional "_max" (map Just string) Nothing
        |> optional "_default" (map Just string) Nothing
        |> required "_type" (string |> andThen typeFromString)
        |> required "doc" string
        |> optional "options" (field "option" (list parameterOption)) []


parameterOption : Decoder ParameterOption
parameterOption =
    decode ParameterOption
        |> required "_name" string
        |> required "_value" (string |> andThen optionValueFromString)



json : String
json =
    """[
    {
        "authors": "Chopra, Paras, modified by Alex Oshika Avilla and Fabricio Augusto Rodrigues",
        "link": "http://openmodeller.sourceforge.net/algorithms/ann.html",
        "software": "openModeller",
        "description": "         An artificial neural network (ANN), also called a simulated neural          network (SNN) or commonly just neural network (NN), is an          interconnected group of artificial neurons that uses a mathematical or          computational model for information processing based on a          connectionistic approach to computation. In most cases an ANN is an          adaptive system that changes its structure based on external or          internal information that flows through the network. In more practical          terms, neural networks are non-linear statistical data modeling or          decision making tools. They can be used to model complex relationships          between inputs and outputs or to find patterns in data. Content          retrieved from Wikipedia on the 06th of May, 2008:          http://en.wikipedia.org/wiki/Neural_network      ",
        "parameters": {
            "parameter": [
                {
                    "doc": "               Number of neurons in the hidden layer (additional layer to the                input and output layers, not connected externally).            ",
                    "_name": "HiddenLayerNeurons",
                    "_displayName": "Hidden Layer Neurons",
                    "_min": "1",
                    "_type": "Integer",
                    "_default": "14"
                },
                {
                    "doc": "               Learning Rate. Training parameter that controls the size of                weight and bias changes during learning.            ",
                    "_name": "LearningRate",
                    "_displayName": "Learning Rate",
                    "_min": "0",
                    "_max": "1",
                    "_type": "Float",
                    "_default": "0.3"
                },
                {
                    "doc": "               Momentum simply adds a fraction m of the previous weight update                to the current one. The momentum parameter is used to prevent                the system from converging to a local minimum or saddle point. A                high momentum parameter can also help to increase the speed of                convergence of the system. However, setting the momentum                parameter too high can create a risk of overshooting the                minimum, which can cause the system to become unstable. A                momentum coefficient that is too low cannot reliably avoid local                minima, and can also slow down the training of the system.            ",
                    "_name": "Momentum",
                    "_displayName": "Momentum",
                    "_min": "0",
                    "_max": "1",
                    "_type": "Float",
                    "_default": "0.05"
                },
                {
                    "doc": "               0 = train by epoch, 1 = train by minimum error            ",
                    "options": {
                        "option": [
                            {
                                "_name": "Train by Epoch",
                                "_value": "0"
                            },
                            {
                                "_name": "Train by Minimum Error",
                                "_value": "1"
                            }
                        ]
                    },
                    "_name": "Choice",
                    "_displayName": "Choice",
                    "_min": "0",
                    "_max": "1",
                    "_type": "Integer",
                    "_default": "1"
                },
                {
                    "doc": "               Determines when training will stop once the number of iterations                exceeds epochs. When training by minimum error, this represents                the maximum number of iterations.            ",
                    "_name": "Epoch",
                    "_displayName": "Epoch",
                    "_min": "1",
                    "_type": "Integer",
                    "_default": "5000000"
                },
                {
                    "doc": "               Minimum mean square error of the epoch. Square root of the sum                of squared differences between the network targets and actual                outputs divided by number of patterns (only for training by                minimum error).            ",
                    "_name": "MinimumError",
                    "_displayName": "Minimum Error",
                    "_min": "0",
                    "_max": "0.5",
                    "_type": "Float",
                    "_default": "0.01"
                }
            ]
        },
        "_code": "ANN",
        "_name": "Artificial Neural Network",
        "_version": "0.2"
    }

    ]
"""
