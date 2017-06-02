module Decoder exposing (..)

import Json.Decode exposing (Decoder, string, int, float, dict, list, bool, map, value, decodeValue, decodeString, lazy, succeed, fail, andThen)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Dict exposing (Dict)


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe name decoder =
    optional name (map Just decoder) Nothing


customDecoder : Decoder a -> (a -> Result String b) -> Decoder b
customDecoder decoder toResult =
    andThen
        (\a ->
            case toResult a of
                Ok b ->
                    succeed b

                Err err ->
                    fail err
        )
        decoder


type alias AlgorithmRecord =
    { parameters : AlgorithmParameters
    , code : String
    }


type Algorithm
    = Algorithm AlgorithmRecord


decodeAlgorithm : Decoder Algorithm
decodeAlgorithm =
    decode AlgorithmRecord
        |> required "parameters" decodeAlgorithmParameters
        |> required "code" string
        |> map Algorithm


type AlgorithmParameters
    = AlgorithmParameters (List AlgorithmParametersItem)


decodeAlgorithmParameters : Decoder AlgorithmParameters
decodeAlgorithmParameters =
    list (lazy (\_ -> decodeAlgorithmParametersItem))
        |> map AlgorithmParameters


type alias AlgorithmParametersItemRecord =
    { value : String
    , name : String
    }


type AlgorithmParametersItem
    = AlgorithmParametersItem AlgorithmParametersItemRecord


decodeAlgorithmParametersItem : Decoder AlgorithmParametersItem
decodeAlgorithmParametersItem =
    decode AlgorithmParametersItemRecord
        |> required "value" string
        |> required "name" string
        |> map AlgorithmParametersItem


type AtomList
    = AtomList (List AtomObject)


decodeAtomList : Decoder AtomList
decodeAtomList =
    list (lazy (\_ -> decodeAtomObject))
        |> map AtomList


type alias AtomObjectRecord =
    { url : String
    , name : String
    , modificationTime : String
    , id : Int
    , epsg : Int
    }


type AtomObject
    = AtomObject AtomObjectRecord


decodeAtomObject : Decoder AtomObject
decodeAtomObject =
    decode AtomObjectRecord
        |> required "url" string
        |> required "name" string
        |> required "modificationTime" string
        |> required "id" int
        |> required "epsg" int
        |> map AtomObject


type alias EnvLayerRecord =
    { dateCode : Maybe String
    , altPredCode : Maybe String
    , gcmCode : Maybe String
    , envCode : Maybe String
    , spatialRaster : Maybe SpatialRaster
    , map : Maybe Map
    , metadata : Maybe EnvLayerMetadata
    , etag : Maybe String
    , statusModTime : Maybe String
    , status : Maybe Int
    , user : Maybe String
    , url : Maybe String
    , id : Maybe String
    , objectType : Maybe String
    }


type EnvLayer
    = EnvLayer EnvLayerRecord


decodeEnvLayer : Decoder EnvLayer
decodeEnvLayer =
    decode EnvLayerRecord
        |> maybe "dateCode" string
        |> maybe "altPredCode" string
        |> maybe "gcmCode" string
        |> maybe "envCode" string
        |> maybe "spatialRaster" (lazy (\_ -> decodeSpatialRaster))
        |> maybe "map" (lazy (\_ -> decodeMap))
        |> maybe "metadata" (lazy (\_ -> decodeEnvLayerMetadata))
        |> maybe "etag" string
        |> maybe "statusModTime" string
        |> maybe "status" int
        |> maybe "user" string
        |> maybe "url" string
        |> maybe "id" string
        |> maybe "objectType" string
        |> map EnvLayer


type alias EnvLayerMetadataRecord =
    {}


type EnvLayerMetadata
    = EnvLayerMetadata EnvLayerMetadataRecord


decodeEnvLayerMetadata : Decoder EnvLayerMetadata
decodeEnvLayerMetadata =
    decode EnvLayerMetadataRecord
        |> map EnvLayerMetadata


type alias GridSetRecord =
    {}


type GridSet
    = GridSet GridSetRecord


decodeGridSet : Decoder GridSet
decodeGridSet =
    decode GridSetRecord
        |> map GridSet


type alias LayerRecord =
    { spatialRaster : Maybe SpatialRaster
    , metadata : Maybe LayerMetadata
    , etag : Maybe String
    , statusModTime : Maybe String
    , status : Maybe Int
    , user : Maybe String
    , url : Maybe String
    , id : Maybe String
    , objectType : Maybe String
    }


type Layer
    = Layer LayerRecord


decodeLayer : Decoder Layer
decodeLayer =
    decode LayerRecord
        |> maybe "spatialRaster" (lazy (\_ -> decodeSpatialRaster))
        |> maybe "metadata" (lazy (\_ -> decodeLayerMetadata))
        |> maybe "etag" string
        |> maybe "statusModTime" string
        |> maybe "status" int
        |> maybe "user" string
        |> maybe "url" string
        |> maybe "id" string
        |> maybe "objectType" string
        |> map Layer


type alias LayerMetadataRecord =
    {}


type LayerMetadata
    = LayerMetadata LayerMetadataRecord


decodeLayerMetadata : Decoder LayerMetadata
decodeLayerMetadata =
    decode LayerMetadataRecord
        |> map LayerMetadata


type alias MapRecord =
    { layerNames : Maybe MapLayerNames
    , mapName : Maybe String
    , ogcEndpoint : Maybe String
    }


type Map
    = Map MapRecord


decodeMap : Decoder Map
decodeMap =
    decode MapRecord
        |> maybe "layerNames" decodeMapLayerNames
        |> maybe "mapName" string
        |> maybe "ogcEndpoint" string
        |> map Map


type MapLayerNames
    = MapLayerNames (List MapLayerNamesItem)


decodeMapLayerNames : Decoder MapLayerNames
decodeMapLayerNames =
    list (lazy (\_ -> decodeMapLayerNamesItem))
        |> map MapLayerNames


type alias MapLayerNamesItemRecord =
    { layerName : Maybe String
    , metadataUrl : Maybe String
    }


type MapLayerNamesItem
    = MapLayerNamesItem MapLayerNamesItemRecord


decodeMapLayerNamesItem : Decoder MapLayerNamesItem
decodeMapLayerNamesItem =
    decode MapLayerNamesItemRecord
        |> maybe "layerName" string
        |> maybe "metadataUrl" string
        |> map MapLayerNamesItem


type alias MatrixRecord =
    {}


type Matrix
    = Matrix MatrixRecord


decodeMatrix : Decoder Matrix
decodeMatrix =
    decode MatrixRecord
        |> map Matrix


type alias MatrixColumnRecord =
    {}


type MatrixColumn
    = MatrixColumn MatrixColumnRecord


decodeMatrixColumn : Decoder MatrixColumn
decodeMatrixColumn =
    decode MatrixColumnRecord
        |> map MatrixColumn


type alias ObjectCountRecord =
    { count : Maybe Int
    }


type ObjectCount
    = ObjectCount ObjectCountRecord


decodeObjectCount : Decoder ObjectCount
decodeObjectCount =
    decode ObjectCountRecord
        |> maybe "count" int
        |> map ObjectCount


type alias ObjectRefRecord =
    { metadataUrl : Maybe String
    , id : Maybe Int
    }


type ObjectRef
    = ObjectRef ObjectRefRecord


decodeObjectRef : Decoder ObjectRef
decodeObjectRef =
    decode ObjectRefRecord
        |> maybe "metadataUrl" string
        |> maybe "id" int
        |> map ObjectRef


type alias OccurrenceSetRecord =
    { squid : Maybe String
    , speciesName : Maybe String
    , spatialVector : Maybe SpatialVector
    , map : Maybe Map
    , metadata : Maybe OccurrenceSetMetadata
    , etag : Maybe String
    , statusModTime : Maybe String
    , status : Maybe Int
    , user : Maybe String
    , url : Maybe String
    , id : Maybe String
    , objectType : Maybe String
    }


type OccurrenceSet
    = OccurrenceSet OccurrenceSetRecord


decodeOccurrenceSet : Decoder OccurrenceSet
decodeOccurrenceSet =
    decode OccurrenceSetRecord
        |> maybe "squid" string
        |> maybe "speciesName" string
        |> maybe "spatialVector" (lazy (\_ -> decodeSpatialVector))
        |> maybe "map" (lazy (\_ -> decodeMap))
        |> maybe "metadata" (lazy (\_ -> decodeOccurrenceSetMetadata))
        |> maybe "etag" string
        |> maybe "statusModTime" string
        |> maybe "status" int
        |> maybe "user" string
        |> maybe "url" string
        |> maybe "id" string
        |> maybe "objectType" string
        |> map OccurrenceSet


type alias OccurrenceSetMetadataRecord =
    {}


type OccurrenceSetMetadata
    = OccurrenceSetMetadata OccurrenceSetMetadataRecord


decodeOccurrenceSetMetadata : Decoder OccurrenceSetMetadata
decodeOccurrenceSetMetadata =
    decode OccurrenceSetMetadataRecord
        |> map OccurrenceSetMetadata


type alias ProjectionRecord =
    { occurrenceSet : Maybe ObjectRef
    , squid : Maybe String
    , speciesName : Maybe String
    , projectionScenario : Maybe ScenarioRef
    , modelScenario : Maybe ScenarioRef
    , algorithm : Maybe Algorithm
    , spatialRaster : Maybe SpatialRaster
    , map : Maybe Map
    , metadata : Maybe ProjectionMetadata
    , etag : Maybe String
    , statusModTime : Maybe String
    , status : Maybe Int
    , user : Maybe String
    , url : Maybe String
    , id : Maybe String
    , objectType : Maybe String
    }


type Projection
    = Projection ProjectionRecord


decodeProjection : Decoder Projection
decodeProjection =
    decode ProjectionRecord
        |> maybe "occurrenceSet" (lazy (\_ -> decodeObjectRef))
        |> maybe "squid" string
        |> maybe "speciesName" string
        |> maybe "projectionScenario" (lazy (\_ -> decodeScenarioRef))
        |> maybe "modelScenario" (lazy (\_ -> decodeScenarioRef))
        |> maybe "algorithm" (lazy (\_ -> decodeAlgorithm))
        |> maybe "spatialRaster" (lazy (\_ -> decodeSpatialRaster))
        |> maybe "map" (lazy (\_ -> decodeMap))
        |> maybe "metadata" (lazy (\_ -> decodeProjectionMetadata))
        |> maybe "etag" string
        |> maybe "statusModTime" string
        |> maybe "status" int
        |> maybe "user" string
        |> maybe "url" string
        |> maybe "id" string
        |> maybe "objectType" string
        |> map Projection


type alias ProjectionMetadataRecord =
    {}


type ProjectionMetadata
    = ProjectionMetadata ProjectionMetadataRecord


decodeProjectionMetadata : Decoder ProjectionMetadata
decodeProjectionMetadata =
    decode ProjectionMetadataRecord
        |> map ProjectionMetadata


type alias ProjectionPOSTRecord =
    { projectionScenarios : ProjectionPOSTProjectionScenarios
    , modelScenario : ProjectionPOSTModelScenario
    , occurrenceSets : ProjectionPOSTOccurrenceSets
    , algorithms : ProjectionPOSTAlgorithms
    }


type ProjectionPOST
    = ProjectionPOST ProjectionPOSTRecord


decodeProjectionPOST : Decoder ProjectionPOST
decodeProjectionPOST =
    decode ProjectionPOSTRecord
        |> required "projectionScenarios" decodeProjectionPOSTProjectionScenarios
        |> required "modelScenario" (lazy (\_ -> decodeProjectionPOSTModelScenario))
        |> required "occurrenceSets" decodeProjectionPOSTOccurrenceSets
        |> required "algorithms" decodeProjectionPOSTAlgorithms
        |> map ProjectionPOST


type ProjectionPOSTAlgorithms
    = ProjectionPOSTAlgorithms (List Algorithm)


decodeProjectionPOSTAlgorithms : Decoder ProjectionPOSTAlgorithms
decodeProjectionPOSTAlgorithms =
    list (lazy (\_ -> decodeAlgorithm))
        |> map ProjectionPOSTAlgorithms


type ProjectionPOSTOccurrenceSets
    = ProjectionPOSTOccurrenceSets (List ProjectionPOSTOccurrenceSetsItem)


decodeProjectionPOSTOccurrenceSets : Decoder ProjectionPOSTOccurrenceSets
decodeProjectionPOSTOccurrenceSets =
    list (lazy (\_ -> decodeProjectionPOSTOccurrenceSetsItem))
        |> map ProjectionPOSTOccurrenceSets


type alias ProjectionPOSTOccurrenceSetsItemRecord =
    { occurrenceSetId : Maybe Int
    }


type ProjectionPOSTOccurrenceSetsItem
    = ProjectionPOSTOccurrenceSetsItem ProjectionPOSTOccurrenceSetsItemRecord


decodeProjectionPOSTOccurrenceSetsItem : Decoder ProjectionPOSTOccurrenceSetsItem
decodeProjectionPOSTOccurrenceSetsItem =
    decode ProjectionPOSTOccurrenceSetsItemRecord
        |> maybe "occurrenceSetId" int
        |> map ProjectionPOSTOccurrenceSetsItem


type alias ProjectionPOSTModelScenarioRecord =
    { scenarioCode : Maybe String
    , scenarioId : Maybe Int
    }


type ProjectionPOSTModelScenario
    = ProjectionPOSTModelScenario ProjectionPOSTModelScenarioRecord


decodeProjectionPOSTModelScenario : Decoder ProjectionPOSTModelScenario
decodeProjectionPOSTModelScenario =
    decode ProjectionPOSTModelScenarioRecord
        |> maybe "scenarioCode" string
        |> maybe "scenarioId" int
        |> map ProjectionPOSTModelScenario


type ProjectionPOSTProjectionScenarios
    = ProjectionPOSTProjectionScenarios (List ProjectionPOSTProjectionScenariosItem)


decodeProjectionPOSTProjectionScenarios : Decoder ProjectionPOSTProjectionScenarios
decodeProjectionPOSTProjectionScenarios =
    list (lazy (\_ -> decodeProjectionPOSTProjectionScenariosItem))
        |> map ProjectionPOSTProjectionScenarios


type alias ProjectionPOSTProjectionScenariosItemRecord =
    { scenarioCode : Maybe String
    , scenarioId : Maybe Int
    }


type ProjectionPOSTProjectionScenariosItem
    = ProjectionPOSTProjectionScenariosItem ProjectionPOSTProjectionScenariosItemRecord


decodeProjectionPOSTProjectionScenariosItem : Decoder ProjectionPOSTProjectionScenariosItem
decodeProjectionPOSTProjectionScenariosItem =
    decode ProjectionPOSTProjectionScenariosItemRecord
        |> maybe "scenarioCode" string
        |> maybe "scenarioId" int
        |> map ProjectionPOSTProjectionScenariosItem


type alias ScenarioRecord =
    { envTypeId : Maybe String
    , dateCode : Maybe String
    , alternatePrediction : Maybe String
    , gcmCode : Maybe String
    , environmentalCode : Maybe String
    , spatial : Maybe Spatial
    , map : Maybe Map
    , metadata : Maybe ScenarioMetadata
    , userId : Maybe String
    , code : Maybe String
    , etag : Maybe String
    , metadataUrl : Maybe String
    , id : Maybe String
    , objectType : Maybe String
    }


type Scenario
    = Scenario ScenarioRecord


decodeScenario : Decoder Scenario
decodeScenario =
    decode ScenarioRecord
        |> maybe "envTypeId" string
        |> maybe "dateCode" string
        |> maybe "alternatePrediction" string
        |> maybe "gcmCode" string
        |> maybe "environmentalCode" string
        |> maybe "spatial" (lazy (\_ -> decodeSpatial))
        |> maybe "map" (lazy (\_ -> decodeMap))
        |> maybe "metadata" (lazy (\_ -> decodeScenarioMetadata))
        |> maybe "userId" string
        |> maybe "code" string
        |> maybe "etag" string
        |> maybe "metadataUrl" string
        |> maybe "id" string
        |> maybe "objectType" string
        |> map Scenario


type alias ScenarioMetadataRecord =
    {}


type ScenarioMetadata
    = ScenarioMetadata ScenarioMetadataRecord


decodeScenarioMetadata : Decoder ScenarioMetadata
decodeScenarioMetadata =
    decode ScenarioMetadataRecord
        |> map ScenarioMetadata


type alias ScenarioPOSTRecord =
    { metadata : Maybe ScenarioPOSTMetadata
    , units : Maybe String
    , resolution : Maybe Float
    , layers : Maybe ScenarioPOSTLayers
    , epsgCode : Maybe String
    , scenarioCode : Maybe String
    }


type ScenarioPOST
    = ScenarioPOST ScenarioPOSTRecord


decodeScenarioPOST : Decoder ScenarioPOST
decodeScenarioPOST =
    decode ScenarioPOSTRecord
        |> maybe "metadata" (lazy (\_ -> decodeScenarioPOSTMetadata))
        |> maybe "units" string
        |> maybe "resolution" float
        |> maybe "layers" decodeScenarioPOSTLayers
        |> maybe "epsgCode" string
        |> maybe "scenarioCode" string
        |> map ScenarioPOST


type ScenarioPOSTLayers
    = ScenarioPOSTLayers (List Int)


decodeScenarioPOSTLayers : Decoder ScenarioPOSTLayers
decodeScenarioPOSTLayers =
    list int
        |> map ScenarioPOSTLayers


type alias ScenarioPOSTMetadataRecord =
    {}


type ScenarioPOSTMetadata
    = ScenarioPOSTMetadata ScenarioPOSTMetadataRecord


decodeScenarioPOSTMetadata : Decoder ScenarioPOSTMetadata
decodeScenarioPOSTMetadata =
    decode ScenarioPOSTMetadataRecord
        |> map ScenarioPOSTMetadata


type alias ScenarioRefRecord =
    { metadataUrl : Maybe String
    , id : Maybe Int
    , code : Maybe String
    }


type ScenarioRef
    = ScenarioRef ScenarioRefRecord


decodeScenarioRef : Decoder ScenarioRef
decodeScenarioRef =
    decode ScenarioRefRecord
        |> maybe "metadataUrl" string
        |> maybe "id" int
        |> maybe "code" string
        |> map ScenarioRef


type alias SpatialRecord =
    { resolution : Maybe Float
    , mapUnits : Maybe String
    , bbox : Maybe String
    , epsg : Maybe String
    }


type Spatial
    = Spatial SpatialRecord


decodeSpatial : Decoder Spatial
decodeSpatial =
    decode SpatialRecord
        |> maybe "resolution" float
        |> maybe "mapUnits" string
        |> maybe "bbox" string
        |> maybe "epsg" string
        |> map Spatial


type alias ShapegridRecord =
    {}


type Shapegrid
    = Shapegrid ShapegridRecord


decodeShapegrid : Decoder Shapegrid
decodeShapegrid =
    decode ShapegridRecord
        |> map Shapegrid


type alias SpatialRasterRecord =
    { dataType : Maybe String
    , valueUnits : Maybe String
    , maxVal : Maybe Float
    , minVal : Maybe Float
    , dataFormat : Maybe String
    , gdalType : Maybe String
    , sha256 : Maybe String
    , dataUrl : Maybe String
    , resolution : Maybe Float
    , mapUnits : Maybe String
    , bbox : Maybe String
    , epsg : Maybe String
    }


type SpatialRaster
    = SpatialRaster SpatialRasterRecord


decodeSpatialRaster : Decoder SpatialRaster
decodeSpatialRaster =
    decode SpatialRasterRecord
        |> maybe "dataType" string
        |> maybe "valueUnits" string
        |> maybe "maxVal" float
        |> maybe "minVal" float
        |> maybe "dataFormat" string
        |> maybe "gdalType" string
        |> maybe "sha256" string
        |> maybe "dataUrl" string
        |> maybe "resolution" float
        |> maybe "mapUnits" string
        |> maybe "bbox" string
        |> maybe "epsg" string
        |> map SpatialRaster


type alias SpatialVectorRecord =
    { numFeatures : Maybe Int
    , dataFormat : Maybe String
    , ogrType : Maybe String
    , sha256 : Maybe String
    , dataUrl : Maybe String
    , resolution : Maybe Float
    , mapUnits : Maybe String
    , bbox : Maybe String
    , epsg : Maybe String
    }


type SpatialVector
    = SpatialVector SpatialVectorRecord


decodeSpatialVector : Decoder SpatialVector
decodeSpatialVector =
    decode SpatialVectorRecord
        |> maybe "numFeatures" int
        |> maybe "dataFormat" string
        |> maybe "ogrType" string
        |> maybe "sha256" string
        |> maybe "dataUrl" string
        |> maybe "resolution" float
        |> maybe "mapUnits" string
        |> maybe "bbox" string
        |> maybe "epsg" string
        |> map SpatialVector


type alias TreeRecord =
    {}


type Tree
    = Tree TreeRecord


decodeTree : Decoder Tree
decodeTree =
    decode TreeRecord
        |> map Tree


type alias TreePOSTRecord =
    {}


type TreePOST
    = TreePOST TreePOSTRecord


decodeTreePOST : Decoder TreePOST
decodeTreePOST =
    decode TreePOSTRecord
        |> map TreePOST
