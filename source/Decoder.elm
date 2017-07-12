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
        |> required "parameters" (lazy (\_ -> decodeAlgorithmParameters))
        |> required "code" string
        |> map Algorithm


type alias AlgorithmParameters =
    List ( String, String )


decodeAlgorithmParameters : Decoder AlgorithmParameters
decodeAlgorithmParameters =
    Json.Decode.keyValuePairs value
        |> map (List.map (\( name, value ) -> ( name, toString value )))


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
    { layers : MapLayers
    , mapName : String
    , endpoint : String
    }


type Map
    = Map MapRecord


decodeMap : Decoder Map
decodeMap =
    decode MapRecord
        |> required "layers" decodeMapLayers
        |> required "mapName" string
        |> required "endpoint" string
        |> map Map


type MapLayers
    = MapLayers (List MapLayersItem)


decodeMapLayers : Decoder MapLayers
decodeMapLayers =
    list (lazy (\_ -> decodeMapLayersItem))
        |> map MapLayers


type alias MapLayersItemRecord =
    { layerName : String
    , metadataUrl : String
    }


type MapLayersItem
    = MapLayersItem MapLayersItemRecord


decodeMapLayersItem : Decoder MapLayersItem
decodeMapLayersItem =
    decode MapLayersItemRecord
        |> required "layerName" string
        |> required "metadataUrl" string
        |> map MapLayersItem


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
    , map : Maybe SingleLayerMap
    , metadata : Maybe OccurrenceSetMetadata
    , etag : Maybe String
    , statusModTime : Maybe String
    , status : Maybe Int
    , user : Maybe String
    , url : Maybe String
    , id : Int
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
        |> maybe "map" (lazy (\_ -> decodeSingleLayerMap))
        |> maybe "metadata" (lazy (\_ -> decodeOccurrenceSetMetadata))
        |> maybe "etag" string
        |> maybe "statusModTime" string
        |> maybe "status" int
        |> maybe "user" string
        |> maybe "url" string
        |> required "id" int
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
    , map : Maybe SingleLayerMap
    , metadata : Maybe ProjectionMetadata
    , etag : Maybe String
    , statusModTime : Maybe String
    , status : Maybe Int
    , user : Maybe String
    , url : Maybe String
    , id : Int
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
        |> maybe "map" (lazy (\_ -> decodeSingleLayerMap))
        |> maybe "metadata" (lazy (\_ -> decodeProjectionMetadata))
        |> maybe "etag" string
        |> maybe "statusModTime" string
        |> maybe "status" int
        |> maybe "user" string
        |> maybe "url" string
        |> required "id" int
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


type alias BoomPOSTRecord =
    { projectionScenarios : BoomPOSTProjectionScenarios
    , modelScenario : BoomPOSTModelScenario
    , occurrenceSets : BoomPOSTOccurrenceSets
    , algorithms : BoomPOSTAlgorithms
    }


type BoomPOST
    = BoomPOST BoomPOSTRecord


decodeBoomPOST : Decoder BoomPOST
decodeBoomPOST =
    decode BoomPOSTRecord
        |> required "projectionScenarios" decodeBoomPOSTProjectionScenarios
        |> required "modelScenario" (lazy (\_ -> decodeBoomPOSTModelScenario))
        |> required "occurrenceSets" decodeBoomPOSTOccurrenceSets
        |> required "algorithms" decodeBoomPOSTAlgorithms
        |> map BoomPOST


type BoomPOSTAlgorithms
    = BoomPOSTAlgorithms (List Algorithm)


decodeBoomPOSTAlgorithms : Decoder BoomPOSTAlgorithms
decodeBoomPOSTAlgorithms =
    list (lazy (\_ -> decodeAlgorithm))
        |> map BoomPOSTAlgorithms


type BoomPOSTOccurrenceSets
    = BoomPOSTOccurrenceSets (List BoomPOSTOccurrenceSetsItem)


decodeBoomPOSTOccurrenceSets : Decoder BoomPOSTOccurrenceSets
decodeBoomPOSTOccurrenceSets =
    list (lazy (\_ -> decodeBoomPOSTOccurrenceSetsItem))
        |> map BoomPOSTOccurrenceSets


type alias BoomPOSTOccurrenceSetsItemRecord =
    { occurrenceMeta : Maybe BoomPOSTOccurrenceSetsItemOccurrenceMeta
    , occurrenceData : Maybe String
    , occurrenceSetId : Maybe Int
    }


type BoomPOSTOccurrenceSetsItem
    = BoomPOSTOccurrenceSetsItem BoomPOSTOccurrenceSetsItemRecord


decodeBoomPOSTOccurrenceSetsItem : Decoder BoomPOSTOccurrenceSetsItem
decodeBoomPOSTOccurrenceSetsItem =
    decode BoomPOSTOccurrenceSetsItemRecord
        |> maybe "occurrenceMeta" (lazy (\_ -> decodeBoomPOSTOccurrenceSetsItemOccurrenceMeta))
        |> maybe "occurrenceData" string
        |> maybe "occurrenceSetId" int
        |> map BoomPOSTOccurrenceSetsItem


type alias BoomPOSTOccurrenceSetsItemOccurrenceMetaRecord =
    {}


type BoomPOSTOccurrenceSetsItemOccurrenceMeta
    = BoomPOSTOccurrenceSetsItemOccurrenceMeta BoomPOSTOccurrenceSetsItemOccurrenceMetaRecord


decodeBoomPOSTOccurrenceSetsItemOccurrenceMeta : Decoder BoomPOSTOccurrenceSetsItemOccurrenceMeta
decodeBoomPOSTOccurrenceSetsItemOccurrenceMeta =
    decode BoomPOSTOccurrenceSetsItemOccurrenceMetaRecord
        |> map BoomPOSTOccurrenceSetsItemOccurrenceMeta


type alias BoomPOSTModelScenarioRecord =
    { scenarioCode : Maybe String
    , scenarioId : Maybe Int
    }


type BoomPOSTModelScenario
    = BoomPOSTModelScenario BoomPOSTModelScenarioRecord


decodeBoomPOSTModelScenario : Decoder BoomPOSTModelScenario
decodeBoomPOSTModelScenario =
    decode BoomPOSTModelScenarioRecord
        |> maybe "scenarioCode" string
        |> maybe "scenarioId" int
        |> map BoomPOSTModelScenario


type BoomPOSTProjectionScenarios
    = BoomPOSTProjectionScenarios (List BoomPOSTProjectionScenariosItem)


decodeBoomPOSTProjectionScenarios : Decoder BoomPOSTProjectionScenarios
decodeBoomPOSTProjectionScenarios =
    list (lazy (\_ -> decodeBoomPOSTProjectionScenariosItem))
        |> map BoomPOSTProjectionScenarios


type alias BoomPOSTProjectionScenariosItemRecord =
    { scenarioCode : Maybe String
    , scenarioId : Maybe Int
    }


type BoomPOSTProjectionScenariosItem
    = BoomPOSTProjectionScenariosItem BoomPOSTProjectionScenariosItemRecord


decodeBoomPOSTProjectionScenariosItem : Decoder BoomPOSTProjectionScenariosItem
decodeBoomPOSTProjectionScenariosItem =
    decode BoomPOSTProjectionScenariosItemRecord
        |> maybe "scenarioCode" string
        |> maybe "scenarioId" int
        |> map BoomPOSTProjectionScenariosItem


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
    , url : Maybe String
    , id : Int
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
        |> maybe "url" string
        |> required "id" int
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


type alias ScenarioPackageRecord =
    { user : Maybe String
    , name : Maybe String
    , url : Maybe String
    , scenarios : ScenarioPackageScenarios
    , id : Int
    , objectType : Maybe String
    }


type ScenarioPackage
    = ScenarioPackage ScenarioPackageRecord


decodeScenarioPackage : Decoder ScenarioPackage
decodeScenarioPackage =
    decode ScenarioPackageRecord
        |> maybe "user" string
        |> maybe "name" string
        |> maybe "url" string
        |> required "scenarios" decodeScenarioPackageScenarios
        |> required "id" int
        |> maybe "objectType" string
        |> map ScenarioPackage


type ScenarioPackageScenarios
    = ScenarioPackageScenarios (List Scenario)


decodeScenarioPackageScenarios : Decoder ScenarioPackageScenarios
decodeScenarioPackageScenarios =
    list (lazy (\_ -> decodeScenario))
        |> map ScenarioPackageScenarios


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
    , bbox : Maybe SpatialBbox
    , epsg : Maybe Int
    }


type Spatial
    = Spatial SpatialRecord


decodeSpatial : Decoder Spatial
decodeSpatial =
    decode SpatialRecord
        |> maybe "resolution" float
        |> maybe "mapUnits" string
        |> maybe "bbox" decodeSpatialBbox
        |> maybe "epsg" int
        |> map Spatial


type SpatialBbox
    = SpatialBbox (List Float)


decodeSpatialBbox : Decoder SpatialBbox
decodeSpatialBbox =
    list float
        |> map SpatialBbox


type alias ShapegridRecord =
    {}


type Shapegrid
    = Shapegrid ShapegridRecord


decodeShapegrid : Decoder Shapegrid
decodeShapegrid =
    decode ShapegridRecord
        |> map Shapegrid


type alias SingleLayerMapRecord =
    { layerName : String
    , mapName : String
    , endpoint : String
    }


type SingleLayerMap
    = SingleLayerMap SingleLayerMapRecord


decodeSingleLayerMap : Decoder SingleLayerMap
decodeSingleLayerMap =
    decode SingleLayerMapRecord
        |> required "layerName" string
        |> required "mapName" string
        |> required "endpoint" string
        |> map SingleLayerMap


type SolrList
    = SolrList (List SolrPAV)


decodeSolrList : Decoder SolrList
decodeSolrList =
    list (lazy (\_ -> decodeSolrPAV))
        |> map SolrList


type alias SolrPAVRecord =
    { userId : String
    , squid : String
    , shapegridMetaUrl : String
    , shapegridId : Int
    , shapegridDataUrl : String
    , sdmProjScenarioUrl : Maybe String
    , sdmProjScenarioId : Maybe Int
    , sdmProjScenarioGCM : Maybe String
    , sdmProjScenarioDateCode : Maybe String
    , sdmProjScenarioCode : Maybe String
    , sdmProjScenarioAltPredCode : Maybe String
    , sdmProjMetaUrl : Maybe String
    , sdmProjId : Maybe Int
    , sdmProjDataUrl : Maybe String
    , pavMetaUrl : String
    , pointCount : Maybe Int
    , occurrenceMetaUrl : Maybe String
    , occurrenceId : Maybe Int
    , occurrenceDataUrl : Maybe String
    , modelScenarioUrl : Maybe String
    , modelScenarioId : Maybe Int
    , modelScenarioCode : Maybe String
    , id : Int
    , gridSetMetaUrl : String
    , gridSetId : Int
    , epsgCode : Int
    , displayName : String
    , compressedPAV : String
    , algorithmParameters : Maybe String
    , algorithmCode : Maybe String
    }


type SolrPAV
    = SolrPAV SolrPAVRecord


decodeSolrPAV : Decoder SolrPAV
decodeSolrPAV =
    decode SolrPAVRecord
        |> required "userId" string
        |> required "squid" string
        |> required "shapegridMetaUrl" string
        |> required "shapegridId" int
        |> required "shapegridDataUrl" string
        |> maybe "sdmProjScenarioUrl" string
        |> maybe "sdmProjScenarioId" int
        |> maybe "sdmProjScenarioGCM" string
        |> maybe "sdmProjScenarioDateCode" string
        |> maybe "sdmProjScenarioCode" string
        |> maybe "sdmProjScenarioAltPredCode" string
        |> maybe "sdmProjMetaUrl" string
        |> maybe "sdmProjId" int
        |> maybe "sdmProjDataUrl" string
        |> required "pavMetaUrl" string
        |> maybe "pointCount" int
        |> maybe "occurrenceMetaUrl" string
        |> maybe "occurrenceId" int
        |> maybe "occurrenceDataUrl" string
        |> maybe "modelScenarioUrl" string
        |> maybe "modelScenarioId" int
        |> maybe "modelScenarioCode" string
        |> required "id" int
        |> required "gridSetMetaUrl" string
        |> required "gridSetId" int
        |> required "epsgCode" int
        |> required "displayName" string
        |> required "compressedPAV" string
        |> maybe "algorithmParameters" string
        |> maybe "algorithmCode" string
        |> map SolrPAV


type alias SpatialRasterRecord =
    { dataType : Maybe Int
    , valueUnits : Maybe String
    , maxVal : Maybe Float
    , minVal : Maybe Float
    , dataFormat : Maybe String
    , gdalType : Maybe Int
    , sha256 : Maybe String
    , dataUrl : Maybe String
    , resolution : Maybe Float
    , mapUnits : Maybe String
    , bbox : Maybe SpatialRasterBbox
    , epsg : Maybe Int
    }


type SpatialRaster
    = SpatialRaster SpatialRasterRecord


decodeSpatialRaster : Decoder SpatialRaster
decodeSpatialRaster =
    decode SpatialRasterRecord
        |> maybe "dataType" int
        |> maybe "valueUnits" string
        |> maybe "maxVal" float
        |> maybe "minVal" float
        |> maybe "dataFormat" string
        |> maybe "gdalType" int
        |> maybe "sha256" string
        |> maybe "dataUrl" string
        |> maybe "resolution" float
        |> maybe "mapUnits" string
        |> maybe "bbox" decodeSpatialRasterBbox
        |> maybe "epsg" int
        |> map SpatialRaster


type SpatialRasterBbox
    = SpatialRasterBbox (List Float)


decodeSpatialRasterBbox : Decoder SpatialRasterBbox
decodeSpatialRasterBbox =
    list float
        |> map SpatialRasterBbox


type alias SpatialVectorRecord =
    { numFeatures : Maybe Int
    , dataFormat : Maybe String
    , ogrType : Maybe Int
    , sha256 : Maybe String
    , dataUrl : Maybe String
    , resolution : Maybe Float
    , mapUnits : Maybe String
    , bbox : Maybe SpatialVectorBbox
    , epsg : Maybe Int
    }


type SpatialVector
    = SpatialVector SpatialVectorRecord


decodeSpatialVector : Decoder SpatialVector
decodeSpatialVector =
    decode SpatialVectorRecord
        |> maybe "numFeatures" int
        |> maybe "dataFormat" string
        |> maybe "ogrType" int
        |> maybe "sha256" string
        |> maybe "dataUrl" string
        |> maybe "resolution" float
        |> maybe "mapUnits" string
        |> maybe "bbox" decodeSpatialVectorBbox
        |> maybe "epsg" int
        |> map SpatialVector


type SpatialVectorBbox
    = SpatialVectorBbox (List Float)


decodeSpatialVectorBbox : Decoder SpatialVectorBbox
decodeSpatialVectorBbox =
    list float
        |> map SpatialVectorBbox


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
