module TestDecodeMcpa exposing (..)

{-
   Copyright (C) 2018, University of Kansas Center for Research

   Lifemapper Project, lifemapper [at] ku [dot] edu,
   Biodiversity Institute,
   1345 Jayhawk Boulevard, Lawrence, Kansas, 66045, USA

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
-}

import Expect exposing (Expectation)
import Test exposing (..)
import Dict
import Json.Decode exposing (decodeString)
import DecodeMcpa exposing (McpaData, decodeMcpa)


suite : Test
suite =
    describe "Decode Mcpa JSON"
        [ test "heuchera mcpa" <|
            \_ ->
                Expect.equal (Ok ( exampleMcpaVariables, decodedMcpa )) (decodeString decodeMcpa exampleMcpa)
        ]


exampleMcpa : String
exampleMcpa =
    """{
   "headers": {
      "1": [
         "GTOPO30_ELEVATION",
         "GTOPO30_SLOPE_reduced",
         "GTOPO30_ASPECT_reduced",
         "LandCover_1_Needleleaf",
         "LandCover_2_Evergreenbroadleaf",
         "LandCover_3_Deciduousbroadleaf",
         "LandCover_4_Mixedtrees",
         "LandCover_5_Shrubs",
         "LandCover_6_Herbaceous",
         "ISRICSOILGRIDS_new_average_phx10percent_reduced",
         "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced",
         "ISRICSOILGRIDS_new_average_sandpercent_reduced",
         "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced",
         "ISRICSOILGRIDS_new_average_bulkdensity_reduced",
         "ISRICSOILGRIDS_new_average_claypercent_reduced",
         "ISRICSOILGRIDS_new_average_siltpercent_reduced",
         "BIOCLIM_1",
         "BIOCLIM_2",
         "BIOCLIM_3",
         "BIOCLIM_4",
         "BIOCLIM_5",
         "BIOCLIM_6",
         "BIOCLIM_7",
         "BIOCLIM_8",
         "BIOCLIM_9",
         "BIOCLIM_10",
         "BIOCLIM_11",
         "BIOCLIM_12",
         "BIOCLIM_13",
         "BIOCLIM_14",
         "BIOCLIM_15",
         "BIOCLIM_16",
         "BIOCLIM_17",
         "BIOCLIM_18",
         "BIOCLIM_19",
         "ENV - Adjusted R-squared",
         "ECO_NAME - Gulf Of California Xeric Scrub",
         "ECO_NAME - GREAT BASIN",
         "ECO_NAME - EAST ASIA",
         "ECO_NAME - ROCKIES",
         "ECO_NAME - EASTERN NA AND PLAINS",
         "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests",
         "BG - Adjusted R-squared"
      ],
      "0": [
         "66",
         "112",
         "68"
      ],
      "2": [
         "Observed",
         "P-Values",
         "BH Significant"
      ]
   },
   "data": [
      [
         [
            0.28993935950951183,
            0.0,
            1.0
         ],
         [
            0.28080527460195653,
            0.0,
            1.0
         ],
         [
            0.28216114079625437,
            0.0,
            1.0
         ],
         [
            -0.29298959244219175,
            0.0,
            1.0
         ],
         [
            -0.28265770997933248,
            0.0,
            1.0
         ],
         [
            -0.29014516492068293,
            0.0,
            1.0
         ],
         [
            -0.28086582854850428,
            0.0,
            1.0
         ],
         [
            -0.28562203577369599,
            0.0,
            1.0
         ],
         [
            -0.28236168602139139,
            0.0,
            1.0
         ],
         [
            0.28681742392720722,
            0.0,
            1.0
         ],
         [
            0.29475600611578107,
            0.0,
            1.0
         ],
         [
            0.29340914978903426,
            0.0,
            1.0
         ],
         [
            0.29064414126049382,
            0.0,
            1.0
         ],
         [
            0.28054686345947244,
            0.0,
            1.0
         ],
         [
            0.28438666669193063,
            0.0,
            1.0
         ],
         [
            0.29487350269693724,
            0.0,
            1.0
         ],
         [
            0.28733774823582764,
            0.0,
            1.0
         ],
         [
            0.29261596057271733,
            0.0,
            1.0
         ],
         [
            0.28659339972967113,
            0.0,
            1.0
         ],
         [
            -0.28269155295421144,
            0.0,
            1.0
         ],
         [
            0.28050769241771262,
            0.0,
            1.0
         ],
         [
            0.28338384811284834,
            0.0,
            1.0
         ],
         [
            0.29099357729950093,
            0.0,
            1.0
         ],
         [
            0.28610420521107055,
            0.0,
            1.0
         ],
         [
            0.28214374187272639,
            0.0,
            1.0
         ],
         [
            0.2846801681791431,
            0.0,
            1.0
         ],
         [
            0.28212883523183435,
            0.0,
            1.0
         ],
         [
            0.28435913259943502,
            0.0,
            1.0
         ],
         [
            0.28994571303142308,
            0.0,
            1.0
         ],
         [
            0.28273572715172268,
            0.0,
            1.0
         ],
         [
            0.34209324272890113,
            0.0,
            1.0
         ],
         [
            0.28129012303077927,
            0.0,
            1.0
         ],
         [
            0.28374907653319587,
            0.0,
            1.0
         ],
         [
            0.28177040253027286,
            0.0,
            1.0
         ],
         [
            0.28398172656590176,
            0.0,
            1.0
         ],
         [
            0.92986906873528108,
            0.0,
            1.0
         ],
         [
            -0.38359444952184502,
            0.0,
            1.0
         ],
         [
            0.22513884599056311,
            0.023333333333333334,
            0.0
         ],
         [
            0.16347736311698352,
            0.9966666666666667,
            0.0
         ],
         [
            0.20497623048753433,
            0.96999999999999997,
            0.0
         ],
         [
            0.25559003156578597,
            0.0,
            1.0
         ],
         [
            0.17078432837774479,
            1.0,
            0.0
         ],
         [
            0.62316663931908678,
            0.0,
            1.0
         ]
      ],
      [
         [
            0.36101428507464378,
            0.0,
            1.0
         ],
         [
            -0.36003617543260269,
            0.0,
            1.0
         ],
         [
            -0.35833889965044008,
            0.0,
            1.0
         ],
         [
            0.36005924124505478,
            0.0,
            1.0
         ],
         [
            0.35874337087095703,
            0.0,
            1.0
         ],
         [
            -0.35923993267495669,
            0.0,
            1.0
         ],
         [
            0.36038828592294192,
            0.0,
            1.0
         ],
         [
            0.35795219389557581,
            0.0,
            1.0
         ],
         [
            0.35960555483671219,
            0.0,
            1.0
         ],
         [
            -0.36646759998315143,
            0.0,
            1.0
         ],
         [
            -0.35835075110466197,
            0.0,
            1.0
         ],
         [
            -0.36434095386346349,
            0.0,
            1.0
         ],
         [
            -0.35937779720171037,
            0.0,
            1.0
         ],
         [
            0.36095131535259994,
            0.0,
            1.0
         ],
         [
            -0.36778020989969779,
            0.0,
            1.0
         ],
         [
            -0.35611676003696502,
            0.0,
            1.0
         ],
         [
            -0.35888533006967549,
            0.0,
            1.0
         ],
         [
            -0.36714276830128012,
            0.0,
            1.0
         ],
         [
            -0.36155329523500046,
            0.0,
            1.0
         ],
         [
            0.36212510226340494,
            0.0,
            1.0
         ],
         [
            -0.35909139136708595,
            0.0,
            1.0
         ],
         [
            -0.35900742940140523,
            0.0,
            1.0
         ],
         [
            -0.36674142356978007,
            0.0,
            1.0
         ],
         [
            -0.36041536130257584,
            0.0,
            1.0
         ],
         [
            -0.35983779120188203,
            0.0,
            1.0
         ],
         [
            -0.35975833808849195,
            0.0,
            1.0
         ],
         [
            -0.35952716972119447,
            0.0,
            1.0
         ],
         [
            -0.35944843921449526,
            0.0,
            1.0
         ],
         [
            -0.35919286138902345,
            0.0,
            1.0
         ],
         [
            -0.35958592011226298,
            0.0,
            1.0
         ],
         [
            -0.36579633930056604,
            0.0,
            1.0
         ],
         [
            -0.35953836301733977,
            0.0,
            1.0
         ],
         [
            -0.36275762304993048,
            0.0,
            1.0
         ],
         [
            -0.35952051229569743,
            0.0,
            1.0
         ],
         [
            -0.36466012639422929,
            0.0,
            1.0
         ],
         [
            0.94574515252383573,
            0.0,
            1.0
         ],
         [
            0.28689672947357192,
            0.0,
            1.0
         ],
         [
            0.33920339331552657,
            0.0,
            1.0
         ],
         [
            -0.28600558254488173,
            0.0,
            1.0
         ],
         [
            0.31474434001588042,
            0.0,
            1.0
         ],
         [
            -0.3801723960631393,
            0.0,
            1.0
         ],
         [
            -0.29927079876732859,
            0.0,
            1.0
         ],
         [
            0.66350022591565361,
            0.0,
            1.0
         ]
      ],
      [
         [
            -0.27043852245599387,
            0.0,
            1.0
         ],
         [
            -0.26714614675535814,
            0.0,
            1.0
         ],
         [
            -0.26512273418096266,
            0.0,
            1.0
         ],
         [
            0.26471157825270641,
            0.0,
            1.0
         ],
         [
            0.26225314770271119,
            0.0,
            1.0
         ],
         [
            0.26642266311821822,
            0.0,
            1.0
         ],
         [
            0.30851837298387202,
            0.0,
            1.0
         ],
         [
            -0.27017226672390143,
            0.0,
            1.0
         ],
         [
            0.25866327013511087,
            0.0,
            1.0
         ],
         [
            -0.26255633426353508,
            0.0,
            1.0
         ],
         [
            -0.26976231484565349,
            0.0,
            1.0
         ],
         [
            -0.25915952055398089,
            0.0,
            1.0
         ],
         [
            -0.26361596217708261,
            0.0,
            1.0
         ],
         [
            -0.26327895803053369,
            0.0,
            1.0
         ],
         [
            -0.28556136215898481,
            0.0,
            1.0
         ],
         [
            -0.28614883340209674,
            0.0,
            1.0
         ],
         [
            -0.2600769011109636,
            0.0,
            1.0
         ],
         [
            -0.26406516955102471,
            0.0,
            1.0
         ],
         [
            -0.2585108407141643,
            0.0,
            1.0
         ],
         [
            0.26163477032274834,
            0.0,
            1.0
         ],
         [
            -0.24557472690568061,
            0.0,
            1.0
         ],
         [
            -0.25798650412402041,
            0.0,
            1.0
         ],
         [
            -0.26098767892347136,
            0.0,
            1.0
         ],
         [
            -0.28168190360611511,
            0.0,
            1.0
         ],
         [
            -0.26484964952423057,
            0.0,
            1.0
         ],
         [
            -0.25954860030354393,
            0.0,
            1.0
         ],
         [
            -0.26466690039939361,
            0.0,
            1.0
         ],
         [
            0.26475558068313337,
            0.0,
            1.0
         ],
         [
            -0.26434999885011973,
            0.0,
            1.0
         ],
         [
            -0.28444230736896275,
            0.0,
            1.0
         ],
         [
            -0.27228648119287835,
            0.0,
            1.0
         ],
         [
            -0.26020519677265419,
            0.0,
            1.0
         ],
         [
            -0.26957746098924573,
            0.0,
            1.0
         ],
         [
            -0.26060106271483824,
            0.0,
            1.0
         ],
         [
            -0.26420997789001638,
            0.0,
            1.0
         ],
         [
            0.98941426476748651,
            0.0,
            1.0
         ],
         [
            0.3267901274264875,
            0.0,
            1.0
         ],
         [
            -0.25892011365426498,
            0.0,
            1.0
         ],
         [
            -0.20050354585172567,
            0.92000000000000004,
            0.0
         ],
         [
            -0.24913936077380905,
            0.0,
            1.0
         ],
         [
            -0.24935089099962177,
            0.0,
            1.0
         ],
         [
            -0.39036916620301482,
            0.0,
            1.0
         ],
         [
            0.59425567094815857,
            0.0,
            1.0
         ]
      ]
   ]
}
"""


decodedMcpa : McpaData
decodedMcpa =
    Dict.fromList
        [ ( ( 66, "BH Significant", "BG - Adjusted R-squared" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_1" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_10" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_11" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_12" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_13" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_14" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_15" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_16" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_17" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_18" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_19" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_2" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_3" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_4" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_5" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_6" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_7" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_8" ), 1 )
        , ( ( 66, "BH Significant", "BIOCLIM_9" ), 1 )
        , ( ( 66, "BH Significant", "ECO_NAME - EAST ASIA" ), 0 )
        , ( ( 66, "BH Significant", "ECO_NAME - EASTERN NA AND PLAINS" ), 1 )
        , ( ( 66, "BH Significant", "ECO_NAME - GREAT BASIN" ), 0 )
        , ( ( 66, "BH Significant", "ECO_NAME - Gulf Of California Xeric Scrub" ), 1 )
        , ( ( 66, "BH Significant", "ECO_NAME - ROCKIES" ), 0 )
        , ( ( 66, "BH Significant", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), 0 )
        , ( ( 66, "BH Significant", "ENV - Adjusted R-squared" ), 1 )
        , ( ( 66, "BH Significant", "GTOPO30_ASPECT_reduced" ), 1 )
        , ( ( 66, "BH Significant", "GTOPO30_ELEVATION" ), 1 )
        , ( ( 66, "BH Significant", "GTOPO30_SLOPE_reduced" ), 1 )
        , ( ( 66, "BH Significant", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 1 )
        , ( ( 66, "BH Significant", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), 1 )
        , ( ( 66, "BH Significant", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), 1 )
        , ( ( 66, "BH Significant", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), 1 )
        , ( ( 66, "BH Significant", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), 1 )
        , ( ( 66, "BH Significant", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), 1 )
        , ( ( 66, "BH Significant", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), 1 )
        , ( ( 66, "BH Significant", "LandCover_1_Needleleaf" ), 1 )
        , ( ( 66, "BH Significant", "LandCover_2_Evergreenbroadleaf" ), 1 )
        , ( ( 66, "BH Significant", "LandCover_3_Deciduousbroadleaf" ), 1 )
        , ( ( 66, "BH Significant", "LandCover_4_Mixedtrees" ), 1 )
        , ( ( 66, "BH Significant", "LandCover_5_Shrubs" ), 1 )
        , ( ( 66, "BH Significant", "LandCover_6_Herbaceous" ), 1 )
        , ( ( 66, "Observed", "BG - Adjusted R-squared" ), 0.6231666393190868 )
        , ( ( 66, "Observed", "BIOCLIM_1" ), 0.28733774823582764 )
        , ( ( 66, "Observed", "BIOCLIM_10" ), 0.2846801681791431 )
        , ( ( 66, "Observed", "BIOCLIM_11" ), 0.28212883523183435 )
        , ( ( 66, "Observed", "BIOCLIM_12" ), 0.284359132599435 )
        , ( ( 66, "Observed", "BIOCLIM_13" ), 0.2899457130314231 )
        , ( ( 66, "Observed", "BIOCLIM_14" ), 0.2827357271517227 )
        , ( ( 66, "Observed", "BIOCLIM_15" ), 0.3420932427289011 )
        , ( ( 66, "Observed", "BIOCLIM_16" ), 0.28129012303077927 )
        , ( ( 66, "Observed", "BIOCLIM_17" ), 0.28374907653319587 )
        , ( ( 66, "Observed", "BIOCLIM_18" ), 0.28177040253027286 )
        , ( ( 66, "Observed", "BIOCLIM_19" ), 0.28398172656590176 )
        , ( ( 66, "Observed", "BIOCLIM_2" ), 0.2926159605727173 )
        , ( ( 66, "Observed", "BIOCLIM_3" ), 0.2865933997296711 )
        , ( ( 66, "Observed", "BIOCLIM_4" ), -0.28269155295421144 )
        , ( ( 66, "Observed", "BIOCLIM_5" ), 0.2805076924177126 )
        , ( ( 66, "Observed", "BIOCLIM_6" ), 0.28338384811284834 )
        , ( ( 66, "Observed", "BIOCLIM_7" ), 0.2909935772995009 )
        , ( ( 66, "Observed", "BIOCLIM_8" ), 0.28610420521107055 )
        , ( ( 66, "Observed", "BIOCLIM_9" ), 0.2821437418727264 )
        , ( ( 66, "Observed", "ECO_NAME - EAST ASIA" ), 0.16347736311698352 )
        , ( ( 66, "Observed", "ECO_NAME - EASTERN NA AND PLAINS" ), 0.25559003156578597 )
        , ( ( 66, "Observed", "ECO_NAME - GREAT BASIN" ), 0.2251388459905631 )
        , ( ( 66, "Observed", "ECO_NAME - Gulf Of California Xeric Scrub" ), -0.383594449521845 )
        , ( ( 66, "Observed", "ECO_NAME - ROCKIES" ), 0.20497623048753433 )
        , ( ( 66, "Observed", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), 0.1707843283777448 )
        , ( ( 66, "Observed", "ENV - Adjusted R-squared" ), 0.9298690687352811 )
        , ( ( 66, "Observed", "GTOPO30_ASPECT_reduced" ), 0.2821611407962544 )
        , ( ( 66, "Observed", "GTOPO30_ELEVATION" ), 0.28993935950951183 )
        , ( ( 66, "Observed", "GTOPO30_SLOPE_reduced" ), 0.28080527460195653 )
        , ( ( 66, "Observed", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 0.28054686345947244 )
        , ( ( 66, "Observed", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), 0.28438666669193063 )
        , ( ( 66, "Observed", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), 0.2906441412604938 )
        , ( ( 66, "Observed", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), 0.2868174239272072 )
        , ( ( 66, "Observed", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), 0.29340914978903426 )
        , ( ( 66, "Observed", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), 0.29487350269693724 )
        , ( ( 66, "Observed", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), 0.29475600611578107 )
        , ( ( 66, "Observed", "LandCover_1_Needleleaf" ), -0.29298959244219175 )
        , ( ( 66, "Observed", "LandCover_2_Evergreenbroadleaf" ), -0.2826577099793325 )
        , ( ( 66, "Observed", "LandCover_3_Deciduousbroadleaf" ), -0.29014516492068293 )
        , ( ( 66, "Observed", "LandCover_4_Mixedtrees" ), -0.2808658285485043 )
        , ( ( 66, "Observed", "LandCover_5_Shrubs" ), -0.285622035773696 )
        , ( ( 66, "Observed", "LandCover_6_Herbaceous" ), -0.2823616860213914 )
        , ( ( 66, "P-Values", "BG - Adjusted R-squared" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_1" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_10" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_11" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_12" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_13" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_14" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_15" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_16" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_17" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_18" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_19" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_2" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_3" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_4" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_5" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_6" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_7" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_8" ), 0 )
        , ( ( 66, "P-Values", "BIOCLIM_9" ), 0 )
        , ( ( 66, "P-Values", "ECO_NAME - EAST ASIA" ), 0.9966666666666667 )
        , ( ( 66, "P-Values", "ECO_NAME - EASTERN NA AND PLAINS" ), 0 )
        , ( ( 66, "P-Values", "ECO_NAME - GREAT BASIN" ), 0.023333333333333334 )
        , ( ( 66, "P-Values", "ECO_NAME - Gulf Of California Xeric Scrub" ), 0 )
        , ( ( 66, "P-Values", "ECO_NAME - ROCKIES" ), 0.97 )
        , ( ( 66, "P-Values", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), 1 )
        , ( ( 66, "P-Values", "ENV - Adjusted R-squared" ), 0 )
        , ( ( 66, "P-Values", "GTOPO30_ASPECT_reduced" ), 0 )
        , ( ( 66, "P-Values", "GTOPO30_ELEVATION" ), 0 )
        , ( ( 66, "P-Values", "GTOPO30_SLOPE_reduced" ), 0 )
        , ( ( 66, "P-Values", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 0 )
        , ( ( 66, "P-Values", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), 0 )
        , ( ( 66, "P-Values", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), 0 )
        , ( ( 66, "P-Values", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), 0 )
        , ( ( 66, "P-Values", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), 0 )
        , ( ( 66, "P-Values", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), 0 )
        , ( ( 66, "P-Values", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), 0 )
        , ( ( 66, "P-Values", "LandCover_1_Needleleaf" ), 0 )
        , ( ( 66, "P-Values", "LandCover_2_Evergreenbroadleaf" ), 0 )
        , ( ( 66, "P-Values", "LandCover_3_Deciduousbroadleaf" ), 0 )
        , ( ( 66, "P-Values", "LandCover_4_Mixedtrees" ), 0 )
        , ( ( 66, "P-Values", "LandCover_5_Shrubs" ), 0 )
        , ( ( 66, "P-Values", "LandCover_6_Herbaceous" ), 0 )
        , ( ( 68, "BH Significant", "BG - Adjusted R-squared" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_1" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_10" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_11" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_12" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_13" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_14" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_15" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_16" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_17" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_18" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_19" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_2" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_3" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_4" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_5" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_6" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_7" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_8" ), 1 )
        , ( ( 68, "BH Significant", "BIOCLIM_9" ), 1 )
        , ( ( 68, "BH Significant", "ECO_NAME - EAST ASIA" ), 0 )
        , ( ( 68, "BH Significant", "ECO_NAME - EASTERN NA AND PLAINS" ), 1 )
        , ( ( 68, "BH Significant", "ECO_NAME - GREAT BASIN" ), 1 )
        , ( ( 68, "BH Significant", "ECO_NAME - Gulf Of California Xeric Scrub" ), 1 )
        , ( ( 68, "BH Significant", "ECO_NAME - ROCKIES" ), 1 )
        , ( ( 68, "BH Significant", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), 1 )
        , ( ( 68, "BH Significant", "ENV - Adjusted R-squared" ), 1 )
        , ( ( 68, "BH Significant", "GTOPO30_ASPECT_reduced" ), 1 )
        , ( ( 68, "BH Significant", "GTOPO30_ELEVATION" ), 1 )
        , ( ( 68, "BH Significant", "GTOPO30_SLOPE_reduced" ), 1 )
        , ( ( 68, "BH Significant", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 1 )
        , ( ( 68, "BH Significant", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), 1 )
        , ( ( 68, "BH Significant", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), 1 )
        , ( ( 68, "BH Significant", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), 1 )
        , ( ( 68, "BH Significant", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), 1 )
        , ( ( 68, "BH Significant", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), 1 )
        , ( ( 68, "BH Significant", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), 1 )
        , ( ( 68, "BH Significant", "LandCover_1_Needleleaf" ), 1 )
        , ( ( 68, "BH Significant", "LandCover_2_Evergreenbroadleaf" ), 1 )
        , ( ( 68, "BH Significant", "LandCover_3_Deciduousbroadleaf" ), 1 )
        , ( ( 68, "BH Significant", "LandCover_4_Mixedtrees" ), 1 )
        , ( ( 68, "BH Significant", "LandCover_5_Shrubs" ), 1 )
        , ( ( 68, "BH Significant", "LandCover_6_Herbaceous" ), 1 )
        , ( ( 68, "Observed", "BG - Adjusted R-squared" ), 0.5942556709481586 )
        , ( ( 68, "Observed", "BIOCLIM_1" ), -0.2600769011109636 )
        , ( ( 68, "Observed", "BIOCLIM_10" ), -0.25954860030354393 )
        , ( ( 68, "Observed", "BIOCLIM_11" ), -0.2646669003993936 )
        , ( ( 68, "Observed", "BIOCLIM_12" ), 0.2647555806831334 )
        , ( ( 68, "Observed", "BIOCLIM_13" ), -0.2643499988501197 )
        , ( ( 68, "Observed", "BIOCLIM_14" ), -0.28444230736896275 )
        , ( ( 68, "Observed", "BIOCLIM_15" ), -0.27228648119287835 )
        , ( ( 68, "Observed", "BIOCLIM_16" ), -0.2602051967726542 )
        , ( ( 68, "Observed", "BIOCLIM_17" ), -0.26957746098924573 )
        , ( ( 68, "Observed", "BIOCLIM_18" ), -0.26060106271483824 )
        , ( ( 68, "Observed", "BIOCLIM_19" ), -0.2642099778900164 )
        , ( ( 68, "Observed", "BIOCLIM_2" ), -0.2640651695510247 )
        , ( ( 68, "Observed", "BIOCLIM_3" ), -0.2585108407141643 )
        , ( ( 68, "Observed", "BIOCLIM_4" ), 0.26163477032274834 )
        , ( ( 68, "Observed", "BIOCLIM_5" ), -0.2455747269056806 )
        , ( ( 68, "Observed", "BIOCLIM_6" ), -0.2579865041240204 )
        , ( ( 68, "Observed", "BIOCLIM_7" ), -0.26098767892347136 )
        , ( ( 68, "Observed", "BIOCLIM_8" ), -0.2816819036061151 )
        , ( ( 68, "Observed", "BIOCLIM_9" ), -0.26484964952423057 )
        , ( ( 68, "Observed", "ECO_NAME - EAST ASIA" ), -0.20050354585172567 )
        , ( ( 68, "Observed", "ECO_NAME - EASTERN NA AND PLAINS" ), -0.24935089099962177 )
        , ( ( 68, "Observed", "ECO_NAME - GREAT BASIN" ), -0.258920113654265 )
        , ( ( 68, "Observed", "ECO_NAME - Gulf Of California Xeric Scrub" ), 0.3267901274264875 )
        , ( ( 68, "Observed", "ECO_NAME - ROCKIES" ), -0.24913936077380905 )
        , ( ( 68, "Observed", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), -0.3903691662030148 )
        , ( ( 68, "Observed", "ENV - Adjusted R-squared" ), 0.9894142647674865 )
        , ( ( 68, "Observed", "GTOPO30_ASPECT_reduced" ), -0.26512273418096266 )
        , ( ( 68, "Observed", "GTOPO30_ELEVATION" ), -0.27043852245599387 )
        , ( ( 68, "Observed", "GTOPO30_SLOPE_reduced" ), -0.26714614675535814 )
        , ( ( 68, "Observed", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), -0.2632789580305337 )
        , ( ( 68, "Observed", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), -0.2855613621589848 )
        , ( ( 68, "Observed", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), -0.2636159621770826 )
        , ( ( 68, "Observed", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), -0.2625563342635351 )
        , ( ( 68, "Observed", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), -0.2591595205539809 )
        , ( ( 68, "Observed", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), -0.28614883340209674 )
        , ( ( 68, "Observed", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), -0.2697623148456535 )
        , ( ( 68, "Observed", "LandCover_1_Needleleaf" ), 0.2647115782527064 )
        , ( ( 68, "Observed", "LandCover_2_Evergreenbroadleaf" ), 0.2622531477027112 )
        , ( ( 68, "Observed", "LandCover_3_Deciduousbroadleaf" ), 0.2664226631182182 )
        , ( ( 68, "Observed", "LandCover_4_Mixedtrees" ), 0.308518372983872 )
        , ( ( 68, "Observed", "LandCover_5_Shrubs" ), -0.27017226672390143 )
        , ( ( 68, "Observed", "LandCover_6_Herbaceous" ), 0.25866327013511087 )
        , ( ( 68, "P-Values", "BG - Adjusted R-squared" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_1" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_10" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_11" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_12" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_13" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_14" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_15" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_16" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_17" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_18" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_19" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_2" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_3" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_4" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_5" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_6" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_7" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_8" ), 0 )
        , ( ( 68, "P-Values", "BIOCLIM_9" ), 0 )
        , ( ( 68, "P-Values", "ECO_NAME - EAST ASIA" ), 0.92 )
        , ( ( 68, "P-Values", "ECO_NAME - EASTERN NA AND PLAINS" ), 0 )
        , ( ( 68, "P-Values", "ECO_NAME - GREAT BASIN" ), 0 )
        , ( ( 68, "P-Values", "ECO_NAME - Gulf Of California Xeric Scrub" ), 0 )
        , ( ( 68, "P-Values", "ECO_NAME - ROCKIES" ), 0 )
        , ( ( 68, "P-Values", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), 0 )
        , ( ( 68, "P-Values", "ENV - Adjusted R-squared" ), 0 )
        , ( ( 68, "P-Values", "GTOPO30_ASPECT_reduced" ), 0 )
        , ( ( 68, "P-Values", "GTOPO30_ELEVATION" ), 0 )
        , ( ( 68, "P-Values", "GTOPO30_SLOPE_reduced" ), 0 )
        , ( ( 68, "P-Values", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 0 )
        , ( ( 68, "P-Values", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), 0 )
        , ( ( 68, "P-Values", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), 0 )
        , ( ( 68, "P-Values", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), 0 )
        , ( ( 68, "P-Values", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), 0 )
        , ( ( 68, "P-Values", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), 0 )
        , ( ( 68, "P-Values", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), 0 )
        , ( ( 68, "P-Values", "LandCover_1_Needleleaf" ), 0 )
        , ( ( 68, "P-Values", "LandCover_2_Evergreenbroadleaf" ), 0 )
        , ( ( 68, "P-Values", "LandCover_3_Deciduousbroadleaf" ), 0 )
        , ( ( 68, "P-Values", "LandCover_4_Mixedtrees" ), 0 )
        , ( ( 68, "P-Values", "LandCover_5_Shrubs" ), 0 )
        , ( ( 68, "P-Values", "LandCover_6_Herbaceous" ), 0 )
        , ( ( 112, "BH Significant", "BG - Adjusted R-squared" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_1" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_10" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_11" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_12" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_13" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_14" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_15" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_16" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_17" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_18" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_19" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_2" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_3" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_4" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_5" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_6" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_7" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_8" ), 1 )
        , ( ( 112, "BH Significant", "BIOCLIM_9" ), 1 )
        , ( ( 112, "BH Significant", "ECO_NAME - EAST ASIA" ), 1 )
        , ( ( 112, "BH Significant", "ECO_NAME - EASTERN NA AND PLAINS" ), 1 )
        , ( ( 112, "BH Significant", "ECO_NAME - GREAT BASIN" ), 1 )
        , ( ( 112, "BH Significant", "ECO_NAME - Gulf Of California Xeric Scrub" ), 1 )
        , ( ( 112, "BH Significant", "ECO_NAME - ROCKIES" ), 1 )
        , ( ( 112, "BH Significant", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), 1 )
        , ( ( 112, "BH Significant", "ENV - Adjusted R-squared" ), 1 )
        , ( ( 112, "BH Significant", "GTOPO30_ASPECT_reduced" ), 1 )
        , ( ( 112, "BH Significant", "GTOPO30_ELEVATION" ), 1 )
        , ( ( 112, "BH Significant", "GTOPO30_SLOPE_reduced" ), 1 )
        , ( ( 112, "BH Significant", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 1 )
        , ( ( 112, "BH Significant", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), 1 )
        , ( ( 112, "BH Significant", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), 1 )
        , ( ( 112, "BH Significant", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), 1 )
        , ( ( 112, "BH Significant", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), 1 )
        , ( ( 112, "BH Significant", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), 1 )
        , ( ( 112, "BH Significant", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), 1 )
        , ( ( 112, "BH Significant", "LandCover_1_Needleleaf" ), 1 )
        , ( ( 112, "BH Significant", "LandCover_2_Evergreenbroadleaf" ), 1 )
        , ( ( 112, "BH Significant", "LandCover_3_Deciduousbroadleaf" ), 1 )
        , ( ( 112, "BH Significant", "LandCover_4_Mixedtrees" ), 1 )
        , ( ( 112, "BH Significant", "LandCover_5_Shrubs" ), 1 )
        , ( ( 112, "BH Significant", "LandCover_6_Herbaceous" ), 1 )
        , ( ( 112, "Observed", "BG - Adjusted R-squared" ), 0.6635002259156536 )
        , ( ( 112, "Observed", "BIOCLIM_1" ), -0.3588853300696755 )
        , ( ( 112, "Observed", "BIOCLIM_10" ), -0.35975833808849195 )
        , ( ( 112, "Observed", "BIOCLIM_11" ), -0.3595271697211945 )
        , ( ( 112, "Observed", "BIOCLIM_12" ), -0.35944843921449526 )
        , ( ( 112, "Observed", "BIOCLIM_13" ), -0.35919286138902345 )
        , ( ( 112, "Observed", "BIOCLIM_14" ), -0.359585920112263 )
        , ( ( 112, "Observed", "BIOCLIM_15" ), -0.36579633930056604 )
        , ( ( 112, "Observed", "BIOCLIM_16" ), -0.35953836301733977 )
        , ( ( 112, "Observed", "BIOCLIM_17" ), -0.3627576230499305 )
        , ( ( 112, "Observed", "BIOCLIM_18" ), -0.3595205122956974 )
        , ( ( 112, "Observed", "BIOCLIM_19" ), -0.3646601263942293 )
        , ( ( 112, "Observed", "BIOCLIM_2" ), -0.3671427683012801 )
        , ( ( 112, "Observed", "BIOCLIM_3" ), -0.36155329523500046 )
        , ( ( 112, "Observed", "BIOCLIM_4" ), 0.36212510226340494 )
        , ( ( 112, "Observed", "BIOCLIM_5" ), -0.35909139136708595 )
        , ( ( 112, "Observed", "BIOCLIM_6" ), -0.35900742940140523 )
        , ( ( 112, "Observed", "BIOCLIM_7" ), -0.36674142356978007 )
        , ( ( 112, "Observed", "BIOCLIM_8" ), -0.36041536130257584 )
        , ( ( 112, "Observed", "BIOCLIM_9" ), -0.35983779120188203 )
        , ( ( 112, "Observed", "ECO_NAME - EAST ASIA" ), -0.28600558254488173 )
        , ( ( 112, "Observed", "ECO_NAME - EASTERN NA AND PLAINS" ), -0.3801723960631393 )
        , ( ( 112, "Observed", "ECO_NAME - GREAT BASIN" ), 0.33920339331552657 )
        , ( ( 112, "Observed", "ECO_NAME - Gulf Of California Xeric Scrub" ), 0.2868967294735719 )
        , ( ( 112, "Observed", "ECO_NAME - ROCKIES" ), 0.3147443400158804 )
        , ( ( 112, "Observed", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), -0.2992707987673286 )
        , ( ( 112, "Observed", "ENV - Adjusted R-squared" ), 0.9457451525238357 )
        , ( ( 112, "Observed", "GTOPO30_ASPECT_reduced" ), -0.3583388996504401 )
        , ( ( 112, "Observed", "GTOPO30_ELEVATION" ), 0.3610142850746438 )
        , ( ( 112, "Observed", "GTOPO30_SLOPE_reduced" ), -0.3600361754326027 )
        , ( ( 112, "Observed", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 0.36095131535259994 )
        , ( ( 112, "Observed", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), -0.3677802098996978 )
        , ( ( 112, "Observed", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), -0.35937779720171037 )
        , ( ( 112, "Observed", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), -0.3664675999831514 )
        , ( ( 112, "Observed", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), -0.3643409538634635 )
        , ( ( 112, "Observed", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), -0.356116760036965 )
        , ( ( 112, "Observed", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), -0.35835075110466197 )
        , ( ( 112, "Observed", "LandCover_1_Needleleaf" ), 0.3600592412450548 )
        , ( ( 112, "Observed", "LandCover_2_Evergreenbroadleaf" ), 0.35874337087095703 )
        , ( ( 112, "Observed", "LandCover_3_Deciduousbroadleaf" ), -0.3592399326749567 )
        , ( ( 112, "Observed", "LandCover_4_Mixedtrees" ), 0.3603882859229419 )
        , ( ( 112, "Observed", "LandCover_5_Shrubs" ), 0.3579521938955758 )
        , ( ( 112, "Observed", "LandCover_6_Herbaceous" ), 0.3596055548367122 )
        , ( ( 112, "P-Values", "BG - Adjusted R-squared" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_1" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_10" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_11" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_12" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_13" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_14" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_15" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_16" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_17" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_18" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_19" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_2" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_3" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_4" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_5" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_6" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_7" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_8" ), 0 )
        , ( ( 112, "P-Values", "BIOCLIM_9" ), 0 )
        , ( ( 112, "P-Values", "ECO_NAME - EAST ASIA" ), 0 )
        , ( ( 112, "P-Values", "ECO_NAME - EASTERN NA AND PLAINS" ), 0 )
        , ( ( 112, "P-Values", "ECO_NAME - GREAT BASIN" ), 0 )
        , ( ( 112, "P-Values", "ECO_NAME - Gulf Of California Xeric Scrub" ), 0 )
        , ( ( 112, "P-Values", "ECO_NAME - ROCKIES" ), 0 )
        , ( ( 112, "P-Values", "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests" ), 0 )
        , ( ( 112, "P-Values", "ENV - Adjusted R-squared" ), 0 )
        , ( ( 112, "P-Values", "GTOPO30_ASPECT_reduced" ), 0 )
        , ( ( 112, "P-Values", "GTOPO30_ELEVATION" ), 0 )
        , ( ( 112, "P-Values", "GTOPO30_SLOPE_reduced" ), 0 )
        , ( ( 112, "P-Values", "ISRICSOILGRIDS_new_average_bulkdensity_reduced" ), 0 )
        , ( ( 112, "P-Values", "ISRICSOILGRIDS_new_average_claypercent_reduced" ), 0 )
        , ( ( 112, "P-Values", "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced" ), 0 )
        , ( ( 112, "P-Values", "ISRICSOILGRIDS_new_average_phx10percent_reduced" ), 0 )
        , ( ( 112, "P-Values", "ISRICSOILGRIDS_new_average_sandpercent_reduced" ), 0 )
        , ( ( 112, "P-Values", "ISRICSOILGRIDS_new_average_siltpercent_reduced" ), 0 )
        , ( ( 112, "P-Values", "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced" ), 0 )
        , ( ( 112, "P-Values", "LandCover_1_Needleleaf" ), 0 )
        , ( ( 112, "P-Values", "LandCover_2_Evergreenbroadleaf" ), 0 )
        , ( ( 112, "P-Values", "LandCover_3_Deciduousbroadleaf" ), 0 )
        , ( ( 112, "P-Values", "LandCover_4_Mixedtrees" ), 0 )
        , ( ( 112, "P-Values", "LandCover_5_Shrubs" ), 0 )
        , ( ( 112, "P-Values", "LandCover_6_Herbaceous" ), 0 )
        ]


exampleMcpaVariables : List String
exampleMcpaVariables =
    [ "GTOPO30_ELEVATION"
    , "GTOPO30_SLOPE_reduced"
    , "GTOPO30_ASPECT_reduced"
    , "LandCover_1_Needleleaf"
    , "LandCover_2_Evergreenbroadleaf"
    , "LandCover_3_Deciduousbroadleaf"
    , "LandCover_4_Mixedtrees"
    , "LandCover_5_Shrubs"
    , "LandCover_6_Herbaceous"
    , "ISRICSOILGRIDS_new_average_phx10percent_reduced"
    , "ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced"
    , "ISRICSOILGRIDS_new_average_sandpercent_reduced"
    , "ISRICSOILGRIDS_new_average_coarsefragmentpercent_reduced"
    , "ISRICSOILGRIDS_new_average_bulkdensity_reduced"
    , "ISRICSOILGRIDS_new_average_claypercent_reduced"
    , "ISRICSOILGRIDS_new_average_siltpercent_reduced"
    , "BIOCLIM_1"
    , "BIOCLIM_2"
    , "BIOCLIM_3"
    , "BIOCLIM_4"
    , "BIOCLIM_5"
    , "BIOCLIM_6"
    , "BIOCLIM_7"
    , "BIOCLIM_8"
    , "BIOCLIM_9"
    , "BIOCLIM_10"
    , "BIOCLIM_11"
    , "BIOCLIM_12"
    , "BIOCLIM_13"
    , "BIOCLIM_14"
    , "BIOCLIM_15"
    , "BIOCLIM_16"
    , "BIOCLIM_17"
    , "BIOCLIM_18"
    , "BIOCLIM_19"
    , "ENV - Adjusted R-squared"
    , "ECO_NAME - Gulf Of California Xeric Scrub"
    , "ECO_NAME - GREAT BASIN"
    , "ECO_NAME - EAST ASIA"
    , "ECO_NAME - ROCKIES"
    , "ECO_NAME - EASTERN NA AND PLAINS"
    , "ECO_NAME - Sierra Madre Oriental Pine-Oak Forests"
    , "BG - Adjusted R-squared"
    ]
