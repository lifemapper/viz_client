module ExampleTrees exposing (..)

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

import Newick exposing (..)
import TaxLabels exposing (..)
import DecodeTree as Binary


heucheraNewick =
    (SubTree (Branches ([ ( Branches ([ ( Leaf "Peltoboykinia_watanabei", Just 17.7616558596 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_richardsonii", Just 1.59667580986 ), ( Branches ([ ( Leaf "Heuchera_caroliniana", Just 1.25045275727 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_pubescens", Just 0.6012456036 ), ( Leaf "Heuchera_alba", Just 0.6012456036 ) ]) "Node_13", Just 0.307630228026 ), ( Branches ([ ( Leaf "Heuchera_longiflora", Just 0.678578784091 ), ( Leaf "Heuchera_americana", Just 0.678578784091 ) ]) "Node_16", Just 0.230297047535 ) ]) "Node_12", Just 0.341576925645 ) ]) "Node_10", Just 0.346223052593 ) ]) "Node_8", Just 0.455037436176 ), ( Branches ([ ( Leaf "Heuchera_parvifolia", Just 1.57983930474 ), ( Branches ([ ( Leaf "Heuchera_wootonii", Just 1.03733670369 ), ( Branches ([ ( Leaf "Heuchera_inconstans", Just 0.806678945231 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_soltisii", Just 0.368017442136 ), ( Leaf "Heuchera_novomexicana", Just 0.368017442136 ) ]) "Node_27", Just 0.113213409433 ), ( Leaf "Heuchera_glomerulata", Just 0.481230851569 ) ]) "Node_26", Just 0.200196385551 ), ( Leaf "Heuchera_eastwoodiae", Just 0.681427237119 ) ]) "Node_25", Just 0.125251708112 ) ]) "Node_23", Just 0.23065775846 ) ]) "Node_21", Just 0.542502601047 ) ]) "Node_19", Just 0.471873941302 ) ]) "Node_7", Just 1.17677859772 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_woodsiaphila", Just 2.26765108241 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_bracteata", Just 1.11223623101 ), ( Leaf "Heuchera_hallii", Just 1.11223623101 ) ]) "Node_36", Just 0.799972223411 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_mexicana", Just 1.00985960274 ), ( Branches ([ ( Leaf "Heuchera_longipetala", Just 0.672702201725 ), ( Leaf "Heuchera_acutifolia", Just 0.672702201725 ) ]) "Node_43", Just 0.337157401018 ) ]) "Node_41", Just 0.452796325447 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_hirsutissima", Just 0.908985338716 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_caespitosa", Just 0.492761287085 ), ( Leaf "Heuchera_abramsii", Just 0.492761287085 ) ]) "Node_51", Just 0.217512119899 ), ( Branches ([ ( Leaf "Heuchera_elegans", Just 0.570276420827 ), ( Leaf "Heuchera_parishii", Just 0.570276420827 ) ]) "Node_54", Just 0.139996986157 ) ]) "Node_50", Just 0.198711931733 ) ]) "Node_48", Just 0.152249046392 ), ( Leaf "Heuchera_brevistaminea", Just 1.06123438511 ) ]) "Node_47", Just 0.23571097096 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_rosendahlii", Just 0.436237554309 ), ( Leaf "Heuchera_wellsiae", Just 0.436237554309 ) ]) "Node_60", Just 0.481744506695 ), ( Leaf "Heuchera_sanguinea", Just 0.917982061004 ) ]) "Node_59", Just 0.18098598279 ), ( Leaf "Heuchera_versicolor", Just 1.09896804379 ) ]) "Node_58", Just 0.197977312275 ) ]) "Node_46", Just 0.165710572121 ) ]) "Node_40", Just 0.211113420755 ), ( Branches ([ ( Leaf "Heuchera_pulchella", Just 1.19380805918 ), ( Leaf "Heuchera_rubescens", Just 1.19380805918 ) ]) "Node_65", Just 0.479961289764 ) ]) "Node_39", Just 0.238439105472 ) ]) "Node_35", Just 0.355442627994 ) ]) "Node_33", Just 0.472005667159 ), ( Branches ([ ( Leaf "Heuchera_merriamii", Just 2.38905826701 ), ( Leaf "Heuchera_grossulariifolia", Just 2.38905826701 ) ]) "Node_68", Just 0.350598482559 ) ]) "Node_32", Just 0.48883509419 ) ]) "Node_6", Just 0.61160272986 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_pilosissima", Just 0.47153892037 ), ( Leaf "Heuchera_maxima", Just 0.47153892037 ) ]) "Node_73", Just 0.554782803323 ), ( Leaf "Heuchera_micrantha", Just 1.02632172369 ) ]) "Node_72", Just 2.51457414479 ), ( Branches ([ ( Leaf "Heuchera_glabra", Just 3.22414919189 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_villosa", Just 0.981506873297 ), ( Leaf "Heuchera_puberula", Just 0.981506873297 ) ]) "Node_81", Just 0.507115394285 ), ( Branches ([ ( Leaf "Heuchera_missouriensis", Just 0.942214782098 ), ( Leaf "Heuchera_parviflora", Just 0.942214782098 ) ]) "Node_84", Just 0.546407485484 ) ]) "Node_80", Just 1.18263380777 ), ( Branches ([ ( Leaf "Heuchera_chlorantha", Just 1.74505145726 ), ( Leaf "Heuchera_cylindrica", Just 1.74505145726 ) ]) "Node_87", Just 0.926204618094 ) ]) "Node_79", Just 0.552893116534 ) ]) "Node_77", Just 0.316746676598 ) ]) "Node_71", Just 0.299198705132 ) ]) "Node_5", Just 2.27176201455 ), ( Branches ([ ( Branches ([ ( Leaf "menziesii", Just 1.84479470104 ), ( Leaf "diplomenziesii", Just 1.84479470104 ) ]) "Node_91", Just 3.90021276384 ), ( Branches ([ ( Branches ([ ( Leaf "Tellima_grandiflora", Just 4.63906336711 ), ( Branches ([ ( Leaf "Mitella_pentandra", Just 4.09770275642 ), ( Branches ([ ( Branches ([ ( Leaf "Mitella_stylosa", Just 0.638709303912 ), ( Branches ([ ( Leaf "Mitella_furusei", Just 0.344251767186 ), ( Leaf "Mitella_pauciflora", Just 0.344251767186 ) ]) "Node_102", Just 0.294457536726 ) ]) "Node_100", Just 0.564868364663 ), ( Leaf "Mitella_japonica", Just 1.20357766858 ) ]) "Node_99", Just 2.89412508785 ) ]) "Node_97", Just 0.54136061069 ) ]) "Node_95", Just 0.731361706037 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Mitella_breweri", Just 1.53688075103 ), ( Leaf "Mitella_ovalis", Just 1.53688075103 ) ]) "Node_108", Just 2.97988329947 ), ( Leaf "Bensoniella_oregona", Just 4.5167640505 ) ]) "Node_107", Just 0.503874826255 ), ( Branches ([ ( Leaf "Lithophragma_parviflorum", Just 4.44492705346 ), ( Branches ([ ( Leaf "Mitella_nuda", Just 2.76219772512 ), ( Leaf "Mitella_diphylla", Just 2.76219772512 ) ]) "Node_114", Just 1.68272932833 ) ]) "Node_112", Just 0.5757118233 ) ]) "Node_106", Just 0.349786196394 ) ]) "Node_94", Just 0.374582391731 ) ]) "Node_90", Just 0.366849123291 ) ]) "Node_4", Just 0.305415140583 ), ( Branches ([ ( Leaf "Tiarella_polyphylla", Just 5.93681616118 ), ( Branches ([ ( Branches ([ ( Leaf "Elmera_racemosa", Just 3.27000742488 ), ( Leaf "Mitella_caulescens", Just 3.27000742488 ) ]) "Node_120", Just 1.2373578646 ), ( Branches ([ ( Leaf "Conimitella_williamsii", Just 3.00554587516 ), ( Leaf "Mitella_stauropetala", Just 3.00554587516 ) ]) "Node_123", Just 1.50181941431 ) ]) "Node_119", Just 1.42945087171 ) ]) "Node_117", Just 0.480455567573 ) ]) "Node_3", Just 11.3443841308 ) ]) "Node_1", Just 2.29974099235 ), ( Leaf "Telesonix_jamesii", Just 20.0613968519 ) ]) "Node_0"))


heucheraNewick2 =
    SubTree (Branches ([ ( Branches ([ ( Leaf "Peltoboykinia_watanabei", Just 17.7616558596 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_richardsonii", Just 1.59667580986 ), ( Branches ([ ( Leaf "Heuchera_caroliniana", Just 1.25045275727 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_pubescens", Just 0.6012456036 ), ( Leaf "Heuchera_alba", Just 0.6012456036 ) ]) "74", Just 0.307630228026 ), ( Branches ([ ( Leaf "Heuchera_longiflora", Just 0.678578784091 ), ( Leaf "Heuchera_americana", Just 0.678578784091 ) ]) "75", Just 0.230297047535 ) ]) "73", Just 0.341576925645 ) ]) "72", Just 0.346223052593 ) ]) "71", Just 0.455037436176 ), ( Branches ([ ( Leaf "Heuchera_parvifolia", Just 1.57983930474 ), ( Branches ([ ( Leaf "Heuchera_wootonii", Just 1.03733670369 ), ( Branches ([ ( Leaf "Heuchera_inconstans", Just 0.806678945231 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_soltisii", Just 0.368017442136 ), ( Leaf "Heuchera_novomexicana", Just 0.368017442136 ) ]) "81", Just 0.113213409433 ), ( Leaf "Heuchera_glomerulata", Just 0.481230851569 ) ]) "80", Just 0.200196385551 ), ( Leaf "Heuchera_eastwoodiae", Just 0.681427237119 ) ]) "79", Just 0.125251708112 ) ]) "78", Just 0.23065775846 ) ]) "77", Just 0.542502601047 ) ]) "76", Just 0.471873941302 ) ]) "70", Just 1.17677859772 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_woodsiaphila", Just 2.26765108241 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_bracteata", Just 1.11223623101 ), ( Leaf "Heuchera_hallii", Just 1.11223623101 ) ]) "85", Just 0.799972223411 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_mexicana", Just 1.00985960274 ), ( Branches ([ ( Leaf "Heuchera_longipetala", Just 0.672702201725 ), ( Leaf "Heuchera_acutifolia", Just 0.672702201725 ) ]) "89", Just 0.337157401018 ) ]) "88", Just 0.452796325447 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_hirsutissima", Just 0.908985338716 ), ( Branches ([ ( Branches ([ ( Leaf "Heuchera_caespitosa", Just 0.492761287085 ), ( Leaf "Heuchera_abramsii", Just 0.492761287085 ) ]) "94", Just 0.217512119899 ), ( Branches ([ ( Leaf "Heuchera_elegans", Just 0.570276420827 ), ( Leaf "Heuchera_parishii", Just 0.570276420827 ) ]) "95", Just 0.139996986157 ) ]) "93", Just 0.198711931733 ) ]) "92", Just 0.152249046392 ), ( Leaf "Heuchera_brevistaminea", Just 1.06123438511 ) ]) "91", Just 0.23571097096 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_rosendahlii", Just 0.436237554309 ), ( Leaf "Heuchera_wellsiae", Just 0.436237554309 ) ]) "98", Just 0.481744506695 ), ( Leaf "Heuchera_sanguinea", Just 0.917982061004 ) ]) "97", Just 0.18098598279 ), ( Leaf "Heuchera_versicolor", Just 1.09896804379 ) ]) "96", Just 0.197977312275 ) ]) "90", Just 0.165710572121 ) ]) "87", Just 0.211113420755 ), ( Branches ([ ( Leaf "Heuchera_pulchella", Just 1.19380805918 ), ( Leaf "Heuchera_rubescens", Just 1.19380805918 ) ]) "99", Just 0.479961289764 ) ]) "86", Just 0.238439105472 ) ]) "84", Just 0.355442627994 ) ]) "83", Just 0.472005667159 ), ( Branches ([ ( Leaf "Heuchera_merriamii", Just 2.38905826701 ), ( Leaf "Heuchera_grossulariifolia", Just 2.38905826701 ) ]) "100", Just 0.350598482559 ) ]) "82", Just 0.48883509419 ) ]) "69", Just 0.61160272986 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_pilosissima", Just 0.47153892037 ), ( Leaf "Heuchera_maxima", Just 0.47153892037 ) ]) "103", Just 0.554782803323 ), ( Leaf "Heuchera_micrantha", Just 1.02632172369 ) ]) "102", Just 2.51457414479 ), ( Branches ([ ( Leaf "Heuchera_glabra", Just 3.22414919189 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Heuchera_villosa", Just 0.981506873297 ), ( Leaf "Heuchera_puberula", Just 0.981506873297 ) ]) "107", Just 0.507115394285 ), ( Branches ([ ( Leaf "Heuchera_missouriensis", Just 0.942214782098 ), ( Leaf "Heuchera_parviflora", Just 0.942214782098 ) ]) "108", Just 0.546407485484 ) ]) "106", Just 1.18263380777 ), ( Branches ([ ( Leaf "Heuchera_chlorantha", Just 1.74505145726 ), ( Leaf "Heuchera_cylindrica", Just 1.74505145726 ) ]) "109", Just 0.926204618094 ) ]) "105", Just 0.552893116534 ) ]) "104", Just 0.316746676598 ) ]) "101", Just 0.299198705132 ) ]) "68", Just 2.27176201455 ), ( Branches ([ ( Branches ([ ( Leaf "menziesii", Just 1.84479470104 ), ( Leaf "diplomenziesii", Just 1.84479470104 ) ]) "111", Just 3.90021276384 ), ( Branches ([ ( Branches ([ ( Leaf "Tellima_grandiflora", Just 4.63906336711 ), ( Branches ([ ( Leaf "Mitella_pentandra", Just 4.09770275642 ), ( Branches ([ ( Branches ([ ( Leaf "Mitella_stylosa", Just 0.638709303912 ), ( Branches ([ ( Leaf "Mitella_furusei", Just 0.344251767186 ), ( Leaf "Mitella_pauciflora", Just 0.344251767186 ) ]) "117", Just 0.294457536726 ) ]) "116", Just 0.564868364663 ), ( Leaf "Mitella_japonica", Just 1.20357766858 ) ]) "115", Just 2.89412508785 ) ]) "114", Just 0.54136061069 ) ]) "113", Just 0.731361706037 ), ( Branches ([ ( Branches ([ ( Branches ([ ( Leaf "Mitella_breweri", Just 1.53688075103 ), ( Leaf "Mitella_ovalis", Just 1.53688075103 ) ]) "120", Just 2.97988329947 ), ( Leaf "Bensoniella_oregona", Just 4.5167640505 ) ]) "119", Just 0.503874826255 ), ( Branches ([ ( Leaf "Lithophragma_parviflorum", Just 4.44492705346 ), ( Branches ([ ( Leaf "Mitella_nuda", Just 2.76219772512 ), ( Leaf "Mitella_diphylla", Just 2.76219772512 ) ]) "122", Just 1.68272932833 ) ]) "121", Just 0.5757118233 ) ]) "118", Just 0.349786196394 ) ]) "112", Just 0.374582391731 ) ]) "110", Just 0.366849123291 ) ]) "67", Just 0.305415140583 ), ( Branches ([ ( Leaf "Tiarella_polyphylla", Just 5.93681616118 ), ( Branches ([ ( Branches ([ ( Leaf "Elmera_racemosa", Just 3.27000742488 ), ( Leaf "Mitella_caulescens", Just 3.27000742488 ) ]) "125", Just 1.2373578646 ), ( Branches ([ ( Leaf "Conimitella_williamsii", Just 3.00554587516 ), ( Leaf "Mitella_stauropetala", Just 3.00554587516 ) ]) "126", Just 1.50181941431 ) ]) "124", Just 1.42945087171 ) ]) "123", Just 0.480455567573 ) ]) "66", Just 11.3443841308 ) ]) "65", Just 2.29974099235 ), ( Leaf "Telesonix_jamesii", Just 20.0613968519 ) ]) "64")


heucheraString =
    "((Peltoboykinia_watanabei:17.7616558596,((((((Heuchera_richardsonii:1.59667580986,(Heuchera_caroliniana:1.25045275727,((Heuchera_pubescens:0.6012456036,Heuchera_alba:0.6012456036)Node_13:0.307630228026,(Heuchera_longiflora:0.678578784091,Heuchera_americana:0.678578784091)Node_16:0.230297047535)Node_12:0.341576925645)Node_10:0.346223052593)Node_8:0.455037436176,(Heuchera_parvifolia:1.57983930474,(Heuchera_wootonii:1.03733670369,(Heuchera_inconstans:0.806678945231,(((Heuchera_soltisii:0.368017442136,Heuchera_novomexicana:0.368017442136)Node_27:0.113213409433,Heuchera_glomerulata:0.481230851569)Node_26:0.200196385551,Heuchera_eastwoodiae:0.681427237119)Node_25:0.125251708112)Node_23:0.23065775846)Node_21:0.542502601047)Node_19:0.471873941302)Node_7:1.17677859772,((Heuchera_woodsiaphila:2.26765108241,((Heuchera_bracteata:1.11223623101,Heuchera_hallii:1.11223623101)Node_36:0.799972223411,(((Heuchera_mexicana:1.00985960274,(Heuchera_longipetala:0.672702201725,Heuchera_acutifolia:0.672702201725)Node_43:0.337157401018)Node_41:0.452796325447,(((Heuchera_hirsutissima:0.908985338716,((Heuchera_caespitosa:0.492761287085,Heuchera_abramsii:0.492761287085)Node_51:0.217512119899,(Heuchera_elegans:0.570276420827,Heuchera_parishii:0.570276420827)Node_54:0.139996986157)Node_50:0.198711931733)Node_48:0.152249046392,Heuchera_brevistaminea:1.06123438511)Node_47:0.23571097096,(((Heuchera_rosendahlii:0.436237554309,Heuchera_wellsiae:0.436237554309)Node_60:0.481744506695,Heuchera_sanguinea:0.917982061004)Node_59:0.18098598279,Heuchera_versicolor:1.09896804379)Node_58:0.197977312275)Node_46:0.165710572121)Node_40:0.211113420755,(Heuchera_pulchella:1.19380805918,Heuchera_rubescens:1.19380805918)Node_65:0.479961289764)Node_39:0.238439105472)Node_35:0.355442627994)Node_33:0.472005667159,(Heuchera_merriamii:2.38905826701,Heuchera_grossulariifolia:2.38905826701)Node_68:0.350598482559)Node_32:0.48883509419)Node_6:0.61160272986,(((Heuchera_pilosissima:0.47153892037,Heuchera_maxima:0.47153892037)Node_73:0.554782803323,Heuchera_micrantha:1.02632172369)Node_72:2.51457414479,(Heuchera_glabra:3.22414919189,(((Heuchera_villosa:0.981506873297,Heuchera_puberula:0.981506873297)Node_81:0.507115394285,(Heuchera_missouriensis:0.942214782098,Heuchera_parviflora:0.942214782098)Node_84:0.546407485484)Node_80:1.18263380777,(Heuchera_chlorantha:1.74505145726,Heuchera_cylindrica:1.74505145726)Node_87:0.926204618094)Node_79:0.552893116534)Node_77:0.316746676598)Node_71:0.299198705132)Node_5:2.27176201455,((menziesii:1.84479470104,diplomenziesii:1.84479470104)Node_91:3.90021276384,((Tellima_grandiflora:4.63906336711,(Mitella_pentandra:4.09770275642,((Mitella_stylosa:0.638709303912,(Mitella_furusei:0.344251767186,Mitella_pauciflora:0.344251767186)Node_102:0.294457536726)Node_100:0.564868364663,Mitella_japonica:1.20357766858)Node_99:2.89412508785)Node_97:0.54136061069)Node_95:0.731361706037,(((Mitella_breweri:1.53688075103,Mitella_ovalis:1.53688075103)Node_108:2.97988329947,Bensoniella_oregona:4.5167640505)Node_107:0.503874826255,(Lithophragma_parviflorum:4.44492705346,(Mitella_nuda:2.76219772512,Mitella_diphylla:2.76219772512)Node_114:1.68272932833)Node_112:0.5757118233)Node_106:0.349786196394)Node_94:0.374582391731)Node_90:0.366849123291)Node_4:0.305415140583,(Tiarella_polyphylla:5.93681616118,((Elmera_racemosa:3.27000742488,Mitella_caulescens:3.27000742488)Node_120:1.2373578646,(Conimitella_williamsii:3.00554587516,Mitella_stauropetala:3.00554587516)Node_123:1.50181941431)Node_119:1.42945087171)Node_117:0.480455567573)Node_3:11.3443841308)Node_1:2.29974099235,Telesonix_jamesii:20.0613968519)Node_0;"


heucheraString2 =
    "((Peltoboykinia_watanabei:17.7616558596,((((((Heuchera_richardsonii:1.59667580986,(Heuchera_caroliniana:1.25045275727,((Heuchera_pubescens:0.6012456036,Heuchera_alba:0.6012456036)74:0.307630228026,(Heuchera_longiflora:0.678578784091,Heuchera_americana:0.678578784091)75:0.230297047535)73:0.341576925645)72:0.346223052593)71:0.455037436176,(Heuchera_parvifolia:1.57983930474,(Heuchera_wootonii:1.03733670369,(Heuchera_inconstans:0.806678945231,(((Heuchera_soltisii:0.368017442136,Heuchera_novomexicana:0.368017442136)81:0.113213409433,Heuchera_glomerulata:0.481230851569)80:0.200196385551,Heuchera_eastwoodiae:0.681427237119)79:0.125251708112)78:0.23065775846)77:0.542502601047)76:0.471873941302)70:1.17677859772,((Heuchera_woodsiaphila:2.26765108241,((Heuchera_bracteata:1.11223623101,Heuchera_hallii:1.11223623101)85:0.799972223411,(((Heuchera_mexicana:1.00985960274,(Heuchera_longipetala:0.672702201725,Heuchera_acutifolia:0.672702201725)89:0.337157401018)88:0.452796325447,(((Heuchera_hirsutissima:0.908985338716,((Heuchera_caespitosa:0.492761287085,Heuchera_abramsii:0.492761287085)94:0.217512119899,(Heuchera_elegans:0.570276420827,Heuchera_parishii:0.570276420827)95:0.139996986157)93:0.198711931733)92:0.152249046392,Heuchera_brevistaminea:1.06123438511)91:0.23571097096,(((Heuchera_rosendahlii:0.436237554309,Heuchera_wellsiae:0.436237554309)98:0.481744506695,Heuchera_sanguinea:0.917982061004)97:0.18098598279,Heuchera_versicolor:1.09896804379)96:0.197977312275)90:0.165710572121)87:0.211113420755,(Heuchera_pulchella:1.19380805918,Heuchera_rubescens:1.19380805918)99:0.479961289764)86:0.238439105472)84:0.355442627994)83:0.472005667159,(Heuchera_merriamii:2.38905826701,Heuchera_grossulariifolia:2.38905826701)100:0.350598482559)82:0.48883509419)69:0.61160272986,(((Heuchera_pilosissima:0.47153892037,Heuchera_maxima:0.47153892037)103:0.554782803323,Heuchera_micrantha:1.02632172369)102:2.51457414479,(Heuchera_glabra:3.22414919189,(((Heuchera_villosa:0.981506873297,Heuchera_puberula:0.981506873297)107:0.507115394285,(Heuchera_missouriensis:0.942214782098,Heuchera_parviflora:0.942214782098)108:0.546407485484)106:1.18263380777,(Heuchera_chlorantha:1.74505145726,Heuchera_cylindrica:1.74505145726)109:0.926204618094)105:0.552893116534)104:0.316746676598)101:0.299198705132)68:2.27176201455,((menziesii:1.84479470104,diplomenziesii:1.84479470104)111:3.90021276384,((Tellima_grandiflora:4.63906336711,(Mitella_pentandra:4.09770275642,((Mitella_stylosa:0.638709303912,(Mitella_furusei:0.344251767186,Mitella_pauciflora:0.344251767186)117:0.294457536726)116:0.564868364663,Mitella_japonica:1.20357766858)115:2.89412508785)114:0.54136061069)113:0.731361706037,(((Mitella_breweri:1.53688075103,Mitella_ovalis:1.53688075103)120:2.97988329947,Bensoniella_oregona:4.5167640505)119:0.503874826255,(Lithophragma_parviflorum:4.44492705346,(Mitella_nuda:2.76219772512,Mitella_diphylla:2.76219772512)122:1.68272932833)121:0.5757118233)118:0.349786196394)112:0.374582391731)110:0.366849123291)67:0.305415140583,(Tiarella_polyphylla:5.93681616118,((Elmera_racemosa:3.27000742488,Mitella_caulescens:3.27000742488)125:1.2373578646,(Conimitella_williamsii:3.00554587516,Mitella_stauropetala:3.00554587516)126:1.50181941431)124:1.42945087171)123:0.480455567573)66:11.3443841308)65:2.29974099235,Telesonix_jamesii:20.0613968519)64;"


heucheraBinary =
    Binary.Node { cladeId = 0, length = Just 0, name = "Node_0", squid = Nothing }
        (Binary.Node { cladeId = 1, length = Just 2.29974099235, name = "Node_1", squid = Nothing }
            (Binary.Leaf { cladeId = -1, length = Just 17.7616558596, name = "Peltoboykinia_watanabei", squid = Nothing })
            (Binary.Node { cladeId = 3, length = Just 11.3443841308, name = "Node_3", squid = Nothing }
                (Binary.Node { cladeId = 4, length = Just 0.305415140583, name = "Node_4", squid = Nothing }
                    (Binary.Node { cladeId = 5, length = Just 2.27176201455, name = "Node_5", squid = Nothing }
                        (Binary.Node { cladeId = 6, length = Just 0.61160272986, name = "Node_6", squid = Nothing }
                            (Binary.Node { cladeId = 7, length = Just 1.17677859772, name = "Node_7", squid = Nothing }
                                (Binary.Node { cladeId = 8, length = Just 0.455037436176, name = "Node_8", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 1.59667580986, name = "Heuchera_richardsonii", squid = Just "38812fa5d76005b182799ffa21780abe6e1d1008504765ce359682e891ecf6d7" })
                                    (Binary.Node { cladeId = 10, length = Just 0.346223052593, name = "Node_10", squid = Nothing }
                                        (Binary.Leaf { cladeId = -1, length = Just 1.25045275727, name = "Heuchera_caroliniana", squid = Just "fa8cbaa3c4d0df586b295964573ecf06a1907f06af6184f1c1719cb785ab8101" })
                                        (Binary.Node { cladeId = 12, length = Just 0.341576925645, name = "Node_12", squid = Nothing }
                                            (Binary.Node { cladeId = 13, length = Just 0.307630228026, name = "Node_13", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.6012456036, name = "Heuchera_pubescens", squid = Just "56d66eaf3474c32e3ed02c6f2a2fd1a78392fbf3d5c9be2e3e551ecb69232d45" }) (Binary.Leaf { cladeId = -1, length = Just 0.6012456036, name = "Heuchera_alba", squid = Just "cc8bbb6758a31f6faf88dddb82426e23ab32daa2a12840250204edae8b63616a" }))
                                            (Binary.Node { cladeId = 16, length = Just 0.230297047535, name = "Node_16", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.678578784091, name = "Heuchera_longiflora", squid = Just "04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9" }) (Binary.Leaf { cladeId = -1, length = Just 0.678578784091, name = "Heuchera_americana", squid = Just "92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e" }))
                                        )
                                    )
                                )
                                (Binary.Node { cladeId = 19, length = Just 0.471873941302, name = "Node_19", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 1.57983930474, name = "Heuchera_parvifolia", squid = Just "266f28def683bf788e174a02789e9bd5bf0f3466a96ae748fd3c0315bd1fa4ec" })
                                    (Binary.Node { cladeId = 21, length = Just 0.542502601047, name = "Node_21", squid = Nothing }
                                        (Binary.Leaf { cladeId = -1, length = Just 1.03733670369, name = "Heuchera_wootonii", squid = Just "4dca0a5d7515a0ff07371d9a61ed6ee3f037bb07381a117a0d406eedefa2e421" })
                                        (Binary.Node { cladeId = 23, length = Just 0.23065775846, name = "Node_23", squid = Nothing }
                                            (Binary.Leaf { cladeId = -1, length = Just 0.806678945231, name = "Heuchera_inconstans", squid = Just "84b8213adfb1e8777bf3df0d39bb808bae2f07f31eff0f886c7535197700d542" })
                                            (Binary.Node { cladeId = 25, length = Just 0.125251708112, name = "Node_25", squid = Nothing }
                                                (Binary.Node { cladeId = 26, length = Just 0.200196385551, name = "Node_26", squid = Nothing }
                                                    (Binary.Node { cladeId = 27, length = Just 0.113213409433, name = "Node_27", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.368017442136, name = "Heuchera_soltisii", squid = Just "f9e19dcad2616458da2df7e524cf09108401f0ed1d7df97dc2533b17c05c45a1" }) (Binary.Leaf { cladeId = -1, length = Just 0.368017442136, name = "Heuchera_novomexicana", squid = Just "3037e536df9ecc0c181f7059df2cf4369ec36a331b943b677a1a9dd1098e0542" }))
                                                    (Binary.Leaf { cladeId = -1, length = Just 0.481230851569, name = "Heuchera_glomerulata", squid = Just "adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98" })
                                                )
                                                (Binary.Leaf { cladeId = -1, length = Just 0.681427237119, name = "Heuchera_eastwoodiae", squid = Just "88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c" })
                                            )
                                        )
                                    )
                                )
                            )
                            (Binary.Node { cladeId = 32, length = Just 0.48883509419, name = "Node_32", squid = Nothing }
                                (Binary.Node { cladeId = 33, length = Just 0.472005667159, name = "Node_33", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 2.26765108241, name = "Heuchera_woodsiaphila", squid = Just "b71d73ef9406056c1a523545adfddb40248f085ea6e84f6aca39194a2e597516" })
                                    (Binary.Node { cladeId = 35, length = Just 0.355442627994, name = "Node_35", squid = Nothing }
                                        (Binary.Node { cladeId = 36, length = Just 0.799972223411, name = "Node_36", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.11223623101, name = "Heuchera_bracteata", squid = Just "5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9" }) (Binary.Leaf { cladeId = -1, length = Just 1.11223623101, name = "Heuchera_hallii", squid = Just "9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe" }))
                                        (Binary.Node { cladeId = 39, length = Just 0.238439105472, name = "Node_39", squid = Nothing }
                                            (Binary.Node { cladeId = 40, length = Just 0.211113420755, name = "Node_40", squid = Nothing }
                                                (Binary.Node { cladeId = 41, length = Just 0.452796325447, name = "Node_41", squid = Nothing }
                                                    (Binary.Leaf { cladeId = -1, length = Just 1.00985960274, name = "Heuchera_mexicana", squid = Just "0f2535ee120c326895347fb6421a943c1a7a3f255bbe33e689246a51763994f2" })
                                                    (Binary.Node { cladeId = 43, length = Just 0.337157401018, name = "Node_43", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.672702201725, name = "Heuchera_longipetala", squid = Just "19b5c2ba6ca08b4027e75aa1d267c923da1d4348aca42c75af45c4f39c1c91cd" }) (Binary.Leaf { cladeId = -1, length = Just 0.672702201725, name = "Heuchera_acutifolia", squid = Just "d773b8cab5c0bbcc101697ed6f99fa1d0ac5c38b30590dfd52d8a232ccc40f2c" }))
                                                )
                                                (Binary.Node { cladeId = 46, length = Just 0.165710572121, name = "Node_46", squid = Nothing }
                                                    (Binary.Node { cladeId = 47, length = Just 0.23571097096, name = "Node_47", squid = Nothing }
                                                        (Binary.Node { cladeId = 48, length = Just 0.152249046392, name = "Node_48", squid = Nothing }
                                                            (Binary.Leaf { cladeId = -1, length = Just 0.908985338716, name = "Heuchera_hirsutissima", squid = Just "36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5" })
                                                            (Binary.Node { cladeId = 50, length = Just 0.198711931733, name = "Node_50", squid = Nothing }
                                                                (Binary.Node { cladeId = 51, length = Just 0.217512119899, name = "Node_51", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.492761287085, name = "Heuchera_caespitosa", squid = Just "16af2ce027f58cfd878cdbf33b5722fbdea17bc8fe3ddfcd49a358f8229e0629" }) (Binary.Leaf { cladeId = -1, length = Just 0.492761287085, name = "Heuchera_abramsii", squid = Just "683835815e83a910b4877de7c7f2ef1b2ba671d176d78425dde229c1c21c0b72" }))
                                                                (Binary.Node { cladeId = 54, length = Just 0.139996986157, name = "Node_54", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.570276420827, name = "Heuchera_elegans", squid = Just "c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104" }) (Binary.Leaf { cladeId = -1, length = Just 0.570276420827, name = "Heuchera_parishii", squid = Just "d4645ba5ab9f1fe267c915f27dcfa8f8a69cc61538889dacc874c0fb604593d2" }))
                                                            )
                                                        )
                                                        (Binary.Leaf { cladeId = -1, length = Just 1.06123438511, name = "Heuchera_brevistaminea", squid = Just "a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651" })
                                                    )
                                                    (Binary.Node { cladeId = 58, length = Just 0.197977312275, name = "Node_58", squid = Nothing }
                                                        (Binary.Node { cladeId = 59, length = Just 0.18098598279, name = "Node_59", squid = Nothing }
                                                            (Binary.Node { cladeId = 60, length = Just 0.481744506695, name = "Node_60", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.436237554309, name = "Heuchera_rosendahlii", squid = Just "14b24a9ee9bc2d482a68ab018015beac1b72c8398446419c5eb86c837a6c0e22" }) (Binary.Leaf { cladeId = -1, length = Just 0.436237554309, name = "Heuchera_wellsiae", squid = Just "696313d6cf37c5663432fa7b3bf4be09ef6881c0f454a43293162ba469d5c31e" }))
                                                            (Binary.Leaf { cladeId = -1, length = Just 0.917982061004, name = "Heuchera_sanguinea", squid = Just "2dfeb84c36af48dcfac361fe3498e8d07a9a9415cc86d4e77e82f356adfe7d81" })
                                                        )
                                                        (Binary.Leaf { cladeId = -1, length = Just 1.09896804379, name = "Heuchera_versicolor", squid = Just "e3cfdec7d79241229e23aa5873d1cbe45495f1d3bd42a5719e909023c4dca5ce" })
                                                    )
                                                )
                                            )
                                            (Binary.Node { cladeId = 65, length = Just 0.479961289764, name = "Node_65", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.19380805918, name = "Heuchera_pulchella", squid = Just "8fd0068c4ab7cf36d9aeeb608e41d05e4fcc8f96a435e01b8450c059fa1e5362" }) (Binary.Leaf { cladeId = -1, length = Just 1.19380805918, name = "Heuchera_rubescens", squid = Just "ef1ab7833784d81ea21db11e3d0e9b0d9bedb743b3c97a82f5b45d4f979ef97c" }))
                                        )
                                    )
                                )
                                (Binary.Node { cladeId = 68, length = Just 0.350598482559, name = "Node_68", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 2.38905826701, name = "Heuchera_merriamii", squid = Just "6fe3c631902b97cbb48f7943774679dff5bd86d4c54e2c1b92c3c95ef2d1e056" }) (Binary.Leaf { cladeId = -1, length = Just 2.38905826701, name = "Heuchera_grossulariifolia", squid = Just "87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84" }))
                            )
                        )
                        (Binary.Node { cladeId = 71, length = Just 0.299198705132, name = "Node_71", squid = Nothing }
                            (Binary.Node { cladeId = 72, length = Just 2.51457414479, name = "Node_72", squid = Nothing }
                                (Binary.Node { cladeId = 73, length = Just 0.554782803323, name = "Node_73", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.47153892037, name = "Heuchera_pilosissima", squid = Just "e8d7add6ffc2f3bfc23623d340f2b358643ff961dafc8e73df6c7b8e3ef89b66" }) (Binary.Leaf { cladeId = -1, length = Just 0.47153892037, name = "Heuchera_maxima", squid = Just "47b697873b763836ae7c3e2f43bac664d050bb88282efe93c94c60c8ab1dbeed" }))
                                (Binary.Leaf { cladeId = -1, length = Just 1.02632172369, name = "Heuchera_micrantha", squid = Just "190e31374a4538d19563aa9c985c90991959c02ecd2bcbe471cddad76a27c399" })
                            )
                            (Binary.Node { cladeId = 77, length = Just 0.316746676598, name = "Node_77", squid = Nothing }
                                (Binary.Leaf { cladeId = -1, length = Just 3.22414919189, name = "Heuchera_glabra", squid = Just "cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10" })
                                (Binary.Node { cladeId = 79, length = Just 0.552893116534, name = "Node_79", squid = Nothing }
                                    (Binary.Node { cladeId = 80, length = Just 1.18263380777, name = "Node_80", squid = Nothing }
                                        (Binary.Node { cladeId = 81, length = Just 0.507115394285, name = "Node_81", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.981506873297, name = "Heuchera_villosa", squid = Just "e8f27ed1d833be70dd71fe6da8c2ad629db1822919eed00562c3e52535e987ec" }) (Binary.Leaf { cladeId = -1, length = Just 0.981506873297, name = "Heuchera_puberula", squid = Just "de5aaf6d3c94b6b76b67805a26ece3448e3ed3ff82a2c79fc9de8da577e35c28" }))
                                        (Binary.Node { cladeId = 84, length = Just 0.546407485484, name = "Node_84", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.942214782098, name = "Heuchera_missouriensis", squid = Just "0fcc9b4ca9d87f9ebab1ef56b6ad4f60fd53a54374ae916143f9ad14d0952002" }) (Binary.Leaf { cladeId = -1, length = Just 0.942214782098, name = "Heuchera_parviflora", squid = Just "fc44011d8599702797e95e89427d7bc3b153d6858ce1e51c188513e92e313fcc" }))
                                    )
                                    (Binary.Node { cladeId = 87, length = Just 0.926204618094, name = "Node_87", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.74505145726, name = "Heuchera_chlorantha", squid = Just "277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10" }) (Binary.Leaf { cladeId = -1, length = Just 1.74505145726, name = "Heuchera_cylindrica", squid = Just "31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33" }))
                                )
                            )
                        )
                    )
                    (Binary.Node { cladeId = 90, length = Just 0.366849123291, name = "Node_90", squid = Nothing }
                        (Binary.Node { cladeId = 91, length = Just 3.90021276384, name = "Node_91", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.84479470104, name = "menziesii", squid = Just "d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c" }) (Binary.Leaf { cladeId = -1, length = Just 1.84479470104, name = "diplomenziesii", squid = Just "5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a" }))
                        (Binary.Node { cladeId = 94, length = Just 0.374582391731, name = "Node_94", squid = Nothing }
                            (Binary.Node { cladeId = 95, length = Just 0.731361706037, name = "Node_95", squid = Nothing }
                                (Binary.Leaf { cladeId = -1, length = Just 4.63906336711, name = "Tellima_grandiflora", squid = Just "cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888" })
                                (Binary.Node { cladeId = 97, length = Just 0.54136061069, name = "Node_97", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 4.09770275642, name = "Mitella_pentandra", squid = Just "eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12" })
                                    (Binary.Node { cladeId = 99, length = Just 2.89412508785, name = "Node_99", squid = Nothing }
                                        (Binary.Node { cladeId = 100, length = Just 0.564868364663, name = "Node_100", squid = Nothing }
                                            (Binary.Leaf { cladeId = -1, length = Just 0.638709303912, name = "Mitella_stylosa", squid = Just "557ccfded960e4ff692bb0bd82477149be49a7ce0a4b92f1488ba4ae50ef3fa1" })
                                            (Binary.Node { cladeId = 102, length = Just 0.294457536726, name = "Node_102", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.344251767186, name = "Mitella_furusei", squid = Just "6655ecf0325609f0c196fe7b9fb6c74be2a5704bb6ffc31e9d1926ef9860e1be" }) (Binary.Leaf { cladeId = -1, length = Just 0.344251767186, name = "Mitella_pauciflora", squid = Just "1e8d5bfd8d2a4d26cb3750f0974fc1c94f4c74da77a330c17397b5d47a621d9e" }))
                                        )
                                        (Binary.Leaf { cladeId = -1, length = Just 1.20357766858, name = "Mitella_japonica", squid = Just "006429bb3a05e2ee6245ab9d7e0262bffd1432e9226c362d1d6e575e8aad8c0c" })
                                    )
                                )
                            )
                            (Binary.Node { cladeId = 106, length = Just 0.349786196394, name = "Node_106", squid = Nothing }
                                (Binary.Node { cladeId = 107, length = Just 0.503874826255, name = "Node_107", squid = Nothing }
                                    (Binary.Node { cladeId = 108, length = Just 2.97988329947, name = "Node_108", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.53688075103, name = "Mitella_breweri", squid = Just "de203e04a3c569d028f8583157c051142e24376caa8ffe770152518ff27af4da" }) (Binary.Leaf { cladeId = -1, length = Just 1.53688075103, name = "Mitella_ovalis", squid = Just "5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be" }))
                                    (Binary.Leaf { cladeId = -1, length = Just 4.5167640505, name = "Bensoniella_oregona", squid = Just "e90cdd2736753a418d051642325264f4307047bfadcdc5b154d72e70e182f9a7" })
                                )
                                (Binary.Node { cladeId = 112, length = Just 0.5757118233, name = "Node_112", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 4.44492705346, name = "Lithophragma_parviflorum", squid = Just "3437951f02c03c705bd14abd7f093684bb22bd0ed9f6ab74cf90827df1361a31" })
                                    (Binary.Node { cladeId = 114, length = Just 1.68272932833, name = "Node_114", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 2.76219772512, name = "Mitella_nuda", squid = Just "eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f" }) (Binary.Leaf { cladeId = -1, length = Just 2.76219772512, name = "Mitella_diphylla", squid = Just "6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6" }))
                                )
                            )
                        )
                    )
                )
                (Binary.Node { cladeId = 117, length = Just 0.480455567573, name = "Node_117", squid = Nothing }
                    (Binary.Leaf { cladeId = -1, length = Just 5.93681616118, name = "Tiarella_polyphylla", squid = Just "84fb5356b01f7010a952df74a8bf7d6f48ce357ed63697ce0ec7724008c8ed38" })
                    (Binary.Node { cladeId = 119, length = Just 1.42945087171, name = "Node_119", squid = Nothing }
                        (Binary.Node { cladeId = 120, length = Just 1.2373578646, name = "Node_120", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 3.27000742488, name = "Elmera_racemosa", squid = Just "113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b" }) (Binary.Leaf { cladeId = -1, length = Just 3.27000742488, name = "Mitella_caulescens", squid = Just "3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516" }))
                        (Binary.Node { cladeId = 123, length = Just 1.50181941431, name = "Node_123", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 3.00554587516, name = "Conimitella_williamsii", squid = Just "5460b06e839e09674ce4d759175a3ad5b76d266ed4d7a104605ae3918f17e412" }) (Binary.Leaf { cladeId = -1, length = Just 3.00554587516, name = "Mitella_stauropetala", squid = Just "9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1" }))
                    )
                )
            )
        )
        (Binary.Leaf { cladeId = -1, length = Just 20.0613968519, name = "Telesonix_jamesii", squid = Just "fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692" })


heucheraBinary2 =
    Binary.Node { cladeId = 64, length = Just 0, name = "64", squid = Nothing }
        (Binary.Node { cladeId = 65, length = Just 2.29974099235, name = "65", squid = Nothing }
            (Binary.Leaf { cladeId = -1, length = Just 17.7616558596, name = "Peltoboykinia_watanabei", squid = Nothing })
            (Binary.Node { cladeId = 66, length = Just 11.3443841308, name = "66", squid = Nothing }
                (Binary.Node { cladeId = 67, length = Just 0.305415140583, name = "67", squid = Nothing }
                    (Binary.Node { cladeId = 68, length = Just 2.27176201455, name = "68", squid = Nothing }
                        (Binary.Node { cladeId = 69, length = Just 0.61160272986, name = "69", squid = Nothing }
                            (Binary.Node { cladeId = 70, length = Just 1.17677859772, name = "70", squid = Nothing }
                                (Binary.Node { cladeId = 71, length = Just 0.455037436176, name = "71", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 1.59667580986, name = "Heuchera_richardsonii", squid = Just "38812fa5d76005b182799ffa21780abe6e1d1008504765ce359682e891ecf6d7" })
                                    (Binary.Node { cladeId = 72, length = Just 0.346223052593, name = "72", squid = Nothing }
                                        (Binary.Leaf { cladeId = -1, length = Just 1.25045275727, name = "Heuchera_caroliniana", squid = Just "fa8cbaa3c4d0df586b295964573ecf06a1907f06af6184f1c1719cb785ab8101" })
                                        (Binary.Node { cladeId = 73, length = Just 0.341576925645, name = "73", squid = Nothing }
                                            (Binary.Node { cladeId = 74, length = Just 0.307630228026, name = "74", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.6012456036, name = "Heuchera_pubescens", squid = Just "56d66eaf3474c32e3ed02c6f2a2fd1a78392fbf3d5c9be2e3e551ecb69232d45" }) (Binary.Leaf { cladeId = -1, length = Just 0.6012456036, name = "Heuchera_alba", squid = Just "cc8bbb6758a31f6faf88dddb82426e23ab32daa2a12840250204edae8b63616a" }))
                                            (Binary.Node { cladeId = 75, length = Just 0.230297047535, name = "75", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.678578784091, name = "Heuchera_longiflora", squid = Just "04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9" }) (Binary.Leaf { cladeId = -1, length = Just 0.678578784091, name = "Heuchera_americana", squid = Just "92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e" }))
                                        )
                                    )
                                )
                                (Binary.Node { cladeId = 76, length = Just 0.471873941302, name = "76", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 1.57983930474, name = "Heuchera_parvifolia", squid = Just "266f28def683bf788e174a02789e9bd5bf0f3466a96ae748fd3c0315bd1fa4ec" })
                                    (Binary.Node { cladeId = 77, length = Just 0.542502601047, name = "77", squid = Nothing }
                                        (Binary.Leaf { cladeId = -1, length = Just 1.03733670369, name = "Heuchera_wootonii", squid = Just "4dca0a5d7515a0ff07371d9a61ed6ee3f037bb07381a117a0d406eedefa2e421" })
                                        (Binary.Node { cladeId = 78, length = Just 0.23065775846, name = "78", squid = Nothing }
                                            (Binary.Leaf { cladeId = -1, length = Just 0.806678945231, name = "Heuchera_inconstans", squid = Just "84b8213adfb1e8777bf3df0d39bb808bae2f07f31eff0f886c7535197700d542" })
                                            (Binary.Node { cladeId = 79, length = Just 0.125251708112, name = "79", squid = Nothing }
                                                (Binary.Node { cladeId = 80, length = Just 0.200196385551, name = "80", squid = Nothing }
                                                    (Binary.Node { cladeId = 81, length = Just 0.113213409433, name = "81", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.368017442136, name = "Heuchera_soltisii", squid = Just "f9e19dcad2616458da2df7e524cf09108401f0ed1d7df97dc2533b17c05c45a1" }) (Binary.Leaf { cladeId = -1, length = Just 0.368017442136, name = "Heuchera_novomexicana", squid = Just "3037e536df9ecc0c181f7059df2cf4369ec36a331b943b677a1a9dd1098e0542" }))
                                                    (Binary.Leaf { cladeId = -1, length = Just 0.481230851569, name = "Heuchera_glomerulata", squid = Just "adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98" })
                                                )
                                                (Binary.Leaf { cladeId = -1, length = Just 0.681427237119, name = "Heuchera_eastwoodiae", squid = Just "88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c" })
                                            )
                                        )
                                    )
                                )
                            )
                            (Binary.Node { cladeId = 82, length = Just 0.48883509419, name = "82", squid = Nothing }
                                (Binary.Node { cladeId = 83, length = Just 0.472005667159, name = "83", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 2.26765108241, name = "Heuchera_woodsiaphila", squid = Just "b71d73ef9406056c1a523545adfddb40248f085ea6e84f6aca39194a2e597516" })
                                    (Binary.Node { cladeId = 84, length = Just 0.355442627994, name = "84", squid = Nothing }
                                        (Binary.Node { cladeId = 85, length = Just 0.799972223411, name = "85", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.11223623101, name = "Heuchera_bracteata", squid = Just "5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9" }) (Binary.Leaf { cladeId = -1, length = Just 1.11223623101, name = "Heuchera_hallii", squid = Just "9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe" }))
                                        (Binary.Node { cladeId = 86, length = Just 0.238439105472, name = "86", squid = Nothing }
                                            (Binary.Node { cladeId = 87, length = Just 0.211113420755, name = "87", squid = Nothing }
                                                (Binary.Node { cladeId = 88, length = Just 0.452796325447, name = "88", squid = Nothing }
                                                    (Binary.Leaf { cladeId = -1, length = Just 1.00985960274, name = "Heuchera_mexicana", squid = Just "0f2535ee120c326895347fb6421a943c1a7a3f255bbe33e689246a51763994f2" })
                                                    (Binary.Node { cladeId = 89, length = Just 0.337157401018, name = "89", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.672702201725, name = "Heuchera_longipetala", squid = Just "19b5c2ba6ca08b4027e75aa1d267c923da1d4348aca42c75af45c4f39c1c91cd" }) (Binary.Leaf { cladeId = -1, length = Just 0.672702201725, name = "Heuchera_acutifolia", squid = Just "d773b8cab5c0bbcc101697ed6f99fa1d0ac5c38b30590dfd52d8a232ccc40f2c" }))
                                                )
                                                (Binary.Node { cladeId = 90, length = Just 0.165710572121, name = "90", squid = Nothing }
                                                    (Binary.Node { cladeId = 91, length = Just 0.23571097096, name = "91", squid = Nothing }
                                                        (Binary.Node { cladeId = 92, length = Just 0.152249046392, name = "92", squid = Nothing }
                                                            (Binary.Leaf { cladeId = -1, length = Just 0.908985338716, name = "Heuchera_hirsutissima", squid = Just "36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5" })
                                                            (Binary.Node { cladeId = 93, length = Just 0.198711931733, name = "93", squid = Nothing }
                                                                (Binary.Node { cladeId = 94, length = Just 0.217512119899, name = "94", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.492761287085, name = "Heuchera_caespitosa", squid = Just "16af2ce027f58cfd878cdbf33b5722fbdea17bc8fe3ddfcd49a358f8229e0629" }) (Binary.Leaf { cladeId = -1, length = Just 0.492761287085, name = "Heuchera_abramsii", squid = Just "683835815e83a910b4877de7c7f2ef1b2ba671d176d78425dde229c1c21c0b72" }))
                                                                (Binary.Node { cladeId = 95, length = Just 0.139996986157, name = "95", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.570276420827, name = "Heuchera_elegans", squid = Just "c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104" }) (Binary.Leaf { cladeId = -1, length = Just 0.570276420827, name = "Heuchera_parishii", squid = Just "d4645ba5ab9f1fe267c915f27dcfa8f8a69cc61538889dacc874c0fb604593d2" }))
                                                            )
                                                        )
                                                        (Binary.Leaf { cladeId = -1, length = Just 1.06123438511, name = "Heuchera_brevistaminea", squid = Just "a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651" })
                                                    )
                                                    (Binary.Node { cladeId = 96, length = Just 0.197977312275, name = "96", squid = Nothing }
                                                        (Binary.Node { cladeId = 97, length = Just 0.18098598279, name = "97", squid = Nothing }
                                                            (Binary.Node { cladeId = 98, length = Just 0.481744506695, name = "98", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.436237554309, name = "Heuchera_rosendahlii", squid = Just "14b24a9ee9bc2d482a68ab018015beac1b72c8398446419c5eb86c837a6c0e22" }) (Binary.Leaf { cladeId = -1, length = Just 0.436237554309, name = "Heuchera_wellsiae", squid = Just "696313d6cf37c5663432fa7b3bf4be09ef6881c0f454a43293162ba469d5c31e" }))
                                                            (Binary.Leaf { cladeId = -1, length = Just 0.917982061004, name = "Heuchera_sanguinea", squid = Just "2dfeb84c36af48dcfac361fe3498e8d07a9a9415cc86d4e77e82f356adfe7d81" })
                                                        )
                                                        (Binary.Leaf { cladeId = -1, length = Just 1.09896804379, name = "Heuchera_versicolor", squid = Just "e3cfdec7d79241229e23aa5873d1cbe45495f1d3bd42a5719e909023c4dca5ce" })
                                                    )
                                                )
                                            )
                                            (Binary.Node { cladeId = 99, length = Just 0.479961289764, name = "99", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.19380805918, name = "Heuchera_pulchella", squid = Just "8fd0068c4ab7cf36d9aeeb608e41d05e4fcc8f96a435e01b8450c059fa1e5362" }) (Binary.Leaf { cladeId = -1, length = Just 1.19380805918, name = "Heuchera_rubescens", squid = Just "ef1ab7833784d81ea21db11e3d0e9b0d9bedb743b3c97a82f5b45d4f979ef97c" }))
                                        )
                                    )
                                )
                                (Binary.Node { cladeId = 100, length = Just 0.350598482559, name = "100", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 2.38905826701, name = "Heuchera_merriamii", squid = Just "6fe3c631902b97cbb48f7943774679dff5bd86d4c54e2c1b92c3c95ef2d1e056" }) (Binary.Leaf { cladeId = -1, length = Just 2.38905826701, name = "Heuchera_grossulariifolia", squid = Just "87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84" }))
                            )
                        )
                        (Binary.Node { cladeId = 101, length = Just 0.299198705132, name = "101", squid = Nothing }
                            (Binary.Node { cladeId = 102, length = Just 2.51457414479, name = "102", squid = Nothing }
                                (Binary.Node { cladeId = 103, length = Just 0.554782803323, name = "103", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.47153892037, name = "Heuchera_pilosissima", squid = Just "e8d7add6ffc2f3bfc23623d340f2b358643ff961dafc8e73df6c7b8e3ef89b66" }) (Binary.Leaf { cladeId = -1, length = Just 0.47153892037, name = "Heuchera_maxima", squid = Just "47b697873b763836ae7c3e2f43bac664d050bb88282efe93c94c60c8ab1dbeed" }))
                                (Binary.Leaf { cladeId = -1, length = Just 1.02632172369, name = "Heuchera_micrantha", squid = Just "190e31374a4538d19563aa9c985c90991959c02ecd2bcbe471cddad76a27c399" })
                            )
                            (Binary.Node { cladeId = 104, length = Just 0.316746676598, name = "104", squid = Nothing }
                                (Binary.Leaf { cladeId = -1, length = Just 3.22414919189, name = "Heuchera_glabra", squid = Just "cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10" })
                                (Binary.Node { cladeId = 105, length = Just 0.552893116534, name = "105", squid = Nothing }
                                    (Binary.Node { cladeId = 106, length = Just 1.18263380777, name = "106", squid = Nothing }
                                        (Binary.Node { cladeId = 107, length = Just 0.507115394285, name = "107", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.981506873297, name = "Heuchera_villosa", squid = Just "e8f27ed1d833be70dd71fe6da8c2ad629db1822919eed00562c3e52535e987ec" }) (Binary.Leaf { cladeId = -1, length = Just 0.981506873297, name = "Heuchera_puberula", squid = Just "de5aaf6d3c94b6b76b67805a26ece3448e3ed3ff82a2c79fc9de8da577e35c28" }))
                                        (Binary.Node { cladeId = 108, length = Just 0.546407485484, name = "108", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.942214782098, name = "Heuchera_missouriensis", squid = Just "0fcc9b4ca9d87f9ebab1ef56b6ad4f60fd53a54374ae916143f9ad14d0952002" }) (Binary.Leaf { cladeId = -1, length = Just 0.942214782098, name = "Heuchera_parviflora", squid = Just "fc44011d8599702797e95e89427d7bc3b153d6858ce1e51c188513e92e313fcc" }))
                                    )
                                    (Binary.Node { cladeId = 109, length = Just 0.926204618094, name = "109", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.74505145726, name = "Heuchera_chlorantha", squid = Just "277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10" }) (Binary.Leaf { cladeId = -1, length = Just 1.74505145726, name = "Heuchera_cylindrica", squid = Just "31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33" }))
                                )
                            )
                        )
                    )
                    (Binary.Node { cladeId = 110, length = Just 0.366849123291, name = "110", squid = Nothing }
                        (Binary.Node { cladeId = 111, length = Just 3.90021276384, name = "111", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.84479470104, name = "menziesii", squid = Just "d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c" }) (Binary.Leaf { cladeId = -1, length = Just 1.84479470104, name = "diplomenziesii", squid = Just "5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a" }))
                        (Binary.Node { cladeId = 112, length = Just 0.374582391731, name = "112", squid = Nothing }
                            (Binary.Node { cladeId = 113, length = Just 0.731361706037, name = "113", squid = Nothing }
                                (Binary.Leaf { cladeId = -1, length = Just 4.63906336711, name = "Tellima_grandiflora", squid = Just "cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888" })
                                (Binary.Node { cladeId = 114, length = Just 0.54136061069, name = "114", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 4.09770275642, name = "Mitella_pentandra", squid = Just "eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12" })
                                    (Binary.Node { cladeId = 115, length = Just 2.89412508785, name = "115", squid = Nothing }
                                        (Binary.Node { cladeId = 116, length = Just 0.564868364663, name = "116", squid = Nothing }
                                            (Binary.Leaf { cladeId = -1, length = Just 0.638709303912, name = "Mitella_stylosa", squid = Just "557ccfded960e4ff692bb0bd82477149be49a7ce0a4b92f1488ba4ae50ef3fa1" })
                                            (Binary.Node { cladeId = 117, length = Just 0.294457536726, name = "117", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.344251767186, name = "Mitella_furusei", squid = Just "6655ecf0325609f0c196fe7b9fb6c74be2a5704bb6ffc31e9d1926ef9860e1be" }) (Binary.Leaf { cladeId = -1, length = Just 0.344251767186, name = "Mitella_pauciflora", squid = Just "1e8d5bfd8d2a4d26cb3750f0974fc1c94f4c74da77a330c17397b5d47a621d9e" }))
                                        )
                                        (Binary.Leaf { cladeId = -1, length = Just 1.20357766858, name = "Mitella_japonica", squid = Just "006429bb3a05e2ee6245ab9d7e0262bffd1432e9226c362d1d6e575e8aad8c0c" })
                                    )
                                )
                            )
                            (Binary.Node { cladeId = 118, length = Just 0.349786196394, name = "118", squid = Nothing }
                                (Binary.Node { cladeId = 119, length = Just 0.503874826255, name = "119", squid = Nothing }
                                    (Binary.Node { cladeId = 120, length = Just 2.97988329947, name = "120", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.53688075103, name = "Mitella_breweri", squid = Just "de203e04a3c569d028f8583157c051142e24376caa8ffe770152518ff27af4da" }) (Binary.Leaf { cladeId = -1, length = Just 1.53688075103, name = "Mitella_ovalis", squid = Just "5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be" }))
                                    (Binary.Leaf { cladeId = -1, length = Just 4.5167640505, name = "Bensoniella_oregona", squid = Just "e90cdd2736753a418d051642325264f4307047bfadcdc5b154d72e70e182f9a7" })
                                )
                                (Binary.Node { cladeId = 121, length = Just 0.5757118233, name = "121", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 4.44492705346, name = "Lithophragma_parviflorum", squid = Just "3437951f02c03c705bd14abd7f093684bb22bd0ed9f6ab74cf90827df1361a31" })
                                    (Binary.Node { cladeId = 122, length = Just 1.68272932833, name = "122", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 2.76219772512, name = "Mitella_nuda", squid = Just "eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f" }) (Binary.Leaf { cladeId = -1, length = Just 2.76219772512, name = "Mitella_diphylla", squid = Just "6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6" }))
                                )
                            )
                        )
                    )
                )
                (Binary.Node { cladeId = 123, length = Just 0.480455567573, name = "123", squid = Nothing }
                    (Binary.Leaf { cladeId = -1, length = Just 5.93681616118, name = "Tiarella_polyphylla", squid = Just "84fb5356b01f7010a952df74a8bf7d6f48ce357ed63697ce0ec7724008c8ed38" })
                    (Binary.Node { cladeId = 124, length = Just 1.42945087171, name = "124", squid = Nothing }
                        (Binary.Node { cladeId = 125, length = Just 1.2373578646, name = "125", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 3.27000742488, name = "Elmera_racemosa", squid = Just "113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b" }) (Binary.Leaf { cladeId = -1, length = Just 3.27000742488, name = "Mitella_caulescens", squid = Just "3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516" }))
                        (Binary.Node { cladeId = 126, length = Just 1.50181941431, name = "126", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 3.00554587516, name = "Conimitella_williamsii", squid = Just "5460b06e839e09674ce4d759175a3ad5b76d266ed4d7a104605ae3918f17e412" }) (Binary.Leaf { cladeId = -1, length = Just 3.00554587516, name = "Mitella_stauropetala", squid = Just "9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1" }))
                    )
                )
            )
        )
        (Binary.Leaf { cladeId = -1, length = Just 20.0613968519, name = "Telesonix_jamesii", squid = Just "fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692" })


taxLabelsText : String
taxLabelsText =
    """TAXLABELS
        Peltoboykinia_watanabei
        Heuchera_richardsonii
[&squid=38812fa5d76005b182799ffa21780abe6e1d1008504765ce359682e891ecf6d7]
        Heuchera_caroliniana
[&squid=fa8cbaa3c4d0df586b295964573ecf06a1907f06af6184f1c1719cb785ab8101]
        Heuchera_pubescens
[&squid=56d66eaf3474c32e3ed02c6f2a2fd1a78392fbf3d5c9be2e3e551ecb69232d45]
        Heuchera_alba
[&squid=cc8bbb6758a31f6faf88dddb82426e23ab32daa2a12840250204edae8b63616a]
        Heuchera_longiflora
[&squid=04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9]
        Heuchera_americana
[&squid=92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e]
        Heuchera_parvifolia
[&squid=266f28def683bf788e174a02789e9bd5bf0f3466a96ae748fd3c0315bd1fa4ec]
        Heuchera_wootonii
[&squid=4dca0a5d7515a0ff07371d9a61ed6ee3f037bb07381a117a0d406eedefa2e421]
        Heuchera_inconstans
[&squid=84b8213adfb1e8777bf3df0d39bb808bae2f07f31eff0f886c7535197700d542]
        Heuchera_soltisii
[&squid=f9e19dcad2616458da2df7e524cf09108401f0ed1d7df97dc2533b17c05c45a1]
        Heuchera_novomexicana
[&squid=3037e536df9ecc0c181f7059df2cf4369ec36a331b943b677a1a9dd1098e0542]
        Heuchera_glomerulata
[&squid=adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98]
        Heuchera_eastwoodiae
[&squid=88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c]
        Heuchera_woodsiaphila
[&squid=b71d73ef9406056c1a523545adfddb40248f085ea6e84f6aca39194a2e597516]
        Heuchera_bracteata
[&squid=5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9]
        Heuchera_hallii
[&squid=9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe]
        Heuchera_mexicana
[&squid=0f2535ee120c326895347fb6421a943c1a7a3f255bbe33e689246a51763994f2]
        Heuchera_longipetala
[&squid=19b5c2ba6ca08b4027e75aa1d267c923da1d4348aca42c75af45c4f39c1c91cd]
        Heuchera_acutifolia
[&squid=d773b8cab5c0bbcc101697ed6f99fa1d0ac5c38b30590dfd52d8a232ccc40f2c]
        Heuchera_hirsutissima
[&squid=36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5]
        Heuchera_caespitosa
[&squid=16af2ce027f58cfd878cdbf33b5722fbdea17bc8fe3ddfcd49a358f8229e0629]
        Heuchera_abramsii
[&squid=683835815e83a910b4877de7c7f2ef1b2ba671d176d78425dde229c1c21c0b72]
        Heuchera_elegans
[&squid=c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104]
        Heuchera_parishii
[&squid=d4645ba5ab9f1fe267c915f27dcfa8f8a69cc61538889dacc874c0fb604593d2]
        Heuchera_brevistaminea
[&squid=a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651]
        Heuchera_rosendahlii
[&squid=14b24a9ee9bc2d482a68ab018015beac1b72c8398446419c5eb86c837a6c0e22]
        Heuchera_wellsiae
[&squid=696313d6cf37c5663432fa7b3bf4be09ef6881c0f454a43293162ba469d5c31e]
        Heuchera_sanguinea
[&squid=2dfeb84c36af48dcfac361fe3498e8d07a9a9415cc86d4e77e82f356adfe7d81]
        Heuchera_versicolor
[&squid=e3cfdec7d79241229e23aa5873d1cbe45495f1d3bd42a5719e909023c4dca5ce]
        Heuchera_pulchella
[&squid=8fd0068c4ab7cf36d9aeeb608e41d05e4fcc8f96a435e01b8450c059fa1e5362]
        Heuchera_rubescens
[&squid=ef1ab7833784d81ea21db11e3d0e9b0d9bedb743b3c97a82f5b45d4f979ef97c]
        Heuchera_merriamii
[&squid=6fe3c631902b97cbb48f7943774679dff5bd86d4c54e2c1b92c3c95ef2d1e056]
        Heuchera_grossulariifolia
[&squid=87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84]
        Heuchera_pilosissima
[&squid=e8d7add6ffc2f3bfc23623d340f2b358643ff961dafc8e73df6c7b8e3ef89b66]
        Heuchera_maxima
[&squid=47b697873b763836ae7c3e2f43bac664d050bb88282efe93c94c60c8ab1dbeed]
        Heuchera_micrantha
[&squid=190e31374a4538d19563aa9c985c90991959c02ecd2bcbe471cddad76a27c399]
        Heuchera_glabra
[&squid=cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10]
        Heuchera_villosa
[&squid=e8f27ed1d833be70dd71fe6da8c2ad629db1822919eed00562c3e52535e987ec]
        Heuchera_puberula
[&squid=de5aaf6d3c94b6b76b67805a26ece3448e3ed3ff82a2c79fc9de8da577e35c28]
        Heuchera_missouriensis
[&squid=0fcc9b4ca9d87f9ebab1ef56b6ad4f60fd53a54374ae916143f9ad14d0952002]
        Heuchera_parviflora
[&squid=fc44011d8599702797e95e89427d7bc3b153d6858ce1e51c188513e92e313fcc]
        Heuchera_chlorantha
[&squid=277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10]
        Heuchera_cylindrica
[&squid=31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33]
        menziesii
[&squid=d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c]
        diplomenziesii
[&squid=5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a]
        Tellima_grandiflora
[&squid=cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888]
        Mitella_pentandra
[&squid=eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12]
        Mitella_stylosa
[&squid=557ccfded960e4ff692bb0bd82477149be49a7ce0a4b92f1488ba4ae50ef3fa1]
        Mitella_furusei
[&squid=6655ecf0325609f0c196fe7b9fb6c74be2a5704bb6ffc31e9d1926ef9860e1be]
        Mitella_pauciflora
[&squid=1e8d5bfd8d2a4d26cb3750f0974fc1c94f4c74da77a330c17397b5d47a621d9e]
        Mitella_japonica
[&squid=006429bb3a05e2ee6245ab9d7e0262bffd1432e9226c362d1d6e575e8aad8c0c]
        Mitella_breweri
[&squid=de203e04a3c569d028f8583157c051142e24376caa8ffe770152518ff27af4da]
        Mitella_ovalis
[&squid=5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be]
        Bensoniella_oregona
[&squid=e90cdd2736753a418d051642325264f4307047bfadcdc5b154d72e70e182f9a7]
        Lithophragma_parviflorum
[&squid=3437951f02c03c705bd14abd7f093684bb22bd0ed9f6ab74cf90827df1361a31]
        Mitella_nuda
[&squid=eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f]
        Mitella_diphylla
[&squid=6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6]
        Tiarella_polyphylla
[&squid=84fb5356b01f7010a952df74a8bf7d6f48ce357ed63697ce0ec7724008c8ed38]
        Elmera_racemosa
[&squid=113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b]
        Mitella_caulescens
[&squid=3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516]
        Conimitella_williamsii
[&squid=5460b06e839e09674ce4d759175a3ad5b76d266ed4d7a104605ae3918f17e412]
        Mitella_stauropetala
[&squid=9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1]
        Telesonix_jamesii
[&squid=fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692];"""


taxLabelsParsed : List TaxLabel
taxLabelsParsed =
    [ { name = "Peltoboykinia_watanabei", squid = Nothing }
    , { name = "Heuchera_richardsonii", squid = Just "38812fa5d76005b182799ffa21780abe6e1d1008504765ce359682e891ecf6d7" }
    , { name = "Heuchera_caroliniana", squid = Just "fa8cbaa3c4d0df586b295964573ecf06a1907f06af6184f1c1719cb785ab8101" }
    , { name = "Heuchera_pubescens", squid = Just "56d66eaf3474c32e3ed02c6f2a2fd1a78392fbf3d5c9be2e3e551ecb69232d45" }
    , { name = "Heuchera_alba", squid = Just "cc8bbb6758a31f6faf88dddb82426e23ab32daa2a12840250204edae8b63616a" }
    , { name = "Heuchera_longiflora", squid = Just "04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9" }
    , { name = "Heuchera_americana", squid = Just "92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e" }
    , { name = "Heuchera_parvifolia", squid = Just "266f28def683bf788e174a02789e9bd5bf0f3466a96ae748fd3c0315bd1fa4ec" }
    , { name = "Heuchera_wootonii", squid = Just "4dca0a5d7515a0ff07371d9a61ed6ee3f037bb07381a117a0d406eedefa2e421" }
    , { name = "Heuchera_inconstans", squid = Just "84b8213adfb1e8777bf3df0d39bb808bae2f07f31eff0f886c7535197700d542" }
    , { name = "Heuchera_soltisii", squid = Just "f9e19dcad2616458da2df7e524cf09108401f0ed1d7df97dc2533b17c05c45a1" }
    , { name = "Heuchera_novomexicana", squid = Just "3037e536df9ecc0c181f7059df2cf4369ec36a331b943b677a1a9dd1098e0542" }
    , { name = "Heuchera_glomerulata", squid = Just "adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98" }
    , { name = "Heuchera_eastwoodiae", squid = Just "88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c" }
    , { name = "Heuchera_woodsiaphila", squid = Just "b71d73ef9406056c1a523545adfddb40248f085ea6e84f6aca39194a2e597516" }
    , { name = "Heuchera_bracteata", squid = Just "5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9" }
    , { name = "Heuchera_hallii", squid = Just "9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe" }
    , { name = "Heuchera_mexicana", squid = Just "0f2535ee120c326895347fb6421a943c1a7a3f255bbe33e689246a51763994f2" }
    , { name = "Heuchera_longipetala", squid = Just "19b5c2ba6ca08b4027e75aa1d267c923da1d4348aca42c75af45c4f39c1c91cd" }
    , { name = "Heuchera_acutifolia", squid = Just "d773b8cab5c0bbcc101697ed6f99fa1d0ac5c38b30590dfd52d8a232ccc40f2c" }
    , { name = "Heuchera_hirsutissima", squid = Just "36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5" }
    , { name = "Heuchera_caespitosa", squid = Just "16af2ce027f58cfd878cdbf33b5722fbdea17bc8fe3ddfcd49a358f8229e0629" }
    , { name = "Heuchera_abramsii", squid = Just "683835815e83a910b4877de7c7f2ef1b2ba671d176d78425dde229c1c21c0b72" }
    , { name = "Heuchera_elegans", squid = Just "c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104" }
    , { name = "Heuchera_parishii", squid = Just "d4645ba5ab9f1fe267c915f27dcfa8f8a69cc61538889dacc874c0fb604593d2" }
    , { name = "Heuchera_brevistaminea", squid = Just "a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651" }
    , { name = "Heuchera_rosendahlii", squid = Just "14b24a9ee9bc2d482a68ab018015beac1b72c8398446419c5eb86c837a6c0e22" }
    , { name = "Heuchera_wellsiae", squid = Just "696313d6cf37c5663432fa7b3bf4be09ef6881c0f454a43293162ba469d5c31e" }
    , { name = "Heuchera_sanguinea", squid = Just "2dfeb84c36af48dcfac361fe3498e8d07a9a9415cc86d4e77e82f356adfe7d81" }
    , { name = "Heuchera_versicolor", squid = Just "e3cfdec7d79241229e23aa5873d1cbe45495f1d3bd42a5719e909023c4dca5ce" }
    , { name = "Heuchera_pulchella", squid = Just "8fd0068c4ab7cf36d9aeeb608e41d05e4fcc8f96a435e01b8450c059fa1e5362" }
    , { name = "Heuchera_rubescens", squid = Just "ef1ab7833784d81ea21db11e3d0e9b0d9bedb743b3c97a82f5b45d4f979ef97c" }
    , { name = "Heuchera_merriamii", squid = Just "6fe3c631902b97cbb48f7943774679dff5bd86d4c54e2c1b92c3c95ef2d1e056" }
    , { name = "Heuchera_grossulariifolia", squid = Just "87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84" }
    , { name = "Heuchera_pilosissima", squid = Just "e8d7add6ffc2f3bfc23623d340f2b358643ff961dafc8e73df6c7b8e3ef89b66" }
    , { name = "Heuchera_maxima", squid = Just "47b697873b763836ae7c3e2f43bac664d050bb88282efe93c94c60c8ab1dbeed" }
    , { name = "Heuchera_micrantha", squid = Just "190e31374a4538d19563aa9c985c90991959c02ecd2bcbe471cddad76a27c399" }
    , { name = "Heuchera_glabra", squid = Just "cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10" }
    , { name = "Heuchera_villosa", squid = Just "e8f27ed1d833be70dd71fe6da8c2ad629db1822919eed00562c3e52535e987ec" }
    , { name = "Heuchera_puberula", squid = Just "de5aaf6d3c94b6b76b67805a26ece3448e3ed3ff82a2c79fc9de8da577e35c28" }
    , { name = "Heuchera_missouriensis", squid = Just "0fcc9b4ca9d87f9ebab1ef56b6ad4f60fd53a54374ae916143f9ad14d0952002" }
    , { name = "Heuchera_parviflora", squid = Just "fc44011d8599702797e95e89427d7bc3b153d6858ce1e51c188513e92e313fcc" }
    , { name = "Heuchera_chlorantha", squid = Just "277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10" }
    , { name = "Heuchera_cylindrica", squid = Just "31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33" }
    , { name = "menziesii", squid = Just "d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c" }
    , { name = "diplomenziesii", squid = Just "5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a" }
    , { name = "Tellima_grandiflora", squid = Just "cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888" }
    , { name = "Mitella_pentandra", squid = Just "eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12" }
    , { name = "Mitella_stylosa", squid = Just "557ccfded960e4ff692bb0bd82477149be49a7ce0a4b92f1488ba4ae50ef3fa1" }
    , { name = "Mitella_furusei", squid = Just "6655ecf0325609f0c196fe7b9fb6c74be2a5704bb6ffc31e9d1926ef9860e1be" }
    , { name = "Mitella_pauciflora", squid = Just "1e8d5bfd8d2a4d26cb3750f0974fc1c94f4c74da77a330c17397b5d47a621d9e" }
    , { name = "Mitella_japonica", squid = Just "006429bb3a05e2ee6245ab9d7e0262bffd1432e9226c362d1d6e575e8aad8c0c" }
    , { name = "Mitella_breweri", squid = Just "de203e04a3c569d028f8583157c051142e24376caa8ffe770152518ff27af4da" }
    , { name = "Mitella_ovalis", squid = Just "5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be" }
    , { name = "Bensoniella_oregona", squid = Just "e90cdd2736753a418d051642325264f4307047bfadcdc5b154d72e70e182f9a7" }
    , { name = "Lithophragma_parviflorum", squid = Just "3437951f02c03c705bd14abd7f093684bb22bd0ed9f6ab74cf90827df1361a31" }
    , { name = "Mitella_nuda", squid = Just "eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f" }
    , { name = "Mitella_diphylla", squid = Just "6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6" }
    , { name = "Tiarella_polyphylla", squid = Just "84fb5356b01f7010a952df74a8bf7d6f48ce357ed63697ce0ec7724008c8ed38" }
    , { name = "Elmera_racemosa", squid = Just "113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b" }
    , { name = "Mitella_caulescens", squid = Just "3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516" }
    , { name = "Conimitella_williamsii", squid = Just "5460b06e839e09674ce4d759175a3ad5b76d266ed4d7a104605ae3918f17e412" }
    , { name = "Mitella_stauropetala", squid = Just "9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1" }
    , { name = "Telesonix_jamesii", squid = Just "fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692" }
    ]


heucheraNexus =
    """#NEXUS

BEGIN TAXA;
    DIMENSIONS NTAX=64;
    TAXLABELS
        Peltoboykinia_watanabei
[&squid=5c669168e6cd2b339d47db9ca8a1743280baeedc4a738664f8a531d48661d5c9]
        Heuchera_richardsonii
[&squid=38812fa5d76005b182799ffa21780abe6e1d1008504765ce359682e891ecf6d7]
        Heuchera_caroliniana
[&squid=fa8cbaa3c4d0df586b295964573ecf06a1907f06af6184f1c1719cb785ab8101]
        Heuchera_pubescens
[&squid=56d66eaf3474c32e3ed02c6f2a2fd1a78392fbf3d5c9be2e3e551ecb69232d45]
        Heuchera_alba
[&squid=cc8bbb6758a31f6faf88dddb82426e23ab32daa2a12840250204edae8b63616a]
        Heuchera_longiflora
[&squid=04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9]
        Heuchera_americana
[&squid=92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e]
        Heuchera_parvifolia
[&squid=266f28def683bf788e174a02789e9bd5bf0f3466a96ae748fd3c0315bd1fa4ec]
        Heuchera_wootonii
[&squid=4dca0a5d7515a0ff07371d9a61ed6ee3f037bb07381a117a0d406eedefa2e421]
        Heuchera_inconstans
[&squid=84b8213adfb1e8777bf3df0d39bb808bae2f07f31eff0f886c7535197700d542]
        Heuchera_soltisii
[&squid=f9e19dcad2616458da2df7e524cf09108401f0ed1d7df97dc2533b17c05c45a1]
        Heuchera_novomexicana
[&squid=3037e536df9ecc0c181f7059df2cf4369ec36a331b943b677a1a9dd1098e0542]
        Heuchera_glomerulata
[&squid=adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98]
        Heuchera_eastwoodiae
[&squid=88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c]
        Heuchera_woodsiaphila
[&squid=b71d73ef9406056c1a523545adfddb40248f085ea6e84f6aca39194a2e597516]
        Heuchera_bracteata
[&squid=5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9]
        Heuchera_hallii
[&squid=9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe]
        Heuchera_mexicana
[&squid=0f2535ee120c326895347fb6421a943c1a7a3f255bbe33e689246a51763994f2]
        Heuchera_longipetala
[&squid=19b5c2ba6ca08b4027e75aa1d267c923da1d4348aca42c75af45c4f39c1c91cd]
        Heuchera_acutifolia
[&squid=d773b8cab5c0bbcc101697ed6f99fa1d0ac5c38b30590dfd52d8a232ccc40f2c]
        Heuchera_hirsutissima
[&squid=36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5]
        Heuchera_caespitosa
[&squid=16af2ce027f58cfd878cdbf33b5722fbdea17bc8fe3ddfcd49a358f8229e0629]
        Heuchera_abramsii
[&squid=683835815e83a910b4877de7c7f2ef1b2ba671d176d78425dde229c1c21c0b72]
        Heuchera_elegans
[&squid=c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104]
        Heuchera_parishii
[&squid=d4645ba5ab9f1fe267c915f27dcfa8f8a69cc61538889dacc874c0fb604593d2]
        Heuchera_brevistaminea
[&squid=a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651]
        Heuchera_rosendahlii
[&squid=14b24a9ee9bc2d482a68ab018015beac1b72c8398446419c5eb86c837a6c0e22]
        Heuchera_wellsiae
[&squid=696313d6cf37c5663432fa7b3bf4be09ef6881c0f454a43293162ba469d5c31e]
        Heuchera_sanguinea
[&squid=2dfeb84c36af48dcfac361fe3498e8d07a9a9415cc86d4e77e82f356adfe7d81]
        Heuchera_versicolor
[&squid=e3cfdec7d79241229e23aa5873d1cbe45495f1d3bd42a5719e909023c4dca5ce]
        Heuchera_pulchella
[&squid=8fd0068c4ab7cf36d9aeeb608e41d05e4fcc8f96a435e01b8450c059fa1e5362]
        Heuchera_rubescens
[&squid=ef1ab7833784d81ea21db11e3d0e9b0d9bedb743b3c97a82f5b45d4f979ef97c]
        Heuchera_merriamii
[&squid=6fe3c631902b97cbb48f7943774679dff5bd86d4c54e2c1b92c3c95ef2d1e056]
        Heuchera_grossulariifolia
[&squid=87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84]
        Heuchera_pilosissima
[&squid=e8d7add6ffc2f3bfc23623d340f2b358643ff961dafc8e73df6c7b8e3ef89b66]
        Heuchera_maxima
[&squid=47b697873b763836ae7c3e2f43bac664d050bb88282efe93c94c60c8ab1dbeed]
        Heuchera_micrantha
[&squid=190e31374a4538d19563aa9c985c90991959c02ecd2bcbe471cddad76a27c399]
        Heuchera_glabra
[&squid=cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10]
        Heuchera_villosa
[&squid=e8f27ed1d833be70dd71fe6da8c2ad629db1822919eed00562c3e52535e987ec]
        Heuchera_puberula
[&squid=de5aaf6d3c94b6b76b67805a26ece3448e3ed3ff82a2c79fc9de8da577e35c28]
        Heuchera_missouriensis
[&squid=0fcc9b4ca9d87f9ebab1ef56b6ad4f60fd53a54374ae916143f9ad14d0952002]
        Heuchera_parviflora
[&squid=fc44011d8599702797e95e89427d7bc3b153d6858ce1e51c188513e92e313fcc]
        Heuchera_chlorantha
[&squid=277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10]
        Heuchera_cylindrica
[&squid=31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33]
        menziesii
[&squid=d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c]
        diplomenziesii
[&squid=5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a]
        Tellima_grandiflora
[&squid=cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888]
        Mitella_pentandra
[&squid=eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12]
        Mitella_stylosa
[&squid=557ccfded960e4ff692bb0bd82477149be49a7ce0a4b92f1488ba4ae50ef3fa1]
        Mitella_furusei
[&squid=6655ecf0325609f0c196fe7b9fb6c74be2a5704bb6ffc31e9d1926ef9860e1be]
        Mitella_pauciflora
[&squid=1e8d5bfd8d2a4d26cb3750f0974fc1c94f4c74da77a330c17397b5d47a621d9e]
        Mitella_japonica
[&squid=006429bb3a05e2ee6245ab9d7e0262bffd1432e9226c362d1d6e575e8aad8c0c]
        Mitella_breweri
[&squid=de203e04a3c569d028f8583157c051142e24376caa8ffe770152518ff27af4da]
        Mitella_ovalis
[&squid=5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be]
        Bensoniella_oregona
[&squid=e90cdd2736753a418d051642325264f4307047bfadcdc5b154d72e70e182f9a7]
        Lithophragma_parviflorum
[&squid=3437951f02c03c705bd14abd7f093684bb22bd0ed9f6ab74cf90827df1361a31]
        Mitella_nuda
[&squid=eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f]
        Mitella_diphylla
[&squid=6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6]
        Tiarella_polyphylla
[&squid=84fb5356b01f7010a952df74a8bf7d6f48ce357ed63697ce0ec7724008c8ed38]
        Elmera_racemosa
[&squid=113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b]
        Mitella_caulescens
[&squid=3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516]
        Conimitella_williamsii
[&squid=5460b06e839e09674ce4d759175a3ad5b76d266ed4d7a104605ae3918f17e412]
        Mitella_stauropetala
[&squid=9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1]
        Telesonix_jamesii
[&squid=fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692]
  ;
END;

BEGIN TREES;
    TREE 1 = ((Peltoboykinia_watanabei:17.7616558596,((((((Heuchera_richardsonii:1.59667580986,(Heuchera_caroliniana:1.25045275727,((Heuchera_pubescens:0.6012456036,Heuchera_alba:0.6012456036)74:0.307630228026,(Heuchera_longiflora:0.678578784091,Heuchera_americana:0.678578784091)75:0.230297047535)73:0.341576925645)72:0.346223052593)71:0.455037436176,(Heuchera_parvifolia:1.57983930474,(Heuchera_wootonii:1.03733670369,(Heuchera_inconstans:0.806678945231,(((Heuchera_soltisii:0.368017442136,Heuchera_novomexicana:0.368017442136)81:0.113213409433,Heuchera_glomerulata:0.481230851569)80:0.200196385551,Heuchera_eastwoodiae:0.681427237119)79:0.125251708112)78:0.23065775846)77:0.542502601047)76:0.471873941302)70:1.17677859772,((Heuchera_woodsiaphila:2.26765108241,((Heuchera_bracteata:1.11223623101,Heuchera_hallii:1.11223623101)85:0.799972223411,(((Heuchera_mexicana:1.00985960274,(Heuchera_longipetala:0.672702201725,Heuchera_acutifolia:0.672702201725)89:0.337157401018)88:0.452796325447,(((Heuchera_hirsutissima:0.908985338716,((Heuchera_caespitosa:0.492761287085,Heuchera_abramsii:0.492761287085)94:0.217512119899,(Heuchera_elegans:0.570276420827,Heuchera_parishii:0.570276420827)95:0.139996986157)93:0.198711931733)92:0.152249046392,Heuchera_brevistaminea:1.06123438511)91:0.23571097096,(((Heuchera_rosendahlii:0.436237554309,Heuchera_wellsiae:0.436237554309)98:0.481744506695,Heuchera_sanguinea:0.917982061004)97:0.18098598279,Heuchera_versicolor:1.09896804379)96:0.197977312275)90:0.165710572121)87:0.211113420755,(Heuchera_pulchella:1.19380805918,Heuchera_rubescens:1.19380805918)99:0.479961289764)86:0.238439105472)84:0.355442627994)83:0.472005667159,(Heuchera_merriamii:2.38905826701,Heuchera_grossulariifolia:2.38905826701)100:0.350598482559)82:0.48883509419)69:0.61160272986,(((Heuchera_pilosissima:0.47153892037,Heuchera_maxima:0.47153892037)103:0.554782803323,Heuchera_micrantha:1.02632172369)102:2.51457414479,(Heuchera_glabra:3.22414919189,(((Heuchera_villosa:0.981506873297,Heuchera_puberula:0.981506873297)107:0.507115394285,(Heuchera_missouriensis:0.942214782098,Heuchera_parviflora:0.942214782098)108:0.546407485484)106:1.18263380777,(Heuchera_chlorantha:1.74505145726,Heuchera_cylindrica:1.74505145726)109:0.926204618094)105:0.552893116534)104:0.316746676598)101:0.299198705132)68:2.27176201455,((menziesii:1.84479470104,diplomenziesii:1.84479470104)111:3.90021276384,((Tellima_grandiflora:4.63906336711,(Mitella_pentandra:4.09770275642,((Mitella_stylosa:0.638709303912,(Mitella_furusei:0.344251767186,Mitella_pauciflora:0.344251767186)117:0.294457536726)116:0.564868364663,Mitella_japonica:1.20357766858)115:2.89412508785)114:0.54136061069)113:0.731361706037,(((Mitella_breweri:1.53688075103,Mitella_ovalis:1.53688075103)120:2.97988329947,Bensoniella_oregona:4.5167640505)119:0.503874826255,(Lithophragma_parviflorum:4.44492705346,(Mitella_nuda:2.76219772512,Mitella_diphylla:2.76219772512)122:1.68272932833)121:0.5757118233)118:0.349786196394)112:0.374582391731)110:0.366849123291)67:0.305415140583,(Tiarella_polyphylla:5.93681616118,((Elmera_racemosa:3.27000742488,Mitella_caulescens:3.27000742488)125:1.2373578646,(Conimitella_williamsii:3.00554587516,Mitella_stauropetala:3.00554587516)126:1.50181941431)124:1.42945087171)123:0.480455567573)66:11.3443841308)65:2.29974099235,Telesonix_jamesii:20.0613968519)64;
END;
     """


heucheraNexusParsed =
    Binary.Node { cladeId = 64, length = Just 0, name = "64", squid = Nothing }
        (Binary.Node { cladeId = 65, length = Just 2.29974099235, name = "65", squid = Nothing }
            (Binary.Leaf { cladeId = -1, length = Just 17.7616558596, name = "Peltoboykinia_watanabei", squid = Just "5c669168e6cd2b339d47db9ca8a1743280baeedc4a738664f8a531d48661d5c9" })
            (Binary.Node { cladeId = 66, length = Just 11.3443841308, name = "66", squid = Nothing }
                (Binary.Node { cladeId = 67, length = Just 0.305415140583, name = "67", squid = Nothing }
                    (Binary.Node { cladeId = 68, length = Just 2.27176201455, name = "68", squid = Nothing }
                        (Binary.Node { cladeId = 69, length = Just 0.61160272986, name = "69", squid = Nothing }
                            (Binary.Node { cladeId = 70, length = Just 1.17677859772, name = "70", squid = Nothing }
                                (Binary.Node { cladeId = 71, length = Just 0.455037436176, name = "71", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 1.59667580986, name = "Heuchera_richardsonii", squid = Just "38812fa5d76005b182799ffa21780abe6e1d1008504765ce359682e891ecf6d7" })
                                    (Binary.Node { cladeId = 72, length = Just 0.346223052593, name = "72", squid = Nothing }
                                        (Binary.Leaf { cladeId = -1, length = Just 1.25045275727, name = "Heuchera_caroliniana", squid = Just "fa8cbaa3c4d0df586b295964573ecf06a1907f06af6184f1c1719cb785ab8101" })
                                        (Binary.Node { cladeId = 73, length = Just 0.341576925645, name = "73", squid = Nothing }
                                            (Binary.Node { cladeId = 74, length = Just 0.307630228026, name = "74", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.6012456036, name = "Heuchera_pubescens", squid = Just "56d66eaf3474c32e3ed02c6f2a2fd1a78392fbf3d5c9be2e3e551ecb69232d45" }) (Binary.Leaf { cladeId = -1, length = Just 0.6012456036, name = "Heuchera_alba", squid = Just "cc8bbb6758a31f6faf88dddb82426e23ab32daa2a12840250204edae8b63616a" }))
                                            (Binary.Node { cladeId = 75, length = Just 0.230297047535, name = "75", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.678578784091, name = "Heuchera_longiflora", squid = Just "04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9" }) (Binary.Leaf { cladeId = -1, length = Just 0.678578784091, name = "Heuchera_americana", squid = Just "92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e" }))
                                        )
                                    )
                                )
                                (Binary.Node { cladeId = 76, length = Just 0.471873941302, name = "76", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 1.57983930474, name = "Heuchera_parvifolia", squid = Just "266f28def683bf788e174a02789e9bd5bf0f3466a96ae748fd3c0315bd1fa4ec" })
                                    (Binary.Node { cladeId = 77, length = Just 0.542502601047, name = "77", squid = Nothing }
                                        (Binary.Leaf { cladeId = -1, length = Just 1.03733670369, name = "Heuchera_wootonii", squid = Just "4dca0a5d7515a0ff07371d9a61ed6ee3f037bb07381a117a0d406eedefa2e421" })
                                        (Binary.Node { cladeId = 78, length = Just 0.23065775846, name = "78", squid = Nothing }
                                            (Binary.Leaf { cladeId = -1, length = Just 0.806678945231, name = "Heuchera_inconstans", squid = Just "84b8213adfb1e8777bf3df0d39bb808bae2f07f31eff0f886c7535197700d542" })
                                            (Binary.Node { cladeId = 79, length = Just 0.125251708112, name = "79", squid = Nothing }
                                                (Binary.Node { cladeId = 80, length = Just 0.200196385551, name = "80", squid = Nothing }
                                                    (Binary.Node { cladeId = 81, length = Just 0.113213409433, name = "81", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.368017442136, name = "Heuchera_soltisii", squid = Just "f9e19dcad2616458da2df7e524cf09108401f0ed1d7df97dc2533b17c05c45a1" }) (Binary.Leaf { cladeId = -1, length = Just 0.368017442136, name = "Heuchera_novomexicana", squid = Just "3037e536df9ecc0c181f7059df2cf4369ec36a331b943b677a1a9dd1098e0542" }))
                                                    (Binary.Leaf { cladeId = -1, length = Just 0.481230851569, name = "Heuchera_glomerulata", squid = Just "adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98" })
                                                )
                                                (Binary.Leaf { cladeId = -1, length = Just 0.681427237119, name = "Heuchera_eastwoodiae", squid = Just "88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c" })
                                            )
                                        )
                                    )
                                )
                            )
                            (Binary.Node { cladeId = 82, length = Just 0.48883509419, name = "82", squid = Nothing }
                                (Binary.Node { cladeId = 83, length = Just 0.472005667159, name = "83", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 2.26765108241, name = "Heuchera_woodsiaphila", squid = Just "b71d73ef9406056c1a523545adfddb40248f085ea6e84f6aca39194a2e597516" })
                                    (Binary.Node { cladeId = 84, length = Just 0.355442627994, name = "84", squid = Nothing }
                                        (Binary.Node { cladeId = 85, length = Just 0.799972223411, name = "85", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.11223623101, name = "Heuchera_bracteata", squid = Just "5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9" }) (Binary.Leaf { cladeId = -1, length = Just 1.11223623101, name = "Heuchera_hallii", squid = Just "9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe" }))
                                        (Binary.Node { cladeId = 86, length = Just 0.238439105472, name = "86", squid = Nothing }
                                            (Binary.Node { cladeId = 87, length = Just 0.211113420755, name = "87", squid = Nothing }
                                                (Binary.Node { cladeId = 88, length = Just 0.452796325447, name = "88", squid = Nothing }
                                                    (Binary.Leaf { cladeId = -1, length = Just 1.00985960274, name = "Heuchera_mexicana", squid = Just "0f2535ee120c326895347fb6421a943c1a7a3f255bbe33e689246a51763994f2" })
                                                    (Binary.Node { cladeId = 89, length = Just 0.337157401018, name = "89", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.672702201725, name = "Heuchera_longipetala", squid = Just "19b5c2ba6ca08b4027e75aa1d267c923da1d4348aca42c75af45c4f39c1c91cd" }) (Binary.Leaf { cladeId = -1, length = Just 0.672702201725, name = "Heuchera_acutifolia", squid = Just "d773b8cab5c0bbcc101697ed6f99fa1d0ac5c38b30590dfd52d8a232ccc40f2c" }))
                                                )
                                                (Binary.Node { cladeId = 90, length = Just 0.165710572121, name = "90", squid = Nothing }
                                                    (Binary.Node { cladeId = 91, length = Just 0.23571097096, name = "91", squid = Nothing }
                                                        (Binary.Node { cladeId = 92, length = Just 0.152249046392, name = "92", squid = Nothing }
                                                            (Binary.Leaf { cladeId = -1, length = Just 0.908985338716, name = "Heuchera_hirsutissima", squid = Just "36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5" })
                                                            (Binary.Node { cladeId = 93, length = Just 0.198711931733, name = "93", squid = Nothing }
                                                                (Binary.Node { cladeId = 94, length = Just 0.217512119899, name = "94", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.492761287085, name = "Heuchera_caespitosa", squid = Just "16af2ce027f58cfd878cdbf33b5722fbdea17bc8fe3ddfcd49a358f8229e0629" }) (Binary.Leaf { cladeId = -1, length = Just 0.492761287085, name = "Heuchera_abramsii", squid = Just "683835815e83a910b4877de7c7f2ef1b2ba671d176d78425dde229c1c21c0b72" }))
                                                                (Binary.Node { cladeId = 95, length = Just 0.139996986157, name = "95", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.570276420827, name = "Heuchera_elegans", squid = Just "c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104" }) (Binary.Leaf { cladeId = -1, length = Just 0.570276420827, name = "Heuchera_parishii", squid = Just "d4645ba5ab9f1fe267c915f27dcfa8f8a69cc61538889dacc874c0fb604593d2" }))
                                                            )
                                                        )
                                                        (Binary.Leaf { cladeId = -1, length = Just 1.06123438511, name = "Heuchera_brevistaminea", squid = Just "a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651" })
                                                    )
                                                    (Binary.Node { cladeId = 96, length = Just 0.197977312275, name = "96", squid = Nothing }
                                                        (Binary.Node { cladeId = 97, length = Just 0.18098598279, name = "97", squid = Nothing }
                                                            (Binary.Node { cladeId = 98, length = Just 0.481744506695, name = "98", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.436237554309, name = "Heuchera_rosendahlii", squid = Just "14b24a9ee9bc2d482a68ab018015beac1b72c8398446419c5eb86c837a6c0e22" }) (Binary.Leaf { cladeId = -1, length = Just 0.436237554309, name = "Heuchera_wellsiae", squid = Just "696313d6cf37c5663432fa7b3bf4be09ef6881c0f454a43293162ba469d5c31e" }))
                                                            (Binary.Leaf { cladeId = -1, length = Just 0.917982061004, name = "Heuchera_sanguinea", squid = Just "2dfeb84c36af48dcfac361fe3498e8d07a9a9415cc86d4e77e82f356adfe7d81" })
                                                        )
                                                        (Binary.Leaf { cladeId = -1, length = Just 1.09896804379, name = "Heuchera_versicolor", squid = Just "e3cfdec7d79241229e23aa5873d1cbe45495f1d3bd42a5719e909023c4dca5ce" })
                                                    )
                                                )
                                            )
                                            (Binary.Node { cladeId = 99, length = Just 0.479961289764, name = "99", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.19380805918, name = "Heuchera_pulchella", squid = Just "8fd0068c4ab7cf36d9aeeb608e41d05e4fcc8f96a435e01b8450c059fa1e5362" }) (Binary.Leaf { cladeId = -1, length = Just 1.19380805918, name = "Heuchera_rubescens", squid = Just "ef1ab7833784d81ea21db11e3d0e9b0d9bedb743b3c97a82f5b45d4f979ef97c" }))
                                        )
                                    )
                                )
                                (Binary.Node { cladeId = 100, length = Just 0.350598482559, name = "100", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 2.38905826701, name = "Heuchera_merriamii", squid = Just "6fe3c631902b97cbb48f7943774679dff5bd86d4c54e2c1b92c3c95ef2d1e056" }) (Binary.Leaf { cladeId = -1, length = Just 2.38905826701, name = "Heuchera_grossulariifolia", squid = Just "87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84" }))
                            )
                        )
                        (Binary.Node { cladeId = 101, length = Just 0.299198705132, name = "101", squid = Nothing }
                            (Binary.Node { cladeId = 102, length = Just 2.51457414479, name = "102", squid = Nothing }
                                (Binary.Node { cladeId = 103, length = Just 0.554782803323, name = "103", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.47153892037, name = "Heuchera_pilosissima", squid = Just "e8d7add6ffc2f3bfc23623d340f2b358643ff961dafc8e73df6c7b8e3ef89b66" }) (Binary.Leaf { cladeId = -1, length = Just 0.47153892037, name = "Heuchera_maxima", squid = Just "47b697873b763836ae7c3e2f43bac664d050bb88282efe93c94c60c8ab1dbeed" }))
                                (Binary.Leaf { cladeId = -1, length = Just 1.02632172369, name = "Heuchera_micrantha", squid = Just "190e31374a4538d19563aa9c985c90991959c02ecd2bcbe471cddad76a27c399" })
                            )
                            (Binary.Node { cladeId = 104, length = Just 0.316746676598, name = "104", squid = Nothing }
                                (Binary.Leaf { cladeId = -1, length = Just 3.22414919189, name = "Heuchera_glabra", squid = Just "cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10" })
                                (Binary.Node { cladeId = 105, length = Just 0.552893116534, name = "105", squid = Nothing }
                                    (Binary.Node { cladeId = 106, length = Just 1.18263380777, name = "106", squid = Nothing }
                                        (Binary.Node { cladeId = 107, length = Just 0.507115394285, name = "107", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.981506873297, name = "Heuchera_villosa", squid = Just "e8f27ed1d833be70dd71fe6da8c2ad629db1822919eed00562c3e52535e987ec" }) (Binary.Leaf { cladeId = -1, length = Just 0.981506873297, name = "Heuchera_puberula", squid = Just "de5aaf6d3c94b6b76b67805a26ece3448e3ed3ff82a2c79fc9de8da577e35c28" }))
                                        (Binary.Node { cladeId = 108, length = Just 0.546407485484, name = "108", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.942214782098, name = "Heuchera_missouriensis", squid = Just "0fcc9b4ca9d87f9ebab1ef56b6ad4f60fd53a54374ae916143f9ad14d0952002" }) (Binary.Leaf { cladeId = -1, length = Just 0.942214782098, name = "Heuchera_parviflora", squid = Just "fc44011d8599702797e95e89427d7bc3b153d6858ce1e51c188513e92e313fcc" }))
                                    )
                                    (Binary.Node { cladeId = 109, length = Just 0.926204618094, name = "109", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.74505145726, name = "Heuchera_chlorantha", squid = Just "277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10" }) (Binary.Leaf { cladeId = -1, length = Just 1.74505145726, name = "Heuchera_cylindrica", squid = Just "31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33" }))
                                )
                            )
                        )
                    )
                    (Binary.Node { cladeId = 110, length = Just 0.366849123291, name = "110", squid = Nothing }
                        (Binary.Node { cladeId = 111, length = Just 3.90021276384, name = "111", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.84479470104, name = "menziesii", squid = Just "d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c" }) (Binary.Leaf { cladeId = -1, length = Just 1.84479470104, name = "diplomenziesii", squid = Just "5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a" }))
                        (Binary.Node { cladeId = 112, length = Just 0.374582391731, name = "112", squid = Nothing }
                            (Binary.Node { cladeId = 113, length = Just 0.731361706037, name = "113", squid = Nothing }
                                (Binary.Leaf { cladeId = -1, length = Just 4.63906336711, name = "Tellima_grandiflora", squid = Just "cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888" })
                                (Binary.Node { cladeId = 114, length = Just 0.54136061069, name = "114", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 4.09770275642, name = "Mitella_pentandra", squid = Just "eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12" })
                                    (Binary.Node { cladeId = 115, length = Just 2.89412508785, name = "115", squid = Nothing }
                                        (Binary.Node { cladeId = 116, length = Just 0.564868364663, name = "116", squid = Nothing }
                                            (Binary.Leaf { cladeId = -1, length = Just 0.638709303912, name = "Mitella_stylosa", squid = Just "557ccfded960e4ff692bb0bd82477149be49a7ce0a4b92f1488ba4ae50ef3fa1" })
                                            (Binary.Node { cladeId = 117, length = Just 0.294457536726, name = "117", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 0.344251767186, name = "Mitella_furusei", squid = Just "6655ecf0325609f0c196fe7b9fb6c74be2a5704bb6ffc31e9d1926ef9860e1be" }) (Binary.Leaf { cladeId = -1, length = Just 0.344251767186, name = "Mitella_pauciflora", squid = Just "1e8d5bfd8d2a4d26cb3750f0974fc1c94f4c74da77a330c17397b5d47a621d9e" }))
                                        )
                                        (Binary.Leaf { cladeId = -1, length = Just 1.20357766858, name = "Mitella_japonica", squid = Just "006429bb3a05e2ee6245ab9d7e0262bffd1432e9226c362d1d6e575e8aad8c0c" })
                                    )
                                )
                            )
                            (Binary.Node { cladeId = 118, length = Just 0.349786196394, name = "118", squid = Nothing }
                                (Binary.Node { cladeId = 119, length = Just 0.503874826255, name = "119", squid = Nothing }
                                    (Binary.Node { cladeId = 120, length = Just 2.97988329947, name = "120", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 1.53688075103, name = "Mitella_breweri", squid = Just "de203e04a3c569d028f8583157c051142e24376caa8ffe770152518ff27af4da" }) (Binary.Leaf { cladeId = -1, length = Just 1.53688075103, name = "Mitella_ovalis", squid = Just "5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be" }))
                                    (Binary.Leaf { cladeId = -1, length = Just 4.5167640505, name = "Bensoniella_oregona", squid = Just "e90cdd2736753a418d051642325264f4307047bfadcdc5b154d72e70e182f9a7" })
                                )
                                (Binary.Node { cladeId = 121, length = Just 0.5757118233, name = "121", squid = Nothing }
                                    (Binary.Leaf { cladeId = -1, length = Just 4.44492705346, name = "Lithophragma_parviflorum", squid = Just "3437951f02c03c705bd14abd7f093684bb22bd0ed9f6ab74cf90827df1361a31" })
                                    (Binary.Node { cladeId = 122, length = Just 1.68272932833, name = "122", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 2.76219772512, name = "Mitella_nuda", squid = Just "eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f" }) (Binary.Leaf { cladeId = -1, length = Just 2.76219772512, name = "Mitella_diphylla", squid = Just "6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6" }))
                                )
                            )
                        )
                    )
                )
                (Binary.Node { cladeId = 123, length = Just 0.480455567573, name = "123", squid = Nothing }
                    (Binary.Leaf { cladeId = -1, length = Just 5.93681616118, name = "Tiarella_polyphylla", squid = Just "84fb5356b01f7010a952df74a8bf7d6f48ce357ed63697ce0ec7724008c8ed38" })
                    (Binary.Node { cladeId = 124, length = Just 1.42945087171, name = "124", squid = Nothing }
                        (Binary.Node { cladeId = 125, length = Just 1.2373578646, name = "125", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 3.27000742488, name = "Elmera_racemosa", squid = Just "113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b" }) (Binary.Leaf { cladeId = -1, length = Just 3.27000742488, name = "Mitella_caulescens", squid = Just "3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516" }))
                        (Binary.Node { cladeId = 126, length = Just 1.50181941431, name = "126", squid = Nothing } (Binary.Leaf { cladeId = -1, length = Just 3.00554587516, name = "Conimitella_williamsii", squid = Just "5460b06e839e09674ce4d759175a3ad5b76d266ed4d7a104605ae3918f17e412" }) (Binary.Leaf { cladeId = -1, length = Just 3.00554587516, name = "Mitella_stauropetala", squid = Just "9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1" }))
                    )
                )
            )
        )
        (Binary.Leaf { cladeId = -1, length = Just 20.0613968519, name = "Telesonix_jamesii", squid = Just "fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692" })
