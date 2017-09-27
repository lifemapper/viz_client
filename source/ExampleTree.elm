module ExampleTree exposing (..)

import DecodeTree exposing (..)
import Json.Decode exposing (decodeString)


tree : Tree
tree =
    case decodeString treeDecoder treeJson of
        Ok tree ->
            tree

        Err e ->
            Debug.crash e


treeJson : String
treeJson =
    """
{
   "pathId": 0,
   "length" : 0.0,
   "children": [
      {
         "pathId": 1,
         "length": 2.2997409923456544,
         "children": [
            {
               "pathId": 2,
               "length": 17.761655859533043,
               "children": [],
               "name": "Peltoboykinia_tellimoides"
            },
            {
               "pathId": 3,
               "length": 11.344384130779105,
               "children": [
                  {
                     "pathId": 4,
                     "length": 0.3054151405834489,
                     "children": [
                        {
                           "pathId": 5,
                           "length": 2.2717620145505304,
                           "children": [
                              {
                                 "pathId": 6,
                                 "length": 0.611602729860282,
                                 "children": [
                                    {
                                       "pathId": 7,
                                       "length": 1.176778597719423,
                                       "children": [
                                          {
                                             "pathId": 8,
                                             "length": 0.4550374361764966,
                                             "children": [
                                                {
                                                   "pathId": 9,
                                                   "length": 1.5966758098637577,
                                                   "children": [],
                                                   "name": "Heuchera_richardsonii"
                                                },
                                                {
                                                   "pathId": 10,
                                                   "length": 0.3462230525932135,
                                                   "children": [
                                                      {
                                                         "pathId": 11,
                                                         "length": 1.2504527572705442,
                                                         "children": [],
                                                         "name": "Heuchera_caroliniana"
                                                      },
                                                      {
                                                         "pathId": 12,
                                                         "length": 0.3415769256445209,
                                                         "children": [
                                                            {
                                                               "pathId": 13,
                                                               "length": 0.3076302280262695,
                                                               "children": [
                                                                  {
                                                                     "pathId": 14,
                                                                     "length": 0.6012456035997538,
                                                                     "children": [],
                                                                     "name": "Heuchera_pubescens"
                                                                  },
                                                                  {
                                                                     "pathId": 15,
                                                                     "length": 0.6012456035997538,
                                                                     "children": [],
                                                                     "name": "Heuchera_alba"
                                                                  }
                                                               ]
                                                            },
                                                            {
                                                               "pathId": 16,
                                                               "length": 0.2302970475353341,
                                                               "children": [
                                                                  {
                                                                     "pathId": 17,
                                                                     "length": 0.6785787840906892,
                                                                     "children": [],
                                                                     "name": "Heuchera_longiflora"
                                                                  },
                                                                  {
                                                                     "pathId": 18,
                                                                     "length": 0.6785787840906892,
                                                                     "children": [],
                                                                     "name": "Heuchera_americana"
                                                                  }
                                                               ]
                                                            }
                                                         ]
                                                      }
                                                   ]
                                                }
                                             ]
                                          },
                                          {
                                             "pathId": 19,
                                             "length": 0.47187394130160243,
                                             "children": [
                                                {
                                                   "pathId": 20,
                                                   "length": 1.5798393047386519,
                                                   "children": [],
                                                   "name": "Heuchera_parvifolia"
                                                },
                                                {
                                                   "pathId": 21,
                                                   "length": 0.5425026010470795,
                                                   "children": [
                                                      {
                                                         "pathId": 22,
                                                         "length": 1.0373367036915724,
                                                         "children": [],
                                                         "name": "Heuchera_wootonii"
                                                      },
                                                      {
                                                         "pathId": 23,
                                                         "length": 0.23065775846020387,
                                                         "children": [
                                                            {
                                                               "pathId": 24,
                                                               "length": 0.8066789452313685,
                                                               "children": [],
                                                               "name": "Heuchera_inconstans"
                                                            },
                                                            {
                                                               "pathId": 25,
                                                               "length": 0.12525170811208852,
                                                               "children": [
                                                                  {
                                                                     "pathId": 26,
                                                                     "length": 0.20019638555055153,
                                                                     "children": [
                                                                        {
                                                                           "pathId": 27,
                                                                           "length": 0.11321340943251812,
                                                                           "children": [
                                                                              {
                                                                                 "pathId": 28,
                                                                                 "length": 0.36801744213621035,
                                                                                 "children": [],
                                                                                 "name": "Heuchera_soltisii"
                                                                              },
                                                                              {
                                                                                 "pathId": 29,
                                                                                 "length": 0.36801744213621035,
                                                                                 "children": [],
                                                                                 "name": "Heuchera_novomexicana"
                                                                              }
                                                                           ]
                                                                        },
                                                                        {
                                                                           "pathId": 30,
                                                                           "length": 0.48123085156872847,
                                                                           "children": [],
                                                                           "name": "Heuchera_glomerulata"
                                                                        }
                                                                     ]
                                                                  },
                                                                  {
                                                                     "pathId": 31,
                                                                     "length": 0.68142723711928,
                                                                     "children": [],
                                                                     "name": "Heuchera_eastwoodiae"
                                                                  }
                                                               ]
                                                            }
                                                         ]
                                                      }
                                                   ]
                                                }
                                             ]
                                          }
                                       ]
                                    },
                                    {
                                       "pathId": 32,
                                       "length": 0.4888350941898132,
                                       "children": [
                                          {
                                             "pathId": 33,
                                             "length": 0.47200566715888925,
                                             "children": [
                                                {
                                                   "pathId": 34,
                                                   "length": 2.267651082410975,
                                                   "children": [],
                                                   "name": "Heuchera_woodsiaphila"
                                                },
                                                {
                                                   "pathId": 35,
                                                   "length": 0.3554426279939733,
                                                   "children": [
                                                      {
                                                         "pathId": 36,
                                                         "length": 0.7999722234109434,
                                                         "children": [
                                                            {
                                                               "pathId": 37,
                                                               "length": 1.112236231006058,
                                                               "children": [],
                                                               "name": "Heuchera_bracteata"
                                                            },
                                                            {
                                                               "pathId": 38,
                                                               "length": 1.112236231006058,
                                                               "children": [],
                                                               "name": "Heuchera_hallii"
                                                            }
                                                         ]
                                                      },
                                                      {
                                                         "pathId": 39,
                                                         "length": 0.2384391054724091,
                                                         "children": [
                                                            {
                                                               "pathId": 40,
                                                               "length": 0.21111342075537465,
                                                               "children": [
                                                                  {
                                                                     "pathId": 41,
                                                                     "length": 0.4527963254466094,
                                                                     "children": [
                                                                        {
                                                                           "pathId": 42,
                                                                           "length": 1.0098596027426083,
                                                                           "children": [],
                                                                           "name": "Heuchera_mexicana"
                                                                        },
                                                                        {
                                                                           "pathId": 43,
                                                                           "length": 0.3371574010178726,
                                                                           "children": [
                                                                              {
                                                                                 "pathId": 44,
                                                                                 "length": 0.6727022017247357,
                                                                                 "children": [],
                                                                                 "name": "Heuchera_longipetala"
                                                                              },
                                                                              {
                                                                                 "pathId": 45,
                                                                                 "length": 0.6727022017247357,
                                                                                 "children": [],
                                                                                 "name": "Heuchera_acutifolia"
                                                                              }
                                                                           ]
                                                                        }
                                                                     ]
                                                                  },
                                                                  {
                                                                     "pathId": 46,
                                                                     "length": 0.16571057212127016,
                                                                     "children": [
                                                                        {
                                                                           "pathId": 47,
                                                                           "length": 0.23571097096003513,
                                                                           "children": [
                                                                              {
                                                                                 "pathId": 48,
                                                                                 "length": 0.15224904639183734,
                                                                                 "children": [
                                                                                    {
                                                                                       "pathId": 49,
                                                                                       "length": 0.9089853387160751,
                                                                                       "children": [],
                                                                                       "name": "Heuchera_hirsutissima"
                                                                                    },
                                                                                    {
                                                                                       "pathId": 50,
                                                                                       "length": 0.19871193173261403,
                                                                                       "children": [
                                                                                          {
                                                                                             "pathId": 51,
                                                                                             "length": 0.21751211989885277,
                                                                                             "children": [
                                                                                                {
                                                                                                   "pathId": 52,
                                                                                                   "length": 0.4927612870846083,
                                                                                                   "children": [],
                                                                                                   "name": "Heuchera_caespitosa"
                                                                                                },
                                                                                                {
                                                                                                   "pathId": 53,
                                                                                                   "length": 0.4927612870846083,
                                                                                                   "children": [],
                                                                                                   "name": "Heuchera_abramsii"
                                                                                                }
                                                                                             ]
                                                                                          },
                                                                                          {
                                                                                             "pathId": 54,
                                                                                             "length": 0.13999698615662126,
                                                                                             "children": [
                                                                                                {
                                                                                                   "pathId": 55,
                                                                                                   "length": 0.5702764208268398,
                                                                                                   "children": [],
                                                                                                   "name": "Heuchera_elegans"
                                                                                                },
                                                                                                {
                                                                                                   "pathId": 56,
                                                                                                   "length": 0.5702764208268398,
                                                                                                   "children": [],
                                                                                                   "name": "Heuchera_parishii"
                                                                                                }
                                                                                             ]
                                                                                          }
                                                                                       ]
                                                                                    }
                                                                                 ]
                                                                              },
                                                                              {
                                                                                 "pathId": 57,
                                                                                 "length": 1.0612343851079125,
                                                                                 "children": [],
                                                                                 "name": "Heuchera_brevistaminea"
                                                                              }
                                                                           ]
                                                                        },
                                                                        {
                                                                           "pathId": 58,
                                                                           "length": 0.19797731227478366,
                                                                           "children": [
                                                                              {
                                                                                 "pathId": 59,
                                                                                 "length": 0.1809859827895579,
                                                                                 "children": [
                                                                                    {
                                                                                       "pathId": 60,
                                                                                       "length": 0.4817445066949446,
                                                                                       "children": [
                                                                                          {
                                                                                             "pathId": 61,
                                                                                             "length": 0.4362375543086614,
                                                                                             "children": [],
                                                                                             "name": "Heuchera_rosendahlii"
                                                                                          },
                                                                                          {
                                                                                             "pathId": 62,
                                                                                             "length": 0.4362375543086614,
                                                                                             "children": [],
                                                                                             "name": "Heuchera_wellsiae"
                                                                                          }
                                                                                       ]
                                                                                    },
                                                                                    {
                                                                                       "pathId": 63,
                                                                                       "length": 0.917982061003606,
                                                                                       "children": [],
                                                                                       "name": "Heuchera_sanguinea"
                                                                                    }
                                                                                 ]
                                                                              },
                                                                              {
                                                                                 "pathId": 64,
                                                                                 "length": 1.098968043793164,
                                                                                 "children": [],
                                                                                 "name": "Heuchera_versicolor"
                                                                              }
                                                                           ]
                                                                        }
                                                                     ]
                                                                  }
                                                               ]
                                                            },
                                                            {
                                                               "pathId": 65,
                                                               "length": 0.4799612897636507,
                                                               "children": [
                                                                  {
                                                                     "pathId": 66,
                                                                     "length": 1.1938080591809417,
                                                                     "children": [],
                                                                     "name": "Heuchera_pulchella"
                                                                  },
                                                                  {
                                                                     "pathId": 67,
                                                                     "length": 1.1938080591809417,
                                                                     "children": [],
                                                                     "name": "Heuchera_rubescens"
                                                                  }
                                                               ]
                                                            }
                                                         ]
                                                      }
                                                   ]
                                                }
                                             ]
                                          },
                                          {
                                             "pathId": 68,
                                             "length": 0.35059848255879444,
                                             "children": [
                                                {
                                                   "pathId": 69,
                                                   "length": 2.3890582670110696,
                                                   "children": [],
                                                   "name": "Heuchera_merriamii"
                                                },
                                                {
                                                   "pathId": 70,
                                                   "length": 2.3890582670110696,
                                                   "children": [],
                                                   "name": "Heuchera_grossulariifolia"
                                                }
                                             ]
                                          }
                                       ]
                                    }
                                 ]
                              },
                              {
                                 "pathId": 71,
                                 "length": 0.2991987051321221,
                                 "children": [
                                    {
                                       "pathId": 72,
                                       "length": 2.5145741447943273,
                                       "children": [
                                          {
                                             "pathId": 73,
                                             "length": 0.5547828033234765,
                                             "children": [
                                                {
                                                   "pathId": 74,
                                                   "length": 0.4715389203700333,
                                                   "children": [],
                                                   "name": "Heuchera_pilosissima"
                                                },
                                                {
                                                   "pathId": 75,
                                                   "length": 0.4715389203700333,
                                                   "children": [],
                                                   "name": "Heuchera_maxima"
                                                }
                                             ]
                                          },
                                          {
                                             "pathId": 76,
                                             "length": 1.0263217236935098,
                                             "children": [],
                                             "name": "Heuchera_micrantha"
                                          }
                                       ]
                                    },
                                    {
                                       "pathId": 77,
                                       "length": 0.31674667659847344,
                                       "children": [
                                          {
                                             "pathId": 78,
                                             "length": 3.2241491918893637,
                                             "children": [],
                                             "name": "Heuchera_glabra"
                                          },
                                          {
                                             "pathId": 79,
                                             "length": 0.5528931165335749,
                                             "children": [
                                                {
                                                   "pathId": 80,
                                                   "length": 1.182633807773751,
                                                   "children": [
                                                      {
                                                         "pathId": 81,
                                                         "length": 0.5071153942854956,
                                                         "children": [
                                                            {
                                                               "pathId": 82,
                                                               "length": 0.9815068732965422,
                                                               "children": [],
                                                               "name": "Heuchera_villosa"
                                                            },
                                                            {
                                                               "pathId": 83,
                                                               "length": 0.9815068732965422,
                                                               "children": [],
                                                               "name": "Heuchera_puberula"
                                                            }
                                                         ]
                                                      },
                                                      {
                                                         "pathId": 84,
                                                         "length": 0.5464074854841101,
                                                         "children": [
                                                            {
                                                               "pathId": 85,
                                                               "length": 0.9422147820979276,
                                                               "children": [],
                                                               "name": "Heuchera_missouriensis"
                                                            },
                                                            {
                                                               "pathId": 86,
                                                               "length": 0.9422147820979276,
                                                               "children": [],
                                                               "name": "Heuchera_parviflora"
                                                            }
                                                         ]
                                                      }
                                                   ]
                                                },
                                                {
                                                   "pathId": 87,
                                                   "length": 0.9262046180939212,
                                                   "children": [
                                                      {
                                                         "pathId": 88,
                                                         "length": 1.7450514572618676,
                                                         "children": [],
                                                         "name": "Heuchera_chlorantha"
                                                      },
                                                      {
                                                         "pathId": 89,
                                                         "length": 1.7450514572618676,
                                                         "children": [],
                                                         "name": "Heuchera_cylindrica"
                                                      }
                                                   ]
                                                }
                                             ]
                                          }
                                       ]
                                    }
                                 ]
                              }
                           ]
                        },
                        {
                           "pathId": 90,
                           "length": 0.36684912329080177,
                           "children": [
                              {
                                 "pathId": 91,
                                 "length": 3.900212763838672,
                                 "children": [
                                    {
                                       "pathId": 92,
                                       "length": 1.8447947010410157,
                                       "children": [],
                                       "name": "menziesii"
                                    },
                                    {
                                       "pathId": 93,
                                       "length": 1.8447947010410157,
                                       "children": [],
                                       "name": "diplomenziesii"
                                    }
                                 ]
                              },
                              {
                                 "pathId": 94,
                                 "length": 0.3745823917305344,
                                 "children": [
                                    {
                                       "pathId": 95,
                                       "length": 0.7313617060367275,
                                       "children": [
                                          {
                                             "pathId": 96,
                                             "length": 4.639063367112426,
                                             "children": [],
                                             "name": "Tellima_grandiflora"
                                          },
                                          {
                                             "pathId": 97,
                                             "length": 0.5413606106903632,
                                             "children": [
                                                {
                                                   "pathId": 98,
                                                   "length": 4.097702756422063,
                                                   "children": [],
                                                   "name": "Mitella_pentandra"
                                                },
                                                {
                                                   "pathId": 99,
                                                   "length": 2.8941250878466516,
                                                   "children": [
                                                      {
                                                         "pathId": 100,
                                                         "length": 0.564868364663317,
                                                         "children": [
                                                            {
                                                               "pathId": 101,
                                                               "length": 0.6387093039120941,
                                                               "children": [],
                                                               "name": "Mitella_stylosa"
                                                            },
                                                            {
                                                               "pathId": 102,
                                                               "length": 0.29445753672593966,
                                                               "children": [
                                                                  {
                                                                     "pathId": 103,
                                                                     "length": 0.34425176718615447,
                                                                     "children": [],
                                                                     "name": "Mitella_furusei"
                                                                  },
                                                                  {
                                                                     "pathId": 104,
                                                                     "length": 0.34425176718615447,
                                                                     "children": [],
                                                                     "name": "Mitella_pauciflora"
                                                                  }
                                                               ]
                                                            }
                                                         ]
                                                      },
                                                      {
                                                         "pathId": 105,
                                                         "length": 1.2035776685754112,
                                                         "children": [],
                                                         "name": "Mitella_japonica"
                                                      }
                                                   ]
                                                }
                                             ]
                                          }
                                       ]
                                    },
                                    {
                                       "pathId": 106,
                                       "length": 0.34978619639352715,
                                       "children": [
                                          {
                                             "pathId": 107,
                                             "length": 0.5038748262552062,
                                             "children": [
                                                {
                                                   "pathId": 108,
                                                   "length": 2.9798832994681135,
                                                   "children": [
                                                      {
                                                         "pathId": 109,
                                                         "length": 1.5368807510323066,
                                                         "children": [],
                                                         "name": "Mitella_breweri"
                                                      },
                                                      {
                                                         "pathId": 110,
                                                         "length": 1.5368807510323066,
                                                         "children": [],
                                                         "name": "Mitella_ovalis"
                                                      }
                                                   ]
                                                },
                                                {
                                                   "pathId": 111,
                                                   "length": 4.51676405050042,
                                                   "children": [],
                                                   "name": "Bensoniella_oregona"
                                                }
                                             ]
                                          },
                                          {
                                             "pathId": 112,
                                             "length": 0.5757118232995779,
                                             "children": [
                                                {
                                                   "pathId": 113,
                                                   "length": 4.444927053456048,
                                                   "children": [],
                                                   "name": "Lithophragma_parviflorum"
                                                },
                                                {
                                                   "pathId": 114,
                                                   "length": 1.682729328332485,
                                                   "children": [
                                                      {
                                                         "pathId": 115,
                                                         "length": 2.7621977251235634,
                                                         "children": [],
                                                         "name": "Mitella_nuda"
                                                      },
                                                      {
                                                         "pathId": 116,
                                                         "length": 2.7621977251235634,
                                                         "children": [],
                                                         "name": "Mitella_diphylla"
                                                      }
                                                   ]
                                                }
                                             ]
                                          }
                                       ]
                                    }
                                 ]
                              }
                           ]
                        }
                     ]
                  },
                  {
                     "pathId": 117,
                     "length": 0.48045556757312013,
                     "children": [
                        {
                           "pathId": 118,
                           "length": 5.936816161180818,
                           "children": [],
                           "name": "Tiarella_polyphylla"
                        },
                        {
                           "pathId": 119,
                           "length": 1.4294508717056722,
                           "children": [
                              {
                                 "pathId": 120,
                                 "length": 1.2373578645983425,
                                 "children": [
                                    {
                                       "pathId": 121,
                                       "length": 3.2700074248768036,
                                       "children": [],
                                       "name": "Elmera_racemosa"
                                    },
                                    {
                                       "pathId": 122,
                                       "length": 3.2700074248768036,
                                       "children": [],
                                       "name": "Mitella_caulescens"
                                    }
                                 ]
                              },
                              {
                                 "pathId": 123,
                                 "length": 1.5018194143115124,
                                 "children": [
                                    {
                                       "pathId": 124,
                                       "length": 3.0055458751636337,
                                       "children": [],
                                       "name": "Conimitella_williamsii"
                                    },
                                    {
                                       "pathId": 125,
                                       "length": 3.0055458751636337,
                                       "children": [],
                                       "name": "Mitella_stauropetala"
                                    }
                                 ]
                              }
                           ]
                        }
                     ]
                  }
               ]
            }
         ]
      },
      {
         "pathId": 126,
         "length": 20.061396851878698,
         "children": [],
         "name": "Telesonix_jamesii"
      }
   ]
}
    """
