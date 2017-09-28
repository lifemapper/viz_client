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
 "children": [
  {
   "children": [
    {
     "children": [
      {
       "children": [
        {
         "children": [],
         "cladeId": 9,
         "name": "Species 9",
         "squid": "a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651"
        },
        {
         "children": [],
         "cladeId": 20,
         "name": "Species 20",
         "squid": "9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1"
        }
       ],
       "cladeId": 51,
       "length": 0.0,
       "name": ""
      },
      {
       "children": [],
       "cladeId": 42,
       "name": "Species 42",
       "squid": "3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516"
      }
     ],
     "cladeId": 50,
     "length": 0.0,
     "name": ""
    },
    {
     "children": [],
     "cladeId": 12,
     "name": "Species 12",
     "squid": "88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c"
    }
   ],
   "cladeId": 49,
   "length": 0.0,
   "name": ""
  },
  {
   "children": [
    {
     "children": [
      {
       "children": [
        {
         "children": [
          {
           "children": [],
           "cladeId": 17,
           "name": "Species 17",
           "squid": "5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a"
          },
          {
           "children": [
            {
             "children": [],
             "cladeId": 8,
             "name": "Species 8",
             "squid": "277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10"
            },
            {
             "children": [
              {
               "children": [],
               "cladeId": 21,
               "name": "Species 21",
               "squid": "cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888"
              },
              {
               "children": [],
               "cladeId": 15,
               "name": "Species 15",
               "squid": "31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33"
              }
             ],
             "cladeId": 57,
             "length": 0.0,
             "name": ""
            }
           ],
           "cladeId": 56,
           "length": 0.0,
           "name": ""
          }
         ],
         "cladeId": 55,
         "length": 0.0,
         "name": ""
        },
        {
         "children": [
          {
           "children": [
            {
             "children": [],
             "cladeId": 32,
             "name": "Species 32",
             "squid": "5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be"
            },
            {
             "children": [],
             "cladeId": 23,
             "name": "Species 23",
             "squid": "cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10"
            }
           ],
           "cladeId": 59,
           "length": 0.0,
           "name": ""
          },
          {
           "children": [
            {
             "children": [],
             "cladeId": 19,
             "name": "Species 19",
             "squid": "c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104"
            },
            {
             "children": [],
             "cladeId": 16,
             "name": "Species 16",
             "squid": "88093eb8272be2733b652edd444b2bd7307e1e692ae078ad0a1c63c2432e500c"
            }
           ],
           "cladeId": 60,
           "length": 0.0,
           "name": ""
          }
         ],
         "cladeId": 58,
         "length": 0.0,
         "name": ""
        }
       ],
       "cladeId": 54,
       "length": 0.0,
       "name": ""
      },
      {
       "children": [
        {
         "children": [
          {
           "children": [
            {
             "children": [
              {
               "children": [],
               "cladeId": 43,
               "name": "Species 43",
               "squid": "6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6"
              },
              {
               "children": [
                {
                 "children": [],
                 "cladeId": 27,
                 "name": "Species 27",
                 "squid": "cb4e4507016daff4363b0afe8c80503d5cb153e4e1be4a888bf56d1e552e6888"
                },
                {
                 "children": [
                  {
                   "children": [
                    {
                     "children": [
                      {
                       "children": [
                        {
                         "children": [],
                         "cladeId": 41,
                         "name": "Species 41",
                         "squid": "6033617be54c97a10e7b2a291f9fa71cea9cdd6b1d93b64af211d77f666370f6"
                        },
                        {
                         "children": [],
                         "cladeId": 33,
                         "name": "Species 33",
                         "squid": "eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12"
                        }
                       ],
                       "cladeId": 69,
                       "length": 0.0,
                       "name": ""
                      },
                      {
                       "children": [],
                       "cladeId": 26,
                       "name": "Species 26",
                       "squid": "fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692"
                      }
                     ],
                     "cladeId": 68,
                     "length": 0.0,
                     "name": ""
                    },
                    {
                     "children": [],
                     "cladeId": 24,
                     "name": "Species 24",
                     "squid": "d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c"
                    }
                   ],
                   "cladeId": 67,
                   "length": 0.0,
                   "name": ""
                  },
                  {
                   "children": [],
                   "cladeId": 2,
                   "name": "Species 2",
                   "squid": "5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9"
                  }
                 ],
                 "cladeId": 66,
                 "length": 0.0,
                 "name": ""
                }
               ],
               "cladeId": 65,
               "length": 0.0,
               "name": ""
              }
             ],
             "cladeId": 64,
             "length": 0.0,
             "name": ""
            },
            {
             "children": [
              {
               "children": [
                {
                 "children": [
                  {
                   "children": [
                    {
                     "children": [],
                     "cladeId": 30,
                     "name": "Species 30",
                     "squid": "87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84"
                    },
                    {
                     "children": [
                      {
                       "children": [],
                       "cladeId": 36,
                       "name": "Species 36",
                       "squid": "9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe"
                      },
                      {
                       "children": [],
                       "cladeId": 45,
                       "name": "Species 45",
                       "squid": "36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5"
                      }
                     ],
                     "cladeId": 74,
                     "length": 0.0,
                     "name": ""
                    }
                   ],
                   "cladeId": 73,
                   "length": 0.0,
                   "name": ""
                  },
                  {
                   "children": [
                    {
                     "children": [],
                     "cladeId": 22,
                     "name": "Species 22",
                     "squid": "fe728a820f7c1f4b958b45dbda521a4de88a78a53b12410abb34eb5227dcb692"
                    },
                    {
                     "children": [
                      {
                       "children": [],
                       "cladeId": 7,
                       "name": "Species 7",
                       "squid": "5c3a6b6e392a815717d66dfbf7c5a4181d338a6917e3413b357e9318e6d233e9"
                      },
                      {
                       "children": [],
                       "cladeId": 6,
                       "name": "Species 6",
                       "squid": "92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e"
                      }
                     ],
                     "cladeId": 76,
                     "length": 0.0,
                     "name": ""
                    }
                   ],
                   "cladeId": 75,
                   "length": 0.0,
                   "name": ""
                  }
                 ],
                 "cladeId": 72,
                 "length": 0.0,
                 "name": ""
                },
                {
                 "children": [
                  {
                   "children": [
                    {
                     "children": [],
                     "cladeId": 1,
                     "name": "Species 1",
                     "squid": "a9655bd78c883ac2469e43b582955b8fcf3a4521d7757d11b9a11f32117a2651"
                    },
                    {
                     "children": [
                      {
                       "children": [],
                       "cladeId": 37,
                       "name": "Species 37",
                       "squid": "5fab5c8bd4cdbfd62a0b53418c4342819504b75937561d1634f5cb52cbf733be"
                      },
                      {
                       "children": [],
                       "cladeId": 11,
                       "name": "Species 11",
                       "squid": "31b01c41e68f714829fe61e49a938c52825aa7418d74e272bc767b9bfd13ad33"
                      }
                     ],
                     "cladeId": 79,
                     "length": 0.0,
                     "name": ""
                    }
                   ],
                   "cladeId": 78,
                   "length": 0.0,
                   "name": ""
                  },
                  {
                   "children": [
                    {
                     "children": [],
                     "cladeId": 25,
                     "name": "Species 25",
                     "squid": "9426f9bec422b5430f4fcec0444acb195cfa3cc9945896fcd84c3908a8d928f1"
                    },
                    {
                     "children": [
                      {
                       "children": [],
                       "cladeId": 10,
                       "name": "Species 10",
                       "squid": "113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b"
                      },
                      {
                       "children": [],
                       "cladeId": 47,
                       "name": "Species 47",
                       "squid": "04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9"
                      }
                     ],
                     "cladeId": 81,
                     "length": 0.0,
                     "name": ""
                    }
                   ],
                   "cladeId": 80,
                   "length": 0.0,
                   "name": ""
                  }
                 ],
                 "cladeId": 77,
                 "length": 0.0,
                 "name": ""
                }
               ],
               "cladeId": 71,
               "length": 0.0,
               "name": ""
              },
              {
               "children": [
                {
                 "children": [],
                 "cladeId": 29,
                 "name": "Species 29",
                 "squid": "eea122035b5aafc233c4aa08a1b34b4c7dd0898c75111801ef4e96809da65d12"
                },
                {
                 "children": [],
                 "cladeId": 48,
                 "name": "Species 48",
                 "squid": "04eb74bde4c5e806744ba3de8ae27ef4c44cb858f36e6022f727ac154ba693f9"
                }
               ],
               "cladeId": 82,
               "length": 0.0,
               "name": ""
              }
             ],
             "cladeId": 70,
             "length": 0.0,
             "name": ""
            }
           ],
           "cladeId": 63,
           "length": 0.0,
           "name": ""
          },
          {
           "children": [
            {
             "children": [],
             "cladeId": 28,
             "name": "Species 28",
             "squid": "d00e7382dbccb6eeb81a2d92746bf446204a4326879443b66f1c02193b25e06c"
            },
            {
             "children": [],
             "cladeId": 31,
             "name": "Species 31",
             "squid": "adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98"
            }
           ],
           "cladeId": 83,
           "length": 0.0,
           "name": ""
          }
         ],
         "cladeId": 62,
         "length": 0.0,
         "name": ""
        },
        {
         "children": [
          {
           "children": [],
           "cladeId": 5,
           "name": "Species 5",
           "squid": "113a3085ac2c5adaa890da87f6499355984807b50695972f34a45937b2c5b24b"
          },
          {
           "children": [
            {
             "children": [],
             "cladeId": 35,
             "name": "Species 35",
             "squid": "adaecc271a630623fbf76b0fbc0380ede2f3947eb9497c70d162632ab98e9d98"
            },
            {
             "children": [],
             "cladeId": 38,
             "name": "Species 38",
             "squid": "eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f"
            }
           ],
           "cladeId": 85,
           "length": 0.0,
           "name": ""
          }
         ],
         "cladeId": 84,
         "length": 0.0,
         "name": ""
        }
       ],
       "cladeId": 61,
       "length": 0.0,
       "name": ""
      }
     ],
     "cladeId": 53,
     "length": 0.0,
     "name": ""
    },
    {
     "children": [
      {
       "children": [],
       "cladeId": 14,
       "name": "Species 14",
       "squid": "c070e509bcaf7cc95ab30d761aae50c8d27dc3cf4cde909a21f557d6f3711104"
      },
      {
       "children": [
        {
         "children": [
          {
           "children": [
            {
             "children": [],
             "cladeId": 3,
             "name": "Species 3",
             "squid": "277e99c4bcf2a34919620ae5a36a4a3f6d1e037cc079fb4472169bc5173b6a10"
            },
            {
             "children": [],
             "cladeId": 34,
             "name": "Species 34",
             "squid": "87ba42b1bdc43261c63c8c0bde591efad69ba7d3e39b167dc03d9ee8bb77eb84"
            }
           ],
           "cladeId": 89,
           "length": 0.0,
           "name": ""
          },
          {
           "children": [
            {
             "children": [],
             "cladeId": 4,
             "name": "Species 4",
             "squid": "92fcc804e96d19c2ffdba835ef507ebe84b07f7b8200bd5f3f4baa73567a693e"
            },
            {
             "children": [
              {
               "children": [],
               "cladeId": 39,
               "name": "Species 39",
               "squid": "36c9b3dd5beae67b937ca31b527c5ae4a80a3ef28fff00903ff1775e4ef0f7c5"
              },
              {
               "children": [],
               "cladeId": 40,
               "name": "Species 40",
               "squid": "9e43608eba7ebc83467687e4959ccabf3c25661a1709e2b035acffa29d5510fe"
              }
             ],
             "cladeId": 91,
             "length": 0.0,
             "name": ""
            }
           ],
           "cladeId": 90,
           "length": 0.0,
           "name": ""
          }
         ],
         "cladeId": 88,
         "length": 0.0,
         "name": ""
        },
        {
         "children": [
          {
           "children": [
            {
             "children": [
              {
               "children": [],
               "cladeId": 46,
               "name": "Species 46",
               "squid": "3f084c13957bf7ee3525b8ac237f769df32e53f0b62b0ae014ad72d2a15f9516"
              },
              {
               "children": [],
               "cladeId": 44,
               "name": "Species 44",
               "squid": "eb5f74f60707bbaffde32b62180549482024e0a4b268eae39cf3758549fe9c1f"
              }
             ],
             "cladeId": 94,
             "length": 0.0,
             "name": ""
            },
            {
             "children": [],
             "cladeId": 18,
             "name": "Species 18",
             "squid": "cdfc61168901288d8af6f0d1b548a951b719c89b9c0081183bb62d21c9a7cf10"
            }
           ],
           "cladeId": 93,
           "length": 0.0,
           "name": ""
          },
          {
           "children": [],
           "cladeId": 13,
           "name": "Species 13",
           "squid": "5c865386fbacbde30931ec2b463bbb3b6c5aca084734f0660fcc34928833d66a"
          }
         ],
         "cladeId": 92,
         "length": 0.0,
         "name": ""
        }
       ],
       "cladeId": 87,
       "length": 0.0,
       "name": ""
      }
     ],
     "cladeId": 86,
     "length": 0.0,
     "name": ""
    }
   ],
   "cladeId": 52,
   "length": 0.0,
   "name": ""
  }
 ],
 "cladeId": 0,
 "name": ""
}
"""
