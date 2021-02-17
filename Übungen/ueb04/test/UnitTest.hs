module Main where

import Test.Tasty
import Test.Tasty.HUnit

import FBox
import Banner

main = defaultMain $ testGroup "0. Übungsblatt" [exercise1, exercise2]

exercise1 = testGroup "Übung 4.1: BIG LETTERS" $ [
  testCase "decode 8 79" $
     decode 8 79 @?= [True, True, True, True, False, False, True, False],
  testCase "encode [...]" $ 
     encode [False, True, True, False, True, False, True] @?= 86,
  testCase "banner1 '*' 'A'" $ 
    banner1 '*' 'A' @?= ["   *    ","  * *   "," *   *  ","*     * ","******* ","*     * ","*     * ","        "],
  testCase "banner '*' \"Happy?\"" $ 
    banner '#' "Happy?" @?= "#     #                                   ####  \n#     #                                  #    # \n#     #  ####   #####   #####    #   #        # \n#######      #   #   #   #   #   #   #     ###  \n#     #  #####   #   #   #   #   #   #     #    \n#     # #    #   ####    ####     #  #          \n#     #  #### #  #       #      #  ##      #    \n                ##      ##       ###            \n"
  ]

exercise2 = testGroup "Übung 4.2: Schleife dran" $ [
  testCase "check [\"abc\", \"def\", \"ghi\"]" $
    check ["abc", "def", "ghi"] @?= True,
  testCase "check [[0,1,2], [3, 4], [5,6,7,8]]" $
    check [[0,1,2], [3, 4], [5,6,7,8]] @?= False,
  testCase "eq_length 0 [[1,2], [3,4,5], [6]]" $
    eq_length 0 [[1,2], [3,4,5], [6]] @?= [[1,2,0],[3,4,5],[6,0,0]],
  testCase "eq_lines \"ab\\ncdef\\ng\"" $
    eq_lines "ab\ncdef\ng" @?= "ab  \ncdef\ng   \n",
  testCase "fbox '@' \"foo\\nbar\\nbaz\"" $
    fbox '@' "foo\nbar\nbaz" @?= "@@@@@@@\n@ foo @\n@ bar @\n@ baz @\n@@@@@@@\n"
  ]
