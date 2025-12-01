{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use void" #-}
import Test.HUnit
import Control.Arrow (Arrow(second))

-- some helper function that describes a single move
move :: Int -> Char -> Char -> String
move n src dst = "move disk " ++ show n ++ " from " ++ [src] ++ " to " ++ [dst]

-- transfers a number of disks (the Int) from a source (the first Char) rod to a destination (the second Char) rod via a temp (the third char) rod
-- returns a list of required operations
hanoi :: (Int,Char,Char,Char) -> [String]
hanoi (n,s,d,t) = 
  hanoi (n-1,s,t,d)
  ++ [move n s d]
  ++ hanoi (n-1,t,d,s)

-- Source, Destination, Temp
hanoiTests :: [((Int, Char, Char, Char), [String])]
hanoiTests =
  [ ( (1, 'a', 'c', 'b'),
      [ "move disk 1 from a to c"
      ]
    ),
    ( (1, 'b', 'a', 'c'),
      [ "move disk 1 from b to a"
      ]
    ),
    ( (2, 'a', 'c', 'b'),
      [ "move disk 1 from a to b",
        "move disk 2 from a to c",
        "move disk 1 from b to c"
      ]
    ),
    ( (3, 'a', 'c', 'b'),
      [ "move disk 1 from a to c",
        "move disk 2 from a to b",
        "move disk 1 from c to b",
        "move disk 3 from a to c",
        "move disk 1 from b to a",
        "move disk 2 from b to c",
        "move disk 1 from a to c"
      ]
    ),
    ( (4, 'a', 'c', 'b'),
      [ "move disk 1 from a to b",
        "move disk 2 from a to c",
        "move disk 1 from b to c",
        "move disk 3 from a to b",
        "move disk 1 from c to a",
        "move disk 2 from c to b",
        "move disk 1 from a to b",
        "move disk 4 from a to c",
        "move disk 1 from b to c",
        "move disk 2 from b to a",
        "move disk 1 from c to a",
        "move disk 3 from b to c",
        "move disk 1 from a to b",
        "move disk 2 from a to c",
        "move disk 1 from b to c"
      ]
    )
  ]

hanoiTestCases :: [Test]
hanoiTestCases = map (\(input, expected) -> TestCase (assertEqual ("hanoi " ++ show input) expected (hanoi input))) hanoiTests

tests :: Test
tests = TestList hanoiTestCases

main :: IO ()
main = runTestTT tests >> return ()
