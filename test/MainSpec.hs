-- MainTest.hs

import Data.Char (isSpace)
import System.IO
import Test.HUnit
import Utils

-- Add your function implementations and imports here...

testIsEmptyLine :: Test
testIsEmptyLine = TestCase $ do
  assertBool "Empty line should return True" (isEmptyLine "")
  assertBool "Line with spaces should return True" (isEmptyLine "    ")
  assertBool "Line with visible characters should return False" (not (isEmptyLine "Hello, World!"))

-- Add more test functions for other functions here...

testSuite :: Test
testSuite =
  TestList
    [ testIsEmptyLine
    -- Add more tests for other functions here
    ]

main :: IO ()
main = do
  putStrLn "Running tests..."
  counts <- runTestTT testSuite
  if errors counts == 0 && failures counts == 0
    then putStrLn "All tests passed."
    else putStrLn "Some tests failed."
