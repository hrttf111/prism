import TestFlags

import Test.Hspec
import TestFlags

main :: IO ()
main = hspec $ do
    testFlagsCF
    testFlagsSF
    testFlagsZF
    testFlagsPF
    testFlagsAF
