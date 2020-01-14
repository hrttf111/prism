import TestFlags

import Test.Hspec
import TestFlags
import TestCpu
import TestPeripherals

main :: IO ()
main = hspec $ do
    testFlagsCF
    testFlagsSF
    testFlagsZF
    testFlagsPF
    testFlagsAF
    testFlagsOF
    testSign
    testDiv
    testPeripherals
