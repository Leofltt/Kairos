import Test.TimePoint (timePointTest)
import Test.Clock (clockTest)
import Test.PfPat (pfPatTest)
import Test.Kit (kitTest)
import Test.Utilities (utilitiesTest)

main :: IO ()
main = do
    putStrLn "Executing Spec tests:"
    putStrLn "======================"
    putStrLn "TimePoint tests:"
    timePointTest
    putStrLn "======================"
    putStrLn "Clock tests:"
    clockTest
    putStrLn "======================"
    putStrLn "PfPat tests:"
    pfPatTest
    putStrLn "======================"
    putStrLn "Kit tests:"
    kitTest
    putStrLn "======================"
    putStrLn "Utilities tests:"
    utilitiesTest
    putStrLn "======================"