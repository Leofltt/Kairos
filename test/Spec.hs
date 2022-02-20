import Kairos.Test.TimePoint (timePointTest)
import Kairos.Test.Clock (clockTest)
import Kairos.Test.PfPat (pfPatTest)

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