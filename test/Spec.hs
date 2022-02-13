import Kairos.Test.TimePoint (timePointTest)
import Kairos.Test.Clock (clockTest)

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