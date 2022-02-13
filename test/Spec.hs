import Kairos.Test.TimePoint (timePointTest)

main :: IO ()
main = do
    putStrLn "Executing Spec tests:"
    putStrLn "======================"
    putStrLn "TimePoint tets:"
    timePointTest
    putStrLn "======================"