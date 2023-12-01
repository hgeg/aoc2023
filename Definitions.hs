module Definitions where
    
type Part1 = [String] -> Int
type Part2 = [String] -> Int

data Solution = Solution Part1 Part2

readInput :: FilePath -> IO [String]
readInput path = lines <$> readFile path

execute :: Show a => ([String] -> a) -> [String] -> IO ()
execute = (.) print

run :: Solution -> FilePath -> IO ()
run (Solution part1 part2) path
    =  input >>= execute part1
    >> input >>= execute part2
    where input = readInput path