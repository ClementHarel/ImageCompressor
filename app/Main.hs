--
-- EPITECH PROJECT, 2019
-- FUN_deBruijn_2018
-- File description:
-- Main
--

import System.Random
import System.Environment
import System.IO.Unsafe
import Data.List
import Control.Monad
import System.IO
import Text.Printf

type Point = (Int, Int)
type Color = (Float, Float, Float)

rand_color :: (Random a, Num a) => IO a
rand_color = randomRIO (0, 255)

add_comma :: String -> Bool -> String
add_comma "" _ = ""
add_comma (x:xs) i
    |   x == ' '                =   add_comma xs i
    |   x == ')' && i == False  =   [x] ++ "," ++ (add_comma xs True)
    |   otherwise               =   [x] ++ (add_comma xs i)

parseString :: String -> (Point, Color)
parseString str = (read ("(" ++ (add_comma str False) ++ ")")) :: (Point, Color)

file_to_list :: FilePath -> IO [(Point, Color)]
file_to_list filename = do
    s <- readFile filename
    return ([parseString x | x <- lines s])

random_color :: IO (Float, Float, Float)
random_color = do
    r <- rand_color 
    g <- rand_color
    b <- rand_color
    return (r, g, b)

print_help progName = putStrLn $ "USAGE: ./" ++ progName ++ " n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

get_color_distance :: Color -> Color -> Float
get_color_distance (r, g, b) (r', g', b') = sqrt ((r-r')^2 + (g-g')^2 + (b-b')^2)

get_closer_cluster' :: [Color] -> Color -> Float
get_closer_cluster' clusters color = minimum [get_color_distance x color | x <- clusters]

get_closer_cluster :: [Color] -> Color -> Int
get_closer_cluster clusters color = [i | i <- [0..(length clusters) - 1], m == (get_color_distance (clusters !! i) color)] !! 0
    where    m = get_closer_cluster' clusters color

get_r :: Color -> Float
get_r (r, _, _) = r

get_g :: Color -> Float
get_g (_, g, _) = g

get_b :: Color -> Float
get_b (_, _, b) = b

average :: [(Float, Float, Float)] -> (Float, Float, Float) -> (Float, Float, Float)
average [] old = old
average colors _ = ((sum r) / fromIntegral(length r), (sum g) / fromIntegral(length g), (sum b) / fromIntegral(length b))
    where   r = [get_r color | color <- colors]
            g = [get_g color | color <- colors]
            b = [get_b color | color <- colors]

calc_new_clusters :: [Color] -> [(Point, Color)] -> [Color]
calc_new_clusters clusters points = [ (average [color | (point, color) <- points, (get_closer_cluster clusters color) == i ] (clusters !! i)) | i <- [0..(length clusters) - 1]]

calc_new_clusters' :: [Color] -> [(Point, Color)] -> IO [Color]
calc_new_clusters' clusters points = return ([ (average [color | (point, color) <- points, (get_closer_cluster clusters color) == i ] (clusters !! i)) | i <- [0..(length clusters) - 1]])


toto :: [Color] -> [Color]
toto cluster = cluster

unique = reverse . nub . reverse

points_to_color_list :: [(Point, Color)] -> [Color]
points_to_color_list points = do
    unique [color | (point, color) <- points]

get_clusters :: Int -> [(Point, Color)] -> IO [Color]
get_clusters nb_colors points = do
    let points' = points_to_color_list points
    if (nb_colors > (length points'))
    then do
        toto <- replicateM (nb_colors - (length points')) random_color
        return (points' ++ toto)
    else return (take nb_colors points')

printt :: Color -> [Color] -> IO ()
printt cluster colors = return ()

--print_point :: (Point, Color) -> IO ()
print_point pts = do
    printf "(%d,%d) (%.0f,%.0f,%.0f)\n" (fst $ fst pts) (snd $ fst pts) (get_r $ snd pts) (get_g $ snd pts) (get_b $ snd pts)
--print_point pd = printf "(%d,%d) (%d,%d,%d)\n" (fst $ fst pd) (snd $ fst pd) (get_r $ snd pd) (get_g $ snd pd) (get_b $ snd pd)

print_result :: [Color] -> [(Point, Color)] -> Int -> IO ()
print_result [] _ _ = return ()
print_result clusters points i
    |   i >= length clusters    = return ()
    |   otherwise               = do
        let ouais = [pd | pd <- points, (get_closer_cluster clusters (snd pd)) == i]
        if (length ouais > 0)
            then do
                printf "--\n(%.2f,%.2f,%.2f)\n-\n" (get_r (clusters !! i)) (get_g (clusters !! i)) (get_b (clusters !! i))
                mapM print_point ouais
            else return [()]
        print_result clusters points (i + 1)


max_diff :: [Color] -> [Color] -> Float
max_diff a b = maximum [get_color_distance x y | x <- a, y <- b]

loop :: [Color] -> [(Point, Color)] -> Float -> Int -> IO [Color]
loop clusters points convergence_limit i
        |   i == 10 = return (clusters)
        |   otherwise = loop (calc_new_clusters clusters points) points convergence_limit (i + 1)

program args = do
    let nb_colors = read (args !! 0) :: Int
        convergence_limit = read (args !! 1) :: Float
        filename = args !! 2
    points <- file_to_list filename
    clusters <- get_clusters nb_colors points
    
    clusters <- loop clusters points convergence_limit 0

    print_result clusters points 0 
    return ()

main = do
    args <- getArgs
    progName <- getProgName
    
    let ac = length args
    
    
    if ac == 3
        then program args
        else print_help progName