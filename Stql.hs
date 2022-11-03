import System.Environment
import RQLTokens
import RQLGrammar
import Data.List
import Data.Char

import RQLQTokens
import RQLQGrammar

type Env = [(String, String)]
type TurtleEnv = [(String, [(String, String, String)])]

main :: IO ()
main = do
          x <- getArgs
          z <- readFile $ head x
          y <- loopNames $ scanForFileNames $ parseQuery . RQLQTokens.alexScanTokens $ z --environment/memory
          putStrLn $ prettyPrint $ filterDuplicates $ sortBy sortAlph $ loopQuery (parseQuery . RQLQTokens.alexScanTokens $ z) (y)

-- Query Handle ----------------------------------------------------------------------------------------------------------------------------------------------------------

--Finds and returns the corresponding to the input name triples in our environment
lookTurtleValue :: String -> TurtleEnv -> [(String, String, String)]
lookTurtleValue a [] = error ("The variable " ++ show a ++ " doesn't exist in the environment!")
lookTurtleValue a env@(x:xs) | a == fst x = snd x
                             | otherwise = lookTurtleValue a xs

--Loops through every query line in the input query and handles its functionallity
loopQuery :: [Query] -> TurtleEnv -> [(String, String, String)]
loopQuery [] env = []
loopQuery ((Where x):ys) env = loopQuery ys (filterConditionals (fst x) (snd x) env "")
loopQuery ((Clone x):ys) env = loopQuery ys (cloneEnvValue (fst x) (snd x) env)
loopQuery ((Update x):ys) env = loopQuery ys (updateEnvValue (fst x) (snd x) env)
loopQuery ((Print [x]):xs) env = (lookTurtleValue x env) ++ (loopQuery xs env)
loopQuery ((Print (x:xs)):ys) env = (lookTurtleValue x env) ++ (loopQuery ((Print xs):ys) env)
loopQuery (_:xs) env = loopQuery xs env

--Updates the environment, as it puts a new tuple that is identical to the input one
cloneEnvValue :: String -> String -> TurtleEnv -> TurtleEnv
cloneEnvValue l1 l2 [] = []
cloneEnvValue l1 l2 env = (l2, (lookTurtleValue l1 env)) : env

--Handles the two types of update - substitution and change of value
updateEnvValue :: String -> UpdateType -> TurtleEnv -> TurtleEnv
updateEnvValue name (NormalUpdate x) env = (name, (updateTripleValue (fst x) (snd x) (lookTurtleValue name env))) : (removeTripleValue name env)
updateEnvValue name (CalcUpdate x) env = (name, (calcUpdateTripleValue (fst x) (snd x) (lookTurtleValue name env))) : (removeTripleValue name env)

--Removes an entry from the environment
removeTripleValue :: String -> TurtleEnv -> TurtleEnv
removeTripleValue name [] = error "The variable doesn't exist in the environment!"
removeTripleValue name (x:env) | (fst x) == name = removeTripleValue name env
                               | otherwise = x : removeTripleValue name env

--Handles the update query for change of value
calcUpdateTripleValue :: Triplet -> LiteralType -> [(String, String, String)] -> [(String, String, String)]
calcUpdateTripleValue Object (QPlusInt x) [] = []
calcUpdateTripleValue Object (QPlusInt x) (y:ys) = (tripleFst y, tripleSnd y, (calcObjValue ("+"++(show x)) (tripleTrd y))) : calcUpdateTripleValue Object (QPlusInt x) ys
calcUpdateTripleValue Object (QMinusInt x) [] = []
calcUpdateTripleValue Object (QMinusInt x) (y:ys) = (tripleFst y, tripleSnd y, (calcObjValue ("-"++(show x)) (tripleTrd y))) : calcUpdateTripleValue Object (QMinusInt x) ys
calcUpdateTripleValue x y z = error "Invalid request - only Integer Objects are accepted!"

--Calculates the upated object value
calcObjValue :: String -> String -> String
calcObjValue (x:xs) y | x == '+' = show ((read y :: Int) + (read xs :: Int))
                      | x == '-' = show ((read y :: Int) - (read xs :: Int))
                      | otherwise = error "Invalid input - not correct format (must specify '+' increase or '-' decrease)!"

--Handles the update query for substitution
updateTripleValue :: Triplet -> LiteralType -> [(String, String, String)] -> [(String, String, String)]
updateTripleValue Subject (QString x) [] = []
updateTripleValue Subject (QString x) (y:ys) = (x, tripleSnd y, tripleTrd y) : updateTripleValue Subject (QString x) ys
updateTripleValue Predicate (QString x) [] = []
updateTripleValue Predicate (QString x) (y:ys) = (tripleFst y, x, tripleTrd y) : updateTripleValue Predicate (QString x) ys
updateTripleValue Object x [] = []
updateTripleValue Object (QString x) (y:ys) = (tripleFst y, tripleSnd y, x) : updateTripleValue Object (QString x) ys
updateTripleValue Object (QBool x) (y:ys) = (tripleFst y, tripleSnd y, (show x)) : updateTripleValue Object (QBool x) ys
updateTripleValue Object (QInt x) (y:ys) = (tripleFst y, tripleSnd y, (show x)) : updateTripleValue Object (QInt x) ys
updateTripleValue Object (QMinusInt x) (y:ys) = (tripleFst y, tripleSnd y, ("-"++(show x))) : updateTripleValue Object (QMinusInt x) ys
updateTripleValue Object (QPlusInt x) (y:ys) = (tripleFst y, tripleSnd y, (show x)) : updateTripleValue Object (QPlusInt x) ys
updateTripleValue x y z = error "Invalid request - the Triplet cannot be compared to such value!"

--Handles all types of where query - and, or, and normal
filterConditionals :: String -> WhereType -> TurtleEnv -> String -> TurtleEnv -- last String is used for threshold for AndWhereRequest
filterConditionals name (NormalWhereRequest (Is a b)) env trshld = (name, (getFilteredIsTriples a b env trshld)) : env
filterConditionals name (NormalWhereRequest (IsLit a b)) env trshld = (name, (getFilteredIsLitTriples a b env trshld)) : env
filterConditionals name (NormalWhereRequest (IsBetween a b)) env trshld = (name, (getFilteredBetweenTriples a b env trshld)) : env
filterConditionals name (NormalWhereRequest (IsNotBetween a b)) env trshld = (name, (getFilteredNotBetweenTriples a b env trshld)) : env
filterConditionals name (OrWhereRequest a b) env trshld = (name, ((lookTurtleValue name (filterConditionals name a env trshld)) ++ (lookTurtleValue name (filterConditionals name b env trshld)))) : env
filterConditionals name (AndWhereRequest a b) env trshld = filterConditionals name a e name
                                                      where e = filterConditionals name b env trshld

--Returns all triples that meet the input condition
getFilteredIsTriples :: (String, Triplet) -> (String, Triplet) -> TurtleEnv -> String -> [(String, String, String)] -- last String is used for threshold for AndWhereRequest
getFilteredIsTriples (n1, t1) (n2, t2) env trshld = (fixTriples t1 a t2 b) ++ (fixTriples t2 b t1 a)
                                                  where a | trshld == "" = lookTurtleValue n1 env
                                                          | otherwise = lookTurtleValue trshld env
                                                        b = lookTurtleValue n2 env

--Returns all triples, which are equal to the input literal value
getFilteredIsLitTriples :: (String, Triplet) -> LiteralType -> TurtleEnv -> String -> [(String, String, String)] -- last String is used for threshold for AndWhereRequest
getFilteredIsLitTriples (n1, t1) l env trshld = (fixTriples' t1 a l)
                                                  where a | trshld == "" = lookTurtleValue n1 env
                                                          | otherwise = lookTurtleValue trshld env

--Returns all triples that have int objects, falling between the input range
getFilteredBetweenTriples :: (String, Triplet) -> (LiteralType, LiteralType) -> TurtleEnv -> String -> [(String, String, String)] -- last String is used for threshold for AndWhereRequest
getFilteredBetweenTriples (n1, t1) (l, r) env trshld = (fixTriples'' l r a)
                                                       where a | trshld == "" = filterNumberObject $ lookTurtleValue n1 env
                                                               | otherwise = filterNumberObject $ lookTurtleValue trshld env

--Returns all triples that have int objects, not falling between the input range
getFilteredNotBetweenTriples :: (String, Triplet) -> (LiteralType, LiteralType) -> TurtleEnv -> String -> [(String, String, String)] -- last String is used for threshold for AndWhereRequest
getFilteredNotBetweenTriples (n1, t1) (l, r) env trshld = (fixTriples''' l r a)
                                                       where a | trshld == "" = filterNumberObject $ lookTurtleValue n1 env
                                                               | otherwise = filterNumberObject $ lookTurtleValue trshld env

--Returns all triples with numerical objects
filterNumberObject :: [(String, String, String)] -> [(String, String, String)]
filterNumberObject [] = []
filterNumberObject (x:xs) | isInt (tripleTrd x) = x : filterNumberObject xs
                          | otherwise = filterNumberObject xs

--Returns all triples, which match the input condition
fixTriples :: Triplet -> [(String, String, String)] -> Triplet -> [(String, String, String)] -> [(String, String, String)]
fixTriples t1 [] t2 [] = error "Empty inputs!"

fixTriples Subject [] Subject ys = []
fixTriples Subject (x:xs) Subject ys = (filter ((==(tripleFst x)).tripleFst) ys) ++ (fixTriples Subject xs Subject ys)
fixTriples Subject [] Predicate ys = []
fixTriples Subject (x:xs) Predicate ys = (filter ((==(tripleFst x)).tripleSnd) ys) ++ (fixTriples Subject xs Predicate ys)
fixTriples Subject [] Object ys = []
fixTriples Subject (x:xs) Object ys = (filter ((==(tripleFst x)).tripleTrd) ys) ++ (fixTriples Subject xs Object ys)

fixTriples Predicate [] Subject ys = []
fixTriples Predicate (x:xs) Subject ys = (filter ((==(tripleSnd x)).tripleFst) ys) ++ (fixTriples Predicate xs Subject ys)
fixTriples Predicate [] Predicate ys = []
fixTriples Predicate (x:xs) Predicate ys = (filter ((==(tripleSnd x)).tripleSnd) ys) ++ (fixTriples Predicate xs Predicate ys)
fixTriples Predicate [] Object ys = []
fixTriples Predicate (x:xs) Object ys = (filter ((==(tripleSnd x)).tripleTrd) ys) ++ (fixTriples Predicate xs Object ys)

fixTriples Object [] Subject ys = []
fixTriples Object (x:xs) Subject ys = (filter ((==(tripleTrd x)).tripleFst) ys) ++ (fixTriples Object xs Subject ys)
fixTriples Object [] Predicate ys = []
fixTriples Object (x:xs) Predicate ys = (filter ((==(tripleTrd x)).tripleSnd) ys) ++ (fixTriples Object xs Predicate ys)
fixTriples Object [] Object ys = []
fixTriples Object (x:xs) Object ys = (filter ((==(tripleTrd x)).tripleTrd) ys) ++ (fixTriples Object xs Object ys)

--Fixes triples for IsLit case
fixTriples' :: Triplet -> [(String, String, String)] -> LiteralType -> [(String, String, String)]
fixTriples' t1 [] l = []
fixTriples' Subject xs (QString l) = (filter ((==(l)).tripleFst) xs)
fixTriples' Predicate xs (QString l) = (filter ((==(l)).tripleSnd) xs)
fixTriples' Object xs (QString l) = (filter ((==(l)).tripleTrd) xs)
fixTriples' Object xs (QBool l) = (filter ((==(show l)).tripleTrd) xs)
fixTriples' Object xs (QInt l) = (filter ((==(show l)).tripleTrd) xs)
fixTriples' Object xs (QPlusInt l) = (filter ((==(show l)).tripleTrd) xs)
fixTriples' Object xs (QMinusInt l) = (filter ((==("-"++show l)).tripleTrd) xs)
fixTriples' t1 xs l = error "Invalid request - the Triplet cannot be compared to such value!"

--Fixes triples for IsBetween case
fixTriples'' :: LiteralType -> LiteralType -> [(String, String, String)] -> [(String, String, String)]
fixTriples'' l1 l2 [] = []
fixTriples'' (QInt a) (QInt b) xs | a > b = error "Invalid range"
                                  | otherwise =  filter ((>=a) . (read')) (filter (((<=b) . (read'))) xs)
fixTriples'' (QMinusInt a) (QInt b) xs = filter ((>=(negate a)) . (read')) (filter (((<=b) . (read'))) xs) --no need for range checking
fixTriples'' (QMinusInt a) (QMinusInt b) xs | negate a > negate b = error "Invalid range"
                                            | otherwise =  filter ((>=(negate a)) . (read')) (filter (((<=(negate b)) . (read'))) xs)
fixTriples'' (QInt a) (QPlusInt b) xs | a > b = error "Invalid range"
                                      | otherwise =  filter ((>=a) . (read')) (filter (((<=b) . (read'))) xs)
fixTriples'' (QPlusInt a) (QPlusInt b) xs  | a > b = error "Invalid range"
                                           | otherwise =  filter ((>=a) . (read')) (filter (((<=b) . (read'))) xs)
fixTriples'' (QMinusInt a) (QPlusInt b) xs = filter ((>=(negate a)) . (read')) (filter (((<=b) . (read'))) xs) --no need for range checking
fixTriples'' (QPlusInt a) (QInt b) xs  | a > b = error "Invalid range"
                                       | otherwise =  filter ((>=a) . (read')) (filter (((<=b) . (read'))) xs)
fixTriples'' l1 l2 xs = error "Invalid request - the Triplet cannot be compared to such value!"

--Fixes triples for IsNotBetween case
fixTriples''' :: LiteralType -> LiteralType -> [(String, String, String)] -> [(String, String, String)]
fixTriples''' l1 l2 [] = []
fixTriples''' (QInt a) (QInt b) xs | a > b = error "Invalid range"
                                   | otherwise = filter ((<a) . (read')) xs ++ filter ((>b) . (read')) xs 
fixTriples''' (QMinusInt a) (QInt b) xs = filter ((<(negate a)) . (read')) xs ++ filter ((>b) . (read')) xs  --no need for range checking
fixTriples''' (QMinusInt a) (QMinusInt b) xs | negate a > negate b = error "Invalid range"
                                             | otherwise =  filter ((<(negate a)) . (read')) xs ++ filter ((>(negate b)) . (read')) xs
fixTriples''' (QInt a) (QPlusInt b) xs | a > b = error "Invalid range"
                                       | otherwise =  filter ((<a) . (read')) xs ++ filter ((>b) . (read')) xs 
fixTriples''' (QPlusInt a) (QPlusInt b) xs  | a > b = error "Invalid range"
                                            | otherwise =  filter ((<a) . (read')) xs ++ filter ((>b) . (read')) xs 
fixTriples''' (QMinusInt a) (QPlusInt b) xs = filter ((<(negate a)) . (read')) xs ++ filter ((>b) . (read')) xs --no need for range checking
fixTriples''' (QPlusInt a) (QInt b) xs  | a > b = error "Invalid range"
                                        | otherwise =  filter ((<a) . (read')) xs ++ filter ((>b) . (read')) xs 
fixTriples''' l1 l2 xs = error "Invalid request - the Triplet cannot be compared to such value!"

--Converts the object(third) value of the triple from String to Int. Filtering before conversion ensures no errors in parsing
read' :: (String, String, String) -> Int
read' x = read (tripleTrd x) :: Int 

--Fills up our environment/memory with all files that would need turtle normalisation for our query
scanForFileNames :: [Query] ->  [(String, String)]
scanForFileNames [] = []
scanForFileNames ((Select []):xs) = []
scanForFileNames ((Select (a:ys)):xs) = (((fst a), (snd a)++".ttl") : (scanForFileNames (((Select (ys)):xs))))
scanForFileNames (_ :xs) = []

--Transforms a file path to IO String type
readQFile :: FilePath -> IO String
readQFile fileName = do 
                        text <- readFile fileName
                        return text

--Reads all file names from our priorly filled up memory and normalises all turtle files
loopNames :: [(String, String)] -> IO TurtleEnv
loopNames [] = return []
loopNames (file:fileNames) = do
                               y <- readQFile (snd file)
                               z <- (ask ((fst file), (changeOccurancesLoop((parseCalc . RQLTokens.alexScanTokens) (y)) ((parseCalc . RQLTokens.alexScanTokens) (y))))) --IO [(String, String, String)]
                               x <- (loopNames fileNames) -- IO [(String, String, String)]
                               return (z : x)

--Transforms tuple into IO tuple type
ask :: (String, [(String,String,String)]) -> IO (String, [(String,String,String)])
ask s = return s

--Checks whether a reference of a variable exists
exists :: String -> TurtleEnv -> Bool
exists c [] = False
exists c (x:xs) | c == fst x = True
                | otherwise = exists c xs

-- Turtle Handle ----------------------------------------------------------------------------------------------------------------------------------------------------------
--Presents the content of the turtle file in a formatted way
prettyPrint :: [(String, String, String)] -> String
prettyPrint [] = []
prettyPrint (x:xs) | (take 7 (tripleTrd x)) == "http://" = "<" ++ (tripleFst x) ++ "><" ++ (tripleSnd x) ++ "><" ++ (tripleTrd x) ++ "> .\n" ++ (prettyPrint xs)
                   | tripleTrd x == "False" = "<" ++ (tripleFst x) ++ "><" ++ (tripleSnd x) ++ "> " ++ "false" ++ " .\n" ++ (prettyPrint xs)
                   | tripleTrd x == "True" = "<" ++ (tripleFst x) ++ "><" ++ (tripleSnd x) ++ "> " ++ "true" ++ " .\n" ++ (prettyPrint xs)
                   | isInt (tripleTrd x) == False = "<" ++ (tripleFst x) ++ "><" ++ (tripleSnd x) ++ "> " ++ "\"" ++ (tripleTrd x) ++ "\"" ++ " .\n" ++ (prettyPrint xs)
                   | otherwise = "<" ++ (tripleFst x) ++ "><" ++ (tripleSnd x) ++ "> " ++ (tripleTrd x) ++ " .\n" ++ (prettyPrint xs)

--Removes all duplicate triples 
filterDuplicates :: [(String, String, String)] -> [(String, String, String)]
filterDuplicates [] = []
filterDuplicates [x] = [x]
filterDuplicates (x:y:xs) | (x == y) = filterDuplicates (y:xs)
                          | otherwise = x : filterDuplicates (y:xs)

--Checks whether a string represents a number
isInt :: String -> Bool
isInt [] = True
isInt (x:xs) | x == '-' = isInt xs
             | x == '+' = isInt xs
             | isDigit x = isInt xs
             | otherwise = False

--Returns the third value from a triple
tripleTrd :: (a,a,a) -> a
tripleTrd (x,y,z) = z

--Returns the second value from a triple
tripleSnd :: (a,a,a) -> a
tripleSnd (x,y,z) = y

--Returns the first value from a triple
tripleFst :: (a,a,a) -> a
tripleFst (x,y,z) = x

--Sorts by the first two elements of a triple
--sortAlph :: (Ord a1, Ord a2, Ord a3) => (a1, a2, a3) -> (a1, a2, a3) -> Ordering
sortAlph :: (String, String, String) -> (String, String, String) -> Ordering
sortAlph (a, b, c) (x, y, z)
  | a < x = LT
  | a > x = GT
  | a == x = sortBeta (a, b, c) (x, y, z)

--Sorts by the second and third elements of a triple
--sortBeta :: (Ord a2, Ord a3) => (b1, a2, a3) -> (b2, a2, a3) -> Ordering
sortBeta :: (String, String, String) -> (String, String, String) -> Ordering
sortBeta (a, b, c) (x, y, z)
  | b < y = LT
  | b > y = GT
  | b == y = sortGamma (a, b, c) (x, y, z)

--sortGamma :: (Ord a3) => (b1, c1, a3) -> (b2, c2, a3) -> Ordering
sortGamma :: (String, String, String) -> (String, String, String) -> Ordering
sortGamma (a, b, c) (x, y, z)
  | take 4 c == "http" && take 4 z /= "http" = LT
  | take 4 c /= "http" && take 4 z == "http" = GT
  | c < z = LT
  | c > z = GT
  | c == z = LT

--Function that creates the final clear environment with base and prefixes
finaliseEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
finaliseEnv [] [] = []
finaliseEnv [] ys = removeEmptyTriples ys
finaliseEnv xs [] = removeEmptyTriples xs
finaliseEnv xs ys = (removeEmptyTriples xs) ++ (removeEmptyTriples ys)

--Removes empty tuples (used when creating environment with the base and prefixes)
removeEmptyTriples :: [(String, String)] -> [(String, String)]
removeEmptyTriples [] = []
removeEmptyTriples (t:tri) | t == ("","") = removeEmptyTriples tri
                           | otherwise = t : removeEmptyTriples tri

--Loop function to find the base
findVarLoop :: [TurtleExp] -> [(String, String)]
findVarLoop [] = []
findVarLoop (e:expr) = findVar e : findVarLoop expr

--Function to find the base
findVar :: TurtleExp -> (String, String)
findVar (Base (AbsExpr st)) = ("base", "http://"++st)
findVar _ = ("","")

--Loop function to find the prefixes
findPrefixesLoop :: [TurtleExp] -> [(String, String)] -> [(String, String)]
findPrefixesLoop [] _ = []
findPrefixesLoop (e:expr) env = findPrefixes e en : findPrefixesLoop expr env
                             where en = removeEmptyTriples env

--Function to find the prefixes
findPrefixes :: TurtleExp -> [(String, String)] -> (String, String)
findPrefixes (Prefix (pr) (AbsExpr st)) env = (pr, "http://"++st)
findPrefixes (Prefix (pr) (URIExpr st)) env = (pr, ((lookValue "base" env)++st))
findPrefixes _ env = ("", "")

--Function to check the value of a token (used to find whether the base needs to be added)
lookValue :: String -> [(String, String)] -> String
lookValue a [] = error "The variable doesn't exist in the environment!"
lookValue a (x:xs) | a == (fst(x)) = snd(x)
                   | otherwise = lookValue a xs

--Loop to process all the turtle tokens
changeOccurancesLoop :: [TurtleExp] -> [TurtleExp] -> [(String, String, String)]
changeOccurancesLoop [] copy = []
changeOccurancesLoop ((Base (AbsExpr e)):xs) copy = changeOccurancesLoop xs copy
changeOccurancesLoop ((Prefix (pr) (AbsExpr e)):xs) copy = changeOccurancesLoop xs copy
changeOccurancesLoop ((Prefix (pr) (URIExpr e)):xs) copy = changeOccurancesLoop xs copy
changeOccurancesLoop (l@(ObjMTriple x y z):xs) copy =
                        (changeOccurancesList l (finaliseEnv (findVarLoop copy) (findPrefixesLoop copy (findVarLoop copy)))) ++ (changeOccurancesLoop xs copy)
changeOccurancesLoop (l@(PredObjMTriple x y):xs) copy =
                        (changeOccurancesList l (finaliseEnv (findVarLoop copy) (findPrefixesLoop copy (findVarLoop copy)))) ++ (changeOccurancesLoop xs copy)
changeOccurancesLoop (x:xs) copy = (changeOccurances x (finaliseEnv (findVarLoop copy) (findPrefixesLoop copy (findVarLoop copy)))) : (changeOccurancesLoop xs copy)

--Processes the turtle tokens into corrrect triples
changeOccurances :: TurtleExp -> [(String, String)] -> (String, String, String)
changeOccurances (Triple (URIExpr x) (URIExpr y) (URI z)) env = ((a++x), (a++y), (a++z))
                                                               where a = lookValue "base" env
changeOccurances (Triple (URIExpr x) (URIExpr y) (Bool z)) env = ((a++x), (a++y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (URIExpr y) (Int z)) env = ((a++x), (a++y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (URIExpr y) (PlusInt z)) env = ((a++x), (a++y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (URIExpr y) (MinusInt z)) env = ((a++x), (a++y), ("-"++show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (URIExpr y) (String z)) env = ((a++x), (a++y), (z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (URIExpr y) (AbsURI z)) env = ((a++x), (a++y), (getAbsOutputString z))
                                                               where a = lookValue "base" env 
                                                               

changeOccurances (Triple (AbsExpr x) (AbsExpr y) (URI z)) env = ((getAbsOutputString x), (getAbsOutputString y), (a++z))
                                                               where a = lookValue "base" env
changeOccurances (Triple (AbsExpr x) (AbsExpr y) (Bool z)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z))
changeOccurances (Triple (AbsExpr x) (AbsExpr y) (Int z)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z))
changeOccurances (Triple (AbsExpr x) (AbsExpr y) (PlusInt z)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z))
changeOccurances (Triple (AbsExpr x) (AbsExpr y) (MinusInt z)) env = ((getAbsOutputString x), (getAbsOutputString y), ("-"++show z))
changeOccurances (Triple (AbsExpr x) (AbsExpr y) (String z)) env = ((getAbsOutputString x), (getAbsOutputString y), (z))
changeOccurances (Triple (AbsExpr x) (AbsExpr y) (AbsURI z)) env = ((getAbsOutputString x), (getAbsOutputString y), (getAbsOutputString z))


changeOccurances (Triple (AbsExpr x) (URIExpr y) (URI z)) env = ((getAbsOutputString x), (a++y), (a++z))
                                                               where a = lookValue "base" env
changeOccurances (Triple (AbsExpr x) (URIExpr y) (Bool z)) env = ((getAbsOutputString x), (a++y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (AbsExpr x) (URIExpr y) (Int z)) env = ((getAbsOutputString x), (a++y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (AbsExpr x) (URIExpr y) (PlusInt z)) env = ((getAbsOutputString x), (a++y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (AbsExpr x) (URIExpr y) (MinusInt z)) env = ((getAbsOutputString x), (a++y), ("-"++show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (AbsExpr x) (URIExpr y) (String z)) env = ((getAbsOutputString x), (a++y), (z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (AbsExpr x) (URIExpr y) (AbsURI z)) env = ((getAbsOutputString x), (a++y), (getAbsOutputString z))
                                                               where a = lookValue "base" env 


changeOccurances (Triple (URIExpr x) (AbsExpr y) (URI z)) env = ((a++x), (getAbsOutputString y), (a++z))
                                                               where a = lookValue "base" env
changeOccurances (Triple (URIExpr x) (AbsExpr y) (Bool z)) env = ((a++x), (getAbsOutputString y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (AbsExpr y) (Int z)) env = ((a++x), (getAbsOutputString y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (AbsExpr y) (PlusInt z)) env = ((a++x), (getAbsOutputString y), (show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (AbsExpr y) (MinusInt z)) env = ((a++x), (getAbsOutputString y), ("-"++show z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (AbsExpr y) (String z)) env = ((a++x), (getAbsOutputString y), (z))
                                                               where a = lookValue "base" env 
changeOccurances (Triple (URIExpr x) (AbsExpr y) (AbsURI z)) env = ((a++x), (getAbsOutputString y), (getAbsOutputString z))
                                                               where a = lookValue "base" env


changeOccurances (SubPrefix a x b y c z) env = ((r++x), (q++y), (l++z))
                                            where r = lookValue a env
                                                  q = lookValue b env
                                                  l = lookValue c env

--Processes the turtle tokens into corrrect triples
changeOccurancesList :: TurtleExp -> [(String, String)] -> [(String, String, String)]
-- ObjMTriple -------------------------------------------------------------------------------------------------------------------
changeOccurancesList (ObjMTriple _ _ []) env = []

changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (URI z: xs)) env = ((a++x), (a++y), (a++z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (Bool z: xs)) env = ((a++x), (a++y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (Int z: xs)) env = ((a++x), (a++y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (PlusInt z: xs)) env = ((a++x), (a++y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env                                                                                                        
changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (MinusInt z: xs)) env = ((a++x), (a++y), ("-"++show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (String z: xs)) env = ((a++x), (a++y), (z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (AbsURI z: xs)) env = ((a++x), (a++y), (getAbsOutputString z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env       


changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (URI z: xs)) env = ((getAbsOutputString x), (getAbsOutputString y), (a++z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (Bool z: xs)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (Int z: xs)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (PlusInt z: xs)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env                                                                                                        
changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (MinusInt z: xs)) env = ((getAbsOutputString x), (getAbsOutputString y), ("-"++show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (String z: xs)) env = ((getAbsOutputString x), (getAbsOutputString y), (z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (AbsURI z: xs)) env = ((getAbsOutputString x), (getAbsOutputString y), (getAbsOutputString z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env   


changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (URI z: xs)) env = ((getAbsOutputString x), (a++y), (a++z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (Bool z: xs)) env = ((getAbsOutputString x), (a++y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (Int z: xs)) env = ((getAbsOutputString x), (a++y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (PlusInt z: xs)) env = ((getAbsOutputString x), (a++y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env                                                                                                        
changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (MinusInt z: xs)) env = ((getAbsOutputString x), (a++y), ("-"++show z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (String z: xs)) env = ((getAbsOutputString x), (a++y), (z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (AbsURI z: xs)) env = ((getAbsOutputString x), (a++y), (getAbsOutputString z)) :
                                                           (changeOccurancesList (ObjMTriple (AbsExpr x) (URIExpr y) (xs)) env)
                                                                        where a = lookValue "base" env 


changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (URI z: xs)) env = ((a++x), (getAbsOutputString y), (a++z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (Bool z: xs)) env = ((a++x), (getAbsOutputString y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (Int z: xs)) env = ((a++x), (getAbsOutputString y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (PlusInt z: xs)) env = ((a++x), (getAbsOutputString y), (show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env                                                                                                        
changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (MinusInt z: xs)) env = ((a++x), (getAbsOutputString y), ("-"++show z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (String z: xs)) env = ((a++x), (getAbsOutputString y), (z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env
changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (AbsURI z: xs)) env = ((a++x), (getAbsOutputString y), (getAbsOutputString z)) :
                                                           (changeOccurancesList (ObjMTriple (URIExpr x) (AbsExpr y) (xs)) env)
                                                                        where a = lookValue "base" env  


-- PredObjMTriple -----------------------------------------------------------------------------------------------------------------------

changeOccurancesList (PredObjMTriple (URIExpr x) []) env = []
changeOccurancesList (PredObjMTriple (AbsExpr x) []) env = []
changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), []):ys)) env = changeOccurancesList (PredObjMTriple (URIExpr x) ys) env

changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), ((URI z):zs)):ys)) env = ((a++x), (a++y), (a++z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), ((Bool z):zs)):ys)) env = ((a++x), (a++y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), ((Int z):zs)):ys)) env = ((a++x), (a++y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), ((PlusInt z):zs)):ys)) env = ((a++x), (a++y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), ((MinusInt z):zs)):ys)) env = ((a++x), (a++y), ("-"++show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), ((String z):zs)):ys)) env = ((a++x), (a++y), (z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), ((AbsURI z):zs)):ys)) env = ((a++x), (a++y), (getAbsOutputString z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env   


changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), []):ys)) env = changeOccurancesList (PredObjMTriple (AbsExpr x) ys) env

changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), ((URI z):zs)):ys)) env = ((getAbsOutputString x), (getAbsOutputString y), (a++z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), ((Bool z):zs)):ys)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), (zs)):ys)) env)
changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), ((Int z):zs)):ys)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), (zs)):ys)) env)
changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), ((PlusInt z):zs)):ys)) env = ((getAbsOutputString x), (getAbsOutputString y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), (zs)):ys)) env)
changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), ((MinusInt z):zs)):ys)) env = ((getAbsOutputString x), (getAbsOutputString y), ("-"++show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), (zs)):ys)) env)
changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), ((String z):zs)):ys)) env = ((getAbsOutputString x), (getAbsOutputString y), (z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), (zs)):ys)) env)
changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), ((AbsURI z):zs)):ys)) env = ((getAbsOutputString x), (getAbsOutputString y), (getAbsOutputString z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((AbsExpr y), (zs)):ys)) env)  
 

changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), []):ys)) env = changeOccurancesList (PredObjMTriple (AbsExpr x) ys) env

changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), ((URI z):zs)):ys)) env = ((getAbsOutputString x), (a++y), (a++z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), ((Bool z):zs)):ys)) env = ((getAbsOutputString x), (a++y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), ((Int z):zs)):ys)) env = ((getAbsOutputString x), (a++y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), ((PlusInt z):zs)):ys)) env = ((getAbsOutputString x), (a++y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), ((MinusInt z):zs)):ys)) env = ((getAbsOutputString x), (a++y), ("-"++show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), ((String z):zs)):ys)) env = ((getAbsOutputString x), (a++y), (z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), ((AbsURI z):zs)):ys)) env = ((getAbsOutputString x), (a++y), (getAbsOutputString z)) :
                                                                  (changeOccurancesList (PredObjMTriple (AbsExpr x) (((URIExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env   
                                                                                                                      
changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), []):ys)) env = changeOccurancesList (PredObjMTriple (URIExpr x) ys) env

changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), ((URI z):zs)):ys)) env = ((a++x), (getAbsOutputString y), (a++z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), ((Bool z):zs)):ys)) env = ((a++x), (getAbsOutputString y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), ((Int z):zs)):ys)) env = ((a++x), (getAbsOutputString y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), ((PlusInt z):zs)):ys)) env = ((a++x), (getAbsOutputString y), (show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), ((MinusInt z):zs)):ys)) env = ((a++x), (getAbsOutputString y), ("-"++show z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), ((String z):zs)):ys)) env = ((a++x), (getAbsOutputString y), (z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env
changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), ((AbsURI z):zs)):ys)) env = ((a++x), (getAbsOutputString y), (getAbsOutputString z)) :
                                                                  (changeOccurancesList (PredObjMTriple (URIExpr x) (((AbsExpr y), (zs)):ys)) env)
                                                                                     where a = lookValue "base" env   

--Adds the "http://" where neccessary
getAbsOutputString :: String -> String
getAbsOutputString a = "http://" ++ a