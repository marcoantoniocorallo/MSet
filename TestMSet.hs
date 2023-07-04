import MultiSet
import Data.List (sort)
import Data.Char (toLower)

in_dir = "./aux_files/"

-- ciao s returns the "ciao" of s: a string having the same length of s 
-- and containing all the characters of s in lower case and alphabetical order.
ciao :: [Char] -> [Char]
ciao s = sort $ map toLower s

-- readMSet filename reads a file whose name is filename 
-- and returns an MSet containing the ciao of all the words of the file with the corresponding mutiplicity.
readMSet :: String -> IO (MSet String)
readMSet filename = readFile filename >>= (\x -> return $ fromList $ map ciao $ words x)

-- writeMSet mset filename writes in the file filename, one per line, 
-- each element of the multiset mset, with its multiplicity in the format "<elem> - <multiplicity>".
writeMSet :: Show a => MSet a -> String -> IO ()
writeMSet mset filename = writeFile filename $ toString mset

main :: IO()
main = do{

  -- load test files into four multisets
  m1 <- readMSet $ in_dir++"anagram.txt";
  m2 <- readMSet $ in_dir++"anagram-s1.txt";
  m3 <- readMSet $ in_dir++"anagram-s2.txt";
  m4 <- readMSet $ in_dir++"margana2.txt";

  -- Checks that m1 and m4 are not equal, but they have the same elements
  if (m1 /= m4) && ((sort $ elems m1) == (sort $ elems m4))
    then putStrLn "Multisets m1 and m4 are not equal, but they have the same elements";
    else putStrLn "ASSERTION ERROR: Multisets m1 and m4 are not equal, but they have the same elements";

  -- Checks that m1 is equal to the union of multisets m2 and m3
  if (m1 == (union m2 m3))
    then putStrLn "Multiset m1 is equal to the union of multisets m2 and m3";
    else putStrLn "ASSERTION ERROR: Multiset m1 is equal to the union of multisets m2 and m3";

  -- writes m1 and m4 to output files anag-out.txt and gana-out.txt, respectively
  writeMSet m1 "anag-out.txt";
  writeMSet m4 "gana-out.txt";
}