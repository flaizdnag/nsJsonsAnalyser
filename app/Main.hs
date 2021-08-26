module Main where


import JsonAnalysis


main :: IO ()
main = analyseJsons
{-
main :: IO ()
main = createAndWriteFile "foo/bar/baz/quux.txt" "something"

createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
  createDirectoryIfMissing True $ takeDirectory path

  writeFile path content
-}
