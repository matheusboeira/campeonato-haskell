module Championship.ReadFile where

import qualified System.IO as IO

--
-- Declaração de um sinônimo para facilitar a leitura
-- da chamada da função splitBy.
--
type Delimiter = Char

--
-- Realiza a leitura do arquivo de banco de dados do projeto.
--
readDatabase :: IO [String]
readDatabase = do
    fileContent <- IO.readFile "src/Championship/database/result.csv"
    let content = lines fileContent
    return (tail content)

--
-- Realiza a leitura do arquivo resultante dos times do
-- campeonato.
--
readTeamResult :: IO [String]
readTeamResult = do
    fileContent <- IO.readFile "src/Championship/database/team_result.csv"
    let content = lines fileContent
    return (tail content)

--
-- Transforma um texto que contém um delimitador em uma lista 
-- de String com os respectivos elementos separados.
--
splitBy :: Delimiter -> String -> [String]
splitBy delimiter [] = [""]
splitBy delimiter (x : xs)
        | x == delimiter = "" : rest
        | otherwise = (x : head rest) : tail rest
    where
        rest = splitBy delimiter xs
