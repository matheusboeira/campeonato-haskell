module Views.ShowResult where

import Text.Printf ( printf )

import Championship.Manipulate as M
import qualified Utils.AppUtils as U
import Championship.Structures ( Match(..), TeamResult (..) )

--
-- Imprime o resultado do desempenho de um time específico (RF1).
--
showTeamPerformance :: Team -> (Wins, Draws, Losses) -> IO ()
showTeamPerformance team (wins, draws, losses) = do
    U.putStrLnColor "purple" "\n[DESEMPENHO DO TIME]\n"
    U.putStrLnColor "blue" $ "> Time: " ++ team
    putStrLn "+----------------------------------------+"
    U.putStrLnColor "green" $ "  Vitórias: " ++ show wins
    putStrLn $ "  Empates: " ++ show draws
    U.putStrLnColor "red" $ "  Derrotas: " ++ show losses
    putStrLn "+----------------------------------------+"

--
-- Imprime a pontuação de um time especificado (RF2).
--
showRankTeam :: Team -> IO ()
showRankTeam team = do
    teams <- getTeamResult
    let sorted = sortTeamResult teams
    let rank = getTeamRank 1 team sorted
    let color | rank >= 1 && rank <= 3 = putStr U.green
              | rank >= 8 && rank <= 10 = putStr U.red
              | otherwise = putStr U.blue
    U.putStrLnColor "purple" "\n[CLASSIFICAÇÃO DO TIME]\n"
    putStrLn "+----------------------------------------------+"
    color
    putStr $ "  " ++ team ++ " está na " ++ show rank
    putStrLn $ "ª posição." ++ U.reset
    putStrLn "+----------------------------------------------+"

--
-- Imprime o aproveitamento de um time especificado (RF3).
--
showRecordsByTeam :: Team -> [Match] -> IO ()
showRecordsByTeam _ [] = putStrLn $ U.red ++ "Não há pontuação." ++ U.reset
showRecordsByTeam team matches = do
    let filtered = M.filterByTeam team matches
    let records = M.getRecordsByTeam team filtered
    U.putStrLnColor "purple" "\n[APROVEITAMENTO DO TIME]\n"
    putStrLn "+----------------------------------------------+"
    putStr $ "  " ++ U.blue ++ team ++ " teve "
    printf "%.1g" records
    putStrLn $ " % de aproveitamento." ++ U.reset
    putStrLn "+----------------------------------------------+"

--
-- Imprime o saldo de gols de um time especificado (RF4).
--
showGoalsDifferenceByTeam :: Team -> [Match] -> IO ()
showGoalsDifferenceByTeam _ [] = putStrLn $ U.red ++ "Não há pontuação." ++ U.reset
showGoalsDifferenceByTeam team matches = do
    U.putStrLnColor "purple" "\n[SALDO DE GOLS DO TIME]\n"
    U.putStrLnColor "blue" $ "> Time: " ++ team
    putStrLn "+----------------------------------------+"
    U.putStrLnColor "cyan" 
        $ "  O saldo é de " ++ show (getGoalsDifferenceByTeam team matches) ++ " gols."
    putStrLn "+----------------------------------------+"

--
-- Imprime o resultado de uma partida específica (RF5).
--
showResultByRoundAndTeam :: Match -> IO ()
showResultByRoundAndTeam match = do
    U.putStrLnColor "purple" "\n[RESULTADO DA PARTIDA]\n"
    putStrLn "+----------------------------------------+"
    U.putStrLnColor "blue" 
        $ "  " ++ homeTeam match ++ " | " ++ show (goalsHomeTeam match) ++ " x "
        ++ show (goalsAwayTeam match) ++ " | " ++ awayTeam match
    putStrLn "+----------------------------------------+"
    putStrLn $ M.getWinnerByRoundAndTeam match ++ U.reset

--
-- Imprime a pontuação de um time especificado (RF6).
--
showPointsByTeam :: Team -> [Match] -> IO ()
showPointsByTeam _ [] = putStrLn $ U.red ++ "Não há pontuação." ++ U.reset
showPointsByTeam team matches = do
    let filtered = M.filterByTeam team matches
    let points = M.getPointsByTeam team filtered
    putStrLn $ U.purple ++ "\n[PONTUAÇÃO DO TIME]\n" ++ U.reset
    putStrLn "+----------------------------------------+"
    U.putStrLnColor "blue" $ "  " ++ team ++ " possui " ++ show points ++ " pontos."
    putStrLn "+----------------------------------------+"

--
-- Imprime os três primeiros colocados (RF7).
--
showPodium :: IO ()
showPodium = do
    teams <- getTeamResult
    let result = sortTeamResult teams
    U.putStrLnColor "purple" "\n[PÓDIO DO CAMPEONATO]\n"
    putStrLn "+----------------------------------------+"
    U.putStrLnColor "green" 
        $  "  1º colocado: " ++ team (head result) ++ "\n"
        ++ "  2º colocado: " ++ team (result !! 1) ++ "\n"
        ++ "  3º colocado: " ++ team (result !! 2)
    putStrLn "+----------------------------------------+"

--
-- Imprime os três últimos colocados (RF8)
--
showLastPlaces :: IO ()
showLastPlaces = do
    teams <- getTeamResult
    let result = sortTeamResult teams
    U.putStrLnColor "purple" "\n[ÚLTIMOS COLOCADOS DO CAMPEONATO]\n"
    putStrLn "+----------------------------------------+"
    U.putStrLnColor "red" 
        $  "   8º colocado: " ++ team (result !! 7) ++ "\n"
        ++ "   9º colocado: " ++ team (result !! 8) ++ "\n"
        ++ "  10º colocado: " ++ team (result !! 9)
    putStrLn "+----------------------------------------+"

--
-- Imprime o resultado geral do campeonato (RF9).
--
showChampionshipResult :: IO ()
showChampionshipResult = do
    teams <- getTeamResult
    let rank = sortTeamResult teams
    putStrLn $ U.purple ++ "\n[RESULTADO DO CAMPEONATO]\n" ++ U.reset
    putStr U.blue
    printf "   \t\t\t   |  %2s  |  %2s  |  %2s  |  %2s  |  %2s  |  %3s  |  %2s  | %3s |\n" 
        "VI" "EM" "DE" "GP" "GC" "SG" "PT" "AP (%)"
    putStr U.reset
    formatResult 1 rank

--
-- Formata e organiza o resultado geral do campeonato (RF9).
--
formatResult :: Int -> [TeamResult] -> IO ()
formatResult _ [] = return ()
formatResult rank (team : teams) = do
    let color | rank >= 1 && rank <= 3 = putStr U.green 
              | rank >= 8 && rank <= 10 = putStr U.red
              | otherwise = putStr U.reset
    color
    formatTeam rank team
    putStr U.reset
    formatResult (rank + 1) teams 

--
-- Formata as informações de um determinado time (RF9).
--
formatTeam :: Int -> TeamResult -> IO ()
formatTeam rank t = do
    let size = length (team t)
    let tab | size >= 7 = printf "   %2dº - %s\t" 
            | otherwise = printf "   %2dº - %s\t\t"
    tab rank (team t)
    printf "   |  %2d  |  %2d  |  %2d  |  %2d  |  %2d  |  %3d  |  %2d  |  %.1f  |\n" 
        (wins t) (draws t) (losses t) (goals t) (goalsConceded t) (goalsDiff t) (points t) (record t)
