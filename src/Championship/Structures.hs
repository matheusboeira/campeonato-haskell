module Championship.Structures where

---
--- Criação de uma "struct" responsável em armazenar informações
--- sobre uma determinada partida.
---
data Match = Match {
    _round :: Integer,
    homeTeam :: String,
    goalsHomeTeam :: Integer,
    goalsAwayTeam :: Integer,
    awayTeam :: String
} deriving (Show)

---
--- Criação de uma "struct" responsável em armazenar informações
--- sobre um determinado time.
---
data TeamResult = TeamResult {
    team :: String,
    goals :: Integer,
    wins :: Integer,
    draws :: Integer,
    losses :: Integer,
    points :: Integer,
    record :: Float,
    goalsDiff :: Integer,
    goalsConceded :: Integer
} deriving (Show, Eq, Ord)
