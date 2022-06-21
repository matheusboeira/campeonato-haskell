module Utils.AppUtils where

type Color = String
type Text = String

--- 
--- Limpa o console
---
cls :: IO ()
cls = putStr "\ESC[2J"

---
--- Cores do terminal
---
red :: String
green :: String
yellow :: String
blue :: String
purple :: String
cyan :: String
white :: String
brown :: String
gray :: String
reset :: String

---
--- Implementação das cores
---
gray = "\27[0;30m"
red = "\27[0;31m"
green = "\27[0;32m"
yellow = "\27[0;33m"
blue = "\27[0;34m"
purple = "\27[1;35m"
cyan = "\27[0;36m"
white = "\27[4;37m"
brown = "\27[22;3m"
reset = "\27[0;0m"

--
-- Função para imprimir algo na tela passando uma cor
-- como parâmetro. Esta não possui '\n' no final da linha.
--
putStrColor :: Color -> Text -> IO ()
putStrColor c t
    | c == "yellow" = putStr $ yellow ++ t ++ reset
    | c == "red" = putStr $ red ++ t ++ reset
    | c == "green" = putStr $ green ++ t ++ reset
    | c == "blue" = putStr $ blue ++ t ++ reset
    | c == "purple" = putStr $ purple ++ t ++ reset
    | c == "cyan" = putStr $ cyan ++ t ++ reset
    | c == "white" = putStr $ white ++ t ++ reset
    | c == "gray" = putStr $ gray ++ t ++ reset
    | otherwise = putStr t

--
-- Função para imprimir algo na tela passando uma cor
-- como parâmetro. Esta possui um '\n' no final da linha.
--
putStrLnColor :: Color -> Text -> IO ()
putStrLnColor c t =
    putStrColor c (t ++ "\n")