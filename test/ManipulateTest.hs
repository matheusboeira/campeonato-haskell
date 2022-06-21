import Test.Hspec

import Text.Printf

import Championship.Manipulate

main :: IO ()
main = do 
    matches <- getMatches
    hspec $ do
        describe "RF1 - (Vitória, Empate, Derrota)" $ do
            it (printf "'%s' deve possuir (%s)" "Avai" "7, 5, 6") $ do
                (getTeamPerformance "Avai" matches == (7, 5, 6)) `shouldBe` True

        describe "RF2 - (Andamento...)" $ do
            it (printf "1 + 1 == 2") $ do
                (1 + 1 == 2) `shouldBe` True
