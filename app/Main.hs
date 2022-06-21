module Main where

import Views.Menu as Menu ( menu )
import Utils.AppUtils as Utils ( cls )

main :: IO ()
main = do
    Utils.cls
    Menu.menu
