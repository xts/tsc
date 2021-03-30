{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Data.Text

main :: IO ()
main = show (parse "1")
