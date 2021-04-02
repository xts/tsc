{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Data.Text

main :: IO ()
main = case parse "1" of
  Left err  -> putStrLn err
  Right ast -> putStrLn $ show ast
