module Main where
import Test.Tasty
import Chapter1Spec

tests = 
  testGroup "Chapter 1:" [chapter1Quick]

main = do defaultMain tests