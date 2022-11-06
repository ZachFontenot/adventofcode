{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SolveDay where

import Control.Exception (SomeException, catch)
import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (liftIO),
    runExceptT,
    when,
  )
import Cookie (getInput)
import Data.Attoparsec.Text.Lazy (Parser, parseOnly)
import Data.Functor (($>))
import Data.Time (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

data LogLevel = Quiet | Debug deriving (Show, Eq)

type Day = LogLevel -> String -> String -> IO (Maybe Double, Maybe Double)

solveDay :: (Show a, Show b, Show i) => Parser i -> (i -> a) -> (i -> b) -> Day
solveDay inputParser partA partB loglevel year day = do
  input <- runExceptT $ do
    fileContents <- liftIO $ Cookie.getInput year day
    case parseOnly inputParser fileContents of
      Left e -> throwError $ "Parser failed to read input. Error:\n" ++ e
      Right i -> do
        when (loglevel == Debug) $ do
          liftIO $ putStrLn "Parser output:"
          liftIO $ print i
        return i

  case input of
    Left x -> putStrLn x >> return (Nothing, Nothing)
    Right i -> do
      putStrLn "Part A:"
      time1 <- getCurrentTime
      successA <- catch (print (partA i) $> True) $
        \(m :: SomeException) -> do
          putStrLn "Couldn't run Part A!"
          when (loglevel == Debug) $ print m
          return False
      time2 <- getCurrentTime

      let timeA = realToFrac $ diffUTCTime time2 time1
      when successA $ putStrLn $ printf "(%.2f)" timeA

      putStrLn "Part B:"
      successB <- catch (print (partB i) $> True) $
        \(m :: SomeException) -> do
          putStrLn "Couldn't run Part B!"
          when (loglevel == Debug) $ print m
          return False
      time3 <- getCurrentTime

      let timeB = realToFrac $ diffUTCTime time3 time2
      when successB $ putStrLn $ printf "(%.2f)" timeB

      return $
        (,)
          (if successA then Just timeA else Nothing)
          (if successB then Just timeB else Nothing)
