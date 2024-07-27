{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
                   ( FromField (..), FromNamedRecord (..), (.:), decodeByName )
import           Data.Either ( partitionEithers )
import           Data.Foldable ( toList )
import qualified Data.List as L
import qualified Data.List.Extra as L
import           Data.Maybe ( fromMaybe )
import           Data.Ord ( Down (..) )
import           Data.Vector ( Vector )

main :: IO ()
main = do
  bs <- BS.readFile "candidate-level-results-general-election-04-07-2024.csv"
  case decodeByName bs of
    Left msg -> putStrLn msg
    Right (_, electionRecords) -> case processElectionRecords electionRecords of
      Left msg -> putStrLn msg
      Right partyResults -> print partyResults

processElectionRecords :: Vector ElectionRecord -> Either String [PartyResult]
processElectionRecords electionRecords = do
  let electionResult = toElectionResult electionRecords
      altWinners' = map toAltWinningParty electionResult
      (ties, altWinners) = partitionEithers altWinners'
  case ties of
    [] -> Right $ printWinners altWinners
    _ -> Left $ printTies ties

toElectionResult :: Vector ElectionRecord -> ElectionResult
toElectionResult electionRecords = L.groupSort $ toList electionRecords'
 where
  electionRecords' = fmap electionRecord electionRecords

toAltWinningParty :: (Constituency, ConstituencyResult) -> Either String Party
toAltWinningParty (_, constituencyResult) = alternativeResult constituencyResult

printWinners :: [Party] -> [PartyResult]
printWinners = L.sortOn (Down . snd) . headcount . L.group . L.sort
 where
  headcount = map (\xs -> (head xs, length xs))

printTies :: [String] -> String
printTies ties = msg
 where
  msg = show (length ties) <> " of the results would have been ties."

newtype ElectionRecord =
  ElectionRecord { electionRecord :: (Constituency, Result) }

instance FromNamedRecord ElectionRecord where

  parseNamedRecord nr = do
    constituency <- nr .: "Constituency name"
    party <- nr .: "Main party abbreviation"
    votes <- nr .: "Candidate vote count"
    CsvBool isIndependent <- nr .: "Candidate is standing as independent"
    CsvBool isSpeaker <- nr .: "Candidate is standing as Commons Speaker"
    let party' = if isIndependent || isSpeaker then "N/A" else party
    pure $ ElectionRecord (constituency, (party', votes))

-- Type representing boolean values in the CVS file.
newtype CsvBool = CsvBool Bool

instance FromField CsvBool where

  parseField s
    | s == "true" = pure $ CsvBool True
    | s == "false" = pure $ CsvBool False
    | otherwise = fail $ "Expected true or false, but got " <> show s

-- Type synonym representing results of elections.
type ElectionResult = [(Constituency, ConstituencyResult)]

-- Type synonym representing results of a constituency.
type ConstituencyResult = [Result]

-- Type synonym representing results of a candidate.
type Result = (Party, Int)

-- Type synonym representing results of a party.
type PartyResult = (Party, Int)

-- Type synonym representing identities of constituencies.
type Constituency = String

-- Type synonym representing identities of parties.
type Party = String

reform :: Party
reform = "RUK"

conservative :: Party
conservative = "Con"

alternativeResult :: ConstituencyResult -> Either String Party
alternativeResult result
  | winner == conservative || reformVote > winnerMajorityOverConservative =
      Right conservative
  | reformVote == winnerMajorityOverConservative = Left "Result tied!"
  | otherwise = Right winner
 where
  reformVote = getVote reform result
  winner = getWinner result
  winnerVote = getVote winner result
  conservativeVote = getVote conservative result
  winnerMajorityOverConservative = winnerVote - conservativeVote

getVote :: Party -> ConstituencyResult -> Int
getVote p cr = fromMaybe 0 $ L.lookup p cr

getWinner :: ConstituencyResult -> Party
getWinner [] = error "getWinner: No results for constituency!"
getWinner result = (fst . L.head . L.sortOn (Down . snd)) result
