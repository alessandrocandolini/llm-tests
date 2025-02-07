module Args where

import Options.Applicative

data Args = Args Flags Command deriving (Eq, Show)

data Exercise = BouncingBalls deriving (Eq, Show)
data Model = O3High deriving (Eq, Show)

data Command
  = Run Exercise Model
  | Doctor
  deriving (Eq, Show)

data Verbosity = Verbose | NotVerbose deriving (Eq, Show)

newtype Flags = Flags Verbosity deriving (Eq, Show)

argsParser :: Parser Args
argsParser = Args <$> flags <*> commands

verbosity :: Parser Verbosity
verbosity =
  flag
    NotVerbose
    Verbose
    (long "verbose" <> short 'v' <> help "Enable verbose output")

flags :: Parser Flags
flags = Flags <$> verbosity

parseModel :: ReadM Model
parseModel = eitherReader $ \arg -> case arg of
  "o3-high" -> Right O3High
  _ -> Left $ "Invalid model: " ++ arg

parseExercise :: ReadM Exercise
parseExercise = eitherReader $ \arg -> case arg of
  "bouncing-balls" -> Right BouncingBalls
  _ -> Left $ "Invalid exercise: " ++ arg

runParser :: Parser Command
runParser =
  Run
    <$> option
      parseExercise
      ( long "exercise"
          <> short 'e'
          <> metavar "EXERCISE"
          <> help "Select the exercise (e.g. bouncing-balls)"
      )
    <*> option
      parseModel
      ( long "model"
          <> short 'm'
          <> metavar "MODEL"
          <> help "Select the model (e.g. o3-high)"
      )

commands :: Parser Command
commands =
  hsubparser
    ( command "doctor" (info (pure Doctor) (progDesc "check the CLI is working"))
        <> command "run" (info runParser (progDesc "check the CLI is working"))
    )

withInfo :: Parser a -> String -> ParserInfo a
withInfo p s = info (helper <*> p) (fullDesc <> progDesc s)

parseArgs :: [String] -> ParserResult Args
parseArgs = execParserPure preferences parserInfo
 where
  parserInfo = withInfo argsParser "CLI title"
  preferences = prefs (disambiguate <> showHelpOnEmpty <> showHelpOnError)
