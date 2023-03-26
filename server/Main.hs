{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Network.Wai
import Servant
import Text.TeXMath
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Text.XML.Light (ppElement)
import Options.Applicative
import Safe (readMay)

-- This is the data to be supplied by the JSON payload
-- of requests.
data Params = Params
  { text           :: Text
  , from           :: Format
  , to             :: Format
  , display        :: Bool
  } deriving (Show)

data Format =
  TeX | MathML | Eqn | OMML | Typst
  deriving (Show, Ord, Eq)

instance FromJSON Format where
   parseJSON (String s) =
               case T.toLower s of
                 "tex" -> pure TeX
                 "mathml" -> pure MathML
                 "eqn" -> pure Eqn
                 "typst" -> pure Typst
                 "omml" -> pure OMML
                 _ -> fail $ "Unknown format " <> T.unpack s
   parseJSON _ = fail "Expecting string format"

instance ToJSON Format where
   toJSON x = String $ T.toLower $ T.pack $ show x

instance FromHttpApiData Format where
   parseQueryParam t =
               case T.toLower t of
                 "tex" -> pure TeX
                 "mathml" -> pure MathML
                 "eqn" -> pure Eqn
                 "typst" -> pure Typst
                 "omml" -> pure OMML
                 _ -> Left $ "Unknown format " <> t

-- Automatically derive code to convert to/from JSON.
$(deriveJSON defaultOptions ''Params)
data Opts = Opts
  { port      :: Int }

optsSpec :: Parser Opts
optsSpec = Opts
      <$> option (maybeReader readMay)
          ( long "port"
         <> short 'p'
         <> metavar "NUMBER"
         <> showDefault
         <> value 8080
         <> help "Port on which to run the server" )

main :: IO ()
main = do
  let options = info (optsSpec <**> helper)
        ( fullDesc
       <> progDesc "Run a server for texmath"
       <> header "texmath-server - an HTTP server for texmath" )
  opts <- execParser options
  putStrLn $ "Starting server on port " <> show (port opts)
  withStdoutLogger $ \logger -> do
    let settings = setPort (port opts) $ setLogger logger defaultSettings
    runSettings settings app
-- This is the API.  The "/convert" endpoint takes a request body
-- consisting of a JSON-encoded Params structure and responds to
-- Get requests with either plain text or JSON, depending on the
-- Accept header.
type API =
  "convert" :> ReqBody '[JSON] Params :> Post '[PlainText, JSON] Text
  :<|>
  "convert" :> QueryParam "text" Text :> QueryParam "from" Format :> QueryParam "to" Format :> QueryFlag "display" :> Get '[PlainText] Text
  :<|>
  "convert-batch" :> ReqBody '[JSON] [Params] :> Post '[JSON] [Text]

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = convert
    :<|> (\text' from' to' display' ->
             convert Params{ text = fromMaybe "" text',
                             from = fromMaybe TeX from',
                             to = fromMaybe MathML to',
                             display = display' })
    :<|> mapM convert
 where
  convert params
    = let dt = if display params
                  then DisplayBlock
                  else DisplayInline
          txt = text params
          reader = case from params of
                     OMML -> readOMML
                     TeX -> readTeX
                     MathML -> readMathML
                     Eqn -> \_ -> Left "eqn reader not implemented"
                     Typst -> \_ -> Left "typst reader not implemented"
          writer = case to params of
                     Eqn -> writeEqn dt
                     Typst -> writeTypst dt
                     OMML -> T.pack . ppElement . writeOMML dt
                     TeX -> writeTeX
                     MathML -> T.pack . ppElement . writeMathML dt
      in  handleErr $ writer <$> reader txt

  handleErr (Right t) = return t
  handleErr (Left err) = throwError $
    err500 { errBody = TLE.encodeUtf8 $ TL.fromStrict err }

