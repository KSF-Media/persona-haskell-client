# Auto-Generated OpenAPI Bindings to `Persona`

The library in `lib` provides auto-generated-from-OpenAPI bindings to the Persona API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

## Main Interface

The main interface to this library is in the `Persona.API` module, which exports the PersonaBackend type. The PersonaBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createPersonaClient` function, which, if provided with a hostname and a port, will generate
a client that can be used to access the API if it is being served at that hostname / port combination. For example, if
`localhost:8080` is serving the Persona API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import Persona.API

main :: IO ()
main = do
  PersonaBackend{..} <- createPersonaClient (ServerConfig "localhost" 8080)
  -- Any Persona API call can go here.
  return ()
```

## Creating a Server

In order to create a server, you must use the `runPersonaServer` function. However, you unlike the client, in which case you *got* a `PersonaBackend`
from the library, you must instead *provide* a `PersonaBackend`. For example, if you have defined handler functions for all the
functions in `Persona.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import Persona.API

-- A module you wrote yourself, containing all handlers needed for the PersonaBackend type.
import Persona.Handlers

-- Run a Persona server on localhost:8080
main :: IO ()
main = do
  let server = PersonaBackend{..}
  runPersonaServer (ServerConfig "localhost" 8080) server
```

You could use `optparse-applicative` or a similar library to read the host and port from command-line arguments:
```
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Persona.API (runPersonaServer, PersonaBackend(..), ServerConfig(..))

import Control.Applicative ((<$>), (<*>))
import Options.Applicative (execParser, option, str, auto, long, metavar, help)

main :: IO ()
main = do
  config <- parseArguments
  runPersonaServer config PersonaBackend{}

-- | Parse host and port from the command line arguments.
parseArguments :: IO ServerConfig
parseArguments =
  execParser $
    ServerConfig
      <$> option str  (long "host" <> metavar "HOST" <> help "Host to serve on")
      <*> option auto (long "port" <> metavar "PORT" <> help "Port to serve on")
```
