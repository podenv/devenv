#! /usr/bin/env nix-shell
#! nix-shell -i "runghc -package-env -" --packages "(import ./nixpkgs.nix).ghc.withPackages (x: [ x.simple-cmd ])"

-- | devenv test script
module Test where

import Data.List (dropWhileEnd, intercalate)
import Data.Tuple.Extra (uncurry3)
import SimpleCmd (cmdStderrToStdout)
import System.Environment (setEnv)

newtype Expected = Expected String

newtype Command = Command (String, [String])

testOutput :: String -> Command -> Expected -> IO Bool
testOutput name (Command (c, args)) (Expected expected) = do
  putStrLn ("* " <> name)
  (_, gotln) <- cmdStderrToStdout c args
  let got = dropWhileEnd (== '\n') gotln
      (result, value) =
        if got == expected
          then ("ok!", True)
          else ("failed! got:" <> got, False)
  putStrLn ("-> " <> result)
  pure value

shellCommand :: String -> String -> Command
shellCommand name command = Command ("nix-shell", args)
  where
    args =
      ["--arg", "withEmacs", "false"]
        <> ["--arg", name, "true"]
        <> ["--command", command]

tests :: [(String, Command, Expected)]
tests =
  [ ( "withHaskell provides hoogle",
      shellCommand "withHaskell" "hoogle --version",
      Expected "Hoogle 5.0.18, https://hoogle.haskell.org/"
    ),
    ( "withVSCode provides extensions",
      shellCommand "withVSCode" "code --list-extensions",
      Expected (intercalate "\n" ["freebroccolo.reasonml", "jaredly.reason-vscode", "ms-python.python"])
    )
  ]

main :: IO ()
main = do
  putStrLn "DevEnv test suite"
  setEnv "NIXPKGS_ALLOW_UNFREE" "1"
  mapM_ (uncurry3 testOutput) tests
