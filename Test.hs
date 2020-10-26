#! /usr/bin/env nix-shell
#! nix-shell -i runghc --packages "(import ./nixpkgs.nix).ghc.withPackages (x: [ x.simple-cmd ])"

-- | devenv test script
module Test where

import Data.List (dropWhileEnd)
import Data.Tuple.Extra (uncurry3)
import SimpleCmd (cmdStderrToStdout)

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
    )
  ]

main :: IO ()
main = do
  putStrLn "DevEnv test suite"
  mapM_ (uncurry3 testOutput) tests
