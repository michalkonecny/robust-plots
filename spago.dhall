{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "robust-plots"
, dependencies = [ "bigints", "canvas", "console", "effect", "lists", "psci-support", "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
