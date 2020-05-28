{ name = "robust-plots"
, dependencies = 
  [ "halogen"
  , "bigints"
  , "canvas"
  , "console"
  , "effect"
  , "lists"
  , "psci-support"
  , "test-unit" 
  , "decimals"
  , "parsing"
  , "rationals"
  , "quickcheck-combinators"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
