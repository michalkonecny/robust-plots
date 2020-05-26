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
  , "rationals"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
