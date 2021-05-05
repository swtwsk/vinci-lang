module ManglingPrefixes where

frontendToCoreVarPrefix :: String
frontendToCoreVarPrefix = "&"

coreToCPSContPrefix :: String
coreToCPSContPrefix = "_k"

coreToCPSVarPrefix :: String
coreToCPSVarPrefix = "_x"

cpsToSsaVarPrefix :: String
cpsToSsaVarPrefix = "$_x_"

ssaToSpirLabelPrefix :: String
ssaToSpirLabelPrefix = "@"
