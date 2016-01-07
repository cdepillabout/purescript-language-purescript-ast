
module Language.PureScript.Operators where

import Language.PureScript.Crash (internalError)


type Precedence = Int

-- | Associativity for infix operators
data Associativity = Infixl | Infixr | Infix
-- deriving (Show, Read, Eq, Ord, D.Data, D.Typeable)

showAssoc :: Associativity -> String
showAssoc Infixl = "infixl"
showAssoc Infixr = "infixr"
showAssoc Infix  = "infix"

readAssoc :: String -> Associativity
readAssoc "infixl" = Infixl
readAssoc "infixr" = Infixr
readAssoc "infix"  = Infix
readAssoc _ = internalError "readAssoc: no parse"

-- instance A.ToJSON Associativity where
--   toJSON = A.toJSON . showAssoc

-- instance A.FromJSON Associativity where
--   parseJSON = fmap readAssoc . A.parseJSON

-- | Fixity data for infix operators
data Fixity = Fixity Associativity Precedence
-- deriving (Show, Read, Eq, Ord, D.Data, D.Typeable)

-- instance A.ToJSON Fixity where
--   toJSON (Fixity associativity precedence) =
--     A.object [ "associativity" .= associativity
--              , "precedence" .= precedence
--              ]
