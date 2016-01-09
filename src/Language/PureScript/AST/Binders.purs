
module Language.PureScript.AST.Binders where

import Prelude (map)

import Data.Array ((:))
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Tuple (Tuple, snd)

import Language.PureScript.Comments (Comment)
import Language.PureScript.Names (Ident, ProperName, Qualified)
import Language.PureScript.SourcePos (SourceSpan)
import Language.PureScript.Types (Type)

-- | Data type for binders
data Binder
  -- | Wildcard binder
  = NullBinder
  -- | A binder which matches a boolean literal
  | BooleanBinder Boolean
  -- | A binder which matches a string literal
  | StringBinder String
  -- | A binder which matches a character literal
  | CharBinder Char
  -- | A binder which matches a numeric literal
  | NumberBinder (Either Int Number)
  -- | A binder which binds an identifier
  | VarBinder Ident
  -- | A binder which matches a data constructor
  | ConstructorBinder (Qualified ProperName) (Array Binder)
  -- | A binder which matches a record and binds its properties
  | ObjectBinder (Array (Tuple String Binder))
  -- | A binder which matches an array and binds its elements
  | ArrayBinder (Array Binder)
  -- | A binder which binds its input to an identifier
  | NamedBinder Ident Binder
  -- | A binder with source position information
  | PositionedBinder SourceSpan (Array Comment) Binder
  -- | A binder with a type annotation
  | TypedBinder Type Binder
-- deriving (Show, Read, Eq, D.Data, D.Typeable)

-- | Collect all names introduced in binders in an expression
binderNames :: Binder -> Array Ident
binderNames = go []
  where
  go ns (VarBinder name) = name : ns
  go ns (ConstructorBinder _ bs) = foldl go ns bs
  go ns (ObjectBinder bs) = foldl go ns (map snd bs)
  go ns (ArrayBinder bs) = foldl go ns bs
  go ns (NamedBinder name b) = go (name : ns) b
  go ns (PositionedBinder _ _ b) = go ns b
  go ns (TypedBinder _ b) = go ns b
  go ns _ = ns
