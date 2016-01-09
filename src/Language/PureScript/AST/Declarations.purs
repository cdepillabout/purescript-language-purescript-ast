
module Language.PureScript.Declarations where

import Prelude

import Data.Array
import Data.Either
import Data.Maybe
import Data.Foldable
import Data.Tuple

import Language.PureScript.AST.Binders
import Language.PureScript.AST.Operators
import Language.PureScript.Comments
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.SourcePos
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

-- | A module declaration, consisting of comments about the module, a module name,
-- a list of declarations, and a list of the declarations that are
-- explicitly exported. If the export list is Nothing, everything is exported.
-- data Module = Module SourceSpan (Array Comment) ModuleName (Array Declaration) (Maybe (Array DeclarationRef))
-- deriving (Show, Read, D.Data, D.Typeable)

-- | Return a module's name.
-- getModuleName :: Module -> ModuleName
-- getModuleName (Module _ _ name _ _) = name

-- | Add an import declaration for a module if it does not already explicitly import it.
-- addDefaultImport :: ModuleName -> Module -> Module
-- addDefaultImport toImport m@(Module ss coms mn decls exps)  =
--   if isExistingImport `any` decls || mn == toImport then m
--   else Module ss coms mn (ImportDeclaration toImport Implicit Nothing False : decls) exps
--   where
--   isExistingImport (ImportDeclaration mn' _ _ _) | mn' == toImport = True
--   isExistingImport (PositionedDeclaration _ _ d) = isExistingImport d
--   isExistingImport _ = False

-- | An item in a list of explicit imports or exports
data DeclarationRef
  -- | A type constructor with data constructors
  = TypeRef ProperName (Maybe (Array ProperName))
  -- | A value
  | ValueRef Ident
  -- | A type class
  | TypeClassRef ProperName
    -- | A type class instance, created during typeclass desugaring (name, class name, instance types)
  | TypeInstanceRef Ident
  -- | A module, in its entirety
  | ModuleRef ModuleName
  -- | An unspecified ProperName ref. This will be replaced with a TypeClassRef
  | ProperRef ProperName
  -- | A declaration reference with source position information
  | PositionedDeclarationRef SourceSpan (Array Comment) DeclarationRef
-- deriving (Show, Read, D.Data, D.Typeable)
-- $(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''DeclarationRef)

instance eqDeclarationRef :: Eq DeclarationRef where
  eq (TypeRef name dctors)            (TypeRef name' dctors')           = name `eq` name' && dctors `eq` dctors'
  eq (ValueRef name)                  (ValueRef name')                  = name `eq` name'
  eq (TypeClassRef name)              (TypeClassRef name')              = name `eq` name'
  eq (TypeInstanceRef name)           (TypeInstanceRef name')           = name `eq` name'
  eq (ModuleRef name)                 (ModuleRef name')                 = name `eq` name'
  eq (ProperRef name)                 (ProperRef name')                 = name `eq` name'
  eq (PositionedDeclarationRef _ _ r) r'                                = r    `eq` r'
  eq r                                (PositionedDeclarationRef _ _ r') = r    `eq` r'
  eq _                                _                                 = false

isModuleRef :: DeclarationRef -> Boolean
isModuleRef (PositionedDeclarationRef _ _ r) = isModuleRef r
isModuleRef (ModuleRef _) = true
isModuleRef _ = false

-- | Finds duplicate values in a list of declaration refs. The returned values
-- are the duplicate refs with data constructors elided, and then a separate
-- list of duplicate data constructors.
findDuplicateRefs :: Array DeclarationRef -> Tuple (Array DeclarationRef) (Array ProperName)
findDuplicateRefs refs =
    let positionless = map stripPosInfo refs
        simplified = map simplifyTypeRefs positionless
        dupeRefs = nub $ simplified \\ nub simplified
        dupeCtors = concat $ flip mapMaybe positionless $ \decRef ->
            case decRef of
                TypeRef _ (Just dctors) ->
                    let dupes = dctors \\ nub dctors
                    in if null dupes then Nothing else Just dupes
                _ -> Nothing
    in Tuple dupeRefs dupeCtors
  where
    stripPosInfo :: DeclarationRef -> DeclarationRef
    stripPosInfo (PositionedDeclarationRef _ _ ref) = stripPosInfo ref
    stripPosInfo other = other

    simplifyTypeRefs :: DeclarationRef -> DeclarationRef
    simplifyTypeRefs (TypeRef pn _) = TypeRef pn Nothing
    simplifyTypeRefs other = other

-- | The data type which specifies type of import declaration
data ImportDeclarationType
  -- | An import with no explicit list: `import M`.
  = Implicit
  -- | An import with an explicit list of references to import: `import M (foo)`
  | Explicit (Array DeclarationRef)
  -- | An import with a list of references to hide: `import M hiding (foo)`
  | Hiding (Array DeclarationRef)
-- deriving (Eq, Show, Read, D.Data, D.Typeable)
-- $(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''ImportDeclarationType)

isImplicit :: ImportDeclarationType -> Boolean
isImplicit Implicit = true
isImplicit _ = false

-- -- | The data type of declarations
-- data Declaration
--   -- | A data type declaration (data or newtype, name, arguments, data constructors)
--   = DataDeclaration DataDeclType ProperName (Array (Tuple String (Maybe Kind))) (Array (Tuple ProperName (Array Type)))
--   -- | A minimal mutually recursive set of data type declarations
--   | DataBindingGroupDeclaration (Array Declaration)
--   -- | A type synonym declaration (name, arguments, type)
--   | TypeSynonymDeclaration ProperName (Array (Tuple String (Maybe Kind))) Type
--   -- | A type declaration for a value (name, ty)
--   | TypeDeclaration Ident Type
--   -- | A value declaration (name, top-level binders, optional guard, value)
--   | ValueDeclaration Ident NameKind (Array Binder) (Either (Array (Tuple Guard Expr)) Expr)
--   -- | A minimal mutually recursive set of value declarations
--   | BindingGroupDeclaration (Array { ident :: Ident, nameKind :: NameKind, expr :: Expr })
--   -- | A foreign import declaration (name, type)
--   | ExternDeclaration Ident Type
--   -- | A data type foreign import (name, kind)
--   | ExternDataDeclaration ProperName Kind
--   -- | A fixity declaration (fixity data, operator name, value the operator is an alias for)
--   | FixityDeclaration Fixity String (Maybe Ident)
--   -- | A module import (module name, qualified/unqualified/hiding, optional "qualified as" name)
--   -- TODO: also a boolean specifying whether the old `qualified` syntax was
--   -- used, so a warning can be raised in desugaring (remove for 0.9)
--   | ImportDeclaration ModuleName ImportDeclarationType (Maybe ModuleName) Bool
--   -- | A type class declaration (name, argument, implies, member declarations)
--   | TypeClassDeclaration ProperName (Array (Tuple String (Maybe Kind))) (Array Constraint) (Array Declaration)
--   -- | A type instance declaration (name, dependencies, class name, instance types, member
--   -- declarations)
--   | TypeInstanceDeclaration Ident (Array Constraint) (Qualified ProperName) (Array Type) TypeInstanceBody
--   -- | A declaration with source position information
--   | PositionedDeclaration SourceSpan (Array Comment) Declaration
-- -- deriving (Show, Read, D.Data, D.Typeable)

-- -- | The members of a type class instance declaration
-- data TypeInstanceBody
--   -- | This is a derived instance
--   = DerivedInstance
--   -- | This is a regular (explicit) instance
--   | ExplicitInstance (Array Declaration)
-- -- deriving (Show, Read, D.Data, D.Typeable)

-- mapTypeInstanceBody :: ([Declaration] -> [Declaration]) -> TypeInstanceBody -> TypeInstanceBody
-- mapTypeInstanceBody f = runIdentity . traverseTypeInstanceBody (Identity . f)

-- -- | A traversal for TypeInstanceBody
-- traverseTypeInstanceBody :: (Applicative f) => ([Declaration] -> f [Declaration]) -> TypeInstanceBody -> f TypeInstanceBody
-- traverseTypeInstanceBody _ DerivedInstance = pure DerivedInstance
-- traverseTypeInstanceBody f (ExplicitInstance ds) = ExplicitInstance <$> f ds

-- -- |
-- -- Test if a declaration is a value declaration
-- --
-- isValueDecl :: Declaration -> Bool
-- isValueDecl ValueDeclaration{} = True
-- isValueDecl (PositionedDeclaration _ _ d) = isValueDecl d
-- isValueDecl _ = False

-- -- |
-- -- Test if a declaration is a data type or type synonym declaration
-- --
-- isDataDecl :: Declaration -> Bool
-- isDataDecl DataDeclaration{} = True
-- isDataDecl TypeSynonymDeclaration{} = True
-- isDataDecl (PositionedDeclaration _ _ d) = isDataDecl d
-- isDataDecl _ = False

-- -- |
-- -- Test if a declaration is a module import
-- --
-- isImportDecl :: Declaration -> Bool
-- isImportDecl ImportDeclaration{} = True
-- isImportDecl (PositionedDeclaration _ _ d) = isImportDecl d
-- isImportDecl _ = False

-- -- |
-- -- Test if a declaration is a data type foreign import
-- --
-- isExternDataDecl :: Declaration -> Bool
-- isExternDataDecl ExternDataDeclaration{} = True
-- isExternDataDecl (PositionedDeclaration _ _ d) = isExternDataDecl d
-- isExternDataDecl _ = False

-- -- |
-- -- Test if a declaration is a fixity declaration
-- --
-- isFixityDecl :: Declaration -> Bool
-- isFixityDecl FixityDeclaration{} = True
-- isFixityDecl (PositionedDeclaration _ _ d) = isFixityDecl d
-- isFixityDecl _ = False

-- -- |
-- -- Test if a declaration is a foreign import
-- --
-- isExternDecl :: Declaration -> Bool
-- isExternDecl ExternDeclaration{} = True
-- isExternDecl (PositionedDeclaration _ _ d) = isExternDecl d
-- isExternDecl _ = False

-- -- |
-- -- Test if a declaration is a type class instance declaration
-- --
-- isTypeClassInstanceDeclaration :: Declaration -> Bool
-- isTypeClassInstanceDeclaration TypeInstanceDeclaration{} = True
-- isTypeClassInstanceDeclaration (PositionedDeclaration _ _ d) = isTypeClassInstanceDeclaration d
-- isTypeClassInstanceDeclaration _ = False

-- -- |
-- -- Test if a declaration is a type class declaration
-- --
-- isTypeClassDeclaration :: Declaration -> Bool
-- isTypeClassDeclaration TypeClassDeclaration{} = True
-- isTypeClassDeclaration (PositionedDeclaration _ _ d) = isTypeClassDeclaration d
-- isTypeClassDeclaration _ = False

-- -- |
-- -- Recursively flatten data binding groups in the list of declarations
-- flattenDecls :: [Declaration] -> [Declaration]
-- flattenDecls = concatMap flattenOne
--     where flattenOne :: Declaration -> [Declaration]
--           flattenOne (DataBindingGroupDeclaration decls) = concatMap flattenOne decls
--           flattenOne d = [d]

-- | A guard is just a boolean-valued expression that appears alongside a set of binders
-- type Guard = Expr

-- | Data type for expressions and terms
data Expr
  -- | A numeric literal
  = NumericLiteral (Either Int Number)
  -- | A string literal
  | StringLiteral String
  -- | A character literal
  | CharLiteral Char
  -- | A boolean literal
  | BooleanLiteral Boolean
  -- | A prefix -, will be desugared
  | UnaryMinus Expr
  -- | Binary operator application. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  | BinaryNoParens Expr Expr Expr
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this data constructor
  -- will be removed.
  | Parens Expr
  -- | Operator section. This will be removed during desugaring and replaced with a partially applied
  -- operator or lambda to flip the arguments.
  | OperatorSection Expr (Either Expr Expr)
  -- | An array literal
  | ArrayLiteral (Array Expr)
  -- | An object literal
  | ObjectLiteral (Array (Tuple String Expr))
  -- | An object constructor (object literal with underscores). This will be removed during
  -- desugaring and expanded into a lambda that returns an object literal.
  | ObjectConstructor (Array (Tuple String (Maybe Expr)))
  -- | An object property getter (e.g. `_.x`). This will be removed during
  -- desugaring and expanded into a lambda that reads a property from an object.
  | ObjectGetter String
  -- | An record property accessor expression
  | Accessor String Expr
  -- | Partial record update
  | ObjectUpdate Expr (Array (Tuple String Expr))
  -- | Partial record updater. This will be removed during desugaring and
  -- expanded into a lambda that returns an object update.
  | ObjectUpdater (Maybe Expr) (Array (Tuple String (Maybe Expr)))
  -- | Function introduction
  | Abs (Either Ident Binder) Expr
  -- | Function application
  | App Expr Expr
  -- | Variable
  | Var (Qualified Ident)
  -- | Conditional (if-then-else expression)
  | IfThenElse Expr Expr Expr
  -- | A data constructor
  | Constructor (Qualified ProperName)
  -- | A case expression. During the case expansion phase of desugaring, top-level binders will get
  -- desugared into case expressions, hence the need for guards and multiple binders per branch here.
  | Case (Array Expr) (Array CaseAlternative)
  -- | A value with a type annotation
  | TypedValue Bool Expr Type
  -- | A let binding
  | Let (Array Declaration) Expr
  -- | A do-notation block
  | Do (Array DoNotationElement)
  -- | An application of a typeclass dictionary constructor. The value should be
  -- an ObjectLiteral.
  | TypeClassDictionaryConstructorApp (Qualified ProperName) Expr
  -- | A placeholder for a type class dictionary to be inserted later. At the end of type checking, these
  -- placeholders will be replaced with actual expressions representing type classes dictionaries which
  -- can be evaluated at runtime. The constructor arguments represent (in order): whether or not to look
  -- at superclass implementations when searching for a dictionary, the type class name and
  -- instance type, and the type class dictionaries in scope.
  | TypeClassDictionary Constraint (Map (Maybe ModuleName)
                                        (Map (Qualified ProperName)
                                             (Map (Qualified Ident)
                                                  TypeClassDictionaryInScope)))
  -- | A typeclass dictionary accessor, the implementation is left unspecified until CoreFn desugaring.
  | TypeClassDictionaryAccessor (Qualified ProperName) Ident
  -- | A placeholder for a superclass dictionary to be turned into a TypeClassDictionary during typechecking
  | SuperClassDictionary (Qualified ProperName) (Array Type)
  -- | A value with source position information
  | PositionedValue SourceSpan (Array Comment) Expr
-- deriving (Show, Read, D.Data, D.Typeable)

-- |An alternative in a case statement
newtype CaseAlternative = CaseAlternative
  { -- | A collection of binders with which to match the inputs
    caseAlternativeBinders :: Array Binder
    -- | The result expression or a collect of guarded expressions
  , caseAlternativeResult :: Either (Array (Tuple Guard Expr)) Expr
  }
-- deriving (Show, Read, D.Data, D.Typeable)

-- | A statement in a do-notation block
-- data DoNotationElement
--   -- | A monadic value without a binder
--   = DoNotationValue Expr
--   -- | A monadic value with a binder
--   | DoNotationBind Binder Expr
--   -- | A let statement, i.e. a pure value with a binder
--   | DoNotationLet (Array Declaration)
--   -- | A do notation element with source position information
--   | PositionedDoNotationElement SourceSpan (Array Comment) DoNotationElement
-- deriving (Show, Read, D.Data, D.Typeable)

