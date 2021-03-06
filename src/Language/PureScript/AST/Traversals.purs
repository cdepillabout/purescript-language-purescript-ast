
module Language.PureScript.AST.Traversals where

import Prelude
    ( class Monad, class Applicative, class Functor, bind, const, append, map
    , (<<<), ($), (<>), (>>>), (#), (<$>), (<*>), pure, return, (++), (>>=)
    )

import Control.Bind ((<=<))
import Data.Array (concatMap, mapMaybe)
import Data.Either (Either(Left, Right))
import Data.Foldable (mconcat, foldMap, fold, foldl)
import Data.List (toList)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Profunctor.Choice ((+++))
import Data.Profunctor.Strong ((***), second)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (Accum, mapAccumL, traverse)
import Data.Tuple (Tuple(Tuple), snd, uncurry)

import Language.PureScript.AST.Binders
    ( Binder ( TypedBinder, PositionedBinder, NamedBinder, ArrayBinder
             , ObjectBinder, ConstructorBinder
             )
    , binderNames
    )

import Language.PureScript.AST.Declarations
    ( CaseAlternative(CaseAlternative)
    , Declaration ( TypeDeclaration, TypeSynonymDeclaration
                  , TypeInstanceDeclaration, TypeClassDeclaration
                  , ExternDeclaration, DataDeclaration, ValueDeclaration
                  , PositionedDeclaration, BindingGroupDeclaration
                  , DataBindingGroupDeclaration
                  )
    , DoNotationElement ( PositionedDoNotationElement, DoNotationLet
                        , DoNotationBind, DoNotationValue
                        )
    , Expr ( TypedValue, SuperClassDictionary, TypeClassDictionary
           , PositionedValue, Do, Let, Case, IfThenElse, App, Abs
           , ObjectUpdater, ObjectUpdate, Accessor
           , TypeClassDictionaryConstructorApp, ObjectConstructor
           , ObjectLiteral, ArrayLiteral, OperatorSection, Parens
           , BinaryNoParens, UnaryMinus
           )
    , TypeInstanceBody(ExplicitInstance), traverseTypeInstanceBody
    , mapTypeInstanceBody
    )

import Language.PureScript.Names (Ident)
import Language.PureScript.Traversals (pairM, eitherM, sndM, maybeM)
import Language.PureScript.Types (Type)

everywhereOnValues :: (Declaration -> Declaration) ->
                      (Expr -> Expr) ->
                      (Binder -> Binder) ->
                      { decl :: Declaration -> Declaration
                      , expr :: Expr -> Expr
                      , binder :: Binder -> Binder
                      }
everywhereOnValues f g h =
    { decl: f'
    , expr: g'
    , binder: h'
    }
  where
    f' :: Declaration -> Declaration
    f' (DataBindingGroupDeclaration ds) = f (DataBindingGroupDeclaration (map f' ds))
    f' (ValueDeclaration name nameKind bs val) =
        f (ValueDeclaration name nameKind (map h' bs) ((map (g' *** g') +++ g') val))
    f' (BindingGroupDeclaration ds) =
        f (BindingGroupDeclaration (map (\{ident: name, nameKind: nameKind, expr: val} -> {ident: name, nameKind: nameKind, expr: g' val}) ds))
    f' (TypeClassDeclaration name args implies ds) =
        f (TypeClassDeclaration name args implies (map f' ds))
    f' (TypeInstanceDeclaration name cs className args ds) =
        f (TypeInstanceDeclaration name cs className args (mapTypeInstanceBody (map f') ds))
    f' (PositionedDeclaration pos com d) = f (PositionedDeclaration pos com (f' d))
    f' other = f other

    g' :: Expr -> Expr
    g' (UnaryMinus v) = g (UnaryMinus (g' v))
    g' (BinaryNoParens op v1 v2) = g (BinaryNoParens (g' op) (g' v1) (g' v2))
    g' (Parens v) = g (Parens (g' v))
    g' (OperatorSection op (Left v)) = g (OperatorSection (g' op) (Left $ g' v))
    g' (OperatorSection op (Right v)) = g (OperatorSection (g' op) (Right $ g' v))
    g' (ArrayLiteral vs) = g (ArrayLiteral (map g' vs))
    g' (ObjectLiteral vs) = g (ObjectLiteral (map (map g') vs))
    g' (ObjectConstructor vs) = g (ObjectConstructor (map (second (map g')) vs))
    g' (TypeClassDictionaryConstructorApp name v) = g (TypeClassDictionaryConstructorApp name (g' v))
    g' (Accessor prop v) = g (Accessor prop (g' v))
    g' (ObjectUpdate obj vs) = g (ObjectUpdate (g' obj) (map (map g') vs))
    g' (ObjectUpdater obj vs) = g (ObjectUpdater (map g' obj) (map (second (map g')) vs))
    g' (Abs name v) = g (Abs name (g' v))
    g' (App v1 v2) = g (App (g' v1) (g' v2))
    g' (IfThenElse v1 v2 v3) = g (IfThenElse (g' v1) (g' v2) (g' v3))
    g' (Case vs alts) = g (Case (map g' vs) (map handleCaseAlternative alts))
    g' (TypedValue check v ty) = g (TypedValue check (g' v) ty)
    g' (Let ds v) = g (Let (map f' ds) (g' v))
    g' (Do es) = g (Do (map handleDoNotationElement es))
    g' (PositionedValue pos com v) = g (PositionedValue pos com (g' v))
    g' other = g other

    h' :: Binder -> Binder
    h' (ConstructorBinder ctor bs) = h (ConstructorBinder ctor (map h' bs))
    h' (ObjectBinder bs) = h (ObjectBinder (map (map h') bs))
    h' (ArrayBinder bs) = h (ArrayBinder (map h' bs))
    h' (NamedBinder name b) = h (NamedBinder name (h' b))
    h' (PositionedBinder pos com b) = h (PositionedBinder pos com (h' b))
    h' (TypedBinder t b) = h (TypedBinder t (h' b))
    h' other = h other

    handleCaseAlternative :: CaseAlternative -> CaseAlternative
    handleCaseAlternative (CaseAlternative ca) =
        CaseAlternative $
            ca { caseAlternativeBinders = map h' ca.caseAlternativeBinders
               , caseAlternativeResult = (map (g' *** g') +++ g') ca.caseAlternativeResult
               }

    handleDoNotationElement :: DoNotationElement -> DoNotationElement
    handleDoNotationElement (DoNotationValue v) = DoNotationValue (g' v)
    handleDoNotationElement (DoNotationBind b v) = DoNotationBind (h' b) (g' v)
    handleDoNotationElement (DoNotationLet ds) = DoNotationLet (map f' ds)
    handleDoNotationElement (PositionedDoNotationElement pos com e) =
        PositionedDoNotationElement pos com (handleDoNotationElement e)

everywhereOnValuesTopDownM :: forall m
                            . (Functor m, Applicative m, Monad m)
                           => (Declaration -> m Declaration)
                           -> (Expr -> m Expr)
                           -> (Binder -> m Binder)
                           -> { decl :: Declaration -> m Declaration
                              , expr :: Expr -> m Expr
                              , binder :: Binder -> m Binder
                              }
everywhereOnValuesTopDownM f g h =
    { decl: f' <=< f
    , expr: g' <=< g
    , binder: h' <=< h
    }
  where
    f' :: Declaration -> m Declaration
    f' (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f' <=< f) ds
    f' (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> traverse (h' <=< h) bs <*> eitherM (traverse (pairM (g' <=< g) (g' <=< g))) (g' <=< g) val
    f' (BindingGroupDeclaration ds) =
        BindingGroupDeclaration <$>
            traverse (\{ident: name, nameKind: nameKind, expr: val} -> {ident: name, nameKind: nameKind, expr: _} <$> (g val >>= g')) ds
    f' (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> traverse (f' <=< f) ds
    f' (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f' <=< f)) ds
    f' (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> (f d >>= f')
    f' other = f other

    g' :: Expr -> m Expr
    g' (UnaryMinus v) = UnaryMinus <$> (g v >>= g')
    g' (BinaryNoParens op v1 v2) = BinaryNoParens <$> (g op >>= g') <*> (g v1 >>= g') <*> (g v2 >>= g')
    g' (Parens v) = Parens <$> (g v >>= g')
    g' (OperatorSection op (Left v)) = OperatorSection <$> (g op >>= g') <*> (Left <$> (g v >>= g'))
    g' (OperatorSection op (Right v)) = OperatorSection <$> (g op >>= g') <*> (Right <$> (g v >>= g'))
    g' (ArrayLiteral vs) = ArrayLiteral <$> traverse (g' <=< g) vs
    g' (ObjectLiteral vs) = ObjectLiteral <$> traverse (sndM (g' <=< g)) vs
    g' (ObjectConstructor vs) = ObjectConstructor <$> traverse (sndM $ maybeM (g' <=< g)) vs
    g' (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> (g v >>= g')
    g' (Accessor prop v) = Accessor prop <$> (g v >>= g')
    g' (ObjectUpdate obj vs) = ObjectUpdate <$> (g obj >>= g') <*> traverse (sndM (g' <=< g)) vs
    g' (ObjectUpdater obj vs) = ObjectUpdater <$> (maybeM g obj >>= maybeM g') <*> traverse (sndM $ maybeM (g' <=< g)) vs
    g' (Abs name v) = Abs name <$> (g v >>= g')
    g' (App v1 v2) = App <$> (g v1 >>= g') <*> (g v2 >>= g')
    g' (IfThenElse v1 v2 v3) = IfThenElse <$> (g v1 >>= g') <*> (g v2 >>= g') <*> (g v3 >>= g')
    g' (Case vs alts) = Case <$> traverse (g' <=< g) vs <*> traverse handleCaseAlternative alts
    g' (TypedValue check v ty) = TypedValue check <$> (g v >>= g') <*> pure ty
    g' (Let ds v) = Let <$> traverse (f' <=< f) ds <*> (g v >>= g')
    g' (Do es) = Do <$> traverse handleDoNotationElement es
    g' (PositionedValue pos com v) = PositionedValue pos com <$> (g v >>= g')
    g' other = g other

    h' :: Binder -> m Binder
    h' (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h' <=< h) bs
    h' (ObjectBinder bs) = ObjectBinder <$> traverse (sndM (h' <=< h)) bs
    h' (ArrayBinder bs) = ArrayBinder <$> traverse (h' <=< h) bs
    h' (NamedBinder name b) = NamedBinder name <$> (h b >>= h')
    h' (PositionedBinder pos com b) = PositionedBinder pos com <$> (h b >>= h')
    h' (TypedBinder t b) = TypedBinder t <$> (h b >>= h')
    h' other = h other

    handleCaseAlternative :: CaseAlternative -> m CaseAlternative
    handleCaseAlternative (CaseAlternative { caseAlternativeBinders: bs
                                           , caseAlternativeResult: val }) = do
        binders <- traverse (h' <=< h) bs
        result <- eitherM (traverse (pairM (g' <=< g) (g' <=< g))) (g' <=< g) val
        pure $ CaseAlternative { caseAlternativeBinders: binders
                               , caseAlternativeResult: result }

    handleDoNotationElement :: DoNotationElement -> m DoNotationElement
    handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> (g' <=< g) v
    handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> (h' <=< h) b <*> (g' <=< g) v
    handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> traverse (f' <=< f) ds
    handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com <$> handleDoNotationElement e

everywhereOnValuesM :: forall m
                     . (Functor m, Applicative m, Monad m)
                    => (Declaration -> m Declaration)
                    -> (Expr -> m Expr)
                    -> (Binder -> m Binder)
                    -> { decl :: Declaration -> m Declaration
                       , expr :: Expr -> m Expr
                       , binder :: Binder -> m Binder
                       }
everywhereOnValuesM f g h =
    { decl: f'
    , expr: g'
    , binder: h'
    }
  where
    f' :: Declaration -> m Declaration
    f' (DataBindingGroupDeclaration ds) = (DataBindingGroupDeclaration <$> traverse f' ds) >>= f
    f' (ValueDeclaration name nameKind bs val) = (ValueDeclaration name nameKind <$> traverse h' bs <*> eitherM (traverse (pairM g' g')) g' val) >>= f
    f' (BindingGroupDeclaration ds) = do
        decls <- traverse (\{ident: name, nameKind: nameKind, expr: val} -> {ident: name, nameKind: nameKind, expr: _} <$> g' val) ds
        f $ BindingGroupDeclaration decls
    f' (TypeClassDeclaration name args implies ds) = (TypeClassDeclaration name args implies <$> traverse f' ds) >>= f
    f' (TypeInstanceDeclaration name cs className args ds) = (TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse f') ds) >>= f
    f' (PositionedDeclaration pos com d) = (PositionedDeclaration pos com <$> f' d) >>= f
    f' other = f other

    g' :: Expr -> m Expr
    g' (UnaryMinus v) = (UnaryMinus <$> g' v) >>= g
    g' (BinaryNoParens op v1 v2) = (BinaryNoParens <$> g' op <*> g' v1 <*> g' v2) >>= g
    g' (Parens v) = (Parens <$> g' v) >>= g
    g' (OperatorSection op (Left v)) = (OperatorSection <$> g' op <*> (Left <$> g' v)) >>= g
    g' (OperatorSection op (Right v)) = (OperatorSection <$> g' op <*> (Right <$> g' v)) >>= g
    g' (ArrayLiteral vs) = (ArrayLiteral <$> traverse g' vs) >>= g
    g' (ObjectLiteral vs) = (ObjectLiteral <$> traverse (sndM g') vs) >>= g
    g' (ObjectConstructor vs) = (ObjectConstructor <$> traverse (sndM $ maybeM g') vs) >>= g
    g' (TypeClassDictionaryConstructorApp name v) = (TypeClassDictionaryConstructorApp name <$> g' v) >>= g
    g' (Accessor prop v) = (Accessor prop <$> g' v) >>= g
    g' (ObjectUpdate obj vs) = (ObjectUpdate <$> g' obj <*> traverse (sndM g') vs) >>= g
    g' (ObjectUpdater obj vs) = (ObjectUpdater <$> maybeM g' obj <*> traverse (sndM $ maybeM g') vs) >>= g
    g' (Abs name v) = (Abs name <$> g' v) >>= g
    g' (App v1 v2) = (App <$> g' v1 <*> g' v2) >>= g
    g' (IfThenElse v1 v2 v3) = (IfThenElse <$> g' v1 <*> g' v2 <*> g' v3) >>= g
    g' (Case vs alts) = (Case <$> traverse g' vs <*> traverse handleCaseAlternative alts) >>= g
    g' (TypedValue check v ty) = (TypedValue check <$> g' v <*> pure ty) >>= g
    g' (Let ds v) = (Let <$> traverse f' ds <*> g' v) >>= g
    g' (Do es) = (Do <$> traverse handleDoNotationElement es) >>= g
    g' (PositionedValue pos com v) = (PositionedValue pos com <$> g' v) >>= g
    g' other = g other

    h' :: Binder -> m Binder
    h' (ConstructorBinder ctor bs) = (ConstructorBinder ctor <$> traverse h' bs) >>= h
    h' (ObjectBinder bs) = (ObjectBinder <$> traverse (sndM h') bs) >>= h
    h' (ArrayBinder bs) = (ArrayBinder <$> traverse h' bs) >>= h
    h' (NamedBinder name b) = (NamedBinder name <$> h' b) >>= h
    h' (PositionedBinder pos com b) = (PositionedBinder pos com <$> h' b) >>= h
    h' (TypedBinder t b) = (TypedBinder t <$> h' b) >>= h
    h' other = h other

    handleCaseAlternative :: CaseAlternative -> m CaseAlternative
    handleCaseAlternative (CaseAlternative { caseAlternativeBinders: bs
                                           , caseAlternativeResult: val}) = do
        binders <- traverse h' bs
        result <- eitherM (traverse (pairM g' g')) g' val
        pure $ CaseAlternative { caseAlternativeBinders: binders, caseAlternativeResult: result }

    handleDoNotationElement :: DoNotationElement -> m DoNotationElement
    handleDoNotationElement (DoNotationValue v) = DoNotationValue <$> g' v
    handleDoNotationElement (DoNotationBind b v) = DoNotationBind <$> h' b <*> g' v
    handleDoNotationElement (DoNotationLet ds) = DoNotationLet <$> traverse f' ds
    handleDoNotationElement (PositionedDoNotationElement pos com e) = PositionedDoNotationElement pos com <$> handleDoNotationElement e

everythingOnValues :: forall r
                    . (r -> r -> r)
                   -> (Declaration -> r)
                   -> (Expr -> r)
                   -> (Binder -> r)
                   -> (CaseAlternative -> r)
                   -> (DoNotationElement -> r)
                   -> { decl :: Declaration -> r
                      , expr :: Expr -> r
                      , binder :: Binder -> r
                      , caseAlt :: CaseAlternative -> r
                      , doNotationElem :: DoNotationElement -> r
                      }
everythingOnValues combiner f g h i j =
    { decl: f'
    , expr: g'
    , binder: h'
    , caseAlt: i'
    , doNotationElem: j'
    }
  where
    f' :: Declaration -> r
    f' d@(DataBindingGroupDeclaration ds) = foldl combiner (f d) (map f' ds)
    f' d@(ValueDeclaration _ _ bs (Right val)) = foldl combiner (f d) (map h' bs) `combiner` g' val
    f' d@(ValueDeclaration _ _ bs (Left gs)) = foldl combiner (f d) (map h' bs <> concatMap (\(Tuple grd val) -> [g' grd, g' val]) gs)
    f' d@(BindingGroupDeclaration ds) = foldl combiner (f d) (map (\{ ident: _, nameKind: _, expr: val } -> g' val) ds)
    f' d@(TypeClassDeclaration _ _ _ ds) = foldl combiner (f d) (map f' ds)
    f' d@(TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl combiner (f d) (map f' ds)
    f' d@(PositionedDeclaration _ _ d1) = f d `combiner` f' d1
    f' d = f d

    g' :: Expr -> r
    g' v@(UnaryMinus v1) = g v `combiner` g' v1
    g' v@(BinaryNoParens op v1 v2) = g v `combiner` g' op `combiner` g' v1 `combiner` g' v2
    g' v@(Parens v1) = g v `combiner` g' v1
    g' v@(OperatorSection op (Left v1)) = g v `combiner` g' op `combiner` g' v1
    g' v@(OperatorSection op (Right v1)) = g v `combiner` g' op `combiner` g' v1
    g' v@(ArrayLiteral vs) = foldl combiner (g v) (map g' vs)
    g' v@(ObjectLiteral vs) = foldl combiner (g v) (map (g' <<< snd) vs)
    g' v@(ObjectConstructor vs) = foldl combiner (g v) (map g' (mapMaybe snd vs))
    g' v@(TypeClassDictionaryConstructorApp _ v1) = g v `combiner` g' v1
    g' v@(Accessor _ v1) = g v `combiner` g' v1
    g' v@(ObjectUpdate obj vs) = foldl combiner (g v `combiner` g' obj) (map (g' <<< snd) vs)
    g' v@(ObjectUpdater obj vs) = foldl combiner (maybe (g v) (\x -> g v `combiner` g' x) obj) (map g' (mapMaybe snd vs))
    g' v@(Abs _ v1) = g v `combiner` g' v1
    g' v@(App v1 v2) = g v `combiner` g' v1 `combiner` g' v2
    g' v@(IfThenElse v1 v2 v3) = g v `combiner` g' v1 `combiner` g' v2 `combiner` g' v3
    g' v@(Case vs alts) = foldl combiner (foldl combiner (g v) (map g' vs)) (map i' alts)
    g' v@(TypedValue _ v1 _) = g v `combiner` g' v1
    g' v@(Let ds v1) = foldl combiner (g v) (map f' ds) `combiner` g' v1
    g' v@(Do es) = foldl combiner (g v) (map j' es)
    g' v@(PositionedValue _ _ v1) = g v `combiner` g' v1
    g' v = g v

    h' :: Binder -> r
    h' b@(ConstructorBinder _ bs) = foldl combiner (h b) (map h' bs)
    h' b@(ObjectBinder bs) = foldl combiner (h b) (map (h' <<< snd) bs)
    h' b@(ArrayBinder bs) = foldl combiner (h b) (map h' bs)
    h' b@(NamedBinder _ b1) = h b `combiner` h' b1
    h' b@(PositionedBinder _ _ b1) = h b `combiner` h' b1
    h' b@(TypedBinder _ b1) = h b `combiner` h' b1
    h' b = h b

    i' :: CaseAlternative -> r
    i' ca@(CaseAlternative { caseAlternativeBinders: bs
                           , caseAlternativeResult: Right val
                           }) =
        foldl combiner (i ca) (map h' bs) `combiner` g' val
    i' ca@(CaseAlternative { caseAlternativeBinders: bs
                           , caseAlternativeResult: Left gs
                           }) =
        foldl combiner (i ca) (map h' bs ++ concatMap (\(Tuple grd val) -> [g' grd, g' val]) gs)

    j' :: DoNotationElement -> r
    j' e@(DoNotationValue v) = j e `combiner` g' v
    j' e@(DoNotationBind b v) = j e `combiner` h' b `combiner` g' v
    j' e@(DoNotationLet ds) = foldl combiner (j e) (map f' ds)
    j' e@(PositionedDoNotationElement _ _ e1) = j e `combiner` j' e1

everythingWithContextOnValues :: forall r s
                               . s
                              -> r
                              -> (r -> r -> r)
                              -> (s -> Declaration       -> Tuple s r)
                              -> (s -> Expr              -> Tuple s r)
                              -> (s -> Binder            -> Tuple s r)
                              -> (s -> CaseAlternative   -> Tuple s r)
                              -> (s -> DoNotationElement -> Tuple s r)
                              -> { decl          :: Declaration       -> r
                                 , expr          :: Expr              -> r
                                 , binder        :: Binder            -> r
                                 , caseAlt       :: CaseAlternative   -> r
                                 , doNotationElm :: DoNotationElement -> r
                                 }
everythingWithContextOnValues s0 r0 combiner f g h i j =
    { decl:          f'' s0
    , expr:          g'' s0
    , binder:        h'' s0
    , caseAlt:       i'' s0
    , doNotationElm: j'' s0
    }
  where
    f'' :: s -> Declaration -> r
    f'' s d = case f s d of
                Tuple s' r -> r `combiner` f' s' d

    f' :: s -> Declaration -> r
    f' s (DataBindingGroupDeclaration ds) = foldl combiner r0 (map (f'' s) ds)
    f' s (ValueDeclaration _ _ bs (Right val)) = foldl combiner r0 (map (h'' s) bs) `combiner` g'' s val
    f' s (ValueDeclaration _ _ bs (Left gs)) = foldl combiner r0 (map (h'' s) bs <> concatMap (\(Tuple grd val) -> [g'' s grd, g'' s val]) gs)
    f' s (BindingGroupDeclaration ds) = foldl combiner r0 $ map (g'' s <<< _.expr) ds
    f' s (TypeClassDeclaration _ _ _ ds) = foldl combiner r0 (map (f'' s) ds)
    f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldl combiner r0 (map (f'' s) ds)
    f' s (PositionedDeclaration _ _ d1) = f'' s d1
    f' _ _ = r0

    g'' :: s -> Expr -> r
    g'' s v = case g s v of
                Tuple s' r -> r `combiner` g' s' v

    g' :: s -> Expr -> r
    g' s (UnaryMinus v1) = g'' s v1
    g' s (BinaryNoParens op v1 v2) = g'' s op `combiner` g'' s v1 `combiner` g'' s v2
    g' s (Parens v1) = g'' s v1
    g' s (OperatorSection op (Left v)) = g'' s op `combiner` g'' s v
    g' s (OperatorSection op (Right v)) = g'' s op `combiner` g'' s v
    g' s (ArrayLiteral vs) = foldl combiner r0 (map (g'' s) vs)
    g' s (ObjectLiteral vs) = foldl combiner r0 (map (g'' s <<< snd) vs)
    g' s (ObjectConstructor vs) = foldl combiner r0 (map (g'' s) (mapMaybe snd vs))
    g' s (TypeClassDictionaryConstructorApp _ v1) = g'' s v1
    g' s (Accessor _ v1) = g'' s v1
    g' s (ObjectUpdate obj vs) = foldl combiner (g'' s obj) (map (g'' s <<< snd) vs)
    g' s (ObjectUpdater obj vs) = foldl combiner (maybe r0 (g'' s) obj) (map (g'' s) (mapMaybe snd vs))
    g' s (Abs _ v1) = g'' s v1
    g' s (App v1 v2) = g'' s v1 `combiner` g'' s v2
    g' s (IfThenElse v1 v2 v3) = g'' s v1 `combiner` g'' s v2 `combiner` g'' s v3
    g' s (Case vs alts) = foldl combiner (foldl combiner r0 (map (g'' s) vs)) (map (i'' s) alts)
    g' s (TypedValue _ v1 _) = g'' s v1
    g' s (Let ds v1) = foldl combiner r0 (map (f'' s) ds) `combiner` g'' s v1
    g' s (Do es) = foldl combiner r0 (map (j'' s) es)
    g' s (PositionedValue _ _ v1) = g'' s v1
    g' _ _ = r0

    h'' :: s -> Binder -> r
    h'' s b = case h s b of
                Tuple s' r -> r `combiner` h' s' b

    h' :: s -> Binder -> r
    h' s (ConstructorBinder _ bs) = foldl combiner r0 (map (h'' s) bs)
    h' s (ObjectBinder bs) = foldl combiner r0 (map (h'' s <<< snd) bs)
    h' s (ArrayBinder bs) = foldl combiner r0 (map (h'' s) bs)
    h' s (NamedBinder _ b1) = h'' s b1
    h' s (PositionedBinder _ _ b1) = h'' s b1
    h' s (TypedBinder _ b1) = h'' s b1
    h' _ _ = r0

    i'' :: s -> CaseAlternative -> r
    i'' s ca = case i s ca of
                Tuple s' r -> r `combiner` i' s' ca

    i' :: s -> CaseAlternative -> r
    i' s (CaseAlternative { caseAlternativeBinders: bs
                          , caseAlternativeResult: Right val }) =
        foldl combiner r0 (map (h'' s) bs) `combiner` g'' s val
    i' s (CaseAlternative { caseAlternativeBinders: bs
                          , caseAlternativeResult: Left gs}) =
        foldl combiner r0 (map (h'' s) bs <> concatMap (\(Tuple grd val) -> [g'' s grd, g'' s val]) gs)

    j'' :: s -> DoNotationElement -> r
    j'' s e = case j s e of
                Tuple s' r -> r `combiner` j' s' e

    j' :: s -> DoNotationElement -> r
    j' s (DoNotationValue v) = g'' s v
    j' s (DoNotationBind b v) = h'' s b `combiner` g'' s v
    j' s (DoNotationLet ds) = foldl combiner r0 (map (f'' s) ds)
    j' s (PositionedDoNotationElement _ _ e1) = j'' s e1

everywhereWithContextOnValuesM :: forall m s
                                . (Functor m, Applicative m, Monad m)
                               => s
                               -> (s -> Declaration       -> m (Tuple s Declaration))
                               -> (s -> Expr              -> m (Tuple s Expr))
                               -> (s -> Binder            -> m (Tuple s Binder))
                               -> (s -> CaseAlternative   -> m (Tuple s CaseAlternative))
                               -> (s -> DoNotationElement -> m (Tuple s DoNotationElement))
                               -> { decl           :: Declaration       -> m Declaration
                                  , expr           :: Expr              -> m Expr
                                  , binder         :: Binder            -> m Binder
                                  , caseAlt        :: CaseAlternative   -> m CaseAlternative
                                  , doNotationElem :: DoNotationElement -> m DoNotationElement
                                  }
everywhereWithContextOnValuesM s0 f g h i j =
    { decl: f'' s0
    , expr: g'' s0
    , binder: h'' s0
    , caseAlt: i'' s0
    , doNotationElem: j'' s0
    }
  where
    f'' :: s -> Declaration -> m Declaration
    f'' s = uncurry f' <=< f s

    f' :: s -> Declaration -> m Declaration
    f' s (DataBindingGroupDeclaration ds) = DataBindingGroupDeclaration <$> traverse (f'' s) ds
    f' s (ValueDeclaration name nameKind bs val) = ValueDeclaration name nameKind <$> traverse (h'' s) bs <*> eitherM (traverse (pairM (g'' s) (g'' s))) (g'' s) val
    f' s (BindingGroupDeclaration ds) = do
        updatedDecs <- traverse updateDecs ds
        pure $ BindingGroupDeclaration updatedDecs
      where
        updateDecs :: forall r . { expr :: Expr | r } -> m { expr :: Expr | r }
        updateDecs row = map (row { expr = _ }) $ g'' s row.expr
    f' s (TypeClassDeclaration name args implies ds) = TypeClassDeclaration name args implies <$> traverse (f'' s) ds
    f' s (TypeInstanceDeclaration name cs className args ds) = TypeInstanceDeclaration name cs className args <$> traverseTypeInstanceBody (traverse (f'' s)) ds
    f' s (PositionedDeclaration pos com d1) = PositionedDeclaration pos com <$> f'' s d1
    f' _ other = return other

    g'' :: s -> Expr -> m Expr
    g'' s = uncurry g' <=< g s

    g' :: s -> Expr -> m Expr
    g' s (UnaryMinus v) = UnaryMinus <$> g'' s v
    g' s (BinaryNoParens op v1 v2) = BinaryNoParens <$> g'' s op <*> g'' s v1 <*> g'' s v2
    g' s (Parens v) = Parens <$> g'' s v
    g' s (OperatorSection op (Left v)) = OperatorSection <$> g'' s op <*> (Left <$> g'' s v)
    g' s (OperatorSection op (Right v)) = OperatorSection <$> g'' s op <*> (Right <$> g'' s v)
    g' s (ArrayLiteral vs) = ArrayLiteral <$> traverse (g'' s) vs
    g' s (ObjectLiteral vs) = ObjectLiteral <$> traverse (sndM (g'' s)) vs
    g' s (ObjectConstructor vs) = ObjectConstructor <$> traverse (sndM $ maybeM (g'' s)) vs
    g' s (TypeClassDictionaryConstructorApp name v) = TypeClassDictionaryConstructorApp name <$> g'' s v
    g' s (Accessor prop v) = Accessor prop <$> g'' s v
    g' s (ObjectUpdate obj vs) = ObjectUpdate <$> g'' s obj <*> traverse (sndM (g'' s)) vs
    g' s (ObjectUpdater obj vs) = ObjectUpdater <$> maybeM (g'' s) obj <*> traverse (sndM $ maybeM (g'' s)) vs
    g' s (Abs name v) = Abs name <$> g'' s v
    g' s (App v1 v2) = App <$> g'' s v1 <*> g'' s v2
    g' s (IfThenElse v1 v2 v3) = IfThenElse <$> g'' s v1 <*> g'' s v2 <*> g'' s v3
    g' s (Case vs alts) = Case <$> traverse (g'' s) vs <*> traverse (i'' s) alts
    g' s (TypedValue check v ty) = TypedValue check <$> g'' s v <*> pure ty
    g' s (Let ds v) = Let <$> traverse (f'' s) ds <*> g'' s v
    g' s (Do es) = Do <$> traverse (j'' s) es
    g' s (PositionedValue pos com v) = PositionedValue pos com <$> g'' s v
    g' _ other = return other

    h'' :: s -> Binder -> m Binder
    h'' s = uncurry h' <=< h s

    h' :: s -> Binder -> m Binder
    h' s (ConstructorBinder ctor bs) = ConstructorBinder ctor <$> traverse (h'' s) bs
    h' s (ObjectBinder bs) = ObjectBinder <$> traverse (sndM (h'' s)) bs
    h' s (ArrayBinder bs) = ArrayBinder <$> traverse (h'' s) bs
    h' s (NamedBinder name b) = NamedBinder name <$> h'' s b
    h' s (PositionedBinder pos com b) = PositionedBinder pos com <$> h'' s b
    h' s (TypedBinder t b) = TypedBinder t <$> h'' s b
    h' _ other = return other

    i'' :: s -> CaseAlternative -> m CaseAlternative
    i'' s = uncurry i' <=< i s

    i' :: s -> CaseAlternative -> m CaseAlternative
    i' s (CaseAlternative { caseAlternativeBinders: bs
                          , caseAlternativeResult: val}) = do
        binders <- traverse (h'' s) bs
        result <- eitherM (traverse (pairM (g'' s) (g'' s))) (g'' s) val
        pure $ CaseAlternative { caseAlternativeBinders: binders, caseAlternativeResult: result }

    j'' :: s -> DoNotationElement -> m DoNotationElement
    j'' s = uncurry j' <=< j s

    j' :: s -> DoNotationElement -> m DoNotationElement
    j' s (DoNotationValue v) = DoNotationValue <$> g'' s v
    j' s (DoNotationBind b v) = DoNotationBind <$> h'' s b <*> g'' s v
    j' s (DoNotationLet ds) = DoNotationLet <$> traverse (f'' s) ds
    j' s (PositionedDoNotationElement pos com e1) = PositionedDoNotationElement pos com <$> j'' s e1

everythingWithScope :: forall r
                     . (Monoid r)
                    => (Set Ident -> Declaration -> r)
                    -> (Set Ident -> Expr -> r)
                    -> (Set Ident -> Binder -> r)
                    -> (Set Ident -> CaseAlternative -> r)
                    -> (Set Ident -> DoNotationElement -> r)
                    -> { decl           :: Set Ident -> Declaration       -> r
                       , expr           :: Set Ident -> Expr              -> r
                       , binder         :: Set Ident -> Binder            -> r
                       , caseAlt        :: Set Ident -> CaseAlternative   -> r
                       , doNotationElem :: Set Ident -> DoNotationElement -> r
                       }
everythingWithScope f g h i j =
    { decl: f''
    , expr: g''
    , binder: h''
    , caseAlt: i''
    , doNotationElem: \s -> snd <<< j'' s
    }
  where
    f'' :: Set Ident -> Declaration -> r
    f'' s a = f s a <> f' s a

    f' :: Set Ident -> Declaration -> r
    f' s (DataBindingGroupDeclaration ds) =
      let s' = Set.union s (Set.fromList $ toList (mapMaybe getDeclIdent ds))
      in foldMap (f'' s') ds
    f' s (ValueDeclaration name _ bs (Right val)) =
      let s' = Set.insert name s
      in foldMap (h'' s') bs <> g'' s' val
    f' s (ValueDeclaration name _ bs (Left gs)) =
        let s' = Set.insert name s
            s'' = Set.union s' (Set.fromList $ toList (concatMap binderNames bs))
        in foldMap (h'' s') bs <> foldMap (\(Tuple grd val) -> g'' s'' grd <> g'' s'' val) gs
    f' s (BindingGroupDeclaration ds) =
        let s' = Set.union s (Set.fromList $ toList (map (\{ ident: name, nameKind: _, expr: _ } -> name) ds))
        in foldMap (\{ ident: _, nameKind: _, expr: val } -> g'' s' val) ds
    f' s (TypeClassDeclaration _ _ _ ds) = foldMap (f'' s) ds
    f' s (TypeInstanceDeclaration _ _ _ _ (ExplicitInstance ds)) = foldMap (f'' s) ds
    f' s (PositionedDeclaration _ _ d) = f'' s d
    f' _ _ = mempty

    g'' :: Set Ident -> Expr -> r
    g'' s a = g s a <> g' s a

    g' :: Set Ident -> Expr -> r
    g' s (UnaryMinus v1) = g'' s v1
    g' s (BinaryNoParens op v1 v2) = g' s op <> g' s v1 <> g' s v2
    g' s (Parens v1) = g'' s v1
    g' s (OperatorSection op (Left v)) = g'' s op <> g'' s v
    g' s (OperatorSection op (Right v)) = g'' s op <> g'' s v
    g' s (ArrayLiteral vs) = foldMap (g'' s) vs
    g' s (ObjectLiteral vs) = foldMap (g'' s <<< snd) vs
    g' s (ObjectConstructor vs) = foldMap (g'' s) (mapMaybe snd vs)
    g' s (TypeClassDictionaryConstructorApp _ v1) = g'' s v1
    g' s (Accessor _ v1) = g'' s v1
    g' s (ObjectUpdate obj vs) = g'' s obj <> foldMap (g'' s <<< snd) vs
    g' s (ObjectUpdater obj vs) = foldMap (g'' s) obj <> foldMap (g'' s) (mapMaybe snd vs)
    g' s (Abs (Left name) v1) =
        let s' = Set.insert name s
        in g'' s' v1
    g' s (Abs (Right b) v1) =
        let s' = Set.union (Set.fromList $ toList (binderNames b)) s
        in g'' s' v1
    g' s (App v1 v2) = g'' s v1 <> g'' s v2
    g' s (IfThenElse v1 v2 v3) = g'' s v1 <> g'' s v2 <> g'' s v3
    g' s (Case vs alts) = foldMap (g'' s) vs <> foldMap (i'' s) alts
    g' s (TypedValue _ v1 _) = g'' s v1
    g' s (Let ds v1) =
        let s' = Set.union s (Set.fromList $ toList (mapMaybe getDeclIdent ds))
        in foldMap (f'' s') ds <> g'' s' v1
    g' s (Do es) = es #
                    mapAccumL (\identSet doNotationElement -> tupleToAccum $ j'' identSet doNotationElement) s >>>
                    _.value >>>
                    fold
      where
        tupleToAccum :: forall x y . Tuple x y -> Accum x y
        tupleToAccum (Tuple s a) = {accum: s, value: a}

        accumToTuple :: forall x y . Accum x y -> Tuple x y
        accumToTuple { accum: s, value: a } = Tuple s a
    g' s (PositionedValue _ _ v1) = g'' s v1
    g' _ _ = mempty

    h'' :: Set Ident -> Binder -> r
    h'' s a = h s a <> h' s a

    h' :: Set Ident -> Binder -> r
    h' s (ConstructorBinder _ bs) = foldMap (h'' s) bs
    h' s (ObjectBinder bs) = foldMap (h'' s <<< snd) bs
    h' s (ArrayBinder bs) = foldMap (h'' s) bs
    h' s (NamedBinder name b1) =
        let s' = Set.insert name s
        in h'' s' b1
    h' s (PositionedBinder _ _ b1) = h'' s b1
    h' s (TypedBinder _ b1) = h'' s b1
    h' _ _ = mempty

    i'' :: Set Ident -> CaseAlternative -> r
    i'' s a = i s a <> i' s a

    i' :: Set Ident -> CaseAlternative -> r
    i' s (CaseAlternative { caseAlternativeBinders: bs
                          , caseAlternativeResult: Right val }) =
        let s' = Set.union s (Set.fromList $ toList (concatMap binderNames bs))
        in foldMap (h'' s) bs <> g'' s' val
    i' s (CaseAlternative { caseAlternativeBinders: bs
                          , caseAlternativeResult: Left gs }) =
        let s' = Set.union s (Set.fromList <<< toList $ concatMap binderNames bs)
        in foldMap (h'' s) bs <> foldMap (\(Tuple grd val) -> g'' s' grd <> g'' s' val) gs

    j'' :: Set Ident -> DoNotationElement -> Tuple (Set Ident) r
    j'' s a =
        case j' s a of
            Tuple s' r -> Tuple s' (j s a <> r)

    j' :: Set Ident -> DoNotationElement -> Tuple (Set Ident) r
    j' s (DoNotationValue v) = Tuple s (g'' s v)
    j' s (DoNotationBind b v) =
      let s' = Set.union (Set.fromList <<< toList $ binderNames b) s
      in Tuple s' (h'' s b <> g'' s' v)
    j' s (DoNotationLet ds) =
      let s' = Set.union s (Set.fromList <<< toList $ mapMaybe getDeclIdent ds)
      in Tuple s' (foldMap (f'' s') ds)
    j' s (PositionedDoNotationElement _ _ e1) = j'' s e1

    getDeclIdent :: Declaration -> Maybe Ident
    getDeclIdent (PositionedDeclaration _ _ d) = getDeclIdent d
    getDeclIdent (ValueDeclaration ident _ _ _) = Just ident
    getDeclIdent (TypeDeclaration ident _) = Just ident
    getDeclIdent _ = Nothing

accumTypes :: forall r . (Monoid r)
           => (Type -> r)
           -> { decl :: Declaration -> r
              , expr :: Expr -> r
              , binder :: Binder -> r
              , caseAlt :: CaseAlternative -> r
              , doNotationElem :: DoNotationElement -> r
              }
accumTypes f = everythingOnValues append forDecls forValues (const mempty) (const mempty) (const mempty)
  where

    forDecls :: Declaration -> r
    forDecls (DataDeclaration _ _ _ dctors) = mconcat (concatMap (map f <<< snd) dctors)
    forDecls (ExternDeclaration _ ty) = f ty
    forDecls (TypeClassDeclaration _ _ implies _) = mconcat (concatMap (map f <<< snd) implies)
    forDecls (TypeInstanceDeclaration _ cs _ tys _) = mconcat (concatMap (map f <<< snd) cs) `append` mconcat (map f tys)
    forDecls (TypeSynonymDeclaration _ _ ty) = f ty
    forDecls (TypeDeclaration _ ty) = f ty
    forDecls _ = mempty

    forValues :: Expr -> r
    forValues (TypeClassDictionary (Tuple _ cs) _) = mconcat (map f cs)
    forValues (SuperClassDictionary _ tys) = mconcat (map f tys)
    forValues (TypedValue _ _ ty) = f ty
    forValues _ = mempty
