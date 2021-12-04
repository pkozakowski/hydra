{-# LANGUAGE RecordWildCards #-}

module Market.Dhall where

import Data.Bifunctor
import Data.Either.Validation
import Data.Functor.Identity
import Data.Text (pack, unpack)
import Data.Void
import Dhall hiding (maybe)
import Dhall.Core hiding (pretty)
import Dhall.Pretty
import Dhall.Src
import Dhall.TypeCheck
import Numeric.Field.Fraction
import Numeric.Truncatable
import Prettyprinter
import Prettyprinter.Render.Terminal

extractRecursive
    :: Text
    -> Expector (Expr Src Void)
    -> (Text -> Maybe (Decoder a))
    -> Expr Src Void
    -> Extractor Src Void a
extractRecursive typeName expector lookupNested expr = case expr of
    Lam Nothing binding expr' -> case expr' of
        App ( Field
                builderVar
                ( FieldSelection
                    { fieldSelectionLabel = nestedTypeName }
                )
            )
            nestedValue -> extractNested
                typeName binding builderVar
                lookupNested nestedTypeName nestedValue
        expr' -> noBuilderCallError typeName expr'
    _ -> typeError expector expr

extractRecursiveT
    :: Text
    -> Expector (Expr Src Void)
    -> (Text -> Maybe (Decoder a))
    -> (Expr Src Void -> Text -> Maybe (Decoder a))
    -> Expr Src Void
    -> Extractor Src Void a
extractRecursiveT typeName expector lookupNested lookupNestedT expr
    = case expr of
        Lam Nothing binding expr' -> case expr' of
            App ( Field
                    builderVar
                    ( FieldSelection
                        { fieldSelectionLabel = nestedTypeName }
                    )
                )
                nestedValue -> extractNested
                    typeName binding builderVar
                    lookupNested nestedTypeName nestedValue
            App ( App
                    ( Field
                        builderVar
                        ( FieldSelection
                            { fieldSelectionLabel = nestedTypeName }
                        )
                    )
                    type_
                )
                nestedValue -> extractNested
                    typeName binding builderVar
                    (lookupNestedT type_) nestedTypeName nestedValue
            expr' -> noBuilderCallError typeName expr'
        _ -> typeError expector expr

extractNested
    :: Text
    -> FunctionBinding Src Void
    -> Expr Src Void
    -> (Text -> Maybe (Decoder a))
    -> Text
    -> Expr Src Void
    -> Extractor Src Void a
extractNested
    typeName binding builderVar lookupNested nestedTypeName nestedValue
        = fromMonadic do
            decoder <- nestedDecoder
            let wrapped = wrapBuilderCalls builderVar binding nestedValue
            expected <- expect decoder
            typeCheck $ Annot wrapped expected
            toMonadic $ extract decoder wrapped
            where
                wrapBuilderCalls builderVar binding = \case
                    app@(App (App (Field builderVar' _) _) _)
                        | builderVar == builderVar'
                            -> Lam Nothing binding app
                        | otherwise
                            -> descend app
                    app@(App (Field builderVar' _) _)
                        | builderVar == builderVar'
                            -> Lam Nothing binding app
                        | otherwise
                            -> descend app
                    expr
                        -> descend expr
                    where
                        descend
                            = runIdentity
                            . subExpressions
                                (Identity . wrapBuilderCalls builderVar binding)

                nestedDecoder
                    = maybe
                        ( toMonadic
                        $ extractError
                        $ "unknown " <> typeName <> " type: " <> nestedTypeName
                        ) pure
                    $ lookupNested nestedTypeName

                expect
                    = first (fmap ExpectedTypeError)
                    . validationToEither
                    . expected

                typeCheck
                    = first
                        ( DhallErrors
                        . pure
                        . ExtractError
                        . pack
                        . show
                        . DetailedTypeError
                        )
                    . typeOf

noBuilderCallError :: Text -> Expr Src Void -> Extractor Src Void a
noBuilderCallError typeName expr = extractError 
    $ "invalid " <> typeName <> ": expected a builder call, got: \n"
   <> pack (show (prettyExpr expr))

import_ :: [Text] -> Text -> Binding s Import
import_ package name
    = makeBinding name $ Embed Import { .. } where
        importHashed = ImportHashed { .. } where
            hash = Nothing
            importType = Local Here File { .. } where
                file = name
                directory = Directory $ reverse package ++ ["dhall"]
        importMode = Code

call :: Text -> Expr s a -> Expr s a
call name = App $ Var $ V name 0

call2 :: Text -> Expr s a -> Expr s a -> Expr s a
call2 name = App . call name

embedScalar :: Fraction Integer -> Expr s a
embedScalar = DoubleLit . DhallDouble . fractionToFractional

embedString :: String -> Expr s a
embedString = TextLit . Chunks [] . pack

showPrettyExpr :: Pretty a => Expr s a -> String
showPrettyExpr
    = unpack
    . renderStrict
    . layout
    . fmap annToAnsiStyle
    . prettyExpr' where
        -- Alternative prettyExpr which groups the imports together.
        prettyExpr' = \case
            Let bnd expr
                -> annotate Keyword "let"
               <+> pretty (variable bnd)
               <+> annotate Syntax "="
               <+> prettyExpr (value bnd)
                <> line
                <> prettyExpr' expr
            expr
                -> line
                <> annotate Keyword "in"
                <> "  "
                <> prettyExpr expr
