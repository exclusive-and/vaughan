
module Vaughan.Frontend.Parser where

import              Vaughan.AVIR.Avir
import              Vaughan.Frontend.Syntax
import              Vaughan.Identifiers

import              Data.ByteString

import              Text.Parsec
import qualified    Text.Parsec.Token as Parsec




type Parser = Parsec ByteString ParserState

data ParserState = ParserState


parseVauDecl :: Parser VauDeclaration
parseVauDecl =
    (reserved "type" *> parseTyDecl) <|> goDecl
  where
    goDecl :: Parser VauDeclaration
    goDecl = do
        declarator <- parseDeclarator
        return $ VauNormalDecl declarator

parseDeclarator :: Parser VauDeclarator
parseDeclarator = do
    declNameM <- optionMaybe parseIdentifier
    declArgsM <- optionMaybe $ parens $ commaSep parseDeclarator
    declType <- reservedOp ":" *> parseVauType
    return $ VauDeclarator declNameM declArgsM declType

parseVauType :: Parser VauType
parseVauType = do
    tyHead <- goPrim <|> goCustom
    case tyHead of
        VauPrimTy Pointer -> do
            tyArg <- goPrim <|> goCustom
            return $ VauTyApp tyHead tyArg
        _ -> return tyHead
  where
    goPrim = reserved "Bool" *> return (VauPrimTy Boolean)
         <|> reserved "Byte" *> return (VauPrimTy Byte)
         <|> reserved "Int"  *> return (VauPrimTy Integer)
         <|> reserved "Ptr"  *> return (VauPrimTy Pointer)

    goCustom = VauTyVar <$> parseIdentifier

parseTyDecl :: Parser VauDeclaration
parseTyDecl = do
    tyName <- parseIdentifier
    reservedOp "="
    tyCtors <- many goCtor
    return $ VauTypeDecl tyName tyCtors
  where
    goCtor = do
        ctorName <- parseIdentifier
        ctorFields <- braces $ parseDeclarator `endBy` semi
        return (ctorName, ctorFields)

parseIdentifier :: Parser Identifier
parseIdentifier = identifier


vauLexer = Parsec.makeTokenParser vauLangDef

-- WORDS
reserved    = Parsec.reserved vauLexer
identifier  = Parsec.identifier vauLexer
-- OPERATORS
reservedOp  = Parsec.reservedOp vauLexer
operator    = Parsec.operator vauLexer
-- SEPARATORS
parens      = Parsec.parens vauLexer
braces      = Parsec.braces vauLexer
brackets    = Parsec.brackets vauLexer
commaSep    = Parsec.commaSep vauLexer
semiSep     = Parsec.semiSep vauLexer
semi        = Parsec.semi vauLexer

vauLangDef = Parsec.LanguageDef
    { Parsec.commentStart      = "/*"
    , Parsec.commentEnd        = "*/"
    , Parsec.commentLine       = "//"
    , Parsec.nestedComments    = True
    , Parsec.identStart        = letter <|> char '_'
    , Parsec.identLetter       = alphaNum <|> char '_'
    , Parsec.opStart           = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Parsec.opLetter          = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Parsec.reservedNames     = resNames
    , Parsec.reservedOpNames   = resOps
    , Parsec.caseSensitive     = True
    }
  where
    resNames =
        [ "Bool"
        , "Byte"
        , "Int"
        , "Ptr"
        , "type"
        , "return"
        ]
    resOps =
        [ ":" , "="
        , "+" , "-" , "*" , "/"
        , "?"
        , "<" , ">" , "=="
        , "&" , "|" , "^"
        ]

