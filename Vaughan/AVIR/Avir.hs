
-- |
-- Module       : Vaughan.AVIR.Avir
-- Description  : Abstract Vaughan Intermediate Representation
--
--
--
module Vaughan.AVIR.Avir where

import Vaughan.Identifiers


data AvirDeclaration
    = AvirFunction
        Identifier          -- ^ Name of the function.
        AvirType            -- ^ Function's type.
        [Identifier]        -- ^ Parameter binders.
        [AvirDeclaration]   -- ^ Body of the function.
    | AvirDeclaration
        Identifier          -- ^ Name of the variable.
        AvirType            -- ^ Variable's type.
        AvirExpression      -- ^ Value to assign.


data AvirType
    = AvirTyCon AvirTyCon
    | AvirTyVar Identifier
    | AvirTyApp AvirType AvirType

data AvirTyCon
    = AvirArrow
    | PrimTyCon AvirPrimTyCon

data AvirPrimTyCon
    = Boolean
    | Byte
    | Integer
    | Array Int
    | Pointer
    deriving Show


data AvirExpression
    = AvirVar Identifier [AvirExpression]
