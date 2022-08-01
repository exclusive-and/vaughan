
module Vaughan.Frontend.Syntax where

import Vaughan.AVIR.Avir
import Vaughan.Identifiers


-- |
-- Top-level language declarations.
--
-- Examples:
--
-- @
-- myFun (Int, (Int) : Bool) : Int
-- @
--
-- Vaughan AST:
-- @
-- normal-decl
--   declarator
--     identifier: myFun
--     parameters:
--       declarator:
--         identifier: Nothing
--         parameters: []
--         type      : Int
--       declarator:
--         identifier: Nothing
--         parameters: []
--         type      : Int
--     type: Int
-- @
--
-- AVIR:
-- @
-- avir-function:
--   identifier: myFun
--   type      : (Int, Bool) -> Int
--   parameters: []
--
-- @
--
data VauDeclaration
    = VauFunDecl
        VauDeclarator       -- ^ Function declarator.
        [VauStatement]      -- ^ Function body.
    | VauNormalDecl
        VauDeclarator       -- ^ Declarator.
    | VauTypeDecl
        Identifier          -- ^ Name to give the new type.
        [VauCtor]           -- ^ List of fields of the record.
    deriving Show

-- |
-- The name, parameters, and type of a declared Vaughan object.
--
data VauDeclarator
    = VauDeclarator
        (Maybe Identifier)      -- ^ Name of variable being declared.
        (Maybe [VauDeclarator]) -- ^ Optional: list of parameters.
        VauType                 -- ^ Type of variable declared.
    deriving Show


-- |
-- A named constructor and its fields.
--
type VauCtor = (Identifier, [VauDeclarator])


data VauType
    = VauTyVar Identifier
    | VauPrimTy AvirPrimTyCon
    | VauTyApp VauType VauType
    deriving Show


data VauStatement
    = DeclStmt
        VauDeclaration
    | AssignmentStmt
        Identifier      -- ^ Variable to assign to.
        VauAssignKind   -- ^ How to do the assignment.
        VauExpression   -- ^ Value to assign.
    | ReturnStmt
        VauExpression   -- ^ Value to return.
    deriving Show

data VauAssignKind
    = DirectAssign      -- ^ =
    | AddAcc            -- ^ +=
    | SubAcc            -- ^ -=
    | MulAcc            -- ^ *=
    | DivAcc            -- ^ /=
    | ShiftLeftAcc      -- ^ <<=
    | ShiftRightAcc     -- ^ >>=
    | AndAcc            -- ^ &=
    | OrAcc             -- ^ |=
    | XorAcc            -- ^ ^=
    deriving Show

data VauExpression
    = VauVar    Identifier
    | VauReference
        Identifier          -- ^ Variable to get a reference to.
    | VauDereference
        VauExpression       -- ^ Expression to dereference.
    | VauApply
        Identifier          -- ^ Function to apply.
        [VauExpression]     -- ^ Arguments to the function.
    deriving Show
