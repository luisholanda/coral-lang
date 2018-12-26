{
{- Parser of the Coral grammar

--------------------------------------------------
Note [Grammar conflicts]

state 59 contains 1 shift/reduce conflicts.

	    Function -> IdentName . '(' sep_by__Argument__','__ ')' ':=' Suite
	    Function -> IdentName . '(' sep_by__Argument__','__ ')' ':=' ExprSuite
	*** Identifier -> IdentName .

    Conflicts: '('.

--------------------------------------------------
state 77 contains 1 shift/reduce conflicts.

	*** TypeTerm -> TypeApp .
	    TypeApp -> TypeApp . TypeFac

    Conflicts: IDENTIFIER.

---------------------------------------------------

state 249 contains 3 shift/reduce conflicts.

	*** TypeFac -> TypeName .
	    Constraint -> TypeName . some__TypeFac__

    Conflicts: '(', IDENTIFIER and TYPENAME.

-}
module Language.Coral.Parser.Parser
    ( parseModule
    , parseDef
    , parseType
    , parseSign
    , parseRecrd
    , parseStmt
    , parseExpr
    )
where

import           Data.Maybe
import           Data.Either                   ( isLeft )
import           Data.Foldable                 ( foldr1 )

import           Language.Coral.Data.Ident
import           Language.Coral.Data.Reversed
import           Language.Coral.Data.SrcSpan
import           Language.Coral.Lexer.Token    hiding ( Identifier )
import           Language.Coral.Parser.Monad
import           Language.Coral.Parser.Commons
import           Language.Coral.Syntax.AST
import           Language.Coral.Syntax.Names
import           Language.Coral.Syntax.Types
}


%name parseModule    Module
%name parseDef       Definition
%name parseType      Type
%name parseSign      Signature
%name parseRecrd     Record
%name parseStmt      Statement
%name parseExpr      Expr

%tokentype { Token }
%monad { P } { >>= } { pure }

%error { tokenError }


%token
    -- Expression operators
    '*'                         { TMult _   }
    '/'                         { TDiv _    }
    '//'                        { TFDiv _   }
    '^'                         { TPow _    }
    '%'                         { TMod _    }
    '+'                         { TAdd _    }
    '-'                         { TMinus _  }
    '<<'                        { TLShift _ }
    '>>'                        { TRShift _ }
    '&'                         { TBitAnd _ }
    '->'                        { TArrow _  }
    '|>'                        { TPipe _   }
    '::'                        { TCons _   }

    -- Boolean operators
    'not in'                    { TNotIn _ }
    not                         { TNot _   }
    '<'                         { TLt _    }
    '<='                        { TLe _    }
    '>'                         { TGt _    }
    '>='                        { TGe _    }
    '!='                        { TNe _    }
    '=='                        { TEq _    }
    and                         { TAnd _   }
    or                          { TOr _    }

    -- Punctuation
    '<-'                        { TLArrow _    }
    '=>'                        { TFatArrow _  }
    '.'                         { TDot _       }
    ','                         { TComma _     }
    ':'                         { TColon _     }
    '='                         { TMutAssign _ }
    ':='                        { TDefine _    }
    '|'                         { TBitOr _     }

    -- Delimiters
    '('                         { TLParen _ }
    ')'                         { TRParen _ }
    '['                         { TLBrack _ }
    ']'                         { TRBrack _ }
    '{'                         { TLCurly _ }
    '}'                         { TRCurly _ }

    -- Keywords
    module                      { TModule _   }
    exports                     { TExports _  }
    use                         { TUse _      }
    as                          { TAs _       }
    if                          { TIf _       }
    unless                      { TUnless _   }
    else                        { TElse _     }
    while                       { TWhile _    }
    until                       { TUntil _    }
    for                         { TFor _      }
    in                          { TIn _       }
    try                         { TTry _      }
    catch                       { TCatch _    }
    finally                     { TFinally _  }
    with                        { TWith _     }
    type                        { TType _     }
    alias                       { TAlias _    }
    record                      { TRecord _   }
    effect                      { TEffect _   }
    handler                     { THandler _  }
    forall                      { TForall _   }
    mut                         { TMut _      }
    break                       { TBreak _    }
    continue                    { TContinue _ }
    return                      { TReturn _   }
    class                       { TClass _    }
    instance                    { TInstance _ }

    -- Identifiers
    IDENTIFIER                  { TIdentifier _ _ }
    TYPENAME                    { TTypeName _ _ }
    QUALIDENT                   { TQualIdent _ _ }
    QUALTYPE                    { TQualType _ _ }

    -- Layout
    INDENT                      { TIndent _ }
    DEDENT                      { TDedent _ }
    NEWLINE                     { TNewLine _ }

    -- Literals
    TRUE                        { TTrue _  }
    FALSE                       { TFalse _ }
    NONE                        { TNone _  }
    EFF                         { TEff _ }
    STRING                      { TString _ _ }
    BYTESTRING                  { TByteString _ _ }
    FMTSTRING                   { TFmtString _ _ }
    RAWSTRING                   { TRawString _ _ }
    RAWBYTESTRING               { TRawByteString _ _ }
    RAWFMTSTRING                { TRawFmtString _ _ }
    INTEGER                     { TInteger _ _ _ }
    FLOAT                       { TFloat _ _ _ }
    IMAGINARY                   { TImaginary _ _ _ }

%left or
%left and
%left NOT
%nonassoc in 'not in' '<' '<=' '>' '>=' '!=' '=='
%left '&'
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '//' '%'
%right '^'
%nonassoc TRY
%left '->' '|>'
%right '::'
%left TPL
%right FUN


%%

---------------------
--    Utilities    --
---------------------

op(p) :: { Maybe _ }
    : p                         { Just $1 }
    | {- empty -}               { Nothing }


some(p) :: { Reversed _ }
    : some(p) p                 { snoc $1 $2 }
    | p                         { rsingleton $1 }


many(p) :: { Reversed _ }
    : some(p)                   { $1 }
    | {- empty -}               { mempty }


sep_by1(p, sep) :: { Reversed _ }
    : sep_by1(p, sep) sep p     { $1 `snoc` $2 }
    | p                         { rsingleton $1 }


sep_by(p, sep) :: { Reversed _ }
    : sep_by1(p, sep)           { $1 }
    | {- empty -}               { mempty }


term_by1(p, sep) :: { Reversed _ }
    : sep_by1(p, sep) sep       { $1 }


term_by(p, sep) :: { Reversed _ }
    : term_by1(p, sep)          { $1 }
    | {- empty -}               { $1 }


between(p, start, end) :: { _ }
    : start p end               { $2 }


next(p, q) :: { _ }
    : p q                       { ($1, $2) }


or(p, q) :: { Either _ _ }
    : p                         { Left  $1 }
    | q                         { Right $1 }


----------------------
--    Base Rules    --
----------------------

Module :: { Module }
    : module QualIdentName op(Exports) NEWLINE sep_by(Import, NEWLINE) many(Definition)
        { let modName = qualIdentToModName $2
          in Module modName $3 (toList $5) (toList $6) }


Exports :: { Reversed (Identifier) }
    : exports '(' sep_by1(Identifier, ',') ')'   { $3 }


Import :: { Import }
    : use QualIdentName                  { ImpMod (qualIdentToModName $2)    }
    | use QualIdentName as IdentName     { ImpAli (qualIdentToModName $2) $4 }


Definition :: { Definition }
    : Signature                       { D $1 }
    | Function                        { D $1 }
    | TypeDef                         { D $1 }
    | Alias                           { D $1 }
    | Record                          { D $1 }
    | Class                           { D $1 }
    | Instance                        { D $1 }


Signature :: { Def Sig () }
    : IdentName ':' Type        { DefSig $1 $3 () }


Function :: { Def Fun () }
    : IdentName '(' sep_by(Argument, ',') ')' ':=' Suite
        { DefFun $1 (toList $3) $6 () }
    | IdentName '(' sep_by(Argument, ',') ')' ':=' ExprSuite
        { DefFun $1 (toList $3) $6 () }


TypeDef :: { Def Typ () }
    : type Type ':=' sep_by1(Constructor, '|')
        { DefTyp $2 (toList $4) () }


Alias :: { Def Ali () }
    : alias Type ':=' Type                { DefAli $2 $4 () }


Record :: { Def Rec () }
    : record Type ':=' NEWLINE INDENT sep_by1(Signature, NEWLINE) DEDENT
        { DefRec $2 (toList $4) () }


Class :: { Def Cla () }
    : class Type with NEWLINE INDENT sep_by1(Definition, NEWLINE) DEDENT
        { DefCla $2 (toList $4) () }


Instance :: { Def Ins () }
    : instance Type with NEWLINE INDENT sep_by1(Definition, NEWLINE) DEDENT
        { DefIns $2 (toList $4) () }


Argument :: { Argument () }
    : IdentName                             { Arg $1 }
    | IdentName '=' Expr                    { ArgDef $1 $3 }


Constructor :: { Constructor () }
    : TypeName                              { ConsValue $1 () }
    | TypeName '(' sep_by1(Type, ',') ')'   { ConsProd $1 (toList $3) () }


-----------------
--    Types    --
-----------------

Type :: { Type }
    : forall sep_by1(IdentName, ',') '.' ConstraintTyp   { TyForAll (toList $2) $4 }
    | TypeTerm                                           { $1 }


ConstraintTyp :: { Type }
    : Constraint '=>' TypeTerm                           { TyConstraint [$1] $3 }
    | '(' sep_by1(Constraint, ',') ')' '=>' TypeTerm     { TyConstraint (toList $2) $5 }
    | TypeTerm                                           { $1 }


TypeTerm :: { Type }
    : EFF '[' sep_by1(TypeApp, ',') op(next('|', IdentName)) ']' TypeFac
        { TyEff (toList $3) $6 (fmap snd $4) }
    | TypeApp
        { $1 }
    | TypeRow
        { $1 }
    | '(' IdentName ':' TypeFac ')' '->' TypeTerm
        { TyFun ($2, $4) $7 }
    | TypeTerm '->' TypeTerm
        %prec FUN { TyFun (IdentName $ mkIdent "", $1) $3 }


TypeApp :: { Type }
    : TypeFac                                            { $1 }
    | TypeApp TypeFac                                    { TyApp $1 $2 }


TypeRow :: { Type }
    : '{' sep_by1(next(IdentName, Type), ',') op(next('|', IDENTIFIER)) '}'
        { TyRow (toList $2) $3 }


TypeFac :: { Type }
    : IdentName                                          { TyVar $1 }
    | TypeName                                           { TyConstructor $1 }
    | '(' TypeTerm ')'                                   { TyParens $2 }


Constraint  :: { Constraint }
    : TypeName some(TypeFac)                             { Constraint $1 (toList $2) }


----------------------
--    Statements    --
----------------------

-- | Special case when a suite has only an expression in it.
-- We use it to provide some level of declarative programming in Coral.
ExprSuite :: { Block () }
    : ExprSuiteP NEWLINE
        {% if fst $1
            then pure [StReturn (snd $1) ()]
            else internalError "Error when parsing function expression"
         }


ExprSuiteP :: { (Bool, Expr ()) }
ExprSuiteP
    :                Expr                   { (True,  $1) }
    |                Expr DEDENT            { (False, $1) }


Suite :: { Block () }
    : NEWLINE INDENT term_by1(Statement, NEWLINE) DEDENT NEWLINE { toList $3 }


Statement :: { Statement () }
    : continue                              { StContinue () }
    | break                                 { StBreak () }
    | return Expr                           { StReturn $2 () }
    | Match ':=' Expr                       { StAssign $1 $3 () }
    | mut IdentName ':=' Expr               { StMutDef $2 $4 () }
    | IdentName '=' Expr                    { StMutAssign $1 $3 () }
    | Expr                                  { StExpr $1 () }
    | Function                              { StFunDef $1 () }
    | Signature                             { StSig $1 () }
    | If                                    { $1 }
    | Unless                                { $1 }
    | Try                                   { $1 }
    | For                                   { $1 }
    | While                                 { $1 }
    | Until                                 { $1 }


------------------------
--    Conditionals    --
------------------------

If :: { Statement () }
    : sep_by1(IfBlock, else) op(ElseBlock)    { StIf (toList $1) $2 () }


IfBlock :: { If () }
    : if BoolExpr Suite                       { If $2 $3 () }


ElseBlock :: { Block () }
    : else Suite                              { $2 }


Unless :: { Statement () }
    : UnlessBlock else sep_by1(IfBlock, else) op(ElseBlock) { StUnless ($1 : toList $3) $4 () }
    | UnlessBlock op(ElseBlock)                             { StUnless [$1]       $2 () }


UnlessBlock :: { If () }
    : unless BoolExpr Suite                   { If $2 $3 () }


----------------
--    Try     --
----------------

Try :: { Statement () }
    : try WithPart Suite some(CatchBlock) op(FinallyBlock)
        { StTry $2 $3 (toList $4) $5 () }


WithPart :: { [Identifier] }
    : with sep_by1(Identifier, ',')        { toList($2) }
    |                                      { [] }


CatchBlock :: { Catch () }
    : catch Identifier op(next(as, IdentName)) Suite
        { Catch $2 (fmap snd $3) $4 () }


FinallyBlock :: { Block () }
    : finally Suite                        { $2 }


-----------------
--    Loops    --
-----------------

For :: { Statement () }
    : for Match in Expr Suite         { StFor $2 $4 $5 () }


While :: { Statement () }
    : while BoolExpr Suite            { StWhile $2 $3 () }


Until :: { Statement () }
    : until BoolExpr Suite            { StUntil $2 $3 () }


-----------------------
--    Expressions    --
-----------------------

Expr :: { Expr () }
    : BoolExpr
        { case $1 of
            BlExpr expr _ -> expr
            other         -> ExpBoolExpr other () }
    | Expr '->' Term                            { ExpOp OpArrow $1 $3 () }
    | Expr '|>' Term                            { ExpOp OpPipe  $1 $3 () }
    | try Expr                                  %prec TRY { ExpTry $2 () }


Term :: { Expr () }
    : Factor                                    { $1 }
    | Comprehension                             { ExpComp $1 () }
    | Literal                                   { ExpLit $1 () }
    | ListLiteral                               { ExpLit $1 () }
    | TupleLiteral                              { ExpLit $1 () }
    | Term '+' Term                             { ExpOp OpPlus  $1 $3 () }
    | Term '-' Term                             { ExpOp OpMinus $1 $3 () }
    | Term '*' Term                             { ExpOp OpMult  $1 $3 () }
    | Term '/' Term                             { ExpOp OpDiv   $1 $3 () }
    | Term '//' Term                            { ExpOp OpFlDiv $1 $3 () }
    | Term '%' Term                             { ExpOp OpMod   $1 $3 () }
    | Term '^' Term                             { ExpOp OpPow   $1 $3 () }
    | Term '&' Term                             { ExpOp OpBAnd  $1 $3 () }
    | Term '<<' Term                            { ExpOp OpLSft  $1 $3 () }
    | Term '>>' Term                            { ExpOp OpRSft  $1 $3 () }
    | Term '::' Term                            { ExpListCons   $1 $3 () }


Factor :: { Expr () }
    : Identifier                                { ExpIdent  $1 () }
    | Factor '(' Parameters ')'                 { ExpCall $1 (reverse $3) () }
    | Factor '.' IdentName                      { ExpAccess $1 $3 () }
    | Factor '[' Expr ']'                       { ExpIndex $1 $3 () }
    | '(' Expr ')'                              { ExpParens $2 }


Parameters :: { [Parameter ()] }
    : Parameters ',' Parameter                  { $3 : $1 }
    | Parameter                                 { [$1] }


Parameter :: { Parameter () }
    : Expr                                      { ParCommon $1 () }
    | IdentName '=' Expr                        { ParNamed $1 $3 () }


BoolExpr :: { BoolExpr () }
    : not BoolExpr                              %prec NOT { BlNot $2 () }
    | BoolExpr and BoolExpr                     { BlAnd $1 $3 () }
    | BoolExpr or BoolExpr                      { BlOr $1 $3 () }
    | Term '==' Term                            { BlComp CmpEq  $1 $3 () }
    | Term '!=' Term                            { BlComp CmpNeq $1 $3 () }
    | Term '>' Term                             { BlComp CmpGt  $1 $3 () }
    | Term '<' Term                             { BlComp CmpLt  $1 $3 () }
    | Term '>=' Term                            { BlComp CmpGe  $1 $3 () }
    | Term '<=' Term                            { BlComp CmpLe  $1 $3 () }
    | Term in Term                              { BlComp CmpIn  $1 $3 () }
    | Term 'not in' Term                        { BlComp CmpNin $1 $3 () }
    | Term
        { case $1 of
            ExpBoolExpr be _ -> be
            _                -> BlExpr $1 () }


--------------------------
--    Comprehensions    --
--------------------------

Comprehension :: { Comprehension () }
    : '[' Expr '|' sep_by1(CompIter, ',') ']'  { Comprehension $2 (toList $4) () }


CompIter :: { CompTest () }
    : IdentName '<-' Expr                       { CompIter $1 $3 () }
    | BoolExpr                                  { CompFil $1 () }


----------------------------
--    Pattern Matching    --
----------------------------

Match :: { Match () }
    : Term
        {%  case exprToMatch $1 of
                Just match -> pure match
                Nothing -> internalError "Syntax error in match." }


-----------------
--    Names    --
-----------------

IdentName :: { Name 'Identifier }
    : IDENTIFIER                            { IdentName (ident $1) }


QualIdentName :: { Name QualIdent }
    : QUALIDENT
        { case $1 of
            TQualIdent _ bs -> QualIdentName (ModName . map mkIdent $ init bs) (IdentName . mkIdent $ last bs) }


Identifier :: { Identifier }
    : IdentName                             { Id $1 }
    | QualIdentName                         { Id $1 }


TypeName :: { Name 'Type }
    : TYPENAME                              { TypeName (ident $1) }


QualTypeName :: { Name QualType }
    : QUALTYPE
        { case $1 of
            TQualType _ bs -> QualTypeName (ModName . map mkIdent $ init bs) (TypeName . mkIdent $ last bs) }


TypeIdentifier :: { TypeIdentifier }
    : TypeName                              { TI $1 }
    | QualTypeName                          { TI $1 }


--------------------
--    Literals    --
--------------------

Literal :: { Lit () }
    : INTEGER                               { LitInt (integer $1)   (literal $1) () }
    | FLOAT                                 { LitFlt (float $1)     (literal $1) () }
    | IMAGINARY                             { LitImg (imaginary $1) (literal $1) () }
    | StrLiteral                            { $1 }
    | TypeIdentifier                        { LitSum $1 () }
    | or(TRUE, FALSE)                       { LitBool (isLeft $1) () }
    | NONE                                  { LitNone () }


StrLiteral :: { Lit () }
    : STRING                                { LitStr Str     (literal $1) () }
    | BYTESTRING                            { LitStr Byte    (literal $1) () }
    | FMTSTRING                             { LitStr Fmt     (literal $1) () }
    | RAWSTRING                             { LitStr Raw     (literal $1) () }
    | RAWBYTESTRING                         { LitStr RawByte (literal $1) () }
    | RAWFMTSTRING                          { LitStr RawFmt  (literal $1) () }


ListLiteral :: { Lit () }
    : '[' sep_by(Expr, ',') ']'       { LitList (toList $2) () }


TupleLiteral :: { Lit () }
    : '(' Expr ',' sep_by1(Expr, ',') ')' { LitTuple ($2 : toList $4) () }


{
ident :: Token -> Ident
ident = mkIdent . literal


qualIdentToModName :: Name 'QualIdent -> Name 'Mod
qualIdentToModName (QualIdentName (ModName mod) (IdentName n)) = ModName $ mod ++ [n]


revertArgs :: Constraint -> Constraint
revertArgs (Constraint cls args) = Constraint cls (reverse args)
}