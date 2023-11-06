{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LANGUAGE DeriveGeneric #-}

module Sdf where

import Data.Data
import Data.Typeable
import Data.ATerm.Lib
import Data.Generics.Strafunski.StrategyLib.Models.Deriving.TermRep
import GHC.Generics (Generic(..))
import Prelude hiding (Left, Right)

instance ATermConvertible Grammar
data Grammar = Aliases     Aliases
	     | Restrictions     Restrictions
	     | Sorts      Symbols
	     | Priorities     Priorities
	     | ImpSection      ImpSection
	     | LexicalSyntax      Productions
	     | ContextFreeSyntax       Productions
	     | Variables     Productions
	     | LexicalVariables      Productions
	     | EmptyGrammar
	     | ConcGrammars      Grammar Grammar
	     | Syntax     Productions
	     | LexicalPriorities      Priorities
	     | ContextFreePriorities       Priorities
	     | LexicalRestrictions      Restrictions
	     | ContextFreeRestrictions       Restrictions
         | ContextFreeStartSymbols [Symbol]
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Alias
data Alias = Alias     Symbol Symbol
      deriving (Data, Typeable, Generic, Show)

type Aliases = [Alias]

instance ATermConvertible Lookahead
data Lookahead = CharClass      CharClass
	       | Seq     CharClass Lookaheads
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Lookaheads
data Lookaheads = Single     Lookahead
		| Alt     Lookaheads Lookaheads
		| List1     [Lookahead]
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Restriction
data Restriction = Follow     Symbols Lookaheads
      deriving (Data, Typeable, Generic, Show)

type Restrictions = [Restriction]

instance ATermConvertible Attribute
data Attribute = Reject
	       | Prefer
	       | Avoid
	       | Cons1     ATerm'
	       | Constructor
	       | Memo
	       | Traverse
	       | Bracket
	       | Atr     Associativity
	       | Id     ModuleName
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible OptExp
data OptExp = Present     IntCon
	    | Absent
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible RealCon
data RealCon = RealCon      IntCon NatCon OptExp
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible AFun
data AFun = Literal     Literal
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible ATerm'
data ATerm' = Fun     AFun
    | Appl AFun [ATerm']
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Symbol
data Symbol = Label     Literal Symbol
	    | Lit     Literal
	    | Sort     Sort
	    | CharClass1      CharClass
	    | Empty1
	    | Seq1     Symbol [Symbol]
	    | Opt     Symbol
	    | Iter     Symbol
	    | IterStar      Symbol
	    | IterSep      Symbol Symbol
	    | IterStarSep       Symbol Symbol
	    | IterN      Symbol NatCon
	    | IterSepN       Symbol Symbol NatCon
	    | Set     Symbol
	    | Pair     Symbol Symbol
	    | Func     Symbols Symbol
	    | Alt1     Symbol Symbol
	    | Perm     Symbols
	    | Cf     Symbol
	    | Lex     Symbol
	    | Varsym     Symbol
	    | Layout
	    | Start
	    | FileStart
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Literal
data Literal = Quoted     QLiteral
	     | Uqlit     UQLiteral
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Production
data Production = ProdFun      Literal [Symbol] Symbol Attributes
		| Prod     Symbols Symbol Attributes
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Character
data Character = Numeric     NumChar
	       | Short     ShortChar
	       | Top
	       | Eof
	       | Bot
	       | LabelStart
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible CharRange
data CharRange = Character     Character
	       | Range     Character Character
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible CharRanges
data CharRanges = CharRange     CharRange
		| Conc     CharRanges CharRanges
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible OptCharRanges
data OptCharRanges = Absent1
		   | Present1     CharRanges
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible CharClass
data CharClass = SimpleCharclass      OptCharRanges
	       | Comp     CharClass
	       | Diff     CharClass CharClass
	       | Isect     CharClass CharClass
	       | Union     CharClass CharClass
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Associativity
data Associativity = Left
		   | Right
		   | NonAssoc
		   | Assoc
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Group
data Group = SimpleGroup      Production
	   | ProdsGroup      Productions
	   | AssocGroup      Associativity Productions
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Priority
data Priority = Chain     [Group]
	      | Assoc1     Group Associativity Group
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Priorities
data Priorities = Comma     [Priority]
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible IntCon
data IntCon = Natural     NatCon
	    | Positive     NatCon
	    | Negative     NatCon
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Renamings
data Renamings = Renamings     [Renaming]
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Renaming
data Renaming = Symbol     Symbol Symbol
	      | Production     Production Production
      deriving (Data, Typeable, Generic, Show)

type Definition = [Module]

instance ATermConvertible Module
data Module = Module      ModuleName [ImpSection] Sections
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible Section
data Section = Exports      Grammar
	     | Hiddens     Grammar
      deriving (Data, Typeable, Generic, Show)

type Sections = [Section]

instance ATermConvertible ModuleName
data ModuleName = Unparameterized     ModuleId
		| Parameterized     ModuleId Symbols
      deriving (Data, Typeable, Generic, Show)

instance ATermConvertible ImpSection
data ImpSection = Imports      Imports
      deriving (Data, Typeable, Generic, Show)

type Imports = [Import]

instance ATermConvertible Import
data Import = Module1     ModuleName
	    | RenamedModule      ModuleName Renamings
      deriving (Data, Typeable, Generic, Show)

type Symbols = [Symbol]

instance ATermConvertible Attributes
data Attributes = Attrs     [Attribute]
		| NoAttrs
      deriving (Data, Typeable, Generic, Show)

type Productions = [Production]

instance ATermConvertible SDF
data SDF = Definition     Definition
       deriving (Data, Typeable, Generic, Show)

type AlphaNumericalEscChar = String

type DecimalEscChar = String

type EscChar = String

type L_Char = String

type QLiteral = String

type UQLiteral = String

type Sort = String

type NumChar = String

type ShortChar = String

type NatCon = String

type ModuleWord = String

type ModuleId = String

-- {-* Generated by DrIFT : Look, but Don't Touch. *-}
-- instance ATermConvertible Grammar where
--     toATerm (Aliases     aa) = (AAppl "Aliases"     [ toATerm aa ])
--     toATerm (Restrictions     aa) = (AAppl "Restrictions"     [ toATerm aa ])
--     toATerm (Sorts      aa) = (AAppl "Sorts"      [ toATerm aa ])
--     toATerm (Priorities     aa) = (AAppl "Priorities"     [ toATerm aa ])
--     toATerm (ImpSection      aa) = (AAppl "ImpSection"      [ toATerm aa ])
--     toATerm (LexicalSyntax      aa) = (AAppl "LexicalSyntax"      [ toATerm aa ])
--     toATerm (ContextFreeSyntax       aa) = (AAppl "ContextFreeSyntax"       [ toATerm aa ])
--     toATerm (Variables     aa) = (AAppl "Variables"     [ toATerm aa ])
--     toATerm (LexicalVariables      aa) = (AAppl "LexicalVariables"      [ toATerm aa ])
--     toATerm EmptyGrammar      = (AAppl "EmptyGrammar"      [ ])
--     toATerm (ConcGrammars      aa ab) = (AAppl "ConcGrammars"      [ toATerm aa,toATerm ab ])
--     toATerm (Syntax     aa) = (AAppl "Syntax"     [ toATerm aa ])
--     toATerm (LexicalPriorities      aa) = (AAppl "LexicalPriorities"      [ toATerm aa ])
--     toATerm (ContextFreePriorities       aa) = (AAppl "ContextFreePriorities"       [ toATerm aa ])
--     toATerm (LexicalRestrictions      aa) = (AAppl "LexicalRestrictions"      [ toATerm aa ])
--     toATerm (ContextFreeRestrictions       aa) = (AAppl "ContextFreeRestrictions"       [ toATerm aa ])
--     fromATerm (AAppl "Aliases"     [ aa ]) = let aa' = fromATerm aa in (Aliases     aa')
--     fromATerm (AAppl "Restrictions"     [ aa ]) = let aa' = fromATerm aa in (Restrictions     aa')
--     fromATerm (AAppl "Sorts"      [ aa ]) = let aa' = fromATerm aa in (Sorts      aa')
--     fromATerm (AAppl "Priorities"     [ aa ]) = let aa' = fromATerm aa in (Priorities     aa')
--     fromATerm (AAppl "ImpSection"      [ aa ]) = let aa' = fromATerm aa in (ImpSection      aa')
--     fromATerm (AAppl "LexicalSyntax"      [ aa ]) = let aa' = fromATerm aa in (LexicalSyntax      aa')
--     fromATerm (AAppl "ContextFreeSyntax"       [ aa ]) = let aa' = fromATerm aa in (ContextFreeSyntax       aa')
--     fromATerm (AAppl "Variables"     [ aa ]) = let aa' = fromATerm aa in (Variables     aa')
--     fromATerm (AAppl "LexicalVariables"      [ aa ]) = let aa' = fromATerm aa in (LexicalVariables      aa')
--     fromATerm (AAppl "EmptyGrammar"      [ ]) = let in EmptyGrammar
--     fromATerm (AAppl "ConcGrammars"      [ aa,ab ]) = let aa' = fromATerm aa
-- 							  ab' = fromATerm ab in (ConcGrammars      aa' ab')
--     fromATerm (AAppl "Syntax"     [ aa ]) = let aa' = fromATerm aa in (Syntax     aa')
--     fromATerm (AAppl "LexicalPriorities"      [ aa ]) = let aa' = fromATerm aa in (LexicalPriorities      aa')
--     fromATerm (AAppl "ContextFreePriorities"       [ aa ]) = let aa' = fromATerm aa in (ContextFreePriorities       aa')
--     fromATerm (AAppl "LexicalRestrictions"      [ aa ]) = let aa' = fromATerm aa in (LexicalRestrictions      aa')
--     fromATerm (AAppl "ContextFreeRestrictions"       [ aa ]) = let aa' = fromATerm aa in (ContextFreeRestrictions       aa')
--     fromATerm u = fromATermError "Grammar" u
--
-- instance ATermConvertible Alias where
--     toATerm (Alias     aa ab) = (AAppl "Alias"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "Alias"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						  ab' = fromATerm ab in (Alias     aa' ab')
--     fromATerm u = fromATermError "Alias" u
--
-- instance ATermConvertible Lookahead where
--     toATerm (CharClass      aa) = (AAppl "CharClass"      [ toATerm aa ])
--     toATerm (Seq     aa ab) = (AAppl "Seq"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "CharClass"      [ aa ]) = let aa' = fromATerm aa in (CharClass      aa')
--     fromATerm (AAppl "Seq"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						ab' = fromATerm ab in (Seq     aa' ab')
--     fromATerm u = fromATermError "Lookahead" u
--
-- instance ATermConvertible Lookaheads where
--     toATerm (Single     aa) = (AAppl "Single"     [ toATerm aa ])
--     toATerm (Alt     aa ab) = (AAppl "Alt"     [ toATerm aa,toATerm ab ])
--     toATerm (List1     aa) = (AAppl "List1"     [ toATerm aa ])
--     fromATerm (AAppl "Single"     [ aa ]) = let aa' = fromATerm aa in (Single     aa')
--     fromATerm (AAppl "Alt"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						ab' = fromATerm ab in (Alt     aa' ab')
--     fromATerm (AAppl "List1"     [ aa ]) = let aa' = fromATerm aa in (List1     aa')
--     fromATerm u = fromATermError "Lookaheads" u
--
-- instance ATermConvertible Restriction where
--     toATerm (Follow     aa ab) = (AAppl "Follow"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "Follow"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						   ab' = fromATerm ab in (Follow     aa' ab')
--     fromATerm u = fromATermError "Restriction" u
--
-- instance ATermConvertible Attribute where
--     toATerm Reject     = (AAppl "Reject"     [ ])
--     toATerm Prefer     = (AAppl "Prefer"     [ ])
--     toATerm Avoid     = (AAppl "Avoid"     [ ])
--     toATerm (Cons1     aa) = (AAppl "Cons1"     [ toATerm aa ])
--     toATerm Constructor     = (AAppl "Constructor"     [ ])
--     toATerm Memo     = (AAppl "Memo"     [ ])
--     toATerm Traverse     = (AAppl "Traverse"     [ ])
--     toATerm Bracket     = (AAppl "Bracket"     [ ])
--     toATerm (Atr     aa) = (AAppl "Atr"     [ toATerm aa ])
--     toATerm (Id     aa) = (AAppl "Id"     [ toATerm aa ])
--     fromATerm (AAppl "Reject"     [ ]) = let in Reject
--     fromATerm (AAppl "Prefer"     [ ]) = let in Prefer
--     fromATerm (AAppl "Avoid"     [ ]) = let in Avoid
--     fromATerm (AAppl "Cons1"     [ aa ]) = let aa' = fromATerm aa in (Cons1     aa')
--     fromATerm (AAppl "Constructor"     [ ]) = let in Constructor
--     fromATerm (AAppl "Memo"     [ ]) = let in Memo
--     fromATerm (AAppl "Traverse"     [ ]) = let in Traverse
--     fromATerm (AAppl "Bracket"     [ ]) = let in Bracket
--     fromATerm (AAppl "Atr"     [ aa ]) = let aa' = fromATerm aa in (Atr     aa')
--     fromATerm (AAppl "Id"     [ aa ]) = let aa' = fromATerm aa in (Id     aa')
--     fromATerm u = fromATermError "Attribute" u
--
-- instance ATermConvertible OptExp where
--     toATerm (Present     aa) = (AAppl "Present"     [ toATerm aa ])
--     toATerm Absent     = (AAppl "Absent"     [ ])
--     fromATerm (AAppl "Present"     [ aa ]) = let aa' = fromATerm aa in (Present     aa')
--     fromATerm (AAppl "Absent"     [ ]) = let in Absent
--     fromATerm u = fromATermError "OptExp" u
--
-- instance ATermConvertible RealCon where
--     toATerm (RealCon      aa ab ac) = (AAppl "RealCon"      [ toATerm aa,toATerm ab,toATerm ac ])
--     fromATerm (AAppl "RealCon"      [ aa,ab,ac ]) = let aa' = fromATerm aa
-- 							ab' = fromATerm ab
-- 							ac' = fromATerm ac in (RealCon      aa' ab' ac')
--     fromATerm u = fromATermError "RealCon" u
--
-- instance ATermConvertible AFun where
--     toATerm (Literal     aa) = (AAppl "Literal"     [ toATerm aa ])
--     fromATerm (AAppl "Literal"     [ aa ]) = let aa' = fromATerm aa in (Literal     aa')
--     fromATerm u = fromATermError "AFun" u
--
-- instance ATermConvertible ATerm' where
--     toATerm (Fun     aa) = (AAppl "Fun"     [ toATerm aa ])
--     fromATerm (AAppl "Fun"     [ aa ]) = let aa' = fromATerm aa in (Fun     aa')
--     fromATerm u = fromATermError "ATerm'" u
--
-- instance ATermConvertible Symbol where
--     toATerm (Label     aa ab) = (AAppl "Label"     [ toATerm aa,toATerm ab ])
--     toATerm (Lit     aa) = (AAppl "Lit"     [ toATerm aa ])
--     toATerm (Sort     aa) = (AAppl "Sort"     [ toATerm aa ])
--     toATerm (CharClass1      aa) = (AAppl "CharClass1"      [ toATerm aa ])
--     toATerm Empty1     = (AAppl "Empty1"     [ ])
--     toATerm (Seq1     aa ab) = (AAppl "Seq1"     [ toATerm aa,toATerm ab ])
--     toATerm (Opt     aa) = (AAppl "Opt"     [ toATerm aa ])
--     toATerm (Iter     aa) = (AAppl "Iter"     [ toATerm aa ])
--     toATerm (IterStar      aa) = (AAppl "IterStar"      [ toATerm aa ])
--     toATerm (IterSep      aa ab) = (AAppl "IterSep"      [ toATerm aa,toATerm ab ])
--     toATerm (IterStarSep       aa ab) = (AAppl "IterStarSep"       [ toATerm aa,toATerm ab ])
--     toATerm (IterN      aa ab) = (AAppl "IterN"      [ toATerm aa,toATerm ab ])
--     toATerm (IterSepN       aa ab ac) = (AAppl "IterSepN"       [ toATerm aa,toATerm ab,toATerm ac ])
--     toATerm (Set     aa) = (AAppl "Set"     [ toATerm aa ])
--     toATerm (Pair     aa ab) = (AAppl "Pair"     [ toATerm aa,toATerm ab ])
--     toATerm (Func     aa ab) = (AAppl "Func"     [ toATerm aa,toATerm ab ])
--     toATerm (Alt1     aa ab) = (AAppl "Alt1"     [ toATerm aa,toATerm ab ])
--     toATerm (Perm     aa) = (AAppl "Perm"     [ toATerm aa ])
--     toATerm (Cf     aa) = (AAppl "Cf"     [ toATerm aa ])
--     toATerm (Lex     aa) = (AAppl "Lex"     [ toATerm aa ])
--     toATerm (Varsym     aa) = (AAppl "Varsym"     [ toATerm aa ])
--     toATerm Layout     = (AAppl "Layout"     [ ])
--     toATerm Start     = (AAppl "Start"     [ ])
--     toATerm FileStart      = (AAppl "FileStart"      [ ])
--     fromATerm (AAppl "Label"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						  ab' = fromATerm ab in (Label     aa' ab')
--     fromATerm (AAppl "Lit"     [ aa ]) = let aa' = fromATerm aa in (Lit     aa')
--     fromATerm (AAppl "Sort"     [ aa ]) = let aa' = fromATerm aa in (Sort     aa')
--     fromATerm (AAppl "CharClass1"      [ aa ]) = let aa' = fromATerm aa in (CharClass1      aa')
--     fromATerm (AAppl "Empty1"     [ ]) = let in Empty1
--     fromATerm (AAppl "Seq1"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						 ab' = fromATerm ab in (Seq1     aa' ab')
--     fromATerm (AAppl "Opt"     [ aa ]) = let aa' = fromATerm aa in (Opt     aa')
--     fromATerm (AAppl "Iter"     [ aa ]) = let aa' = fromATerm aa in (Iter     aa')
--     fromATerm (AAppl "IterStar"      [ aa ]) = let aa' = fromATerm aa in (IterStar      aa')
--     fromATerm (AAppl "IterSep"      [ aa,ab ]) = let aa' = fromATerm aa
-- 						     ab' = fromATerm ab in (IterSep      aa' ab')
--     fromATerm (AAppl "IterStarSep"       [ aa,ab ]) = let aa' = fromATerm aa
-- 							  ab' = fromATerm ab in (IterStarSep       aa' ab')
--     fromATerm (AAppl "IterN"      [ aa,ab ]) = let aa' = fromATerm aa
-- 						   ab' = fromATerm ab in (IterN      aa' ab')
--     fromATerm (AAppl "IterSepN"       [ aa,ab,ac ]) = let aa' = fromATerm aa
-- 							  ab' = fromATerm ab
-- 							  ac' = fromATerm ac in (IterSepN       aa' ab' ac')
--     fromATerm (AAppl "Set"     [ aa ]) = let aa' = fromATerm aa in (Set     aa')
--     fromATerm (AAppl "Pair"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						 ab' = fromATerm ab in (Pair     aa' ab')
--     fromATerm (AAppl "Func"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						 ab' = fromATerm ab in (Func     aa' ab')
--     fromATerm (AAppl "Alt1"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						 ab' = fromATerm ab in (Alt1     aa' ab')
--     fromATerm (AAppl "Perm"     [ aa ]) = let aa' = fromATerm aa in (Perm     aa')
--     fromATerm (AAppl "Cf"     [ aa ]) = let aa' = fromATerm aa in (Cf     aa')
--     fromATerm (AAppl "Lex"     [ aa ]) = let aa' = fromATerm aa in (Lex     aa')
--     fromATerm (AAppl "Varsym"     [ aa ]) = let aa' = fromATerm aa in (Varsym     aa')
--     fromATerm (AAppl "Layout"     [ ]) = let in Layout
--     fromATerm (AAppl "Start"     [ ]) = let in Start
--     fromATerm (AAppl "FileStart"      [ ]) = let in FileStart
--     fromATerm u = fromATermError "Symbol" u
--
-- instance ATermConvertible Literal where
--     toATerm (Quoted     aa) = (AAppl "Quoted"     [ toATerm aa ])
--     toATerm (Uqlit     aa) = (AAppl "Uqlit"     [ toATerm aa ])
--     fromATerm (AAppl "Quoted"     [ aa ]) = let aa' = fromATerm aa in (Quoted     aa')
--     fromATerm (AAppl "Uqlit"     [ aa ]) = let aa' = fromATerm aa in (Uqlit     aa')
--     fromATerm u = fromATermError "Literal" u
--
-- instance ATermConvertible Production where
--     toATerm (ProdFun      aa ab ac ad) = (AAppl "ProdFun"      [ toATerm aa,toATerm ab,toATerm ac,toATerm ad ])
--     toATerm (Prod     aa ab ac) = (AAppl "Prod"     [ toATerm aa,toATerm ab,toATerm ac ])
--     fromATerm (AAppl "ProdFun"      [ aa,ab,ac,ad ]) = let aa' = fromATerm aa
-- 							   ab' = fromATerm ab
-- 							   ac' = fromATerm ac
-- 							   ad' = fromATerm ad in (ProdFun      aa' ab' ac' ad')
--     fromATerm (AAppl "Prod"     [ aa,ab,ac ]) = let aa' = fromATerm aa
-- 						    ab' = fromATerm ab
-- 						    ac' = fromATerm ac in (Prod     aa' ab' ac')
--     fromATerm u = fromATermError "Production" u
--
-- instance ATermConvertible Character where
--     toATerm (Numeric     aa) = (AAppl "Numeric"     [ toATerm aa ])
--     toATerm (Short     aa) = (AAppl "Short"     [ toATerm aa ])
--     toATerm Top     = (AAppl "Top"     [ ])
--     toATerm Eof     = (AAppl "Eof"     [ ])
--     toATerm Bot     = (AAppl "Bot"     [ ])
--     toATerm LabelStart      = (AAppl "LabelStart"      [ ])
--     fromATerm (AAppl "Numeric"     [ aa ]) = let aa' = fromATerm aa in (Numeric     aa')
--     fromATerm (AAppl "Short"     [ aa ]) = let aa' = fromATerm aa in (Short     aa')
--     fromATerm (AAppl "Top"     [ ]) = let in Top
--     fromATerm (AAppl "Eof"     [ ]) = let in Eof
--     fromATerm (AAppl "Bot"     [ ]) = let in Bot
--     fromATerm (AAppl "LabelStart"      [ ]) = let in LabelStart
--     fromATerm u = fromATermError "Character" u
--
-- instance ATermConvertible CharRange where
--     toATerm (Character     aa) = (AAppl "Character"     [ toATerm aa ])
--     toATerm (Range     aa ab) = (AAppl "Range"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "Character"     [ aa ]) = let aa' = fromATerm aa in (Character     aa')
--     fromATerm (AAppl "Range"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						  ab' = fromATerm ab in (Range     aa' ab')
--     fromATerm u = fromATermError "CharRange" u
--
-- instance ATermConvertible CharRanges where
--     toATerm (CharRange     aa) = (AAppl "CharRange"     [ toATerm aa ])
--     toATerm (Conc     aa ab) = (AAppl "Conc"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "CharRange"     [ aa ]) = let aa' = fromATerm aa in (CharRange     aa')
--     fromATerm (AAppl "Conc"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						 ab' = fromATerm ab in (Conc     aa' ab')
--     fromATerm u = fromATermError "CharRanges" u
--
-- instance ATermConvertible OptCharRanges where
--     toATerm Absent1     = (AAppl "Absent1"     [ ])
--     toATerm (Present1     aa) = (AAppl "Present1"     [ toATerm aa ])
--     fromATerm (AAppl "Absent1"     [ ]) = let in Absent1
--     fromATerm (AAppl "Present1"     [ aa ]) = let aa' = fromATerm aa in (Present1     aa')
--     fromATerm u = fromATermError "OptCharRanges" u
--
-- instance ATermConvertible CharClass where
--     toATerm (SimpleCharclass      aa) = (AAppl "SimpleCharclass"      [ toATerm aa ])
--     toATerm (Comp     aa) = (AAppl "Comp"     [ toATerm aa ])
--     toATerm (Diff     aa ab) = (AAppl "Diff"     [ toATerm aa,toATerm ab ])
--     toATerm (Isect     aa ab) = (AAppl "Isect"     [ toATerm aa,toATerm ab ])
--     toATerm (Union     aa ab) = (AAppl "Union"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "SimpleCharclass"      [ aa ]) = let aa' = fromATerm aa in (SimpleCharclass      aa')
--     fromATerm (AAppl "Comp"     [ aa ]) = let aa' = fromATerm aa in (Comp     aa')
--     fromATerm (AAppl "Diff"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						 ab' = fromATerm ab in (Diff     aa' ab')
--     fromATerm (AAppl "Isect"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						  ab' = fromATerm ab in (Isect     aa' ab')
--     fromATerm (AAppl "Union"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						  ab' = fromATerm ab in (Union     aa' ab')
--     fromATerm u = fromATermError "CharClass" u
--
-- instance ATermConvertible Associativity where
--     toATerm Left     = (AAppl "Left"     [ ])
--     toATerm Right     = (AAppl "Right"     [ ])
--     toATerm NonAssoc      = (AAppl "NonAssoc"      [ ])
--     toATerm Assoc     = (AAppl "Assoc"     [ ])
--     fromATerm (AAppl "Left"     [ ]) = let in Left
--     fromATerm (AAppl "Right"     [ ]) = let in Right
--     fromATerm (AAppl "NonAssoc"      [ ]) = let in NonAssoc
--     fromATerm (AAppl "Assoc"     [ ]) = let in Assoc
--     fromATerm u = fromATermError "Associativity" u
--
-- instance ATermConvertible Group where
--     toATerm (SimpleGroup      aa) = (AAppl "SimpleGroup"      [ toATerm aa ])
--     toATerm (ProdsGroup      aa) = (AAppl "ProdsGroup"      [ toATerm aa ])
--     toATerm (AssocGroup      aa ab) = (AAppl "AssocGroup"      [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "SimpleGroup"      [ aa ]) = let aa' = fromATerm aa in (SimpleGroup      aa')
--     fromATerm (AAppl "ProdsGroup"      [ aa ]) = let aa' = fromATerm aa in (ProdsGroup      aa')
--     fromATerm (AAppl "AssocGroup"      [ aa,ab ]) = let aa' = fromATerm aa
-- 							ab' = fromATerm ab in (AssocGroup      aa' ab')
--     fromATerm u = fromATermError "Group" u
--
-- instance ATermConvertible Priority where
--     toATerm (Chain     aa) = (AAppl "Chain"     [ toATerm aa ])
--     toATerm (Assoc1     aa ab ac) = (AAppl "Assoc1"     [ toATerm aa,toATerm ab,toATerm ac ])
--     fromATerm (AAppl "Chain"     [ aa ]) = let aa' = fromATerm aa in (Chain     aa')
--     fromATerm (AAppl "Assoc1"     [ aa,ab,ac ]) = let aa' = fromATerm aa
-- 						      ab' = fromATerm ab
-- 						      ac' = fromATerm ac in (Assoc1     aa' ab' ac')
--     fromATerm u = fromATermError "Priority" u
--
-- instance ATermConvertible Priorities where
--     toATerm (Comma     aa) = (AAppl "Comma"     [ toATerm aa ])
--     fromATerm (AAppl "Comma"     [ aa ]) = let aa' = fromATerm aa in (Comma     aa')
--     fromATerm u = fromATermError "Priorities" u
--
-- instance ATermConvertible IntCon where
--     toATerm (Natural     aa) = (AAppl "Natural"     [ toATerm aa ])
--     toATerm (Positive     aa) = (AAppl "Positive"     [ toATerm aa ])
--     toATerm (Negative     aa) = (AAppl "Negative"     [ toATerm aa ])
--     fromATerm (AAppl "Natural"     [ aa ]) = let aa' = fromATerm aa in (Natural     aa')
--     fromATerm (AAppl "Positive"     [ aa ]) = let aa' = fromATerm aa in (Positive     aa')
--     fromATerm (AAppl "Negative"     [ aa ]) = let aa' = fromATerm aa in (Negative     aa')
--     fromATerm u = fromATermError "IntCon" u
--
-- instance ATermConvertible Renamings where
--     toATerm (Renamings     aa) = (AAppl "Renamings"     [ toATerm aa ])
--     fromATerm (AAppl "Renamings"     [ aa ]) = let aa' = fromATerm aa in (Renamings     aa')
--     fromATerm u = fromATermError "Renamings" u
--
-- instance ATermConvertible Renaming where
--     toATerm (Symbol     aa ab) = (AAppl "Symbol"     [ toATerm aa,toATerm ab ])
--     toATerm (Production     aa ab) = (AAppl "Production"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "Symbol"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						   ab' = fromATerm ab in (Symbol     aa' ab')
--     fromATerm (AAppl "Production"     [ aa,ab ]) = let aa' = fromATerm aa
-- 						       ab' = fromATerm ab in (Production     aa' ab')
--     fromATerm u = fromATermError "Renaming" u
--
-- instance ATermConvertible Module where
--     toATerm (Module      aa ab ac) = (AAppl "Module"      [ toATerm aa,toATerm ab,toATerm ac ])
--     fromATerm (AAppl "Module"      [ aa,ab,ac ]) = let aa' = fromATerm aa
-- 						       ab' = fromATerm ab
-- 						       ac' = fromATerm ac in (Module      aa' ab' ac')
--     fromATerm u = fromATermError "Module" u
--
-- instance ATermConvertible Section where
--     toATerm (Exports      aa) = (AAppl "Exports"      [ toATerm aa ])
--     toATerm (Hiddens     aa) = (AAppl "Hiddens"     [ toATerm aa ])
--     fromATerm (AAppl "Exports"      [ aa ]) = let aa' = fromATerm aa in (Exports      aa')
--     fromATerm (AAppl "Hiddens"     [ aa ]) = let aa' = fromATerm aa in (Hiddens     aa')
--     fromATerm u = fromATermError "Section" u
--
-- instance ATermConvertible ModuleName where
--     toATerm (Unparameterized     aa) = (AAppl "Unparameterized"     [ toATerm aa ])
--     toATerm (Parameterized     aa ab) = (AAppl "Parameterized"     [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "Unparameterized"     [ aa ]) = let aa' = fromATerm aa in (Unparameterized     aa')
--     fromATerm (AAppl "Parameterized"     [ aa,ab ]) = let aa' = fromATerm aa
-- 							  ab' = fromATerm ab in (Parameterized     aa' ab')
--     fromATerm u = fromATermError "ModuleName" u
--
-- instance ATermConvertible ImpSection where
--     toATerm (Imports      aa) = (AAppl "Imports"      [ toATerm aa ])
--     fromATerm (AAppl "Imports"      [ aa ]) = let aa' = fromATerm aa in (Imports      aa')
--     fromATerm u = fromATermError "ImpSection" u
--
-- instance ATermConvertible Import where
--     toATerm (Module1     aa) = (AAppl "Module1"     [ toATerm aa ])
--     toATerm (RenamedModule      aa ab) = (AAppl "RenamedModule"      [ toATerm aa,toATerm ab ])
--     fromATerm (AAppl "Module1"     [ aa ]) = let aa' = fromATerm aa in (Module1     aa')
--     fromATerm (AAppl "RenamedModule"      [ aa,ab ]) = let aa' = fromATerm aa
-- 							   ab' = fromATerm ab in (RenamedModule      aa' ab')
--     fromATerm u = fromATermError "Import" u
--
-- instance ATermConvertible Attributes where
--     toATerm (Attrs     aa) = (AAppl "Attrs"     [ toATerm aa ])
--     toATerm NoAttrs      = (AAppl "NoAttrs"      [ ])
--     fromATerm (AAppl "Attrs"     [ aa ]) = let aa' = fromATerm aa in (Attrs     aa')
--     fromATerm (AAppl "NoAttrs"      [ ]) = let in NoAttrs
--     fromATerm u = fromATermError "Attributes" u
--
-- instance ATermConvertible SDF where
--     toATerm (Definition     aa) = (AAppl "Definition"     [ toATerm aa ])
--     fromATerm (AAppl "Definition"     [ aa ]) = let aa' = fromATerm aa in (Definition     aa')
--     fromATerm u = fromATermError "SDF" u
--
-- --  Imported from other files :-
