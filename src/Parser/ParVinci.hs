{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.ParVinci
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Parser.AbsVinci
import Parser.LexVinci
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ((Parser.AbsVinci.BNFC'Position, Double)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Double))
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ((Parser.AbsVinci.BNFC'Position, Integer)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Integer))
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.VIdent)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.VIdent))
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.SIdent)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.SIdent))
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.TPolyIdent)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.TPolyIdent))
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Program Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Program Parser.AbsVinci.BNFC'Position))
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Phrase Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Phrase Parser.AbsVinci.BNFC'Position))
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.Phrase Parser.AbsVinci.BNFC'Position])) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.Phrase Parser.AbsVinci.BNFC'Position]))
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LetDef Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LetDef Parser.AbsVinci.BNFC'Position))
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LetBind Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LetBind Parser.AbsVinci.BNFC'Position))
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LetLVI Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LetLVI Parser.AbsVinci.BNFC'Position))
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.LetLVI Parser.AbsVinci.BNFC'Position])) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.LetLVI Parser.AbsVinci.BNFC'Position]))
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.LetBind Parser.AbsVinci.BNFC'Position])) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.LetBind Parser.AbsVinci.BNFC'Position]))
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position))
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LambdaVI Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.LambdaVI Parser.AbsVinci.BNFC'Position))
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.FieldDef Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.FieldDef Parser.AbsVinci.BNFC'Position))
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position])) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.Expr Parser.AbsVinci.BNFC'Position]))
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.LambdaVI Parser.AbsVinci.BNFC'Position])) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.LambdaVI Parser.AbsVinci.BNFC'Position]))
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.FieldDef Parser.AbsVinci.BNFC'Position])) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.FieldDef Parser.AbsVinci.BNFC'Position]))
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.ProcName Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.ProcName Parser.AbsVinci.BNFC'Position))
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.StructDef Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.StructDef Parser.AbsVinci.BNFC'Position))
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.FieldDecl Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.FieldDecl Parser.AbsVinci.BNFC'Position))
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.FieldDecl Parser.AbsVinci.BNFC'Position])) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.FieldDecl Parser.AbsVinci.BNFC'Position]))
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.TPolyIdent])) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, [Parser.AbsVinci.TPolyIdent]))
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Type Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Type Parser.AbsVinci.BNFC'Position))
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Type Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.Type Parser.AbsVinci.BNFC'Position))
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.RType Parser.AbsVinci.BNFC'Position)) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ((Parser.AbsVinci.BNFC'Position, Parser.AbsVinci.RType Parser.AbsVinci.BNFC'Position))
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x01\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x02\x00\x01\x00\x00\x00\x00\x20\x00\x00\x10\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x10\x00\x08\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x20\xa3\x91\x07\x00\x00\x00\x00\x40\x00\x00\x20\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x68\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x34\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x24\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x02\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\xec\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x64\x34\xf2\x00\x00\x00\x00\x00\x08\x00\x20\x01\x80\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x08\x00\x04\x00\x00\x00\x00\x80\x10\x00\x32\x1a\x79\x00\x00\x00\x00\x00\x04\x00\x90\x00\xc0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x06\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x40\x00\x20\x00\x00\x00\x00\x00\x04\x00\x68\x00\x00\x06\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\xc8\x68\xe4\x01\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x04\x00\x00\x00\x00\x00\x00\x00\x00\x10\x02\x40\x46\x23\x0f\x00\x00\x00\x00\x80\x10\x00\x12\x10\x38\x00\x00\x00\x00\x00\x84\x00\x90\x80\xc0\x01\x00\x00\x00\x00\x20\x04\x80\x04\x04\x0e\x00\x00\x00\x00\x00\x21\x00\x24\x20\x70\x00\x00\x00\x00\x00\x08\x01\x20\x01\x81\x03\x00\x00\x00\x00\x40\x08\x00\x09\x08\x1c\x00\x00\x00\x00\x00\x42\x00\x48\x40\xe0\x00\x00\x00\x00\x00\x10\x02\x40\x02\x02\x07\x00\x00\x00\x00\x80\x10\x00\x12\x10\x38\x00\x00\x00\x00\x00\x84\x00\x90\x80\xc0\x01\x00\x00\x00\x00\x20\x04\x80\x04\x04\x0e\x00\x00\x00\x00\x00\x21\x00\x24\x20\x70\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x19\x8d\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x10\x02\x40\x46\x23\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x06\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x20\x00\x40\x03\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x20\x00\x00\x00\x00\x00\x00\x00\x00\x40\x04\x01\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x19\x8d\x3c\x00\x00\x00\x00\x00\x02\x00\x34\x00\x00\x03\x00\x00\x00\x00\x10\x02\x40\x46\x23\x0f\x00\x00\x00\x00\x80\x10\x00\x32\x1a\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x21\x00\x64\x34\xf2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x90\xd1\xc8\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x64\x34\xf2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","Double","Integer","VIdent","SIdent","TPolyIdent","Program","Phrase","ListPhrase","LetDef","LetBind","LetLVI","ListLetLVI","ListLetBind","Expr8","Expr7","Expr6","Expr5","Expr4","Expr3","Expr2","Expr1","Expr","LambdaVI","FieldDef","ListExpr","ListLambdaVI","ListFieldDef","ProcName","StructDef","FieldDecl","ListFieldDecl","ListTPolyIdent","Type1","Type","RType","'!='","'%'","'&&'","'('","')'","'*'","'+'","','","'-'","'->'","'.'","'/'","':'","';;'","'<'","'<='","'='","'=='","'>'","'>='","'Bool'","'False'","'Float'","'Int'","'True'","'\\\\'","'_'","'and'","'else'","'if'","'in'","'let'","'not'","'struct'","'then'","'type'","'{'","'||'","'}'","L_doubl","L_integ","L_VIdent","L_SIdent","L_TPolyIdent","%eof"]
        bit_start = st * 83
        bit_end = (st + 1) * 83
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..82]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xc1\x00\xdf\xff\x00\x00\xdb\xff\x0c\x00\x00\x00\x00\x00\x00\x00\x06\x00\xfb\xff\xfb\xff\x30\x00\x00\x00\x16\x00\x3b\x00\x2c\x00\x58\x00\x00\x00\x00\x00\x06\x00\x06\x00\x00\x00\x00\x00\xc1\x00\x00\x00\x00\x00\xfc\xff\x6e\x00\x06\x00\x66\x00\x02\x00\x06\x00\x3e\x00\x53\x00\x00\x00\xfd\xff\x00\x00\x00\x00\x70\x00\x00\x00\xfd\xff\x00\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x69\x00\x87\x00\x21\x00\x00\x00\x00\x00\x0a\x00\x08\x00\xbf\x01\x6d\x00\x00\x00\x02\x00\x37\x00\x00\x00\x00\x00\x06\x00\x02\x00\x37\x00\x6f\x00\x00\x00\x89\x00\xfd\xff\x00\x00\x00\x00\x06\x00\xfd\xff\xa6\x00\xa4\x00\x00\x00\x00\x00\x02\x00\xa1\x00\xab\x00\x96\x00\x00\x00\x99\x00\xb9\x00\x00\x00\x4c\x00\x02\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\x1b\x00\xc5\x00\x02\x00\x9a\x00\x02\x00\x9a\x00\xbe\x00\xc4\x00\xaa\x00\xcd\x00\xfd\xff\x00\x00\x00\x00\x00\x00\xb6\x00\xfd\xff\xbb\x00\x00\x00\x00\x00\xf1\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x0a\x00\x08\x00\x08\x00\x08\x00\x08\x00\x08\x00\x00\x00\x08\x00\x00\x00\x00\x00\x02\x00\xfd\xff\x02\x00\x02\x00\x00\x00\xbc\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x00\x00\x00\xe3\x00\xef\x00\xf3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\xf8\x00\xf9\x00\x00\x00\x00\x00\xfe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x60\x00\x00\x00\x00\x00\xb3\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x00\xdb\x00\xc6\x00\x65\x00\xff\xff\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\x00\x42\x02\x00\x00\x00\x00\x9e\x00\xf2\x00\x45\x02\x6b\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x9f\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1e\x01\xc7\x01\xab\x01\xda\x01\xe0\x01\xf3\x01\xf9\x01\x0c\x02\x12\x02\x24\x02\x29\x02\x2d\x02\x3e\x02\x00\x00\x34\x01\xfc\x00\x4a\x01\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x15\x00\x4b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7c\x00\x54\x00\x60\x01\x76\x01\x00\x00\x85\x00\x8c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x95\x00\x00\x00\xa2\x01\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xf5\xff\x00\x00\xfe\xff\x00\x00\x00\x00\xf9\xff\xf8\xff\xf7\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xb5\xff\xba\xff\xed\xff\x00\x00\xf3\xff\xf0\xff\x00\x00\x00\x00\xc3\xff\xfc\xff\xf5\xff\xf4\xff\xc4\xff\xbe\xff\x00\x00\xef\xff\xab\xff\x00\x00\x00\x00\xb5\xff\x00\x00\xfa\xff\x00\x00\xb0\xff\xaf\xff\xac\xff\xf6\xff\x00\x00\xb1\xff\xb2\xff\xb3\xff\x00\x00\xb4\xff\xec\xff\xe9\xff\xea\xff\xeb\xff\x00\x00\x00\x00\xe2\xff\xe0\xff\xdd\xff\xd9\xff\xd6\xff\xcf\xff\xcd\xff\xc6\xff\xf2\xff\x00\x00\x00\x00\xe7\xff\xe8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xfd\xff\x00\x00\x00\x00\xee\xff\xc2\xff\x00\x00\x00\x00\x00\x00\xbe\xff\xbd\xff\xaa\xff\x00\x00\x00\x00\xbc\xff\x00\x00\xde\xff\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\xff\x00\x00\x00\x00\x00\x00\xad\xff\xae\xff\xb9\xff\x00\x00\x00\x00\x00\x00\xca\xff\xe6\xff\x00\x00\xdb\xff\xdc\xff\xda\xff\xd7\xff\xd8\xff\xd2\xff\xd3\xff\xd1\xff\xd4\xff\xd5\xff\xce\xff\xd0\xff\xcc\xff\xe4\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\x00\x00\x00\x00\xf1\xff\xc5\xff\xc1\xff\xbb\xff\x00\x00\xc9\xff\x00\x00\xc0\xff\x00\x00\xc8\xff\xb8\xff\xb6\xff\xe5\xff\x00\x00\xe1\xff\x00\x00\xcb\xff\xbf\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x04\x00\x04\x00\x04\x00\x08\x00\x02\x00\x04\x00\x28\x00\x2d\x00\x0d\x00\x04\x00\x09\x00\x02\x00\x0a\x00\x0b\x00\x07\x00\x06\x00\x09\x00\x15\x00\x02\x00\x17\x00\x18\x00\x0c\x00\x02\x00\x16\x00\x16\x00\x0e\x00\x19\x00\x1a\x00\x1f\x00\x1f\x00\x04\x00\x1e\x00\x1b\x00\x20\x00\x21\x00\x09\x00\x04\x00\x2b\x00\x25\x00\x2b\x00\x2c\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x1d\x00\x1e\x00\x2a\x00\x16\x00\x1d\x00\x1e\x00\x19\x00\x03\x00\x04\x00\x16\x00\x03\x00\x04\x00\x19\x00\x04\x00\x21\x00\x03\x00\x04\x00\x03\x00\x04\x00\x11\x00\x2c\x00\x28\x00\x29\x00\x2a\x00\x03\x00\x04\x00\x1c\x00\x28\x00\x29\x00\x2a\x00\x11\x00\x16\x00\x03\x00\x04\x00\x19\x00\x05\x00\x20\x00\x21\x00\x08\x00\x20\x00\x21\x00\x03\x00\x04\x00\x0d\x00\x20\x00\x21\x00\x20\x00\x21\x00\x02\x00\x28\x00\x29\x00\x2a\x00\x02\x00\x20\x00\x21\x00\x09\x00\x0a\x00\x02\x00\x0c\x00\x11\x00\x2c\x00\x20\x00\x21\x00\x02\x00\x09\x00\x0a\x00\x0a\x00\x0c\x00\x16\x00\x05\x00\x20\x00\x21\x00\x16\x00\x1b\x00\x25\x00\x19\x00\x0a\x00\x16\x00\x00\x00\x01\x00\x02\x00\x03\x00\x1b\x00\x2a\x00\x17\x00\x25\x00\x08\x00\x1a\x00\x02\x00\x02\x00\x1f\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0b\x00\x26\x00\x18\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2a\x00\x11\x00\x17\x00\x17\x00\x08\x00\x1a\x00\x1a\x00\x02\x00\x02\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x05\x00\x08\x00\x18\x00\x05\x00\x06\x00\x07\x00\x08\x00\x11\x00\x08\x00\x16\x00\x16\x00\x02\x00\x19\x00\x19\x00\x06\x00\x07\x00\x08\x00\x23\x00\x27\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x0a\x00\x2a\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x16\x00\x0d\x00\x08\x00\x0d\x00\x08\x00\x1c\x00\x0b\x00\x27\x00\x05\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x2a\x00\x20\x00\x27\x00\x22\x00\x08\x00\x24\x00\x2a\x00\x1d\x00\x05\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\x05\x00\x08\x00\x05\x00\x08\x00\x08\x00\x03\x00\x03\x00\x22\x00\x02\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x08\x00\x00\x00\x01\x00\x02\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x01\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0f\x00\x10\x00\xff\xff\x12\x00\x13\x00\x14\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\xff\xff\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x0d\x00\x0e\x00\x0f\x00\x0d\x00\x0e\x00\x0f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x29\x00\x20\x00\x20\x00\x4b\x00\x19\x00\x3e\x00\x03\x00\xff\xff\x4c\x00\x15\x00\x3f\x00\x64\x00\x1c\x00\x1d\x00\x62\x00\x65\x00\x63\x00\x2a\x00\x6b\x00\x2b\x00\x2c\x00\x66\x00\x6b\x00\x40\x00\x12\x00\x18\x00\x41\x00\x42\x00\x21\x00\x2d\x00\x3e\x00\x43\x00\x16\x00\x09\x00\x44\x00\x3f\x00\x68\x00\x0d\x00\x45\x00\x0d\x00\x23\x00\x03\x00\x46\x00\x17\x00\x0d\x00\x6c\x00\x6d\x00\x17\x00\x40\x00\x6c\x00\x99\x00\x41\x00\x24\x00\x25\x00\x40\x00\x24\x00\x25\x00\x41\x00\x3e\x00\x44\x00\x24\x00\x25\x00\x24\x00\x25\x00\x24\x00\x23\x00\x03\x00\x46\x00\x17\x00\x24\x00\x25\x00\x20\x00\x03\x00\x46\x00\x17\x00\xc4\xff\x40\x00\x24\x00\x25\x00\x41\x00\x87\x00\x26\x00\x27\x00\x88\x00\x26\x00\x6e\x00\x24\x00\x25\x00\x89\x00\x26\x00\x4f\x00\x26\x00\x4c\x00\x0e\x00\x03\x00\x46\x00\x17\x00\x19\x00\x26\x00\x70\x00\x0f\x00\x10\x00\x0e\x00\x11\x00\x1f\x00\x23\x00\x26\x00\x98\x00\x51\x00\x0f\x00\x10\x00\x48\x00\x2e\x00\x12\x00\x4a\x00\x26\x00\x94\x00\x1a\x00\x13\x00\x2d\x00\x1b\x00\x70\x00\x12\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x13\x00\x17\x00\x52\x00\x6b\x00\x33\x00\x53\x00\x51\x00\x51\x00\x6a\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x95\x00\x69\x00\x5a\x00\x96\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x17\x00\x51\x00\x52\x00\x52\x00\x33\x00\x75\x00\x91\x00\x19\x00\x19\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x95\x00\x90\x00\x4b\x00\x9f\x00\x03\x00\x04\x00\x05\x00\x06\x00\x8e\x00\x8d\x00\x4d\x00\x4d\x00\x19\x00\x56\x00\x4e\x00\x04\x00\x18\x00\x06\x00\x8b\x00\x8c\x00\x1c\x00\x48\x00\x2f\x00\x30\x00\x31\x00\x8a\x00\x17\x00\x07\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x12\x00\x75\x00\x74\x00\x66\x00\x33\x00\x07\x00\x69\x00\x73\x00\x72\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x17\x00\x09\x00\x98\x00\x0a\x00\x33\x00\x0b\x00\x17\x00\x9e\x00\x9d\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x58\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x87\x00\x9c\x00\x9b\x00\x88\x00\x33\x00\x0d\x00\x0b\x00\x46\x00\x77\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x55\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x8e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x85\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x78\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x76\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x93\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x92\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x90\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x33\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x9e\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x83\x00\x5b\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x00\x5e\x00\x00\x00\x5f\x00\x60\x00\x61\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x84\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x82\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x81\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x80\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x7f\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x7e\x00\x34\x00\x35\x00\x36\x00\x37\x00\x7d\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x34\x00\x35\x00\x36\x00\x37\x00\x7c\x00\x34\x00\x35\x00\x36\x00\x7b\x00\x34\x00\x35\x00\x36\x00\x7a\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x2f\x00\x30\x00\x31\x00\x2f\x00\x30\x00\x31\x00\x00\x00\x00\x00\x00\x00\x34\x00\x35\x00\x36\x00\x79\x00\x34\x00\x35\x00\x57\x00\x34\x00\x35\x00\x54\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 85) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85)
	]

happy_n_terms = 46 :: Int
happy_n_nonterms = 35 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Double)
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Integer)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.VIdent (tokenText happy_var_1))
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.SIdent (tokenText happy_var_1))
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.TPolyIdent (tokenText happy_var_1))
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((fst happy_var_1, Parser.AbsVinci.Prog (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((fst happy_var_1, Parser.AbsVinci.Value (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_8 = happySpecReduce_1  6# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut32 happy_x_1 of { happy_var_1 -> 
	happyIn10
		 ((fst happy_var_1, Parser.AbsVinci.StructDecl (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_9 = happyReduce 4# 6# happyReduction_9
happyReduction_9 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	happyIn10
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.TypeSynon (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_10 = happySpecReduce_0  7# happyReduction_10
happyReduction_10  =  happyIn11
		 ((Parser.AbsVinci.BNFC'NoPosition, [])
	)

happyReduce_11 = happySpecReduce_3  7# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_12 = happySpecReduce_2  8# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.Let (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_13 = happySpecReduce_3  9# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 ((fst happy_var_1, Parser.AbsVinci.ConstBind (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_14 = happyReduce 5# 9# happyReduction_14
happyReduction_14 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	case happyOut25 happy_x_5 of { happy_var_5 -> 
	happyIn13
		 ((fst happy_var_1, Parser.AbsVinci.ProcBind (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}}

happyReduce_15 = happySpecReduce_1  10# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ((fst happy_var_1, Parser.AbsVinci.LetLVI (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_16 = happySpecReduce_1  11# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_17 = happySpecReduce_2  11# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn15
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_18 = happySpecReduce_1  12# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_19 = happySpecReduce_3  12# happyReduction_19
happyReduction_19 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn16
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_20 = happySpecReduce_1  13# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 ((fst happy_var_1, Parser.AbsVinci.EId (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_21 = happySpecReduce_1  13# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 ((fst happy_var_1, Parser.AbsVinci.EInt (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_22 = happySpecReduce_1  13# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn17
		 ((fst happy_var_1, Parser.AbsVinci.EFloat (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_23 = happySpecReduce_1  13# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ETrue (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_24 = happySpecReduce_1  13# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.EFalse (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_25 = happySpecReduce_3  13# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	case happyOut6 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 ((fst happy_var_1, Parser.AbsVinci.EFieldGet (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_26 = happyReduce 5# 13# happyReduction_26
happyReduction_26 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut28 happy_x_4 of { happy_var_4 -> 
	happyIn17
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ETuple (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_27 = happySpecReduce_3  13# happyReduction_27
happyReduction_27 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), (snd happy_var_2))
	)}}

happyReduce_28 = happySpecReduce_2  14# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut17 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 ((fst happy_var_1, Parser.AbsVinci.EApp (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_29 = happySpecReduce_1  14# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_30 = happyReduce 5# 15# happyReduction_30
happyReduction_30 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	happyIn19
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ETyped (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_31 = happySpecReduce_1  15# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_32 = happySpecReduce_2  16# happyReduction_32
happyReduction_32 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ENeg (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_33 = happySpecReduce_2  16# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ENot (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_34 = happySpecReduce_1  16# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_35 = happySpecReduce_3  17# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ((fst happy_var_1, Parser.AbsVinci.EMul (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_36 = happySpecReduce_3  17# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ((fst happy_var_1, Parser.AbsVinci.EDiv (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_37 = happySpecReduce_3  17# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ((fst happy_var_1, Parser.AbsVinci.EMod (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_38 = happySpecReduce_1  17# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_39 = happySpecReduce_3  18# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 ((fst happy_var_1, Parser.AbsVinci.EAdd (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_40 = happySpecReduce_3  18# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 ((fst happy_var_1, Parser.AbsVinci.ESub (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_41 = happySpecReduce_1  18# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_42 = happySpecReduce_3  19# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((fst happy_var_1, Parser.AbsVinci.ELTH (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_43 = happySpecReduce_3  19# happyReduction_43
happyReduction_43 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((fst happy_var_1, Parser.AbsVinci.ELE (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_44 = happySpecReduce_3  19# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((fst happy_var_1, Parser.AbsVinci.EGTH (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_45 = happySpecReduce_3  19# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((fst happy_var_1, Parser.AbsVinci.EGE (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_46 = happySpecReduce_3  19# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((fst happy_var_1, Parser.AbsVinci.EEQU (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_47 = happySpecReduce_3  19# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((fst happy_var_1, Parser.AbsVinci.ENE (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_48 = happySpecReduce_1  19# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_49 = happySpecReduce_3  20# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, Parser.AbsVinci.EAnd (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_50 = happySpecReduce_1  20# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_51 = happySpecReduce_3  21# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 ((fst happy_var_1, Parser.AbsVinci.EOr (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_52 = happyReduce 6# 21# happyReduction_52
happyReduction_52 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	case happyOut25 happy_x_6 of { happy_var_6 -> 
	happyIn25
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ECond (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_53 = happySpecReduce_3  21# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 ((fst happy_var_1, Parser.AbsVinci.ELetIn (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_54 = happyReduce 4# 21# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { happy_var_2 -> 
	case happyOut25 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ELambda (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_55 = happyReduce 4# 21# happyReduction_55
happyReduction_55 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut7 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 ((fst happy_var_1, Parser.AbsVinci.ENamedCons (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_56 = happySpecReduce_3  21# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.ECons (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_57 = happySpecReduce_1  21# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_58 = happyReduce 5# 22# happyReduction_58
happyReduction_58 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { happy_var_2 -> 
	case happyOut37 happy_x_4 of { happy_var_4 -> 
	happyIn26
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.TypedVId (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_59 = happySpecReduce_1  22# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ((fst happy_var_1, Parser.AbsVinci.LambdaVId (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_60 = happySpecReduce_1  22# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn26
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.WildVId (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_61 = happySpecReduce_3  22# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn26
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.TupleVId (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_62 = happySpecReduce_3  23# happyReduction_62
happyReduction_62 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 ((fst happy_var_1, Parser.AbsVinci.FieldDef (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_63 = happySpecReduce_1  24# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_64 = happySpecReduce_3  24# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	case happyOut28 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_65 = happySpecReduce_1  25# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_66 = happySpecReduce_3  25# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_67 = happySpecReduce_1  26# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_68 = happySpecReduce_3  26# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_69 = happySpecReduce_1  27# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 ((fst happy_var_1, Parser.AbsVinci.ProcNameId (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_70 = happyReduce 6# 28# happyReduction_70
happyReduction_70 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	case happyOut34 happy_x_5 of { happy_var_5 -> 
	happyIn32
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.SDef (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}}

happyReduce_71 = happySpecReduce_3  29# happyReduction_71
happyReduction_71 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 ((fst happy_var_1, Parser.AbsVinci.FieldDecl (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_72 = happySpecReduce_1  30# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_73 = happySpecReduce_3  30# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut34 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_74 = happySpecReduce_0  31# happyReduction_74
happyReduction_74  =  happyIn35
		 ((Parser.AbsVinci.BNFC'NoPosition, [])
	)

happyReduce_75 = happySpecReduce_2  31# happyReduction_75
happyReduction_75 happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn35
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_76 = happySpecReduce_1  32# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.TInt (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_77 = happySpecReduce_1  32# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.TFloat (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_78 = happySpecReduce_1  32# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.TBool (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_79 = happySpecReduce_1  32# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((fst happy_var_1, Parser.AbsVinci.TStruct (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_80 = happySpecReduce_1  32# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 ((fst happy_var_1, Parser.AbsVinci.TPoly (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_81 = happySpecReduce_3  32# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), (snd happy_var_2))
	)}}

happyReduce_82 = happySpecReduce_3  33# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_3 of { happy_var_3 -> 
	happyIn37
		 ((fst happy_var_1, Parser.AbsVinci.TFun (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_83 = happySpecReduce_1  33# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_84 = happySpecReduce_0  34# happyReduction_84
happyReduction_84  =  happyIn38
		 ((Parser.AbsVinci.BNFC'NoPosition, Parser.AbsVinci.NoRetType Parser.AbsVinci.BNFC'NoPosition)
	)

happyReduce_85 = happySpecReduce_2  34# happyReduction_85
happyReduction_85 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut37 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 ((uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1), Parser.AbsVinci.RetType (uncurry Parser.AbsVinci.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyNewToken action sts stk [] =
	happyDoAction 45# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TD _) -> cont 40#;
	PT _ (TI _) -> cont 41#;
	PT _ (T_VIdent _) -> cont 42#;
	PT _ (T_SIdent _) -> cont 43#;
	PT _ (T_TPolyIdent _) -> cont 44#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 45# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut9 x))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err (Parser.AbsVinci.Program Parser.AbsVinci.BNFC'Position)
pProgram = fmap snd . pProgram_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc6091_0/ghc_2.h" #-}




















































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
