{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS -fno-warn-orphans #-}

module BMake.Base
  ( thenP
  , returnP
  , happyError
  , handleErrorExpList
  , Parser
  , Token(..)
  , TokenClass(..)
  , AlexState(..)
  , parseDCToken
  , lexer
  , AssignType(..)
  , IfCmpType(..)
  , MetaVar(..)
  , MetaVarModifier(..)
  , Makefile(..)
  , substmts
  , Statement(..)
  , Expr
  , ExprF(..)
  , FilePos
  , module BMake.Lexer
  )
  where

--------------------------------------------------------------------------------
import Control.DeepSeq (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.String (IsString)
import GHC.Generics
import Lib.FilePath (FilePath)
----
import BMake.Data
import BMake.Lexer
--------------------------------------------------------------------------------

import Prelude.Compat hiding (FilePath)

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexGetPosition :: Alex AlexPosn
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexStructError (line, col, "syntax error" :: String)

handleErrorExpList :: (Token, [String]) -> Parser a
handleErrorExpList (Token _ cls, opts) = do
    (AlexPn _ line col) <- alexGetPosition
    let expected [x] = ", expected: " ++ x
        expected xs =  ", expected one of: " ++ unwords xs
    alexStructError (line, col,
                     "syntax error: got " ++ tokenDesc cls ++ expected opts)

type FilePos = (FilePath, AlexPosn)
deriving instance Ord AlexPosn

data ExprF text
  = Str text
  | OpenBrace FilePos
  | CloseBrace FilePos
  | Comma
  | Spaces
  | VarSpecial MetaVar MetaVarModifier
  | VarSimple FilePos text
  deriving (Eq, Ord, Show, Generic, Functor)

type Expr = ExprF ByteString

parseMetaVarChar :: Char -> MetaVar
parseMetaVarChar '@' = FirstOutput
parseMetaVarChar '<' = FirstInput
parseMetaVarChar '^' = AllInputs
parseMetaVarChar '|' = AllOOInputs
parseMetaVarChar '*' = Stem
parseMetaVarChar other = error $ "unknown meta-variable: $" ++ [other]

parseModifier :: Maybe Char -> MetaVarModifier
parseModifier Nothing = NoMod
parseModifier (Just 'F') = ModFile
parseModifier (Just 'D') = ModDir
parseModifier (Just other) = error $ "unknown meta-variable modifier: $(," ++ [other] ++ ")"

parseDCToken :: IsString text => (Char, Maybe Char) -> ExprF text
parseDCToken ('.', Nothing) = VarSimple ("", AlexPn 0 0 0) "."
parseDCToken (other, modifier) = VarSpecial (parseMetaVarChar other) (parseModifier modifier)

instance NFData text => NFData (ExprF text) where
  rnf = genericRnf

data AssignType = AssignNormal | AssignConditional
  deriving (Show, Generic)
instance NFData AssignType where
instance ToJSON AssignType where

data IfCmpType = IfEquals | IfNotEquals
  deriving (Show, Generic)
instance NFData IfCmpType where
instance ToJSON IfCmpType where

deriving instance Generic AlexPosn
instance NFData AlexPosn where

data Statement
  = Assign ByteString AssignType [ExprF ByteString]
  | Local [Statement]
  | Target
    FilePos
    [ExprF ByteString]
    [ExprF ByteString]
    [ExprF ByteString]
    [[ExprF ByteString]]
  | Include ByteString
  | IfCmp IfCmpType [ExprF ByteString] [ExprF ByteString] [Statement] [Statement]
  deriving (Show, Generic)

-- | Traversal of direct children of statement
substmts ::
    Applicative f =>
    ([Statement] -> f [Statement]) ->
    Statement -> f Statement
substmts f (Local dl) = Local <$> f dl
substmts f (IfCmp a b c dla dlb) = IfCmp a b c <$> f dla <*> f dlb
substmts _ x = pure x

instance NFData Statement where
    rnf = genericRnf

newtype Makefile = Makefile
    { unit :: [Statement]
    } deriving (Show, Generic)

instance NFData Makefile where

lexer :: (Token -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f
