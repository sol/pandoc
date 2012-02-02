{-
Copyright (C) 2006-2012 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.LaTeX
   Copyright   : Copyright (C) 2006-2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of LaTeX to 'Pandoc' document.
-}
module Text.Pandoc.Readers.LaTeX ( readLaTeX,
                                   rawLaTeXInline,
                                   rawLaTeXEnvironment'
                                 ) where

import Text.ParserCombinators.Parsec hiding ((<|>), space, many, optional)
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Data.Maybe ( fromMaybe )
import Data.Char ( chr, ord )
import Data.List ( intercalate, isPrefixOf, isSuffixOf )
import Control.Monad
import Text.Pandoc.Builder
import Data.Char (isLetter)
import Control.Applicative
import Data.Monoid
import qualified Data.Map as M

-- | Parse LaTeX from string and return 'Pandoc' document.
readLaTeX :: ParserState   -- ^ Parser state, including options for parser
          -> String        -- ^ String to parse (assumes @'\n'@ line endings)
          -> Pandoc
readLaTeX = readWith parseLaTeX

parseLaTeX = do
  bs <- blocks
  return $ doc bs

type LP = GenParser Char ParserState

anyControlSeq :: LP String
anyControlSeq = do
  char '\\'
  next <- option '\n' anyChar
  name <- case next of
               '\n'           -> return ""
               c | isLetter c -> (c:) <$> many letter
                 | otherwise  -> return [c]
  optional sp
  return name

controlSeq :: String -> LP String
controlSeq name = try $ do
  char '\\'
  guard $ not (null name) && (length name == 1 || all isLetter name)
  string name
  optional sp
  return name

sp :: LP ()
sp = skipMany1 $ satisfy (\c -> c == ' ' || c == '\t')
              <|> (try $ newline >>~ notFollowedBy blankline)

isLowerHex :: Char -> Bool
isLowerHex x = x >= '0' && x <= '9' || x >= 'a' && x <= 'f'

tildeEscape :: LP Char
tildeEscape = try $ do
  string "^^"
  c <- satisfy (\x -> x >= '\0' && x <= '\128')
  d <- if isLowerHex c
          then option "" $ count 1 (satisfy isLowerHex)
          else return ""
  if null d
     then case ord c of
           x | x >= 64 && x <= 127 -> return $ chr (x - 64)
             | otherwise           -> return $ chr (x + 64)
     else return $ chr $ read ('0':'x':c:d)

comment :: LP ()
comment = do
  char '%'
  skipMany (satisfy (/='\n'))
  newline
  return ()

grouped :: Monoid a => LP a -> LP a
grouped parser = try $ do
  char '{'
  res <- manyTill parser (char '}')
  return $ mconcat res

braced :: LP String
braced = char '{' *> (concat <$> manyTill
         (  many1 (satisfy (\c -> c /= '\\' && c /= '}' && c /= '{'))
        <|> try (string "\\}")
        <|> try (string "\\{")
        <|> ((\x -> "{" ++ x ++ "}") <$> braced)
        <|> count 1 anyChar
         ) (char '}'))

bracketed :: LP String
bracketed = try $ do
  char '['
  manyTill anyChar (char ']')

trim :: String -> String
trim = removeLeadingTrailingSpace

mathDisplay :: LP String -> LP Inlines
mathDisplay p = displayMath <$> (try p >>= applyMacros' . trim)

mathInline :: LP String -> LP Inlines
mathInline p = math <$> (try p >>= applyMacros')

inline :: LP Inlines
inline = (mempty <$ comment)
     <|> (space  <$ sp)
     <|> inlineText
     <|> inlineCommand
     <|> grouped inline
     <|> (char '-' *> option (str "-")
           ((char '-') *> option (str "–") (str "—" <$ char '-')))
     <|> (char '`' *> option (str "‘") (str "“" <$ char '`'))
     <|> (char '\'' *> option (str "’") (str "”" <$ char '\''))
     <|> (str "\160" <$ char '~')
     <|> (mathDisplay $ string "$$" *> manyTill anyChar (try $ string "$$"))
     <|> (mathInline  $ char '$' *> manyTill anyChar (char '$'))
     <|> (superscript <$> (char '^' *> tok))
     <|> (subscript <$> (char '_' *> tok))
     <|> (failUnlessLHS *> char '|' *> doLHSverb)
     <|> (str <$> count 1 tildeEscape)
     <|> (str <$> count 1 (satisfy (\c -> c /= '\\' && c /='\n')))

inlines :: LP Inlines
inlines = mconcat <$> many (notFollowedBy (char '}') *> inline)

block :: LP Blocks
block = (mempty <$ comment)
    <|> (mempty <$ blanklines)
    <|> environment
    <|> blockCommand
    <|> grouped block
    <|> paragraph

blocks :: LP Blocks
blocks = mconcat <$> many block

blockCommand :: LP Blocks
blockCommand = try $ do
  name <- anyControlSeq
  case M.lookup name blockCommands of
       Just p      -> p
       Nothing     -> mzero

blockCommands :: M.Map String (LP Blocks)
blockCommands = M.fromList
  [ ("par", pure mempty)
  , ("begin", mzero)   -- these are here so they won't be interpreted as inline
  , ("end", mzero)
  , ("item", mzero)
  , ("documentclass", optional opt *> braced *> skipMany (notFollowedBy' (try $ string "\\begin{document}") *> anyChar) *> pure mempty) -- TODO
  , ("newcommand", braced *> optional opt *> tok *> pure mempty) -- TODO
  , ("renewcommand", braced *> optional opt *> tok *> pure mempty)
  , ("newenvironment", braced *> optional opt *> tok *> tok *> pure mempty)
  , ("renewenvironment", braced *> optional opt *> tok *> tok *> pure mempty)
  ]

inlineCommand :: LP Inlines
inlineCommand = try $ do
  name <- anyControlSeq
  guard $ not $ isBlockCommand name
  case M.lookup name inlineCommands of
       Just p      -> p
       Nothing     -> return (str $ "@" ++ name) -- TODO handle raw

isBlockCommand :: String -> Bool
isBlockCommand s = maybe False (const True) $ M.lookup s blockCommands

inlineCommands :: M.Map String (LP Inlines)
inlineCommands = M.fromList
  [ ("emph", emph <$> tok)
  , ("textit", emph <$> tok)
  , ("textsc", smallcaps <$> tok)
  , ("sout", strikeout <$> tok)
  , ("textsuperscript", superscript <$> tok)
  , ("textsubscript", subscript <$> tok)
  , ("textbackslash", lit "\\")
  , ("backslash", lit "\\")
  , ("textbf", strong <$> tok)
  , ("ldots", lit "…")
  , ("dots", lit "…")
  , ("mdots", lit "…")
  , ("sim", lit "~")
  , ("(", mathInline $ manyTill anyChar (try $ string "\\)"))
  , ("[", mathDisplay $ manyTill anyChar (try $ string "\\]"))
  , ("ensuremath", mathInline $ braced)
  , ("$", lit "$")
  , ("%", lit "%")
  , ("&", lit "&")
  , ("#", lit "#")
  , ("_", lit "_")
  , ("{", lit "{")
  , ("}", lit "}")
  -- old TeX commands
  , ("em", emph <$> inlines)
  , ("it", emph <$> inlines)
  , ("sl", emph <$> inlines)
  , ("bf", strong <$> inlines)
  , ("rm", inlines)
  , ("itshape", emph <$> inlines)
  , ("slshape", emph <$> inlines)
  , ("scshape", smallcaps <$> inlines)
  , ("bfseries", strong <$> inlines)
  , ("/", pure mempty) -- italic correction
  , ("cc", lit "ç")
  , ("cC", lit "Ç")
  , ("aa", lit "å")
  , ("AA", lit "Å")
  , ("ss", lit "ß")
  , ("o", lit "ø")
  , ("O", lit "Ø")
  , ("L", lit "Ł")
  , ("l", lit "ł")
  , ("ae", lit "æ")
  , ("AE", lit "Æ")
  , ("pounds", lit "£")
  , ("euro", lit "€")
  , ("copyright", lit "©")
  , ("sect", lit "§")
  , ("`", option (str "`") $ try $ tok >>= accent grave)
  , ("'", option (str "'") $ try $ tok >>= accent acute)
  , ("^", option (str "^") $ try $ tok >>= accent hat)
  , ("~", option (str "~") $ try $ tok >>= accent circ)
  , ("\"", option (str "\"") $ try $ tok >>= accent umlaut)
  , ("i", lit "i")
  , ("\\", linebreak <$ optional (bracketed *> optional sp))
  , (",", pure mempty)
  , ("@", pure mempty)
  , (" ", lit "\160")
  , ("bar", lit "|")
  , ("textless", lit "<")
  , ("textgreater", lit ">")
  , ("thanks", (note . mconcat) <$> many (notFollowedBy (char '}') *> block))
  , ("footnote", (note . mconcat) <$> many (notFollowedBy (char '}') *> block))
  , ("verb", doverb)
  , ("lstinline", doverb)
  , ("texttt", (code . stringify . toList) <$> tok)
  ]

doverb :: LP Inlines
doverb = do
  marker <- anyChar
  code <$> manyTill (satisfy (/='\n')) (char marker)

doLHSverb :: LP Inlines
doLHSverb = codeWith ("",["haskell"],[]) <$> manyTill (satisfy (/='\n')) (char '|')

lit :: String -> LP Inlines
lit = pure . str

accent :: (Char -> Char) -> Inlines -> LP Inlines
accent f ils =
  case toList ils of
       (Str (x:xs) : ys) -> return $ fromList $ (Str (f x : xs) : ys)
       []                -> mzero
       ys                -> return ils

grave :: Char -> Char
grave 'A' = 'À'
grave 'E' = 'È'
grave 'I' = 'Ì'
grave 'O' = 'Ò'
grave 'U' = 'Ù'
grave 'a' = 'à'
grave 'e' = 'è'
grave 'i' = 'ì'
grave 'o' = 'ò'
grave 'u' = 'ù'
grave c   = c

acute :: Char -> Char
acute 'A' = 'Á'
acute 'E' = 'É'
acute 'I' = 'Í'
acute 'O' = 'Ó'
acute 'U' = 'Ú'
acute 'a' = 'á'
acute 'e' = 'é'
acute 'i' = 'í'
acute 'o' = 'ó'
acute 'u' = 'ú'
acute c = c

hat :: Char -> Char
hat 'A' = 'Â'
hat 'E' = 'Ê'
hat 'I' = 'Î'
hat 'O' = 'Ô'
hat 'U' = 'Û'
hat 'a' = 'ã'
hat 'e' = 'ê'
hat 'i' = 'î'
hat 'o' = 'ô'
hat 'u' = 'û'
hat c = c

circ :: Char -> Char
circ 'A' = 'Ã'
circ 'O' = 'Õ'
circ 'o' = 'õ'
circ 'N' = 'Ñ'
circ 'n' = 'ñ'
circ c   = c

umlaut :: Char -> Char
umlaut 'A' = 'Ä'
umlaut 'E' = 'Ë'
umlaut 'I' = 'Ï'
umlaut 'O' = 'Ö'
umlaut 'U' = 'Ü'
umlaut 'a' = 'ä'
umlaut 'e' = 'ë'
umlaut 'i' = 'ï'
umlaut 'o' = 'ö'
umlaut 'u' = 'ü'
umlaut c = c

tok :: LP Inlines
tok = try $ grouped inline <|> inlineCommand <|> str <$> (count 1 $ inlineChar)

opt :: LP String
opt = bracketed <* optional sp

inlineText :: LP Inlines
inlineText = str <$> many1 inlineChar

inlineChar :: LP Char
inlineChar = satisfy $ \c ->
  not (c == '\\' || c == '$' || c == '%' || c == '^' || c == '_' ||
       c == '&'  || c == '~' || c == '#' || c == '{' || c == '}' ||
       c == '^'  || c == '\'' || c == '-' ||
       c == ' ' || c == '\t' || c == '\n' )

specialChars :: [Char]
specialChars = "\\$%^&_~#{}^"

environment :: LP Blocks
environment = do
  controlSeq "begin"
  name <- braced
  case M.lookup name environments of
       Just p      -> p
       Nothing     -> try (blockQuote <$> (env name blocks))
                     <|> codeBlock <$> (verbEnv name)
      -- TODO handle raw

environments :: M.Map String (LP Blocks)
environments = M.fromList
  [ ("document", env "document" blocks)
  , ("quote", blockQuote <$> env "quote" blocks)
  , ("quotation", blockQuote <$> env "quotation" blocks)
  , ("itemize", bulletList <$> env "itemize" (many item))
  , ("enumerate", orderedList <$> (optional opt *> env "enumerate" (many item)))
  , ("code", failUnlessLHS *>
      (codeBlockWith ("",["literate","haskell"],[]) <$> verbEnv "code"))
  , ("verbatim", codeBlock <$> (verbEnv "verbatim"))
  , ("Verbatim", codeBlock <$> (verbEnv "Verbatim"))
  , ("lstlisting", codeBlock <$> (verbEnv "listlisting"))
  -- TODO fix behavior of math environments
  -- eg. align should be displaymath with aligned/gather
  , ("displaymath", mathEnv "displaymath")
  , ("equation", mathEnv "equation")
  , ("equation*", mathEnv "equation*")
  , ("gather", mathEnv "gather")
  , ("gather*", mathEnv "gather*")
  , ("gathered", mathEnv "gathered")
  , ("multiline", mathEnv "multiline")
  , ("multiline*", mathEnv "multiline*")
  , ("eqnarray", mathEnv "eqnarray")
  , ("eqnarray*", mathEnv "eqnarray*")
  , ("align", mathEnv "align")
  , ("align*", mathEnv "align*")
  , ("aligned", mathEnv "aligned")
  , ("alignat", mathEnv "alignat")
  , ("alignat*", mathEnv "alignat*")
  , ("split", mathEnv "split")
  , ("alignedat", mathEnv "alignedat")
  ]

item :: LP Blocks
item = blocks *> controlSeq "item" *> optional opt *> blocks

env :: String -> LP a -> LP a
env name p = p <* (controlSeq "end" *> braced >>= guard . (== name))

mathEnv :: String -> LP Blocks
mathEnv name = para <$> mathDisplay (verbEnv name)

verbEnv :: String -> LP String
verbEnv name = do
  optional opt
  optional blankline
  let endEnv = try $ controlSeq "end" *> braced >>= guard . (== name)
  manyTill anyChar endEnv

paragraph :: LP Blocks
paragraph = (para . mconcat) <$> many1 inline


-------

rawLaTeXInline = undefined

rawLaTeXEnvironment' = undefined



{-

anyEnvironment :: GenParser Char ParserState Block
anyEnvironment =  try $ do
  string "\\begin"
  spaces
  char '{'
  name <- many letter
  star <- option "" (string "*") -- some environments have starred variants
  char '}'
  optional commandArgs
  spaces
  contents <- manyTill block (end (name ++ star))
  spaces
  return $ BlockQuote contents

--
-- parsing documents
--

-- | Process LaTeX preamble, extracting metadata.
processLaTeXPreamble :: GenParser Char ParserState ()
processLaTeXPreamble = do
  try $ string "\\documentclass"
  skipMany $ bibliographic <|> macro <|> commentBlock <|> skipChar

-- | Parse LaTeX and return 'Pandoc'.
parseLaTeX :: GenParser Char ParserState Pandoc
parseLaTeX = do
  spaces
  skipMany $ comment >> spaces
  blocks <- try (processLaTeXPreamble >> environment "document")
           <|> (many block >>~ (spaces >> eof))
  state <- getState
  let blocks' = filter (/= Null) blocks
  let title' = stateTitle state
  let authors' = stateAuthors state
  let date' = stateDate state
  return $ Pandoc (Meta title' authors' date')  blocks'

--
-- parsing blocks
--

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = spaces >> many block

block :: GenParser Char ParserState Block
block = choice [ hrule
               , codeBlock
               , header
               , list
               , blockQuote
               , simpleTable
               , commentBlock
               , macro
               , bibliographic
               , para
               , itemBlock
               , unknownEnvironment
               , ignore
               , unknownCommand
               ] <?> "block"

--
-- header blocks
--

header :: GenParser Char ParserState Block
header = section <|> chapter

chapter :: GenParser Char ParserState Block
chapter = try $ do
  string "\\chapter"
  result <- headerWithLevel 1
  updateState $ \s -> s{ stateHasChapters = True }
  return result

section :: GenParser Char ParserState Block
section = try $ do
  char '\\'
  subs <- many (try (string "sub"))
  base <- try (string "section" >> return 1) <|> (string "paragraph" >> return 4)
  st <- getState
  let lev = if stateHasChapters st
               then length subs + base + 1
               else length subs + base
  headerWithLevel lev

headerWithLevel :: Int -> GenParser Char ParserState Block
headerWithLevel lev = try $ do
  spaces
  optional (char '*')
  spaces
  optional $ bracketedText '[' ']' -- alt title
  spaces
  char '{'
  title' <- manyTill inline (char '}')
  spaces
  return $ Header lev (normalizeSpaces title')

--
-- hrule block
--

hrule :: GenParser Char st Block
hrule = oneOfStrings [ "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n", 
                       "\\newpage" ] >> spaces >> return HorizontalRule

-- tables

simpleTable :: GenParser Char ParserState Block
simpleTable = try $ do
  string "\\begin"
  spaces
  string "{tabular}"
  spaces
  aligns <- parseAligns
  let cols = length aligns
  optional hline
  header' <- option [] $ parseTableHeader cols
  rows <- many (parseTableRow cols >>~ optional hline)
  spaces
  end "tabular"
  spaces
  let header'' = if null header'
                    then replicate cols []
                    else header'
  return $ Table [] aligns (replicate cols 0) header'' rows

hline :: GenParser Char st ()
hline = try $ spaces >> string "\\hline" >> return ()

parseAligns :: GenParser Char ParserState [Alignment]
parseAligns = try $ do
  char '{'
  optional $ char '|'
  let cAlign = char 'c' >> return AlignCenter
  let lAlign = char 'l' >> return AlignLeft
  let rAlign = char 'r' >> return AlignRight
  let alignChar = cAlign <|> lAlign <|> rAlign
  aligns' <- sepEndBy alignChar (optional $ char '|')
  char '}'
  spaces
  return aligns'

parseTableHeader :: Int   -- ^ number of columns
                 -> GenParser Char ParserState [TableCell]
parseTableHeader cols = try $ do
  cells' <- parseTableRow cols
  hline
  return cells'

parseTableRow :: Int  -- ^ number of columns
              -> GenParser Char ParserState [TableCell]
parseTableRow cols = try $ do
  let tableCellInline = notFollowedBy (char '&' <|>
                          (try $ char '\\' >> char '\\')) >> inline
  cells' <- sepBy (spaces >> liftM ((:[]) . Plain . normalizeSpaces)
             (many tableCellInline)) (char '&')
  guard $ length cells' == cols
  spaces
  (try $ string "\\\\" >> spaces) <|>
    (lookAhead (end "tabular") >> return ())
  return cells'

--
-- code blocks
--

codeBlock :: GenParser Char ParserState Block
codeBlock = codeBlockWith "verbatim" <|> codeBlockWith "Verbatim" <|> codeBlockWith "lstlisting" <|> lhsCodeBlock
-- Note:  Verbatim is from fancyvrb.

codeBlockWith :: String -> GenParser Char st Block
codeBlockWith env = try $ do
  string "\\begin"
  spaces                      -- don't use begin function because it
  string $ "{" ++ env ++ "}"  -- gobbles whitespace; we want to gobble
  optional blanklines         -- blank lines, but not leading space
  contents <- manyTill anyChar (try (string $ "\\end{" ++ env ++ "}"))
  spaces
  let classes = if env == "code" then ["haskell"] else []
  return $ CodeBlock ("",classes,[]) (stripTrailingNewlines contents)

listItem :: GenParser Char ParserState ([Inline], [Block])
listItem = try $ do
  ("item", _, args) <- command
  spaces
  state <- getState
  let oldParserContext = stateParserContext state
  updateState (\s -> s {stateParserContext = ListItemState})
  blocks <- many block
  updateState (\s -> s {stateParserContext = oldParserContext})
  opt <- case args of
           ([x]) | "[" `isPrefixOf` x && "]" `isSuffixOf` x -> 
                       parseFromString (many inline) $ tail $ init x
           _        -> return []
  return (opt, blocks)

orderedList :: GenParser Char ParserState Block
orderedList = try $ do
  string "\\begin"
  spaces
  string "{enumerate}"
  spaces
  (_, style, delim) <- option (1, DefaultStyle, DefaultDelim) $
                              try $ do failIfStrict
                                       char '['
                                       res <- anyOrderedListMarker
                                       char ']'
                                       return res
  spaces
  option "" $ try $ do string "\\setlength{\\itemindent}"
                       char '{'
                       manyTill anyChar (char '}')
  spaces
  start <- option 1 $ try $ do failIfStrict
                               string "\\setcounter{enum"
                               many1 (oneOf "iv")
                               string "}{"
                               num <- many1 digit
                               char '}' 
                               spaces
                               return $ (read num) + 1
  items <- many listItem
  end "enumerate"
  spaces
  return $ OrderedList (start, style, delim) $ map snd items

definitionList :: GenParser Char ParserState Block
definitionList = try $ do
  begin "description"
  items <- many listItem
  end "description"
  spaces
  return $ DefinitionList $ map (\(t,d) -> (t,[d])) items

--
-- paragraph block
--

para :: GenParser Char ParserState Block
para = do
  res <- many1 inline
  spaces
  return $ if null (filter (`notElem` [Str "", Space]) res)
              then Null
              else Para $ normalizeSpaces res

--
-- title authors date
--

bibliographic :: GenParser Char ParserState Block
bibliographic = choice [ maketitle, title, subtitle, authors, date ]

maketitle :: GenParser Char st Block
maketitle = try (string "\\maketitle") >> spaces >> return Null

title :: GenParser Char ParserState Block
title = try $ do
  string "\\title{"
  tit <- manyTill inline (char '}')
  spaces
  updateState (\state -> state { stateTitle = tit })
  return Null

subtitle :: GenParser Char ParserState Block
subtitle = try $ do
  string "\\subtitle{"
  tit <- manyTill inline (char '}')
  spaces
  updateState (\state -> state { stateTitle = stateTitle state ++
                                   Str ":" : LineBreak : tit })
  return Null

authors :: GenParser Char ParserState Block
authors = try $ do
  string "\\author{"
  let andsep = try $ string "\\and" >> notFollowedBy letter >>
                     spaces >> return '&'
  raw <- sepBy (many $ notFollowedBy (char '}' <|> andsep) >> inline) andsep
  let authors' = map normalizeSpaces raw
  char '}'
  spaces
  updateState (\s -> s { stateAuthors = authors' })
  return Null

date :: GenParser Char ParserState Block
date = try $ do
  string "\\date{"
  date' <- manyTill inline (char '}')
  spaces
  updateState (\state -> state { stateDate = normalizeSpaces date' })
  return Null

--
-- item block
-- for use in unknown environments that aren't being parsed as raw latex
--

-- this forces items to be parsed in different blocks
itemBlock :: GenParser Char ParserState Block
itemBlock = try $ do
  ("item", _, args) <- command
  state <- getState
  if stateParserContext state == ListItemState
     then fail "item should be handled by list block"
     else if null args 
             then return Null
             else return $ Plain [Str (stripFirstAndLast (head args))]

--
-- raw LaTeX 
--

-- | Parse any LaTeX environment and return a Para block containing
-- the whole literal environment as raw TeX.
rawLaTeXEnvironment :: GenParser Char st Block
rawLaTeXEnvironment = do
  contents <- rawLaTeXEnvironment'
  spaces
  return $ RawBlock "latex" contents

-- | Parse any LaTeX environment and return a string containing
-- the whole literal environment as raw TeX.
rawLaTeXEnvironment' :: GenParser Char st String 
rawLaTeXEnvironment' = try $ do
  string "\\begin"
  spaces
  char '{'
  name <- many1 letter
  star <- option "" (string "*") -- for starred variants
  let name' = name ++ star
  char '}'
  args <- option [] commandArgs
  let argStr = concat args
  contents <- manyTill (choice [ (many1 (noneOf "\\")), 
                                 rawLaTeXEnvironment',
                                 string "\\" ]) 
                       (end name')
  return $ "\\begin{" ++ name' ++ "}" ++ argStr ++ 
                 concat contents ++ "\\end{" ++ name' ++ "}"

unknownEnvironment :: GenParser Char ParserState Block
unknownEnvironment = try $ do
  state <- getState
  result <- if stateParseRaw state -- check whether we should include raw TeX 
               then rawLaTeXEnvironment -- if so, get whole raw environment
               else anyEnvironment      -- otherwise just the contents
  return result

-- \ignore{} is used conventionally in literate haskell for definitions
-- that are to be processed by the compiler but not printed.
ignore :: GenParser Char ParserState Block
ignore = try $ do
  ("ignore", _, _) <- command
  spaces
  return Null

demacro :: (String, String, [String]) -> GenParser Char ParserState Inline
demacro (n,st,args) = try $ do
  let raw = "\\" ++ n ++ st ++ concat args
  s' <- applyMacros' raw
  if raw == s'
     then return $ RawInline "latex" raw
     else do
       inp <- getInput
       setInput $ s' ++ inp
       return $ Str ""

unknownCommand :: GenParser Char ParserState Block
unknownCommand = try $ do
  spaces
  notFollowedBy' $ oneOfStrings ["\\begin","\\end","\\item"] >>
                   notFollowedBy letter
  state <- getState
  when (stateParserContext state == ListItemState) $
     notFollowedBy' (string "\\item")
  if stateParseRaw state
     then command >>= demacro >>= return . Plain . (:[])
     else do
        (name, _, args) <- command
        spaces
        unless (name `elem` commandsToIgnore) $ do
        -- put arguments back in input to be parsed
          inp <- getInput
          setInput $ intercalate " " args ++ inp
        return Null

commandsToIgnore :: [String]
commandsToIgnore = ["special","pdfannot","pdfstringdef", "index","bibliography"]

--
-- links and images
--

url :: GenParser Char ParserState Inline
url = try $ do
  string "\\url"
  url' <- charsInBalanced '{' '}' anyChar
  return $ Link [Code ("",["url"],[]) url'] (escapeURI url', "")

link :: GenParser Char ParserState Inline
link = try $ do
  string "\\href{"
  url' <- manyTill anyChar (char '}')
  char '{'
  label' <- manyTill inline (char '}') 
  return $ Link (normalizeSpaces label') (escapeURI url', "")

image :: GenParser Char ParserState Inline
image = try $ do
  ("includegraphics", _, args) <- command
  let args' = filter isArg args -- filter out options
  let (src,tit) = case args' of
                       []    -> ("", "")
                       (x:_) -> (stripFirstAndLast x, "")
  return $ Image [Str "image"] (escapeURI src, tit)

-- | citations
cite :: GenParser Char ParserState Inline
cite = simpleCite <|> complexNatbibCites

simpleCiteArgs :: GenParser Char ParserState [Citation]
simpleCiteArgs = try $ do
  first  <- optionMaybe $ (char '[') >> manyTill inline (char ']')
  second <- optionMaybe $ (char '[') >> manyTill inline (char ']')
  char '{'
  keys <- many1Till citationLabel (char '}')
  let (pre, suf) = case (first  , second ) of
        (Just s , Nothing) -> ([], s )
        (Just s , Just t ) -> (s , t )
        _                  -> ([], [])
      conv k = Citation { citationId      = k
                        , citationPrefix  = []
                        , citationSuffix  = []
                        , citationMode    = NormalCitation
                        , citationHash    = 0
                        , citationNoteNum = 0
                        }
  return $ addPrefix pre $ addSuffix suf $ map conv keys


simpleCite :: GenParser Char ParserState Inline
simpleCite = try $ do
  char '\\'
  let biblatex     = [a ++ "cite" | a <- ["auto", "foot", "paren", "super", ""]]
                     ++ ["footcitetext"]
      normal       = ["cite" ++ a ++ b | a <- ["al", ""], b <- ["p", "p*", ""]]
                     ++ biblatex
      supress      = ["citeyearpar", "citeyear", "autocite*", "cite*", "parencite*"]
      intext       = ["textcite"] ++ ["cite" ++ a ++ b | a <- ["al", ""], b <- ["t", "t*"]]
      mintext      = ["textcites"]
      mnormal      = map (++ "s") biblatex
      cmdend       = notFollowedBy (letter <|> char '*')
      capit []     = []
      capit (x:xs) = toUpper x : xs
      addUpper xs  = xs ++ map capit xs
      toparser l t = try $ oneOfStrings (addUpper l) >> cmdend >> return t
  (mode, multi) <-  toparser normal  (NormalCitation, False)
                <|> toparser supress (SuppressAuthor, False)
                <|> toparser intext  (AuthorInText  , False)
                <|> toparser mnormal (NormalCitation, True )
                <|> toparser mintext (AuthorInText  , True )
  cits <- if multi then
            many1 simpleCiteArgs
          else
            simpleCiteArgs >>= \c -> return [c]
  let (c:cs) = concat cits
      cits'  = case mode of
        AuthorInText   -> c {citationMode = mode} : cs
        _              -> map (\a -> a {citationMode = mode}) (c:cs)
  return $ Cite cits' []

complexNatbibCites :: GenParser Char ParserState Inline
complexNatbibCites = complexNatbibTextual <|> complexNatbibParenthetical

complexNatbibTextual :: GenParser Char ParserState Inline
complexNatbibTextual = try $ do
  string "\\citeauthor{"
  manyTill (noneOf "}") (char '}')
  skipSpaces
  Cite (c:cs) _ <- complexNatbibParenthetical
  return $ Cite (c {citationMode = AuthorInText} : cs) []


complexNatbibParenthetical :: GenParser Char ParserState Inline
complexNatbibParenthetical = try $ do
  string "\\citetext{"
  cits <- many1Till parseOne (char '}')
  return $ Cite (concat cits) []
  where
    parseOne = do
                 skipSpaces
                 pref           <- many (notFollowedBy (oneOf "\\}") >> inline)
                 (Cite cites _) <- simpleCite
                 suff           <- many (notFollowedBy (oneOf "\\};") >> inline)
                 skipSpaces
                 optional $ char ';'
                 return $ addPrefix pref $ addSuffix suff $ cites

addPrefix :: [Inline] -> [Citation] -> [Citation]
addPrefix p (k:ks)   = k {citationPrefix = p ++ citationPrefix k} : ks
addPrefix _ _ = []

addSuffix :: [Inline] -> [Citation] -> [Citation]
addSuffix s ks@(_:_) = let k = last ks
                        in init ks ++ [k {citationSuffix = citationSuffix k ++ s}]
addSuffix _ _ = []

citationLabel :: GenParser Char ParserState String
citationLabel  = do
  res <- many1 $ noneOf ",}"
  optional $ char ','
  return $ removeLeadingTrailingSpace res

-- | Parse any LaTeX inline command and return it in a raw TeX inline element.
rawLaTeXInline' :: GenParser Char ParserState Inline
rawLaTeXInline' = do
  notFollowedBy' $ oneOfStrings ["\\begin", "\\end", "\\item", "\\ignore",
                                 "\\section"]
  rawLaTeXInline

-- | Parse any LaTeX command and return it in a raw TeX inline element.
rawLaTeXInline :: GenParser Char ParserState Inline
rawLaTeXInline = try $ do
  state <- getState
  if stateParseRaw state
     then command >>= demacro
     else do
        (name,st,args) <- command
        x <- demacro (name,st,args)
        unless (x == Str "" || name `elem` commandsToIgnore) $ do
          inp <- getInput
          setInput $ intercalate " " args ++ inp
        return $ Str ""
-}
