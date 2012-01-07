{-
Copyright (C) 2011 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.OpenXML
   Copyright   : Copyright (C) 2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Office Open XML.
-}
module Text.Pandoc.Writers.OpenXML ( writeOpenXML) where
import Text.Pandoc.Definition
import Text.Pandoc.XML
import Text.Pandoc.Generic
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Readers.TeXMath
import Data.List ( isPrefixOf, intercalate, isSuffixOf )
import Data.Char ( toLower )
import Text.Pandoc.Highlighting ( languages, languagesByExtension )
import Text.Pandoc.Pretty
import Text.XML.Light.Output
import Text.TeXMath
import Control.Monad.State

data WriterState = WriterState{
         stTextProperties :: [Doc]
       , stParaProperties :: [Doc]
       , stFootnotes      :: [Doc]
       }

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stTextProperties = []
      , stParaProperties = []
      , stFootnotes      = []
      }

type WS = State WriterState

-- | Convert Pandoc document to string in OpenXML format.
writeOpenXML :: WriterOptions -> Pandoc -> String
writeOpenXML opts (Pandoc (Meta tit auths dat) blocks) =
  let title = empty -- inlinesToOpenXML opts tit
      authors = [] -- map (authorToOpenXML opts) auths
      date = empty -- inlinesToOpenXML opts dat
      colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
      render' = render colwidth
      convertSpace (Str x : Space : Str y : xs) = Str (x ++ " " ++ y) : xs
      convertSpace (Str x : Str y : xs) = Str (x ++ y) : xs
      convertSpace xs = xs
      blocks' = bottomUp convertSpace $ blocks
      (doc,st) = runState (blocksToOpenXML opts blocks') defaultWriterState
      notes    = case reverse (stFootnotes st) of
                        [] -> empty
                        ns -> inTagsIndented "w:footnotes" $ vcat ns
      main     = render' $ doc $$ notes
      context = writerVariables opts ++
                [ ("body", main)
                , ("title", render' title)
                , ("date", render' date) ] ++
                [ ("author", render' a) | a <- authors ]
  in  if writerStandalone opts
         then renderTemplate context $ writerTemplate opts
         else main

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: WriterOptions -> [Block] -> WS Doc
blocksToOpenXML opts bls = vcat `fmap` mapM (blockToOpenXML opts) bls

{-
-- | Auxiliary function to convert Plain block to Para. DO WE NEED THIS TODO?
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a list of
-- OpenXML varlistentrys.
deflistItemsToOpenXML :: WriterOptions -> [([Inline],[[Block]])] -> Doc
deflistItemsToOpenXML opts items =
  vcat $ map (\(term, defs) -> deflistItemToOpenXML opts term defs) items

-- | Convert a term and a list of blocks into a OpenXML varlistentry.
deflistItemToOpenXML :: WriterOptions -> [Inline] -> [[Block]] -> Doc
deflistItemToOpenXML opts term defs =
  let def' = concatMap (map plainToPara) defs
  in  inTagsIndented "varlistentry" $
      inTagsIndented "term" (inlinesToOpenXML opts term) $$
      inTagsIndented "listitem" (blocksToOpenXML opts def')

-- | Convert a list of lists of blocks to a list of OpenXML list items.
listItemsToOpenXML :: WriterOptions -> [[Block]] -> Doc
listItemsToOpenXML opts items = vcat $ map (listItemToOpenXML opts) items

-- | Convert a list of blocks into a OpenXML list item.
listItemToOpenXML :: WriterOptions -> [Block] -> Doc
listItemToOpenXML opts item =
  inTagsIndented "listitem" $ blocksToOpenXML opts $ map plainToPara item
-}

pStyle :: String -> Doc
pStyle sty = selfClosingTag "w:pStyle" [("w:val",sty)]

rStyle :: String -> Doc
rStyle sty = selfClosingTag "w:rStyle" [("w:val",sty)]

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: WriterOptions -> Block -> WS Doc
blockToOpenXML _ Null = return empty
{-
blockToOpenXML opts (Para [Image txt (src,_)]) =
  let capt = inlinesToOpenXML opts txt
  in  inTagsIndented "figure" $
        inTagsSimple "title" capt $$
        (inTagsIndented "mediaobject" $
           (inTagsIndented "imageobject"
             (selfClosingTag "imagedata" [("fileref",src)])) $$
           inTagsSimple "textobject" (inTagsSimple "phrase" capt))
-}
blockToOpenXML opts (Header lev lst) = do
  contents <- inlinesToOpenXML opts lst
  return $ inTagsIndented "w:p"
         $ (inTagsIndented "w:pPr" $ pStyle ("Heading" ++ show lev)) $$ contents
blockToOpenXML opts (Plain lst) =
  inlinesToOpenXML opts lst
blockToOpenXML opts (Para lst) = inTagsIndented "w:p" `fmap` inlinesToOpenXML opts lst
blockToOpenXML _ (RawBlock format str)
  | format == "openxml" = return $ text str -- raw XML block
  | otherwise           = return empty
{-
blockToOpenXML opts (BlockQuote blocks) =
  inTagsIndented "blockquote" $ blocksToOpenXML opts blocks
blockToOpenXML _ (CodeBlock (_,classes,_) str) =
  text ("<programlisting" ++ lang ++ ">") <> cr <>
     flush (text (escapeStringForXML str) <> cr <> text "</programlisting>")
    where lang  = if null langs
                     then ""
                     else " language=\"" ++ escapeStringForXML (head langs) ++
                          "\""
          isLang l    = map toLower l `elem` map (map toLower) languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . map toLower $ s
          langs       = concatMap langsFrom classes
blockToOpenXML opts (BulletList lst) =
  inTagsIndented "itemizedlist" $ listItemsToOpenXML opts lst
blockToOpenXML _ (OrderedList _ []) = empty
blockToOpenXML opts (OrderedList (start, numstyle, _) (first:rest)) =
  let attribs  = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("numeration", "arabic")]
                       Example      -> [("numeration", "arabic")]
                       UpperAlpha   -> [("numeration", "upperalpha")]
                       LowerAlpha   -> [("numeration", "loweralpha")]
                       UpperRoman   -> [("numeration", "upperroman")]
                       LowerRoman   -> [("numeration", "lowerroman")]
      items    = if start == 1
                    then listItemsToOpenXML opts (first:rest)
                    else (inTags True "listitem" [("override",show start)]
                         (blocksToOpenXML opts $ map plainToPara first)) $$
                         listItemsToOpenXML opts rest
  in  inTags True "orderedlist" attribs items
blockToOpenXML opts (DefinitionList lst) =
  inTagsIndented "variablelist" $ deflistItemsToOpenXML opts lst
blockToOpenXML _ HorizontalRule = empty -- not semantic
blockToOpenXML opts (Table caption aligns widths headers rows) =
  let captionDoc   = if null caption
                        then empty
                        else inTagsIndented "title"
                              (inlinesToOpenXML opts caption)
      tableType    = if isEmpty captionDoc then "informaltable" else "table"
      percent w    = show (truncate (100*w) :: Integer) ++ "*"
      coltags = vcat $ zipWith (\w al -> selfClosingTag "colspec"
                       ([("colwidth", percent w) | w > 0] ++
                        [("align", alignmentToString al)])) widths aligns
      head' = if all null headers
                 then empty
                 else inTagsIndented "thead" $
                         tableRowToOpenXML opts headers
      body' = inTagsIndented "tbody" $
              vcat $ map (tableRowToOpenXML opts) rows
  in  inTagsIndented tableType $ captionDoc $$
        (inTags True "tgroup" [("cols", show (length headers))] $
         coltags $$ head' $$ body')
-}
{-
alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

tableRowToOpenXML :: WriterOptions
                  -> [[Block]]
                  -> Doc
tableRowToOpenXML opts cols =
  inTagsIndented "row" $ vcat $ map (tableItemToOpenXML opts) cols

tableItemToOpenXML :: WriterOptions
                   -> [Block]
                   -> Doc
tableItemToOpenXML opts item =
  inTags True "entry" [] $ vcat $ map (blockToOpenXML opts) item
-}

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: WriterOptions -> [Inline] -> WS Doc
inlinesToOpenXML opts lst = vcat `fmap` mapM (inlineToOpenXML opts) lst

getTextProps :: WS Doc
getTextProps = do
  props <- gets stTextProperties
  return $ if null props
              then empty
              else inTagsIndented "w:rPr" $ vcat props

pushTextProp :: Doc -> WS ()
pushTextProp d = modify $ \s -> s{ stTextProperties = d : stTextProperties s }

popTextProp :: WS ()
popTextProp = modify $ \s -> s{ stTextProperties = drop 1 $ stTextProperties s }

withTextProp :: Doc -> WS a -> WS a
withTextProp d p = do
  pushTextProp d
  res <- p
  popTextProp
  return res

getParaProps :: WS Doc
getParaProps = do
  props <- gets stParaProperties
  return $ if null props
              then empty
              else inTagsIndented "w:pPr" $ vcat props

pushParaProp :: Doc -> WS ()
pushParaProp d = modify $ \s -> s{ stParaProperties = d : stParaProperties s }

popParaProp :: WS ()
popParaProp = modify $ \s -> s{ stParaProperties = drop 1 $ stParaProperties s }

withParaProp :: Doc -> WS a -> WS a
withParaProp d p = do
  pushParaProp d
  res <- p
  popParaProp
  return res

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: WriterOptions -> Inline -> WS Doc
inlineToOpenXML _ (Str str) = do
  props <- getTextProps
  return $
    inTagsIndented "w:r" $
      props $$
      inTags False "w:t" [("xml:space","preserve")]
          (text $ escapeStringForXML str)
inlineToOpenXML opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML opts (Strong lst) =
  withTextProp (selfClosingTag "w:b" []) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Emph lst) =
  withTextProp (selfClosingTag "w:i" []) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Subscript lst) =
  withTextProp (selfClosingTag "w:vertAlign" [("w:val","subscript")])
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Superscript lst) =
  withTextProp (selfClosingTag "w:vertAlign" [("w:val","superscript")])
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (SmallCaps lst) =
  withTextProp (selfClosingTag "w:smallCaps" [])
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Strikeout lst) =
  withTextProp (selfClosingTag "w:strike" [])
  $ inlinesToOpenXML opts lst
inlineToOpenXML _ LineBreak = return $ selfClosingTag "w:br" []
inlineToOpenXML _ (RawInline f x) | f == "openxml" = return $ text x
                                  | otherwise      = return empty
inlineToOpenXML opts (Quoted quoteType lst) =
  inlinesToOpenXML opts $ [Str open] ++ lst ++ [Str close]
    where (open, close) = case quoteType of
                            SingleQuote -> ("\x2018", "\x2019")
                            DoubleQuote -> ("\x201C", "\x201D")
inlineToOpenXML opts (Math t str) =
  case texMathToOMML dt str of
        Right r -> return $ text $ ppcElement conf r
        Left  _ -> inlinesToOpenXML opts (readTeXMath str)
    where dt = if t == InlineMath
                  then DisplayInline
                  else DisplayBlock
          conf = useShortEmptyTags (const False) defaultConfigPP
inlineToOpenXML opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML _ (Code _ str) =
  return $
    inTagsIndented "w:r" $
      inTagsIndented "w:rPr" (rStyle "VerbatimChar") $$
      inTags False "w:t" [("xml:space","preserve")]
          (text $ escapeStringForXML str)
inlineToOpenXML opts (Note bs) = do
  notes <- gets stFootnotes
  let notenum = length notes + 1
  let notemarker = inTagsIndented "w:r"
                   $ inTagsIndented "w:rPr" (rStyle "FootnoteReference")
                   $$ selfClosingTag "w:footnoteRef" []
  let notemarkerXml = RawInline "openxml" $ render Nothing notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs
  contents <- withParaProp (pStyle "FootnoteText") $ blocksToOpenXML opts
                $ insertNoteRef bs
  let newnote = inTags True "w:footnote" [("w:id",show notenum)] $ contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return $ inTagsIndented "w:r"
           $  inTagsIndented "w:rPr" (rStyle "FootnoteReference")
           $$ selfClosingTag "w:footnoteReference" [("w:id", show notenum)]

{-
inlineToOpenXML opts (Link txt (src, _)) =
  if isPrefixOf "mailto:" src
     then let src' = drop 7 src
              emailLink = inTagsSimple "email" $ text $
                          escapeStringForXML $ src'
          in  case txt of
               [Code _ s] | s == src' -> emailLink
               _             -> inlinesToOpenXML opts txt <+>
                                  char '(' <> emailLink <> char ')'
     else (if isPrefixOf "#" src
              then inTags False "link" [("linkend", drop 1 src)]
              else inTags False "ulink" [("url", src)]) $
          inlinesToOpenXML opts txt
inlineToOpenXML _ (Image _ (src, tit)) =
  let titleDoc = if null tit
                   then empty
                   else inTagsIndented "objectinfo" $

  in  inTagsIndented "inlinemediaobject" $ inTagsIndented "imageobject" $
      titleDoc $$ selfClosingTag "imagedata" [("fileref", src)]
-}
