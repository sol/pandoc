{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Docx
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to docx.
-}
module Text.Pandoc.Writers.Docx ( writeDocx ) where
import Data.IORef
import Data.List ( isPrefixOf, nub, sort )
import System.FilePath ( (</>), takeExtension )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 ( fromString )
import Codec.Archive.Zip
import System.Time
import Paths_pandoc ( getDataFileName )
import Text.Pandoc.MIME ( getMimeType )
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import System.Directory
import Control.Monad (liftM)
import Network.URI ( unEscapeString )
import Text.Pandoc.XML
import Text.Pandoc.Pretty
import Text.Pandoc.ImageSize
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Readers.TeXMath
import Data.List ( isPrefixOf, intercalate, isSuffixOf, sort, elemIndex, nub )
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
       , stSectionIds     :: [String]
       , stExternalLinks  :: [String]
       , stImages         :: [B.ByteString]
       }

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stTextProperties = []
      , stParaProperties = []
      , stFootnotes      = []
      , stSectionIds     = []
      , stExternalLinks  = []
      , stImages         = []
      }

type WS a = StateT WriterState IO a

-- | Produce an Docx file from a Pandoc document.
writeDocx :: Maybe FilePath -- ^ Path specified by --reference-docx
          -> WriterOptions  -- ^ Writer options
          -> Pandoc         -- ^ Document to convert
          -> IO B.ByteString
writeDocx mbRefDocx opts doc = do
  let datadir = writerUserDataDir opts
  refArchive <- liftM toArchive $
       case mbRefDocx of
             Just f -> B.readFile f
             Nothing -> do
               let defaultDocx = getDataFileName "reference.docx" >>= B.readFile
               case datadir of
                     Nothing  -> defaultDocx
                     Just d   -> do
                        exists <- doesFileExist (d </> "reference.docx")
                        if exists
                           then B.readFile (d </> "reference.docx")
                           else defaultDocx
  
  {-
  -- handle pictures
  picEntriesRef <- newIORef ([] :: [Entry])
  let sourceDir = writerSourceDirectory opts
  doc' <- bottomUpM (transformPic sourceDir picEntriesRef) doc
  -}
  let doc' = doc  -- TODO temp
  let isInternal ('#':_) = True
      isInternal _       = False
  let findLink x@(Link _ (s,_)) = [s | not (isInternal s)]
      findLink x = []
  let extlinks = nub $ sort $ queryWith findLink doc'
  -- TODO use this to write word/_rels/document.xml.rels
  -- for each link, we need:
  -- <Relationship Id="link0"  Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"  Target="http://google.com/" TargetMode="External" />
  (newContents, st) <- runStateT (writeOpenXML opts{writerWrapText = False} doc') defaultWriterState
  (TOD epochtime _) <- getClockTime
  let contentEntry = toEntry "word/document.xml" epochtime $ fromString newContents
  {-
  picEntries <- readIORef picEntriesRef
  let archive = foldr addEntryToArchive refArchive $ contentEntry : picEntries
  -}
  let archive = foldr addEntryToArchive refArchive [contentEntry]
  {-
  -- construct META-INF/manifest.xml based on archive
  let toFileEntry fp = case getMimeType fp of
                        Nothing  -> empty
                        Just m   -> selfClosingTag "manifest:file-entry"
                                     [("manifest:media-type", m)
                                     ,("manifest:full-path", fp)
                                     ]
  let files = [ ent | ent <- filesInArchive archive, not ("META-INF" `isPrefixOf` ent) ]
  let manifestEntry = toEntry "META-INF/manifest.xml" epochtime
        $ fromString $ show
        $ text "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        $$
         ( inTags True "manifest:manifest"
            [("xmlns:manifest","urn:oasis:names:tc:opendocument:xmlns:manifest:1.0")]
            $ ( selfClosingTag "manifest:file-entry"
                 [("manifest:media-type","application/vnd.oasis.opendocument.text")
                 ,("manifest:version","1.2")
                 ,("manifest:full-path","/")]
                $$ vcat ( map toFileEntry $ files )
              )
         )
  let archive' = addEntryToArchive manifestEntry archive
  -}
  let archive' = archive -- TODO temp
  return $ fromArchive archive'

{-
transformPic :: FilePath -> IORef [Entry] -> Inline -> IO Inline
transformPic sourceDir entriesRef (Image lab (src,tit)) = do
  let src' = unEscapeString src
  entries <- readIORef entriesRef
  let newsrc = "Pictures/" ++ show (length entries) ++ takeExtension src'
  catch (readEntry [] (sourceDir </> src') >>= \entry ->
           modifyIORef entriesRef (entry{ eRelativePath = newsrc } :) >>
           return (Image lab (newsrc, tit)))
        (\_ -> return (Emph lab))
transformPic _ _ x = return x
-}

-- | Convert Pandoc document to string in OpenXML format.
writeOpenXML :: WriterOptions -> Pandoc -> WS String
writeOpenXML opts (Pandoc (Meta tit auths dat) blocks) = do
  let title = empty -- inlinesToOpenXML opts tit
  let authors = [] -- map (authorToOpenXML opts) auths
  let date = empty -- inlinesToOpenXML opts dat
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let render' = render colwidth
  let convertSpace (Str x : Space : Str y : xs) = Str (x ++ " " ++ y) : xs
      convertSpace (Str x : Str y : xs) = Str (x ++ y) : xs
      convertSpace xs = xs
  let blocks' = bottomUp convertSpace $ blocks
  -- let isInternal ('#':_) = True
  --     isInternal _       = False
  -- let findLink x@(Link _ (s,_)) = [s | not (isInternal s)]
  --     findLink x = []
  --     extlinks = nub $ sort $ queryWith findLink blocks'
  doc <- blocksToOpenXML opts blocks' 
  notes' <- reverse `fmap` gets stFootnotes
  let notes    = case notes' of
                      [] -> empty
                      ns -> inTagsIndented "w:footnotes" $ vcat ns
  -- TODO do something with metadata
  let context = writerVariables opts ++
               [ ("title", render' title)
               , ("date", render' date) ] ++
               [ ("author", render' a) | a <- authors ]
  -- TODO eventually use xml module
  return $ render' $ text "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"
        $$ inTags True "w:document"
            [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")
            ,("xmlns:m","http://schemas.openxmlformats.org/officeDocument/2006/math")
            ,("xmlns:r","http://schemas.openxmlformats.org/officeDocument/2006/relationships")
            ,("xmlns:o","urn:schemas-microsoft-com:office:office")
            ,("xmlns:v","urn:schemas-microsoft-com:vml")
            ,("xmlns:w10","urn:schemas-microsoft-com:office:word")
            ,("xmlns:a","http://schemas.openxmlformats.org/drawingml/2006/main")
            ,("xmlns:pic","http://schemas.openxmlformats.org/drawingml/2006/picture")
            ,("xmlns:wp","http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")] (inTagsIndented "w:body" (doc $$ notes))

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
 - see image-example.openxml.xml
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
  contents <- withParaProp (pStyle $ "Heading" ++ show lev) $
               blockToOpenXML opts (Para lst)
  usedIdents <- gets stSectionIds
  let ident = uniqueIdent lst usedIdents
  modify $ \s -> s{ stSectionIds = ident : stSectionIds s }
  let bookmarkStart = selfClosingTag "w:bookmarkStart" [("w:id",ident),
                                                        ("w:name",ident)]
  let bookmarkEnd = selfClosingTag "w:bookmarkEnd" [("w:id",ident)]
  return $ bookmarkStart $$ contents $$ bookmarkEnd
blockToOpenXML opts (Plain lst) = blockToOpenXML opts (Para lst)
blockToOpenXML opts (Para lst) = do
  paraProps <- getParaProps
  contents <- inlinesToOpenXML opts lst
  return $ inTagsIndented "w:p" $ paraProps $$ contents
blockToOpenXML _ (RawBlock format str)
  | format == "openxml" = return $ text str -- raw XML block
  | otherwise           = return empty
blockToOpenXML opts (BlockQuote blocks) =
  withParaProp (pStyle "BlockQuote") $ blocksToOpenXML opts blocks
blockToOpenXML opts x =
  blockToOpenXML opts (Para [Str "BLOCK"])

{-
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
inlineToOpenXML opts (Code _ str) =
  withTextProp (rStyle "VerbatimChar") $ inlineToOpenXML opts (Str str)
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
-- internal link:
inlineToOpenXML opts (Link txt ('#':xs,_)) = do
  contents <- withTextProp (rStyle "Hyperlink") $ inlinesToOpenXML opts txt
  return $ inTags True "w:hyperlink" [("w:anchor",xs)] contents
inlineToOpenXML opts (Link txt (src,_)) = do
  contents <- withTextProp (rStyle "Hyperlink") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  return $
    case elemIndex src extlinks of
         Just ind -> inTags True "w:hyperlink"
                        [("r:id","link" ++ show ind)] contents
         Nothing  -> inTags True "w:hyperlink" [] contents  -- shouldn't happen
-- see image-example.openxml.xml
inlineToOpenXML _ (Image _ (src, tit)) = do
  let ident = "image0" -- FIXME
  let cNvPicPr = inTagsIndented "pic:cNvPicPr" $
                   selfClosingTag "a:picLocks" [("noChangeArrowheads","1"),("noChangeAspect","1")]
  let nvPicPr  = inTagsIndented "pic:nvPicPr" $
                   selfClosingTag "pic:cNvPr"
                    [("descr",tit),("id","0"),("name","Picture")] $$
                   cNvPicPr
  let blipFill = inTagsIndented "pic:blipFill" $
                    selfClosingTag "a:blip" [("r:embed",ident)]
  let xfrm =    inTagsIndented "a:xfrm" $
                  selfClosingTag "a:off" [("x","0"),("y","0")] $$
                  selfClosingTag "a:ext" [("cx","1800000"),("cy","1800000")]
  let prstGeom = inTags True "a:prstGeom" [("prst","rect")] $
                   selfClosingTag "a:avLst" []
  let ln =      inTags True "a:ln" [("w","9525")] $
                   selfClosingTag "a:noFill" [] $$
                   selfClosingTag "a:headEnd" [] $$
                   selfClosingTag "a:tailEnd" []
  let spPr =    inTags True "pic:spPr" [("bwMode","auto")] $
                  xfrm $$ prstGeom $$ selfClosingTag "a:noFill" [] $$ ln
  let graphic = inTagsIndented "a:graphic" $
                  inTags True "a:graphicData" [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")] $
                    inTagsIndented "pic:pic" $ nvPicPr $$ blipFill $$ spPr
  return $ inTagsIndented "w:r" $
      inTagsIndented "w:drawing" $
        inTags True "wp:inline" [] $
          selfClosingTag "wp:extent" [("cx","1800000"),("cy","1800000")] $$
          selfClosingTag "wp:effectExtent" [("b","0"),("l","0"),("r","0"),("t","0")] $$
          selfClosingTag "wp:docPr" [("descr",tit),("id","1"),("name","Picture")] $$
          graphic
-- FIXME
inlineToOpenXML opts x =
  inlineToOpenXML opts (Str "INLINE")

