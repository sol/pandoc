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
import Data.List ( intercalate )
import System.FilePath ( (</>) )
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.ByteString.Lazy.UTF8 ( fromString, toString )
import Codec.Archive.Zip
import System.Time
import Paths_pandoc ( getDataFileName )
import Text.Pandoc.MIME ( getMimeType )
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import System.Directory
import Text.Pandoc.ImageSize
import Text.Pandoc.Shared hiding (Element)
import Text.Pandoc.Readers.TeXMath
import Text.Pandoc.Highlighting ( highlight )
import Text.Highlighting.Kate.Types ()
import Text.XML.Light
import Text.TeXMath
import Control.Monad.State
import Text.Highlighting.Kate

-- TODO: Remove use of Text.Pandoc.XML; instead use the xml module
-- throughout.

data WriterState = WriterState{
         stTextProperties :: [Element]
       , stParaProperties :: [Element]
       , stFootnotes      :: [Element]
       , stSectionIds     :: [String]
       , stExternalLinks  :: M.Map String String
       , stImages         :: M.Map FilePath (String, B.ByteString)
       }

defaultWriterState :: WriterState
defaultWriterState = WriterState{
        stTextProperties = []
      , stParaProperties = []
      , stFootnotes      = []
      , stSectionIds     = []
      , stExternalLinks  = M.empty
      , stImages         = M.empty
      }

type WS a = StateT WriterState IO a

mknode :: Node t => String -> [(String,String)] -> t -> Element
mknode s attrs =
  add_attrs (map (\(k,v) -> Attr (unqual k) v) attrs) . node (unqual s)

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

  (newContents, st) <- runStateT (writeOpenXML opts{writerWrapText = False} doc)
                       defaultWriterState
  (TOD epochtime _) <- getClockTime
  -- TODO modify reldoc by adding image and link info
  let imgs = M.elems $ stImages st
  let imgPath ident img = "media/" ++ ident ++
                            case imageType img of
                                  Just Png  -> ".png"
                                  Just Jpeg -> ".jpeg"
                                  Just Gif  -> ".gif"
                                  Nothing   -> ""
  let toImgRel (ident,img) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/image"),("Id",ident),("Target",imgPath ident img)] ()
  let newrels = map toImgRel imgs
  let relpath = "word/_rels/document.xml.rels"
  let reldoc = case findEntryByPath relpath refArchive >>=
                    parseXMLDoc . toString . fromEntry of
                      Just d  -> d
                      Nothing -> error $ relpath ++ "missing in reference docx"
  let reldoc' = reldoc{ elContent = elContent reldoc ++ map Elem newrels }
  -- create entries for images
  let toImageEntry (ident,img) = toEntry ("word/" ++ imgPath ident img)
         epochtime img
  let imageEntries = map toImageEntry imgs
  -- NOW get list of external links and images from this, and do what's needed
  let toLinkRel (src,ident) =  mknode "Relationship" [("Type","http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"),("Id",ident),("Target",src),("TargetMode","External") ] ()
  let newrels' = map toLinkRel $ M.toList $ stExternalLinks st
  let reldoc'' = reldoc' { elContent = elContent reldoc' ++ map Elem newrels' }
  let relEntry = toEntry relpath epochtime $ fromString $ ppTopElement reldoc''
  let contentEntry = toEntry "word/document.xml" epochtime $ fromString $ ppTopElement newContents
  -- styles
  let newstyles = styleToOpenXml $ writerHighlightStyle opts
  let stylepath = "word/styles.xml"
  let styledoc = case findEntryByPath stylepath refArchive >>=
                      parseXMLDoc . toString . fromEntry of
                        Just d  -> d
                        Nothing -> error $ stylepath ++ "missing in reference docx"
  let styledoc' = styledoc{ elContent = elContent styledoc ++ map Elem newstyles }
  let styleEntry = toEntry stylepath epochtime $ fromString $ ppTopElement styledoc'
  -- TODO add metadata, etc.
  let archive = foldr addEntryToArchive refArchive $
                  contentEntry : relEntry : styleEntry : imageEntries
  return $ fromArchive archive

styleToOpenXml :: Style -> [Element]
styleToOpenXml style = parStyle : map toStyle alltoktypes
  where alltoktypes = enumFromTo KeywordTok NormalTok
        toStyle toktype = mknode "w:style" [("w:type","character"),
                           ("w:customStyle","1"),("w:styleId",show toktype)]
                             [ mknode "w:name" [("w:val",show toktype)] ()
                             , mknode "w:basedOn" [("w:val","VerbatimChar")] ()
                             , mknode "w:rPr" [] $
                               [ mknode "w:color" [("w:val",tokCol toktype)] ()
                                 | tokCol toktype /= "auto" ] ++
                               [ mknode "w:shd" [("w:val","clear"),("w:fill",tokBg toktype)] ()
                                 | tokBg toktype /= "auto" ] ++
                               [ mknode "w:b" [] () | tokFeature tokenBold toktype ] ++
                               [ mknode "w:i" [] () | tokFeature tokenItalic toktype ] ++
                               [ mknode "w:u" [] () | tokFeature tokenUnderline toktype ]
                             ]
        tokStyles = tokenStyles style
        tokFeature f toktype = maybe False f $ lookup toktype tokStyles
        tokCol toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenColor =<< lookup toktype tokStyles)
                           `mplus` defaultColor style
        tokBg toktype = maybe "auto" (drop 1 . fromColor)
                         $ (tokenBackground =<< lookup toktype tokStyles)
                           `mplus` backgroundColor style
        parStyle = mknode "w:style" [("w:type","paragraph"),
                           ("w:customStyle","1"),("w:styleId","SourceCode")]
                             [ mknode "w:name" [("w:val","Source Code")] ()
                             , mknode "w:basedOn" [("w:val","Normal")] ()
                             , mknode "w:link" [("w:val","VerbatimChar")] ()
                             , mknode "w:pPr" []
                               [ mknode "w:shd" [("w:val","clear"),("w:fill",tokBg NormalTok)] ()
                                 | tokBg NormalTok /= "auto" ]
                             ]

-- | Convert Pandoc document to string in OpenXML format.
writeOpenXML :: WriterOptions -> Pandoc -> WS Element
writeOpenXML opts (Pandoc (Meta tit auths dat) blocks) = do
  -- let title = empty -- inlinesToOpenXML opts tit
  -- let authors = [] -- map (authorToOpenXML opts) auths
  -- let date = empty -- inlinesToOpenXML opts dat
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
  let notes = case notes' of
                   [] -> []
                   ns -> [mknode "w:footnotes" [] ns]
  -- TODO do something with metadata (title, date, author)
  -- TODO eventually use xml module
  return $ mknode "w:document"
            [("xmlns:w","http://schemas.openxmlformats.org/wordprocessingml/2006/main")
            ,("xmlns:m","http://schemas.openxmlformats.org/officeDocument/2006/math")
            ,("xmlns:r","http://schemas.openxmlformats.org/officeDocument/2006/relationships")
            ,("xmlns:o","urn:schemas-microsoft-com:office:office")
            ,("xmlns:v","urn:schemas-microsoft-com:vml")
            ,("xmlns:w10","urn:schemas-microsoft-com:office:word")
            ,("xmlns:a","http://schemas.openxmlformats.org/drawingml/2006/main")
            ,("xmlns:pic","http://schemas.openxmlformats.org/drawingml/2006/picture")
            ,("xmlns:wp","http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")] (mknode "w:body" [] (doc ++ notes))

-- | Convert a list of Pandoc blocks to OpenXML.
blocksToOpenXML :: WriterOptions -> [Block] -> WS [Element]
blocksToOpenXML opts bls = concat `fmap` mapM (blockToOpenXML opts) bls

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
  in  mknode "varlistentry" [] $
      mknode "term" [] (inlinesToOpenXML opts term) $$
      mknode "listitem" [] (blocksToOpenXML opts def')

-- | Convert a list of lists of blocks to a list of OpenXML list items.
listItemsToOpenXML :: WriterOptions -> [[Block]] -> Doc
listItemsToOpenXML opts items = vcat $ map (listItemToOpenXML opts) items

-- | Convert a list of blocks into a OpenXML list item.
listItemToOpenXML :: WriterOptions -> [Block] -> Doc
listItemToOpenXML opts item =
  mknode "listitem" [] $ blocksToOpenXML opts $ map plainToPara item
-}

pStyle :: String -> Element
pStyle sty = mknode "w:pStyle" [("w:val",sty)] ()

rStyle :: String -> Element
rStyle sty = mknode "w:rStyle" [("w:val",sty)] ()

-- | Convert a Pandoc block element to OpenXML.
blockToOpenXML :: WriterOptions -> Block -> WS [Element]
blockToOpenXML _ Null = return []
{-
 - see image-example.openxml.xml
blockToOpenXML opts (Para [Image txt (src,_)]) =
  let capt = inlinesToOpenXML opts txt
  in  mknode "figure" [] $
        inTagsSimple "title" capt $$
        (mknode "mediaobject" [] $
           (mknode "imageobject" []
             (mknode "imagedata" [("fileref",src)] ())) $$
           inTagsSimple "textobject" (inTagsSimple "phrase" capt))
-}
blockToOpenXML opts (Header lev lst) = do
  contents <- withParaProp (pStyle $ "Heading" ++ show lev) $
               blockToOpenXML opts (Para lst)
  usedIdents <- gets stSectionIds
  let ident = uniqueIdent lst usedIdents
  modify $ \s -> s{ stSectionIds = ident : stSectionIds s }
  let bookmarkStart = mknode "w:bookmarkStart" [("w:id",ident)
                                               ,("w:name",ident)] ()
  let bookmarkEnd = mknode "w:bookmarkEnd" [("w:id",ident)] ()
  return $ [bookmarkStart] ++ contents ++ [bookmarkEnd]
blockToOpenXML opts (Plain lst) = blockToOpenXML opts (Para lst)
blockToOpenXML opts (Para lst) = do
  paraProps <- getParaProps
  contents <- inlinesToOpenXML opts lst
  return [mknode "w:p" [] (paraProps ++ contents)]
blockToOpenXML _ (RawBlock format str)
  | format == "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise           = return []
blockToOpenXML opts (BlockQuote blocks) =
  withParaProp (pStyle "BlockQuote") $ blocksToOpenXML opts blocks
blockToOpenXML opts (CodeBlock attrs str) = do
  contents <- withParaProp (pStyle "SourceCode")
              $ blockToOpenXML opts $ Para [Code attrs str]
  linenums <- withParaProp (pStyle "SourceCode")
              $ blockToOpenXML opts $ Para [Code attrs $ unlines $ map show [1..(length $ lines str)]]
  return [ mknode "w:tbl" []
           [ mknode "w:tblPr" [] ()
           , mknode "w:tblGrid" []
             [ mknode "w:gridCol" [("w:w","100")] ()
             , mknode "w:gridCol" [("w:w","12120")] () ]
           , mknode "w:tr" []
             [ mknode "w:tc" []
               (mknode "w:tcPr" [] () :  linenums)
             , mknode "w:tc" []
               (mknode "w:tcPr" [] () : contents)
             ]
           ]
         ]
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
  mknode "itemizedlist" [] $ listItemsToOpenXML opts lst
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
                    else (mknode "listitem" [("override",show start)]
                         [ (blocksToOpenXML opts $ map plainToPara first))
                         , listItemsToOpenXML opts rest]
  in  mknode "orderedlist" attribs items
blockToOpenXML opts (DefinitionList lst) =
  mknode "variablelist" [] $ deflistItemsToOpenXML opts lst
blockToOpenXML _ HorizontalRule = empty -- not semantic
blockToOpenXML opts (Table caption aligns widths headers rows) =
  let captionDoc   = if null caption
                        then empty
                        else mknode "title" []
                              (inlinesToOpenXML opts caption)
      tableType    = if isEmpty captionDoc then "informaltable" else "table"
      percent w    = show (truncate (100*w) :: Integer) ++ "*"
      coltags = vcat $ zipWith (\w al -> mknode "colspec"
                       ([("colwidth", percent w) | w > 0] ++
                        [("align", alignmentToString al)]) ()) widths aligns
      head' = if all null headers
                 then empty
                 else mknode "thead" [] $
                         tableRowToOpenXML opts headers
      body' = mknode "tbody" [] $
              vcat $ map (tableRowToOpenXML opts) rows
  in  mknode tableType [] [captionDoc,
        (mknode "tgroup" [("cols", show (length headers))] $
         coltags $$ head' $$ body')]
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
  mknode "row" [] $ vcat $ map (tableItemToOpenXML opts) cols

tableItemToOpenXML :: WriterOptions
                   -> [Block]
                   -> Doc
tableItemToOpenXML opts item =
  mknode "entry" [] $ vcat $ map (blockToOpenXML opts) item
-}

-- | Convert a list of inline elements to OpenXML.
inlinesToOpenXML :: WriterOptions -> [Inline] -> WS [Element]
inlinesToOpenXML opts lst = concat `fmap` mapM (inlineToOpenXML opts) lst

getTextProps :: WS [Element]
getTextProps = do
  props <- gets stTextProperties
  return $ if null props
              then []
              else [mknode "w:rPr" [] $ props]

pushTextProp :: Element -> WS ()
pushTextProp d = modify $ \s -> s{ stTextProperties = d : stTextProperties s }

popTextProp :: WS ()
popTextProp = modify $ \s -> s{ stTextProperties = drop 1 $ stTextProperties s }

withTextProp :: Element -> WS a -> WS a
withTextProp d p = do
  pushTextProp d
  res <- p
  popTextProp
  return res

getParaProps :: WS [Element]
getParaProps = do
  props <- gets stParaProperties
  return $ if null props
              then []
              else [mknode "w:pPr" [] $ props]

pushParaProp :: Element -> WS ()
pushParaProp d = modify $ \s -> s{ stParaProperties = d : stParaProperties s }

popParaProp :: WS ()
popParaProp = modify $ \s -> s{ stParaProperties = drop 1 $ stParaProperties s }

withParaProp :: Element -> WS a -> WS a
withParaProp d p = do
  pushParaProp d
  res <- p
  popParaProp
  return res

formattedString :: String -> WS Element
formattedString str = do
  props <- getTextProps
  return $ mknode "w:r" [] $
             props ++
             [ mknode "w:t" [("xml:space","preserve")] str ]

-- | Convert an inline element to OpenXML.
inlineToOpenXML :: WriterOptions -> Inline -> WS [Element]
inlineToOpenXML _ (Str str) = (:[]) `fmap` formattedString str
inlineToOpenXML opts Space = inlineToOpenXML opts (Str " ")
inlineToOpenXML opts (Strong lst) =
  withTextProp (mknode "w:b" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Emph lst) =
  withTextProp (mknode "w:i" [] ()) $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Subscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","subscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Superscript lst) =
  withTextProp (mknode "w:vertAlign" [("w:val","superscript")] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (SmallCaps lst) =
  withTextProp (mknode "w:smallCaps" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML opts (Strikeout lst) =
  withTextProp (mknode "w:strike" [] ())
  $ inlinesToOpenXML opts lst
inlineToOpenXML _ LineBreak = return [ mknode "w:br" [] () ]
inlineToOpenXML _ (RawInline f str)
  | f == "openxml" = return [ x | Elem x <- parseXML str ]
  | otherwise      = return []
inlineToOpenXML opts (Quoted quoteType lst) =
  inlinesToOpenXML opts $ [Str open] ++ lst ++ [Str close]
    where (open, close) = case quoteType of
                            SingleQuote -> ("\x2018", "\x2019")
                            DoubleQuote -> ("\x201C", "\x201D")
inlineToOpenXML opts (Math t str) =
  case texMathToOMML dt str of
        Right r -> return [r]
        Left  _ -> inlinesToOpenXML opts (readTeXMath str)
    where dt = if t == InlineMath
                  then DisplayInline
                  else DisplayBlock
inlineToOpenXML opts (Cite _ lst) = inlinesToOpenXML opts lst
inlineToOpenXML opts (Code attrs str) =
  withTextProp (rStyle "VerbatimChar")
  $ case highlight formatOpenXML attrs str of
         Nothing  -> (:[]) `fmap` formattedString str
         Just h   -> return h
     where formatOpenXML _fmtOpts = intercalate [mknode "w:br" [] ()] .
                                    map (map toHlTok)
           toHlTok (toktype,tok) = mknode "w:r" []
                                     [ mknode "w:rPr" []
                                       [ rStyle $ show toktype ]
                                     , mknode "w:t" [("xml:space","preserve")] tok ]
inlineToOpenXML opts (Note bs) = do
  notes <- gets stFootnotes
  let notenum = length notes + 1
  let notemarker = mknode "w:r" []
                   [ mknode "w:rPr" [] (rStyle "FootnoteReference")
                   , mknode "w:footnoteRef" [] () ]
  let notemarkerXml = RawInline "openxml" $ ppElement notemarker
  let insertNoteRef (Plain ils : xs) = Plain (notemarkerXml : ils) : xs
      insertNoteRef (Para ils  : xs) = Para  (notemarkerXml : ils) : xs
      insertNoteRef xs               = Para [notemarkerXml] : xs
  contents <- withParaProp (pStyle "FootnoteText") $ blocksToOpenXML opts
                $ insertNoteRef bs
  let newnote = mknode "w:footnote" [("w:id",show notenum)] $ contents
  modify $ \s -> s{ stFootnotes = newnote : notes }
  return [ mknode "w:r" []
           [ mknode "w:rPr" [] (rStyle "FootnoteReference")
           , mknode "w:footnoteReference" [("w:id", show notenum)] () ] ]
-- internal link:
inlineToOpenXML opts (Link txt ('#':xs,_)) = do
  contents <- withTextProp (rStyle "Hyperlink") $ inlinesToOpenXML opts txt
  return [ mknode "w:hyperlink" [("w:anchor",xs)] contents ]
-- external link:
inlineToOpenXML opts (Link txt (src,_)) = do
  contents <- withTextProp (rStyle "Hyperlink") $ inlinesToOpenXML opts txt
  extlinks <- gets stExternalLinks
  ind <- case M.lookup src extlinks of
            Just i   -> return i
            Nothing  -> do
              let i = "link" ++ show (M.size extlinks)
              modify $ \st -> st{ stExternalLinks =
                        M.insert src i extlinks }
              return i
  return [ mknode "w:hyperlink" [("r:id",ind)] contents ]
inlineToOpenXML _ (Image _ (src, tit)) = do
  imgs <- gets stImages
  (ident,size) <- case M.lookup src imgs of
                       Just (i,img) -> return (i, imageSize img)
                       Nothing -> do
                         -- TODO check existence download etc.
                         img <- liftIO $ B.readFile src
                         let ident' = "image" ++ show (M.size imgs + 1)
                         let size'  = imageSize img
                         modify $ \st -> st{
                            stImages = M.insert src (ident',img) $ stImages st }
                         return (ident',size')
  let (xpix,ypix) = maybe (100,100) id size
  let (xemu,yemu) = (xpix * 9525, ypix * 9525)
  let cNvPicPr = mknode "pic:cNvPicPr" [] $
                   mknode "a:picLocks" [("noChangeArrowheads","1"),("noChangeAspect","1")] ()
  let nvPicPr  = mknode "pic:nvPicPr" []
                  [ mknode "pic:cNvPr"
                      [("descr",src),("id","0"),("name","Picture")] ()
                  , cNvPicPr ]
  let blipFill = mknode "pic:blipFill" []
                   [ mknode "a:blip" [("r:embed",ident)] ()
                   , mknode "a:stretch" [] $ mknode "a:fillRect" [] () ]
  let xfrm =    mknode "a:xfrm" []
                  [ mknode "a:off" [("x","0"),("y","0")] ()
                  , mknode "a:ext" [("cx",show xemu),("cy",show yemu)] () ]
  let prstGeom = mknode "a:prstGeom" [("prst","rect")] $
                   mknode "a:avLst" [] ()
  let ln =      mknode "a:ln" [("w","9525")]
                  [ mknode "a:noFill" [] ()
                  , mknode "a:headEnd" [] ()
                  , mknode "a:tailEnd" [] () ]
  let spPr =    mknode "pic:spPr" [("bwMode","auto")]
                  [xfrm, prstGeom, mknode "a:noFill" [] (), ln]
  let graphic = mknode "a:graphic" [] $
                  mknode "a:graphicData" [("uri","http://schemas.openxmlformats.org/drawingml/2006/picture")]
                    [ mknode "pic:pic" []
                      [ nvPicPr
                      , blipFill
                      , spPr ] ]
  return [ mknode "w:r" [] $
      mknode "w:drawing" [] $
        mknode "wp:inline" []
          [ mknode "wp:extent" [("cx",show xemu),("cy",show yemu)] ()
          , mknode "wp:effectExtent" [("b","0"),("l","0"),("r","0"),("t","0")] ()
          , mknode "wp:docPr" [("descr",tit),("id","1"),("name","Picture")] ()
          , graphic ] ]

