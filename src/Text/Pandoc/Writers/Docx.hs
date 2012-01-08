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
import Text.Pandoc.Shared ( WriterOptions(..) )
import Text.Pandoc.MIME ( getMimeType )
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Writers.OpenXML ( writeOpenXML )
import System.Directory
import Control.Monad (liftM)
import Network.URI ( unEscapeString )
import Text.Pandoc.XML
import Text.Pandoc.Pretty

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
  let newContents = writeOpenXML opts{writerWrapText = False} doc'
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
