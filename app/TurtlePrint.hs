module TurtlePrint where
  
import SNE (TurtleFormatterState (..), Formatter, emptyTFS, setIndent, setLineBreak, nextLine, formatLabel, nextObject, nextProperty, nextSubject, newState)
import SNEInternal (NodeGenState(..), NodeGenLookupMap, LabelContext (SubjContext, PredContext, ObjContext), PredTree, SubjTree, formatPrefixLines, quoteB, hasMore, findMaxBnode, emptyNgs)

import Swish.RDF.Graph (RDFGraph, RDFLabel, NamespaceMap, getNamespaces)
import Data.Word (Word32)
import qualified Data.Map as M
import Control.Monad.State as CMS (get, put, runStateT, evalStateT, StateT, runState)
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict)
import qualified Data.Text as T (Text, pack, unpack, stripPrefix, stripSuffix)
import qualified Data.Text.Lazy.Builder as B (Builder, toLazyText, fromText, fromString)
import Control.Monad.State (State, modify)


newlineBuilder :: B.Builder
newlineBuilder = B.fromString "\n"

dotBuilder :: B.Builder
dotBuilder = B.fromString ". \n"

hackIndentTP :: B.Builder
hackIndentTP = B.fromString ""


-- | Convert the graph to text.
formatGraphAsTextTP :: RDFGraph -> T.Text
formatGraphAsTextTP = TL.toStrict . formatGraphAsLazyTextTP

-- | Convert the graph to text.
formatGraphAsLazyTextTP :: RDFGraph -> TL.Text
formatGraphAsLazyTextTP = B.toLazyText . formatGraphAsBuilderTP
  
-- | Convert the graph to a Builder.
-- function changed
formatGraphAsBuilderTP :: RDFGraph -> B.Builder
formatGraphAsBuilderTP = formatGraphIndentTP newlineBuilder False

-- | Convert the graph to a builder using the given indentation text.
formatGraphIndentTP ::
    B.Builder     -- ^ indentation text
    -> Bool       -- ^ are prefixes to be generated?
    -> RDFGraph   -- ^ graph
    -> B.Builder
formatGraphIndentTP indnt flag gr = 
  let (res, _, _, _) = formatGraphDiagTP indnt flag gr
  in res

-- | Format graph and return additional information.
formatGraphDiagTP :: 
  B.Builder  -- ^ indentation
  -> Bool    -- ^ are prefixes to be generated?
  -> RDFGraph 
  -> (B.Builder, NodeGenLookupMap, Word32, [String])
formatGraphDiagTP indnt flag gr = 
  let fg  = formatGraphTP indnt dotBuilder False flag gr
      ngs = emptyNgs { nodeGen = findMaxBnode gr }
             
      (out, fgs) = runState fg (emptyTFS ngs)
      ogs        = nodeGenSt fgs
  
  in (out, nodeMap ogs, nodeGen ogs, traceBuf fgs)

formatGraphTP :: 
  B.Builder     -- indentation string
  -> B.Builder  -- text to be placed after final statement
  -> Bool       -- True if a line break is to be inserted at the start
  -> Bool       -- True if prefix strings are to be generated
  -> RDFGraph   -- graph to convert
  -> Formatter B.Builder
formatGraphTP = formatGraphTP_ setIndent setLineBreak newState formatPrefixesTP subjs formatSubjectsTP

formatPrefixesTP :: NamespaceMap -> Formatter B.Builder
formatPrefixesTP = formatPrefixesTP_ nextLine

formatSubjectsTP :: Formatter B.Builder
formatSubjectsTP = formatSubjectsTP_ nextSubject formatLabel props formatPropertiesTP subjs nextLine

formatPropertiesTP :: RDFLabel -> B.Builder -> Formatter B.Builder
formatPropertiesTP = formatPropertiesTP_ nextProperty formatLabel formatObjectsTP props nextLine

formatObjectsTP :: RDFLabel -> RDFLabel -> B.Builder -> Formatter B.Builder
formatObjectsTP = formatObjectsTP_ nextObject formatLabel objs nextLine

formatGraphTP_ :: 
    (B.Builder -> State a ()) -- set indent
    -> (Bool -> State a ())   -- set line-break flag
    -> (RDFGraph -> a -> a)        -- create a new state from the graph
    -> (NamespaceMap -> State a B.Builder) -- format prefixes
    -> (a -> SubjTree RDFLabel)      -- get the subjects
    -> State a B.Builder              -- format the subjects
    -> B.Builder     -- indentation string
    -> B.Builder  -- text to be placed after final statement
    -> Bool       -- True if a line break is to be inserted at the start
    -> Bool       -- True if prefix strings are to be generated
    -> RDFGraph   -- graph to convert
    -> State a B.Builder
formatGraphTP_ setIndent setLineBreak newState formatPrefixes subjs formatSubjects ind end dobreak dopref gr = do
  setIndent ind
  setLineBreak dobreak
  modify (newState gr)
  
  fp <- if dopref
        then formatPrefixes (getNamespaces gr)
        else return mempty
  more <- hasMore subjs
  if more
    then do
      fr <- formatSubjects
      return $ mconcat [fp, fr, end]
    else return fp

formatSubjectsTP_ ::
    State a RDFLabel     -- ^ next subject
    -> (LabelContext -> RDFLabel -> State a B.Builder)  -- ^ convert label into text
    -> (a -> PredTree RDFLabel) -- ^ extract properties
    -> (RDFLabel -> B.Builder -> State a B.Builder)   -- ^ format properties
    -> (a -> SubjTree RDFLabel) -- ^ extract subjects
    -> (B.Builder -> State a B.Builder) -- ^ next line
    -> State a B.Builder
formatSubjectsTP_ nextSubject formatLabel props formatProperties subjs nextLine = do
  sb    <- nextSubject
  sbstr <- formatLabel SubjContext sb
  
  flagP <- hasMore props
  if flagP
    then do
      prstr <- formatProperties sb sbstr
      flagS <- hasMore subjs
      if flagS
        then do
          fr <- formatSubjectsTP_ nextSubject formatLabel props formatProperties subjs nextLine
          return $ mconcat [prstr, B.fromString " .", fr]
        else return prstr
           
    else do
      txt <- nextLine sbstr
    
      flagS <- hasMore subjs
      if flagS
        then do
          fr <- formatSubjectsTP_ nextSubject formatLabel props formatProperties subjs nextLine
          return $ mconcat [txt, B.fromString " .", fr]
        else return txt

formatPropertiesTP_ :: 
    (RDFLabel -> State a RDFLabel)        -- ^ next property for the given subject
    -> (LabelContext -> RDFLabel -> State a B.Builder) -- ^ convert label into text
    -> (RDFLabel -> RDFLabel -> B.Builder -> State a B.Builder) -- ^ format objects
    -> (a -> PredTree RDFLabel) -- ^ extract properties
    -> (B.Builder -> State a B.Builder) -- ^ next line
    -> RDFLabel             -- ^ property being processed
    -> B.Builder            -- ^ current output
    -> State a B.Builder
formatPropertiesTP_ nextProperty formatLabel formatObjects props nextLine sb sbstr = do
  pr <- nextProperty sb
  prstr <- formatLabel PredContext pr
  obstr <- formatObjects sb pr $ mconcat [sbstr, B.fromString " ", prstr]
  more  <- hasMore props
  let sbindent = hackIndentTP -- mkIndent sbstr
  if more
    then do
      fr <- formatPropertiesTP_ nextProperty formatLabel formatObjects props nextLine sb sbindent
      nl <- nextLine $ obstr `mappend` (B.fromString " ;")
      return $ nl `mappend` fr
    else nextLine obstr

formatObjectsTP_ :: 
    (RDFLabel -> RDFLabel -> State a RDFLabel) -- ^ get the next object for the (subject,property) pair
    -> (LabelContext -> RDFLabel -> State a B.Builder) -- ^ format a label
    -> (a -> [RDFLabel])   -- ^ extract objects
    -> (B.Builder -> State a B.Builder) -- ^ insert a new line
    -> RDFLabel      -- ^ subject
    -> RDFLabel      -- ^ property
    -> B.Builder     -- ^ current text
    -> State a B.Builder
formatObjectsTP_ nextObject formatLabel objs nextLine sb pr prstr = do
  ob    <- nextObject sb pr
  obstr <- formatLabel ObjContext ob
  more  <- hasMore objs
  if more
    then do
      let prindent = hackIndentTP -- mkIndent prstr
      fr <- formatObjectsTP_ nextObject formatLabel objs nextLine sb pr prindent
      nl <- nextLine $ mconcat [prstr, B.fromString " ", obstr, B.fromString ","]
      return $ nl `mappend` fr
    else return $ mconcat [prstr, B.fromString " ", obstr]

formatPrefixesTP_ ::
    (B.Builder -> State a B.Builder)  -- ^ Create a new line
    -> NamespaceMap
    -> State a B.Builder
formatPrefixesTP_ nextLine pmap = 
    mconcat `fmap` mapM nextLine (formatPrefixLines pmap)

quoteBString :: String -> B.Builder
quoteBString = quoteB True