module Text.Markdown.SlamDown.Smolder (toMarkup) where

import Prelude

import Control.Monad.Reader (Reader, runReader, ask)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, intercalate, sequence_, traverse_)
import Data.List (List, filter, range, length, zip, (:))
import Data.Map (unions)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), replaceAll)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Text.Markdown.SlamDown (Block(..), CodeBlockType(..), Inline(..), LinkTarget(..), ListType(..), SlamDownP(..))
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (Markup, MarkupM(..), attribute, text, (!))
import Text.Smolder.Markup as SM

type ReferenceLinks = Map.Map String String
type ReaderMarkup e = Reader ReferenceLinks (Markup e)

toMarkup :: forall a e. SlamDownP a -> Markup e
toMarkup (SlamDown bs) = runReader (toElements bs) linkRefs
  where 
    linkRefs = foldMap getBlockLinkRefs bs

getBlockLinkRefs :: forall a. Block a -> ReferenceLinks
getBlockLinkRefs = getBlockLinkRefs' Map.empty
  where
    getBlockLinkRefs' links block = 
      case block of 
        (LinkReference k url) -> Map.insert k url links
        (Blockquote bs) -> foldl getBlockLinkRefs' links bs
        (Lst _ bss) -> unions $ links : map (foldl getBlockLinkRefs' Map.empty) bss
        _ -> links

toElements :: forall a e. List (Block a) -> ReaderMarkup e
toElements bs = sequence_ <$> traverse toElement bs

toListElement :: forall a e. Block a -> ReaderMarkup e
toListElement block =
  case block of
    (Paragraph is) -> (pure <<< HTML.li) =<< toInlineElements is
    _ -> toElement block

toListElements :: forall a e. List (Block a) -> ReaderMarkup e
toListElements bs = sequence_ <$> traverse toListElement bs

isEmpty :: forall e a. MarkupM e a -> Boolean
isEmpty (Empty _) = true
isEmpty _ = false

isList :: forall e a. MarkupM e a -> Boolean
isList (Element _ n _ _ _ _) = (n == "ul" || n == "ol")
isList _ = false

isParagraph :: forall e a. MarkupM e a -> Boolean
isParagraph (Element _ n _ _ _ _) = n == "p"
isParagraph _ = false

getChild :: forall e a. MarkupM e a -> Markup e
getChild (Element _ n c a e r) = c
getChild _ = SM.empty

encodeInlines :: forall a. List (Inline a) -> String 
encodeInlines inlines = 
  case (regex "[^\\w -]" global) of
    Left _ -> encoded identity
    Right pattern -> encoded (stripInvalidChars pattern)
  where
    replaceSpaces = replaceAll (Pattern " ") (Replacement "_")
    encoded stripFn= (replaceSpaces <<< intercalate "" 
        <<< filter (_ /= "\n") <<< map (stripFn <<< toInline)) inlines
    stripInvalidChars pattern = replace pattern ""
    
toCodeBlockContent :: forall e. List String -> Markup e
toCodeBlockContent ss = flip traverse_ (zip (range 1 (length ss)) ss) \(Tuple n s) -> 
  if n == 1 
    then text s 
    else do 
      HTML.br
      text s 

toElement :: forall a e. Block a -> ReaderMarkup e
toElement block =
  case block of
    (Paragraph is) -> (pure <<< HTML.p) =<< toInlineElements is

    (Header n is) -> toInlineElements is 
      >>= (\children -> pure $ HTML.parent ("h" <> show n) children ! HA.id (encodeInlines is))
    (Blockquote bs) -> (pure <<< HTML.blockquote) =<< toElements bs
    (Lst (Bullet s) bss) -> 
      sequence_ <$> traverse (\e -> (pure <<< HTML.ul) =<< toListElements e) bss
    (Lst (Ordered s) bss) -> 
      sequence_ <$> traverse (\e -> (pure <<< HTML.ol) =<< toListElements e) bss
    (CodeBlock Indented ss) -> 
      pure <<< HTML.pre <<< HTML.code $ toCodeBlockContent ss
    (CodeBlock (Fenced _ info) ss) -> 
      pure $ HTML.pre ! HA.className "code" ! (attribute "data-lang" info) $ HTML.code $ toCodeBlockContent ss

    (Rule) -> pure $ HTML.hr
    _ -> pure $ SM.empty

toInlineElements :: forall a e. List (Inline a) -> ReaderMarkup e
toInlineElements is = sequence_ <$> traverse toInlineElement is

toInlineElement :: forall a e. Inline a -> ReaderMarkup e
toInlineElement il =
  case il of
    (Str s) -> pure $ text s
    (Entity s) -> pure $ text s
    (Space) -> pure $ text " "
    (SoftBreak) -> pure $ text "\n"
    (LineBreak) -> pure HTML.br

    (Emph is) -> (pure <<< HTML.em) =<< toInlineElements is
    (Strong is) -> (pure <<< HTML.strong) =<< toInlineElements is
    (Code e s) -> pure $ HTML.code $ text s
    (Link is (InlineLink url)) -> 
      toInlineElements is >>= 
        (pure <<< (HTML.a ! HA.href url))
    (Link is (ReferenceLink ref)) -> toInlineElements is >>= \el -> do  
      links <- ask
      let 
        url = maybe "" (\k -> fromMaybe "" $ Map.lookup k links) ref
        urlAttr = HA.href url
      pure $ HTML.a ! urlAttr $ el
      
    (Image is url) -> pure $ HTML.img ! HA.src url ! HA.alt (toInlines is)
    (FormField l r e) -> pure $ SM.empty

toInlines :: forall a. List (Inline a) -> String
toInlines is = foldl (\str il -> str <> toInline il) "" is

toInline :: forall a. Inline a -> String
toInline il = case il of
  (Str s) -> s
  (Entity s) -> s
  (Space) -> " "
  (SoftBreak) -> "\n"
  (LineBreak) -> "\n"
  _ -> ""

