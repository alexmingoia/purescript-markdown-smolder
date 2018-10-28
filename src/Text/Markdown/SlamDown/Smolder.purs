module Text.Markdown.SlamDown.Smolder (toMarkup) where

import Prelude

import Control.Monad.Reader (Reader, runReader, ask)
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl, intercalate, sequence_)
import Data.List (List, filter)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.String (Pattern(Pattern), Replacement(Replacement), replaceAll)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Traversable (traverse)
import Text.Markdown.SlamDown (Block(..), Inline(..), LinkTarget(..), ListType(..), SlamDownP(..))
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (Markup, MarkupM(..), text, (!))
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
        (Lst _ bss) -> links <> foldMap (foldl getBlockLinkRefs' Map.empty) bss
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
    Left _ -> encoded
    Right pattern -> stripInvalidChars pattern encoded
  where
    replaceSpaces = replaceAll (Pattern " ") (Replacement "_")
    encoded = (replaceSpaces <<< intercalate "_" <<< filter (_ /= "\n") <<< map toInline) inlines
    stripInvalidChars pattern = replace pattern ""
    

toElement :: forall a e. Block a -> ReaderMarkup e
toElement block =
  case block of
    (Paragraph is) -> (pure <<< HTML.p) =<< toInlineElements is

    (Header n is) -> toInlineElements is 
      >>= (\children -> pure $ HTML.parent ("h" <> show n) children ! HA.id (encodeInlines is))
    (Blockquote bs) -> (pure <<< HTML.blockquote) =<< toElements bs
    (Lst (Bullet s) bss) -> sequence_ <$> traverse (\e -> (pure <<< HTML.ul) =<< toListElements e) bss
    (Lst (Ordered s) bss) -> sequence_ <$> traverse (\e -> (pure <<< HTML.ol) =<< toListElements e) bss
    (CodeBlock ct ss) -> pure $ HTML.pre $ HTML.code $ text $ intercalate "\n" ss
    -- (LinkReference l url) -> pure $ HTML.a ! HA.href url $ text l
    -- (LinkReference l url) -> do
    --   refs <- get
    --   put (Map.insert l url refs)
    --   pure mempty
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
    (LineBreak) -> pure $ text "\n"

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

