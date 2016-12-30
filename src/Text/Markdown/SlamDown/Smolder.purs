module Text.Markdown.SlamDown.Smolder (toMarkup) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (get, put)
import Data.CatList (CatList, cons, empty)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Foldable (foldl, intercalate)
import Data.Function (($))
import Data.Functor (map)
import Data.List (List, head, tail)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.StrMap (StrMap, empty, insert, lookup) as SM
import Data.String (toLower)
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Data.Tuple (fst, snd)
import Data.Unit (unit)
import Prelude ((||))
import Text.Markdown.SlamDown (Block(..), Inline(..), LinkTarget(..), ListType(..), SlamDownP(..))
import Text.Smolder.HTML (a, blockquote, code, em, hr, img, li, ol, p, pre, strong, ul)
import Text.Smolder.HTML.Attributes (alt, href, src)
import Text.Smolder.Markup (Attr(..), Markup, MarkupM(..), text, (!))

type ReferenceLinks = SM.StrMap String

type MarkupState e = State ReferenceLinks (Markup e)

toMarkup :: forall a e. SlamDownP a -> Markup e
toMarkup (SlamDown bs) = replaceLinkRefs (snd markupState) (fst markupState)
  where
    markupState = runState (toElements bs) SM.empty

replaceLinkRefs :: forall e. ReferenceLinks -> Markup e -> Markup e
replaceLinkRefs refs (Element n c a e r) =
  if n == "a" then
    let
      ref = getLinkRef a
    in
      Element n c a e (replaceLinkRefs refs r) ! href (fromMaybe ref (SM.lookup ref refs))
    else
      Element n (map (replaceLinkRefs refs) c) a e (replaceLinkRefs refs r)
replaceLinkRefs refs (Content s r) = Content s (replaceLinkRefs refs r)
replaceLinkRefs refs markup = markup

getLinkRef :: CatList Attr -> String
getLinkRef attrs = foldl foldAttr "" attrs
  where
    foldAttr url (Attr key val) =
      if (key == "href") then val else ""

toElements :: forall a e. List (Block a) -> MarkupState e
toElements bs = foldl f (pure mempty) bs
  where
    f m bl = do
      markup <- m
      block <- toElement bl
      pure $ bind markup (\_ -> block)

toListElements :: forall a e. List (Block a) -> MarkupState e
toListElements bs = do
  let foldBlocks = foldl f (pure mempty)
      f m bl = do
        markup <- m
        block <- toElement bl
        pure $ bind markup $ \_ -> block

  -- if the head element is a paragraph block, and the list only contains a single
  -- paragraph block or a single paragraph block and another list block,
  -- unpack the paragraph block.
  case (head bs) of
    Nothing -> foldBlocks bs
    Just bl -> do
      el <- toElement bl
      case (isParagraph el) of
        false -> foldBlocks bs
        true -> case (tail bs) of
          Nothing -> foldBlocks bs
          Just tbs -> case (getChild el) of
            Nothing -> foldBlocks bs
            Just child -> do
              markup <- foldBlocks tbs
              pure $ bind child $ \_ -> markup


isEmpty :: forall e. Markup e -> Boolean
isEmpty (Return _) = true
isEmpty _ = false

isList :: forall e. Markup e -> Boolean
isList (Element n _ _ _ _) = (n == "ul" || n == "ol")
isList _ = false

isParagraph :: forall e. Markup e -> Boolean
isParagraph (Element n _ _ _ _) = n == "p"
isParagraph _ = false

getChild :: forall e. Markup e -> Maybe (Markup e)
getChild (Element n c a e r) = c
getChild _ = Nothing

toElement :: forall a e. Block a -> MarkupState e
toElement block =
  case block of
    (Paragraph is) -> do
      children <- toInlineElements is
      pure $ p children
    (Header n is) -> do
      children <- toInlineElements is
      let regx = regex "\\W" global
      case regx of
        Left _ ->
          pure $ Element ("h" <> show n) (Just children) (empty) empty (Return unit)
        Right rx -> do
          let id = toLower $ replace rx "" $ textFromElement "" children
              textFromElement txt (Element _ c _ _ r) =
                maybe "" (textFromElement txt) c <> txt <> textFromElement txt r
              textFromElement txt (Content s r) = txt <> s <> textFromElement txt r
              textFromElement txt (Return _) = txt
          pure $ Element ("h" <> show n) (Just children) (cons (Attr "id" id) empty) empty (Return unit)
    (Blockquote bs) -> do
      children <- toElements bs
      pure $ blockquote children
    (Lst (Bullet s) bss) -> do
      let f m bs = do
            markup <- m
            bl <- toListElements bs
            pure $ bind markup (\_ -> li bl)
      children <- foldl f (pure mempty) bss
      pure $ ul children
    (Lst (Ordered s) bss) -> do
      let f m bs = do
            markup <- m
            bl <- toListElements bs
            pure $ bind markup (\_ -> li bl)
      children <- foldl f (pure mempty) bss
      pure $ ol children
    (CodeBlock ct ss) -> pure $ pre $ code $ text $ intercalate "\n" ss
    (LinkReference l url) -> do
      refs <- get
      put (SM.insert l url refs)
      pure mempty
    (Rule) -> pure hr

toInlineElements :: forall a e. List (Inline a) -> MarkupState e
toInlineElements is = foldl f (pure mempty) is
  where
    f m il = do
      markup <- m
      inline <- toInlineElement il
      pure $ bind markup (\_ -> inline)

toInlineElement :: forall a e. Inline a -> MarkupState e
toInlineElement il =
  case il of
    (Str s) -> pure $ text s
    (Entity s) -> pure $ text s
    (Space) -> pure $ text " "
    (SoftBreak) -> pure $ text "\n"
    (LineBreak) -> pure $ text "\n"
    (Emph is) -> do
      children <- toInlineElements is
      pure $ em children
    (Strong is) -> do
      children <- toInlineElements is
      pure $ strong children
    (Code e s) -> pure $ code $ text s
    (Link is (InlineLink url)) -> do
      children <- toInlineElements is
      pure $ a ! href url $ children
    (Link is (ReferenceLink ref)) -> do
      children <- toInlineElements is
      pure $ a ! href (fromMaybe "" ref) $ children
    (Image is url) -> pure $ img ! src url ! alt (toInlines is)
    (FormField l r e) -> pure mempty

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

