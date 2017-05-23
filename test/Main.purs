module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Either (Either, either)
import Prelude (Unit, bind, discard, id, (>>>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder (toMarkup)
import Text.Markdown.SlamDown.Syntax (SlamDownP)
import Text.Smolder.Renderer.String

type TestEffects fx =
  (console :: CONSOLE , testOutput :: TESTOUTPUT , avar :: AVAR | fx)

compileMd :: String -> String
compileMd input =
  either id (toMarkup >>> render)
  (parseMd input :: Either String (SlamDownP String))

main :: forall fx. Eff (TestEffects fx) Unit
main = runTest do
  suite "markdown" do

    test "links" do
      Assert.equal
        "<p><a href=\"http:&#x2F;&#x2F;www.purescript.org&#x2F;\">PureScript</a></p>"
        (compileMd "[PureScript](http://www.purescript.org/)")

    test "headings" do
      Assert.equal
        "<h1 id=\"Hello\">Hello</h1>"
        (compileMd "# Hello")

    test "lists" do
      Assert.equal
        "<ul><li>Hello</li></ul>"
        (compileMd "* Hello")

      Assert.equal
        "<ol><li>Hello</li></ol>"
        (compileMd "1. Hello")

    test "styling" do
      Assert.equal
        "<p>PureScript is <strong>great</strong>!</p>"
        (compileMd "PureScript is **great**!")

      Assert.equal
        "<p>PureScript is <em>great</em>!</p>"
        (compileMd "PureScript is *great*!")
