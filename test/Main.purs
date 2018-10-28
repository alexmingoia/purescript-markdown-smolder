module Test.Main where

import Text.Smolder.Renderer.String

import Data.Either (Either, either)
import Effect (Effect)
import Prelude (Unit, discard, identity, (>>>))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Smolder (toMarkup)
import Text.Markdown.SlamDown.Syntax (SlamDownP)


compileMd :: String -> String
compileMd input =
  either identity (toMarkup >>> render)
  (parseMd input :: Either String (SlamDownP String))

main :: Effect Unit
main = runTest do
  suite "markdown" do

    test "links" do
      Assert.equal
        "<p><a href=\"http://www.purescript.org/\">PureScript</a></p>"
        (compileMd "[PureScript](http://www.purescript.org/)")

      Assert.equal 
        "<p><a href=\"http://slashdot.org\">You can use numbers for reference-style link definitions</a></p>"
        (compileMd "[You can use numbers for reference-style link definitions][1]\n [1]: http://slashdot.org")

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
