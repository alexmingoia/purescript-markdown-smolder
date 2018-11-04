module Test.Main where

import Text.Smolder.Renderer.String

import Data.Either (Either, either)
import Effect (Effect)
import Prelude (Unit, discard, identity, (>>>), ($))
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
      Assert.equal 
        "<h2 id=\"Share_AWS_API_Gateway_Resources\">Share AWS API Gateway Resources</h2>"
        (compileMd "## Share AWS API Gateway Resources")

    test "lists" do
      Assert.equal
        "<ul><li>Hello</li></ul>"
        (compileMd "* Hello")

      Assert.equal
        "<ul><li>Hello</li><li>World</li></ul>"
        (compileMd "* Hello\n* World")

      Assert.equal
        "<ol><li>Hello</li></ol>"
        (compileMd "1. Hello")

      Assert.equal
        "<ol><li>Hello</li><li>World</li></ol>"
        (compileMd "1. Hello\n2. World")

      Assert.equal
        "<ol><li>Hello<ul><li>Purescript</li><li>Haskell</li></ul></li><li>World</li></ol>"
        (compileMd "1. Hello\n    * Purescript\n    * Haskell\n2. World")

      Assert.equal
        "<ul><li>Hello<ol><li>Purescript</li><li>Haskell</li></ol></li><li>World</li></ul>"
        (compileMd "* Hello\n    1. Purescript\n    2. Haskell\n* World")

    test "styling" do
      Assert.equal
        "<p>PureScript is <strong>great</strong>!</p>"
        (compileMd "PureScript is **great**!")

      Assert.equal
        "<p>PureScript is <em>great</em>!</p>"
        (compileMd "PureScript is *great*!")

    test "paragraph" do 
      Assert.equal 
        "<p>Line 1</p><p>Line 2</p>"
        (compileMd $ "Line 1\n\nLine 2")
      Assert.equal
        "<p>Paragraph with a<br/>line break</p>"
        (compileMd "Paragraph with a  \n\
          \line break")

    test "codeblocks" do 
      Assert.equal
        "<pre class=\"code\" data-lang=\"python\"><code># python code<br/>    line 2</code></pre>"
        (compileMd $ "~~~~python \n\
        \# python code\n\
        \    line 2\n\
        \~~~~")
