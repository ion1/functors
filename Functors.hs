{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Data.Graph.Inductive as GI
import Data.GraphViz as GV
import Data.String

instance IsString HtmlTextItem where
  fromString = HtmlStr . fromString

main :: IO ()
main  =  either error (pure . const ())
     =<< runGraphviz (graphFor functorsNodes) Png "functors.png"

graphFor :: NodeMapM Attributes Attributes Gr () -> DotGraph Node
graphFor = graphToDot params . run_ GI.empty
  where
    params = nonClusteredParams { globalAttributes = grAttrs
                                , fmtNode = snd
                                , fmtEdge = \(_, _, eAttrs) -> eAttrs
                                }
    grAttrs = [ GraphAttrs [ Ratio CompressRatio
                           , RankSep . pure $ 0
                           , RankDir FromLeft
                           ]
              , NodeAttrs [ FontName "Sans"
                          , FontSize 12
                          , Shape BoxShape
                          , Style . pure $ SItem Filled []
                          , FillColor (col C.deepskyblue 0.2)
                          , Width 0
                          , Height 0
                          ]
              , EdgeAttrs [ ArrowHead noArrow
                          , ArrowTail noArrow
                          , Color . pure $ col C.deepskyblue 0.5
                          ]
              ]

functorsNodes :: NodeMapM Attributes Attributes Gr ()
functorsNodes =
  do
    mapM_ insMapNodeM [ eqAtt, fmapConstAtt, discardAtt
                      , fmapAtt, apAtt, bindAtt ]

    insMapEdgeM (fmapConstAtt, eqAtt, [])
    insMapEdgeM (discardAtt, eqAtt, [])

    insMapEdgeM (eqAtt, fmapAtt,    [])
    insMapEdgeM (eqAtt, apAtt,      [])
    insMapEdgeM (eqAtt, bindAtt,    [])

    pure ()

  where
    eqAtt = [ FontSize 16
            , Label . HtmlLabel . HtmlText . pure . asTitle $ "≡"
            , Shape Circle
            , Style . pure $ (SItem Filled [])
            , FillColor (fromAColour C.transparent)
            , Color . pure $ col C.deepskyblue 0.5
            ]

    fmapConstAtt =
      table [ [ codeTitle "a <$ f" ]
            , [ "Run the action ", code "f", ", replacing the result"
              , newline
              , "with the pure value ", code "a", "."
              ]
            , [ codeType "(<$) ∷ Functor f ⇒ b → f a → f b" ]
            ]

    discardAtt =
      table [ [ codeTitle "pure a <* f" ]
            , [ "Sequentially run the actions ", code "pure a", " and"
              , newline
              , code "f", ", discarding the result value of ", code "f", "."
              ]
            , [ codeType "(<*) ∷ Applicative f ⇒ f b → f a → f b" ]
            ]

    fmapAtt =
      table [ [ codeTitle "const a <$> f" ]
            , [ "Run the action ", code "f", ", replacing the result"
              , newline
              , "with the function ", code "const a", " applied to it."
              ]
            , [ codeType "(<$>) ∷ Functor f ⇒ (a → b) → f a → f b" ]
            ]

    apAtt =
      table [ [ codeTitle "pure (const a) <*> f" ]
            , [ "Sequentially run the actions ", code "pure (const a)"
              , newline
              , "and ", code "f", "."
              ]
            , [ "The result value is the function resulting from"
              , newline
              , code "pure (const a)", " applied to the value"
              , newline
              , "resulting from ", code "f", "."
              ]
            , [ codeType "(<*>) ∷ Applicative f ⇒ f (a → b) → f a → f b" ]
            ]

    bindAtt =
      table [ [ codeTitle "const (pure a) =<< f" ]
            , [ "Run the action resulting from applying"
              , newline
              , code "const (pure a)", " to the result value of the"
              , newline
              , "action ", code "f", "."
              ]
            , [ code "=<<", " lets you use the result value of "
              , code "f", " to"
              , newline
              , "determine the next action."
              ]
            , [ codeType "(=<<) ∷ Monad f ⇒ (a → f b) → f a → f b" ]
            ]

table :: [HtmlText] -> Attributes
table texts = [ (Label . HtmlLabel) root ]
  where
    root :: HtmlLabel
    root = HtmlTable (HTable Nothing tableAtt rows)

    rows :: [HtmlRow]
    rows = map row texts

    row :: HtmlText -> HtmlRow
    row cnt = HtmlRow . pure . HtmlLabelCell cellAtt . HtmlText $ cnt

    tableAtt = [ HtmlBorder 0, HtmlCellBorder 0 ]
    cellAtt  = [ HtmlAlign HLeft, HtmlBAlign HLeft ]

col :: C.Colour Double -> Double -> Color
col c opa = fromAColour $ c `C.withOpacity` opa

asTitle :: HtmlTextItem -> HtmlTextItem
asTitle = withFontAtt [ HtmlPointSize 20 ]

codeTitle :: HtmlTextItem -> HtmlTextItem
codeTitle = withFontAtt [ HtmlFace "Mono Bold", HtmlPointSize 16 ]

code :: HtmlTextItem -> HtmlTextItem
code = withFontAtt [ HtmlFace "Mono Bold" ]

codeType :: HtmlTextItem -> HtmlTextItem
codeType = withFontAtt [ HtmlPointSize 8, HtmlColor (col C.blue 1) ]

withFontAtt :: HtmlAttributes -> HtmlTextItem -> HtmlTextItem
withFontAtt hatt = HtmlFont hatt . pure

newline :: HtmlTextItem
newline = HtmlNewline []
