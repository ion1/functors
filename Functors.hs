module Main where

import Control.Applicative
import qualified Data.Colour as C
import qualified Data.Colour.Names as C
import Data.Graph.Inductive as GI
import Data.GraphViz as GV

main :: IO ()
main  =  either error (pure . const ())
     =<< runGraphviz theGraph Png "functors.png"

theGraph :: DotGraph Node
theGraph = graphToDot params (run_ GI.empty theNodes)
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

theNodes :: NodeMapM Attributes Attributes Gr ()
theNodes =
  do
    mapM_ insMapNodeM [ eqAtt, fmapConstAtt, discardAtt
                      , fmapAtt, bindAtt, apAtt ]

    insMapEdgeM (fmapConstAtt, eqAtt, [])
    insMapEdgeM (discardAtt, eqAtt, [])

    insMapEdgeM (eqAtt, fmapAtt,    [])
    insMapEdgeM (eqAtt, bindAtt,    [])
    insMapEdgeM (eqAtt, apAtt,      [])

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
      fNode [ [ codeTitle "a <$ f" ]
            , [ HtmlStr "For each value drawn from ", code "f"
              , HtmlStr ", discard it"
              , newline
              , HtmlStr "and result in the pure value ", code "a", HtmlStr "."
              ]
            , [ codeType "(<$) ∷ Functor f ⇒ b → f a → f b" ]
            ]

    discardAtt =
      fNode [ [ codeTitle "pure a <* f" ]
            , [ HtmlStr "Sequentially run the actions "
              , code "pure a", HtmlStr " and"
              , newline
              , code "f", HtmlStr ", discarding the result value of "
              , code "f", HtmlStr "."
              ]
            , [ codeType "(<*) ∷ Applicative f ⇒ f b → f a → f b" ]
            ]

    fmapAtt =
      fNode [ [ codeTitle "const a <$> f" ]
            , [ HtmlStr "For each ", code "x"
              , HtmlStr " drawn from ", code "f", HtmlStr ", result in the"
              , newline
              , HtmlStr "pure value ", code "const a x"
              , HtmlStr ", or ", code "a", HtmlStr "."
              ]
            , [ codeType "(<$>) ∷ Functor f ⇒ (a → b) → f a → f b" ]
            ]

    bindAtt =
      fNode [ [ codeTitle "const (pure a) =<< f" ]
            , [ HtmlStr "For each ", code "x"
              , HtmlStr " drawn from ", code "f"
              , HtmlStr ", run the action"
              , newline
              , code "const (pure a) x"
              , HtmlStr ", or ", code "pure a", HtmlStr "."
              ]
            , [ code "=<<"
              , HtmlStr " lets you use the result value of "
              , code "f", HtmlStr " to"
              , newline
              , HtmlStr "determine the next action."
              ]
            , [ codeType "(=<<) ∷ Monad f ⇒ (a → f b) → f a → f b" ]
            ]

    apAtt =
      fNode [ [ codeTitle "pure (const a) <*> f" ]
            , [ HtmlStr "Sequentially apply the function(s) resulting"
              , newline
              , HtmlStr "from the action ", code "pure (const a)"
              , HtmlStr " to the"
              , newline
              , HtmlStr "result value(s) of the action ", code "f", HtmlStr "."
              ]
            , [ codeType "(<*>) ∷ Applicative f ⇒ f (a → b) → f a → f b" ]
            ]

    fNode :: [HtmlText] -> Attributes
    fNode texts = [ (Label . HtmlLabel) root ]
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

asTitle :: String -> HtmlTextItem
asTitle = withFontAtt [ HtmlPointSize 20 ]

codeTitle :: String -> HtmlTextItem
codeTitle = withFontAtt [ HtmlFace "Mono Bold", HtmlPointSize 16 ]

code :: String -> HtmlTextItem
code = withFontAtt [ HtmlFace "Mono Bold" ]

codeType :: String -> HtmlTextItem
codeType = withFontAtt [ HtmlPointSize 8, HtmlColor (col C.blue 1) ]

withFontAtt :: HtmlAttributes -> String -> HtmlTextItem
withFontAtt hatt = HtmlFont hatt . pure . HtmlStr

newline :: HtmlTextItem
newline = HtmlNewline []
