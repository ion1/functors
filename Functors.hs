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
main =
  do
    render functorsNodes         "functors.png"
    render functionInstanceNodes "function-instance.png"

  where
    render nodeMap filename =
      either error (pure . const ())
        =<< runGraphviz (graphFor nodeMap) Png filename

graphFor :: NodeMapM Attributes Attributes Gr () -> DotGraph Node
graphFor = graphToDot params . run_ GI.empty
  where
    params = nonClusteredParams { globalAttributes = grAttrs
                                , fmtNode = snd
                                , fmtEdge = \(_, _, eAttrs) -> eAttrs
                                }
    grAttrs = [ GraphAttrs [ Ratio CompressRatio
                           , RankSep . pure $ 0.2
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
              , EdgeAttrs [ Color . pure $ col C.deepskyblue 0.5 ]
              ]

functorsNodes :: NodeMapM Attributes Attributes Gr ()
functorsNodes =
  do
    mapM_ insMapNodeM [ eqAtt, fmapConstAtt, discardAtt
                      , fmapAtt, apAtt, bindAtt ]

    insMapEdgeM (fmapConstAtt, eqAtt, noHeadAtt)
    insMapEdgeM (discardAtt,   eqAtt, noHeadAtt)

    insMapEdgeM (eqAtt, fmapAtt,    noHeadAtt)
    insMapEdgeM (eqAtt, apAtt,      noHeadAtt)
    insMapEdgeM (eqAtt, bindAtt,    noHeadAtt)

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

    noHeadAtt = [ ArrowHead noArrow, ArrowTail noArrow ]

functionInstanceNodes :: NodeMapM Attributes Attributes Gr ()
functionInstanceNodes =
  do
    mapM_ insMapNodeM [ lA2XAtt,  lA2GAtt,  lA2HAtt,   lA2FAtt,  lA2Att  ]
    mapM_ insMapNodeM [ bindXAtt, bindGAtt, bindIdAtt, bindFAtt, bindAtt ]
    mapM_ insMapNodeM [ apXAtt,   apIdAtt,  apHAtt,    apFAtt,   apAtt   ]
    mapM_ insMapNodeM [ fmapXAtt, fmapGAtt,            fmapFAtt, fmapAtt ]

    insMapEdgeM (fmapXAtt,  fmapGAtt,  arrowAtt)
    insMapEdgeM (fmapGAtt,  fmapFAtt,  arrowAtt)

    insMapEdgeM (apXAtt,  apIdAtt, arrowAtt)
    insMapEdgeM (apIdAtt, apFAtt,  arrowAtt)
    insMapEdgeM (apXAtt,  apHAtt,  arrowAtt)
    insMapEdgeM (apHAtt,  apFAtt,  arrowAtt)

    insMapEdgeM (bindXAtt,  bindGAtt,  arrowAtt)
    insMapEdgeM (bindGAtt,  bindFAtt,  arrowAtt)
    insMapEdgeM (bindXAtt,  bindIdAtt, arrowAtt)
    insMapEdgeM (bindIdAtt, bindFAtt,  arrowAtt)

    insMapEdgeM (lA2XAtt, lA2GAtt, arrowAtt)
    insMapEdgeM (lA2GAtt, lA2FAtt, arrowAtt)
    insMapEdgeM (lA2XAtt, lA2HAtt, arrowAtt)
    insMapEdgeM (lA2HAtt, lA2FAtt, arrowAtt)

    insMapEdgeM (fmapFAtt, fmapAtt, hiddenArrowAtt)
    insMapEdgeM (apFAtt,   apAtt,   hiddenArrowAtt)
    insMapEdgeM (bindFAtt, bindAtt, hiddenArrowAtt)
    insMapEdgeM (lA2FAtt,  lA2Att,  hiddenArrowAtt)

    pure ()

  where
    fmapFAtt  = funcAtt  "(<$>)" "f"
    fmapGAtt  = funcAtt  "(<$>)" "g"
    fmapXAtt  = valueAtt "(<$>)" "x"

    apFAtt  = funcAtt  "(<*>)" "f"
    apIdAtt = idAtt    "(<*>)"
    apHAtt  = funcAtt  "(<*>)" "h"
    apXAtt  = valueAtt "(<*>)" "x"

    bindFAtt  = funcAtt  "(=<<)" "f"
    bindGAtt  = funcAtt  "(=<<)" "g"
    bindIdAtt = idAtt    "(=<<)"
    bindXAtt  = valueAtt "(=<<)" "x"

    lA2FAtt = funcAtt  "lA2" "f"
    lA2GAtt = funcAtt  "lA2" "g"
    lA2HAtt = funcAtt  "lA2" "h"
    lA2XAtt = valueAtt "lA2" "x"

    fmapAtt =
      table [ [ codeTitle "(<$>) f g x = f (g x)" ]
            , [ codeType "(<$>) ∷ Functor f ⇒ (a → b) → f a → f b" ]
            , [ codeType "(<$>) ∷ (a → b) → (x → a) → x → b"
              , typeComment " (specialized)"
              ]
            ]

    apAtt =
      table [ [ codeTitle "(<*>) f h x = f x (h x)" ]
            , [ "Also known as the S combinator." ]
            , [ codeType "(<*>) ∷ Applicative f ⇒ f (a → b) → f a → f b" ]
            , [ codeType "(<*>) ∷ (x → a → b) → (x → a) → x → b"
              , typeComment " (specialized)"
              ]
            ]

    bindAtt =
      table [ [ codeTitle "(=<<) f g x = f (g x) x" ]
            , [ codeType "(=<<) ∷ Monad f ⇒ (a → f b) → f a → f b" ]
            , [ codeType "(=<<) ∷ (a → x → b) → (x → a) → x → b"
              , typeComment " (specialized)"
              ]
            ]

    lA2Att =
      table [ [ codeTitle "liftA2 f g h x ≡ f (g x) (h x)" ]
            , [ "The definition: ", code "liftA2 f g h = f <$> g <*> h" ]
            , [ codeType "liftA2 ∷ Applicative f ⇒ (a → b → c) → f a → f b → f c" ]
            , [ codeType "liftA2 ∷ (a → b → c) → (x → a) → (x → b) → x → c"
              , typeComment " (specialized)"
              ]
            ]

    funcAtt comment text =
      [ Label . HtmlLabel . HtmlText . pure $ codeTitle text
      , Comment comment
      , FillColor (col C.darkgreen 0.2)
      , Color . pure $ col C.black 1.0
      ]

    idAtt comment =
      [ Label . HtmlLabel . HtmlText . pure $ " "
      , FontSize 2
      , Comment comment
      , Shape Circle
      , FillColor (fromAColour C.transparent)
      , Color . pure $ col C.deepskyblue 0.5
      ]

    valueAtt comment text =
      [ Label . HtmlLabel . HtmlText . pure $ codeTitle text
      , Comment comment
      , FillColor (col C.blue 0.2)
      , Color . pure $ col C.black 1.0
      ]

    arrowAtt = [ ]
    hiddenArrowAtt = [ Style . pure $ (SItem Invisible []) ]

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

typeComment :: HtmlTextItem -> HtmlTextItem
typeComment = withFontAtt [ HtmlPointSize 8, HtmlColor (col C.black 0.5) ]

withFontAtt :: HtmlAttributes -> HtmlTextItem -> HtmlTextItem
withFontAtt hatt = HtmlFont hatt . pure

newline :: HtmlTextItem
newline = HtmlNewline []
