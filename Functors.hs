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
    render functorsNodes     "functors.png"
    render funcInstFmapNodes "function-instance-fmap.png"
    render funcInstApNodes   "function-instance-ap.png"
    render funcInstBindNodes "function-instance-bind.png"
    render funcInstLA2Nodes  "function-instance-lifta2.png"
    render funcInstJoinNodes "function-instance-join.png"
    render onNodes           "on.png"

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
                           , Concentrate True
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
            , Style . pure $ SItem Filled []
            , FillColor (fromAColour C.transparent)
            , Color . pure $ col C.deepskyblue 0.5
            ]

    fmapConstAtt = Ordering "out" :
      table [ [ codeTitle "a <$ f" ]
            , [ "Run the action ", code "f", ", replacing the result"
              , newline
              , "with the pure value ", code "a", "."
              ]
            , [ codeType "(<$) ∷ Functor f ⇒ b → f a → f b" ]
            ]

    discardAtt = Ordering "out" :
      table [ [ codeTitle "pure a <* f" ]
            , [ "Sequentially run the actions ", code "pure a", " and"
              , newline
              , code "f", ", discarding the result value of ", code "f", "."
              ]
            , [ codeType "(<*) ∷ Applicative f ⇒ f b → f a → f b" ]
            ]

    fmapAtt = Ordering "in" :
      table [ [ codeTitle "const a <$> f" ]
            , [ "Run the action ", code "f", ", replacing the result"
              , newline
              , "with the function ", code "const a", " applied to it."
              ]
            , [ codeType "(<$>) ∷ Functor f ⇒ (a → b) → f a → f b" ]
            ]

    apAtt = Ordering "in" :
      table [ [ codeTitle "pure (const a) <*> f" ]
            , [ "Sequentially run the actions ", code "pure (const a)"
              , newline
              , "and ", code "f", "."
              ]
            , [ "The result value is the function resulting from"
              , newline
              , code "pure (const a)", " applied to the value resulting"
              , newline
              , "from ", code "f", "."
              ]
            , [ codeType "(<*>) ∷ Applicative f ⇒ f (a → b) → f a → f b" ]
            ]

    bindAtt = Ordering "in" :
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

    noHeadAtt = [ Dir NoDir ]

funcInstFmapNodes :: NodeMapM Attributes Attributes Gr ()
funcInstFmapNodes =
  do
    mapM_ insMapNodeM [ xA, gA, fA, rA, descrA ]

    insMapEdgeM (xA, gA, [])
    insMapEdgeM (gA, fA, [])

    insMapEdgeM (fA, rA, [])
    insMapEdgeM (rA, descrA, hiddenArrowAtt)

    pure ()

  where
    rA = resultAtt "fmap"
    fA = funcAtt   "fmap" "f"
    gA = funcAtt   "fmap" "g"
    xA = valueAtt  "fmap" "x"

    descrA =
      table [ [ codeTitle "(f <$> g) x = f (g x)" ]
            , [ codeType "(<$>) ∷ Functor f ⇒ (a → b) → f a → f b" ]
            , [ codeType "(<$>) ∷ (a → b) → (x → a) → x → b"
              , typeComment " (specialized)"
              ]
            ]

funcInstApNodes :: NodeMapM Attributes Attributes Gr ()
funcInstApNodes =
  do
    mapM_ insMapNodeM [ xA, idA, hA, fA, rA, descrA ]

    insMapEdgeM (xA,  idA, [ Dir NoDir ])
    insMapEdgeM (idA, fA,  [])
    insMapEdgeM (xA,  hA,  [])
    insMapEdgeM (hA,  fA,  [])

    insMapEdgeM (fA, rA, [])
    insMapEdgeM (rA, descrA, hiddenArrowAtt)

    pure ()

  where
    rA  = resultAtt "ap"
    fA  = funcAtt   "ap" "f"
    idA = idAtt     "ap"
    hA  = funcAtt   "ap" "h"
    xA  = valueAtt  "ap" "x"

    descrA =
      table [ [ codeTitle "(f <*> h) x = f x (h x)" ]
            , [ "Also known as the S combinator." ]
            , [ codeType "(<*>) ∷ Applicative f ⇒ f (a → b) → f a → f b" ]
            , [ codeType "(<*>) ∷ (x → a → b) → (x → a) → x → b"
              , typeComment " (specialized)"
              ]
            ]

funcInstBindNodes :: NodeMapM Attributes Attributes Gr ()
funcInstBindNodes =
  do
    mapM_ insMapNodeM [ xA, gA, idA, fA, rA, descrA ]

    insMapEdgeM (xA,  gA,  [])
    insMapEdgeM (gA,  fA,  [])
    insMapEdgeM (xA,  idA, [ Dir NoDir ])
    insMapEdgeM (idA, fA,  [])

    insMapEdgeM (fA, rA, [])
    insMapEdgeM (rA, descrA, hiddenArrowAtt)

    pure ()

  where
    rA  = resultAtt "bind"
    fA  = funcAtt   "bind" "f"
    gA  = funcAtt   "bind" "g"
    idA = idAtt     "bind"
    xA  = valueAtt  "bind" "x"

    descrA =
      table [ [ codeTitle "(f =<< g) x = f (g x) x" ]
            , [ codeType "(=<<) ∷ Monad f ⇒ (a → f b) → f a → f b" ]
            , [ codeType "(=<<) ∷ (a → x → b) → (x → a) → x → b"
              , typeComment " (specialized)"
              ]
            ]

funcInstLA2Nodes :: NodeMapM Attributes Attributes Gr ()
funcInstLA2Nodes =
  do
    mapM_ insMapNodeM [ xA, gA, hA, fA, rA, descrA ]

    insMapEdgeM (xA, gA, [])
    insMapEdgeM (gA, fA, [])
    insMapEdgeM (xA, hA, [])
    insMapEdgeM (hA, fA, [])

    insMapEdgeM (fA, rA, [])
    insMapEdgeM (rA, descrA, hiddenArrowAtt)

    pure ()

  where
    rA = resultAtt "liftA2"
    fA = funcAtt   "liftA2" "f"
    gA = funcAtt   "liftA2" "g"
    hA = funcAtt   "liftA2" "h"
    xA = valueAtt  "liftA2" "x"

    descrA =
      table [ [ codeTitle "liftA2 f g h x ≡ f (g x) (h x)" ]
            , [ "The definition: ", code "liftA2 f g h = f <$> g <*> h" ]
            , [ codeType "liftA2 ∷ Applicative f ⇒ (a → b → c) → f a → f b → f c" ]
            , [ codeType "liftA2 ∷ (a → b → c) → (x → a) → (x → b) → x → c"
              , typeComment " (specialized)"
              ]
            ]

funcInstJoinNodes :: NodeMapM Attributes Attributes Gr ()
funcInstJoinNodes =
  do
    mapM_ insMapNodeM [ xA, id0A, id1A, fA, rA, descrA ]

    insMapEdgeM (xA, id0A, [ Dir NoDir ])
    insMapEdgeM (xA, id1A, [ Dir NoDir ])
    insMapEdgeM (id0A, fA, [])
    insMapEdgeM (id1A, fA, [])

    insMapEdgeM (fA, rA, [])
    insMapEdgeM (rA, descrA, hiddenArrowAtt)

    pure ()

  where
    rA   = resultAtt "join"
    fA   = funcAtt   "join"   "f"
    id0A = idAtt     "join/0"
    id1A = idAtt     "join/1"
    xA   = valueAtt  "join"   "x"

    descrA =
      table [ [ codeTitle "join f x ≡ f x x" ]
            , [ codeType "join ∷ Monad f ⇒ f (f a) → f a" ]
            , [ codeType "join ∷ (x → x → a) → x → a"
              , typeComment " (specialized)"
              ]
            ]

onNodes :: NodeMapM Attributes Attributes Gr ()
onNodes =
  do
    mapM_ insMapNodeM [ xA, yA, gxA, gyA, fA, rA, descrA ]

    insMapEdgeM (xA,  gxA, [])
    insMapEdgeM (yA,  gyA, [])
    insMapEdgeM (gxA, fA,  [])
    insMapEdgeM (gyA, fA,  [])

    insMapEdgeM (gxA, gyA, eqArrowAtt)

    insMapEdgeM (fA, rA, [])
    insMapEdgeM (rA, descrA, hiddenArrowAtt)

    pure ()

  where
    rA  = resultAtt "on"
    fA  = funcAtt   "on"    "f"
    gxA = funcAtt   "on/gx" "g"
    gyA = funcAtt   "on/gy" "g"
    xA  = valueAtt  "on"    "x"
    yA  = valueAtt  "on"    "y"

    descrA =
      table [ [ codeTitle "(f `on` g) x y = f (g x) (g y)" ]
            , [ codeType "on ∷ (b → b → c) → (a → b) → a → a → c" ]
            ]

    eqArrowAtt = [ Constraint False
                 , Dir NoDir
                 , Style . pure $ SItem Dotted []
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
codeTitle = withFontAtt [ HtmlFace "Sans Bold", HtmlPointSize 15 ]

code :: HtmlTextItem -> HtmlTextItem
code = withFontAtt [ HtmlFace "Sans Bold" ]

codeType :: HtmlTextItem -> HtmlTextItem
codeType = withFontAtt [ HtmlPointSize 8, HtmlColor (col C.darkgreen 1) ]

typeComment :: HtmlTextItem -> HtmlTextItem
typeComment = withFontAtt [ HtmlPointSize 8, HtmlColor (col C.black 0.3) ]

withFontAtt :: HtmlAttributes -> HtmlTextItem -> HtmlTextItem
withFontAtt hatt = HtmlFont hatt . pure

newline :: HtmlTextItem
newline = HtmlNewline []

resultAtt :: String -> Attributes
resultAtt comment =
  [ Label . HtmlLabel . HtmlText . pure $ " "
  , Comment comment
  , FixedSize True, Width 0.1, Height 0.1
  , Style . pure $ SItem Invisible []
  , Ordering "in"
  ]

funcAtt :: String -> HtmlTextItem -> Attributes
funcAtt comment text =
  [ Label . HtmlLabel . HtmlText . pure $ codeTitle text
  , Comment comment
  , FillColor (col C.darkgreen 0.2)
  , Color . pure $ col C.black 1.0
  , Ordering "in"
  ]

idAtt :: String -> Attributes
idAtt comment =
  [ Label . HtmlLabel . HtmlText . pure $ " "
  , Comment comment
  , Shape PointShape
  , FillColor (fromAColour C.transparent)
  , Color . pure $ col C.deepskyblue 0.5
  , Ordering "in"
  ]

valueAtt :: String -> HtmlTextItem -> Attributes
valueAtt comment text =
  [ Label . HtmlLabel . HtmlText . pure $ codeTitle text
  , Comment comment
  , FillColor (col C.blue 0.2)
  , Color . pure $ col C.black 1.0
  , Ordering "in"
  ]

hiddenArrowAtt :: Attributes
hiddenArrowAtt = [ Style . pure $ SItem Invisible [] ]
