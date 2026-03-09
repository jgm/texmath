{-# LANGUAGE OverloadedStrings #-}
module Text.TeXMath.Writers.StarMath
  ( writeStarMath
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import Text.TeXMath.Types
  ( Alignment(..)
  , DisplayType
  , Exp(..)
  , FractionType(..)
  , TeXSymbolType(..)
  , TextType(..)
  )
import Text.TeXMath.Writers.TeX (writeTeX)

-- | Render TeXMath expressions as StarMath syntax.
-- Falls back to TeX output for expressions that are not yet supported.
writeStarMath :: DisplayType -> [Exp] -> T.Text
writeStarMath _dt exps =
  case renderExps (normalizeExps exps) of
    Just rendered -> T.strip rendered
    Nothing       -> writeTeX exps

data AlignContext = AlignDefault | AlignLeftCtx | AlignRightCtx
  deriving (Eq)

renderExps :: [Exp] -> Maybe T.Text
renderExps = renderExpsIn AlignDefault

normalizeExps :: [Exp] -> [Exp]
normalizeExps = normalizeBareBars . map normalizeExp

normalizeExp :: Exp -> Exp
normalizeExp e =
  case e of
    EGrouped xs            -> EGrouped (normalizeExps xs)
    EStyled sty xs         -> EStyled sty (normalizeExps xs)
    EFraction ft num den   -> EFraction ft (normalizeExp num) (normalizeExp den)
    ESqrt x                -> ESqrt (normalizeExp x)
    ERoot idx rad          -> ERoot (normalizeExp idx) (normalizeExp rad)
    EDelimited op cl xs    -> EDelimited op cl (map normalizeDelimitedPiece xs)
    ESub base sub          -> ESub (normalizeExp base) (normalizeExp sub)
    ESuper base sup        -> ESuper (normalizeExp base) (normalizeExp sup)
    ESubsup base sub sup   -> ESubsup (normalizeExp base) (normalizeExp sub) (normalizeExp sup)
    EOver b base over      -> EOver b (normalizeExp base) (normalizeExp over)
    EUnder b base under    -> EUnder b (normalizeExp base) (normalizeExp under)
    EUnderover b base u o  -> EUnderover b (normalizeExp base) (normalizeExp u) (normalizeExp o)
    EArray aligns rows     -> EArray aligns (map (map normalizeExps) rows)
    EPhantom x             -> EPhantom (normalizeExp x)
    _                      -> e

normalizeDelimitedPiece :: Either T.Text Exp -> Either T.Text Exp
normalizeDelimitedPiece p =
  case p of
    Left t  -> Left t
    Right e -> Right (normalizeExp e)

normalizeBareBars :: [Exp] -> [Exp]
normalizeBareBars [] = []
normalizeBareBars (x : xs)
  | Just sym <- bareBarSymbol x =
      case break (matchesBareBar sym) xs of
        (mid, y : rest) | matchesBareBar sym y ->
          EDelimited sym sym (map Right mid) : normalizeBareBars rest
        _ ->
          x : normalizeBareBars xs
normalizeBareBars (x:xs) = x : normalizeBareBars xs

bareBarSymbol :: Exp -> Maybe T.Text
bareBarSymbol e =
  case e of
    ESymbol _ "|" -> Just "|"
    ESymbol _ "∣" -> Just "|"
    ESymbol _ "∥" -> Just "∥"
    _             -> Nothing

matchesBareBar :: T.Text -> Exp -> Bool
matchesBareBar sym e =
  case bareBarSymbol e of
    Just sym' -> sym == sym'
    Nothing   -> False

renderExpsIn :: AlignContext -> [Exp] -> Maybe T.Text
renderExpsIn ctx exps = do
  rendered <- mapM (renderExpIn ctx) exps
  let pieces = zip exps rendered
  let merged = mergePieces pieces
  let withLhs =
        if startsWithInfixNeedingLhs exps
           then "{} " <> T.stripStart merged
           else merged
  pure $ if endsWithInfixNeedingRhs exps
            then T.stripEnd withLhs <> " {}"
            else withLhs

mergePieces :: [(Exp, T.Text)] -> T.Text
mergePieces [] = ""
mergePieces ((e0, t0) : rest) = snd $ List.foldl' step (e0, t0) rest
 where
  step (prevE, acc) (curE, curT) =
    if T.null curT
       then (prevE, acc)
       else
         let sep = if needsSeparator prevE curE then " " else ""
         in (curE, acc <> sep <> curT)

needsSeparator :: Exp -> Exp -> Bool
needsSeparator prevE curE
  | isGreekIdentifierExp prevE && isIdentifierLike curE = True
  | isGreekIdentifierExp prevE && isTerminatingPunctuation curE = True
  | isWordSymbolLike prevE && isIdentifierLike curE      = True
  | isWordStyledExp prevE && isIdentifierLike curE       = True
  | isMathOperatorExp prevE && isIdentifierLike curE     = True
  | isUnaryMinusSymbol prevE && isIdentifierLike curE    = True
  | isIdentifierLike prevE && isMathOperatorExp curE     = True
  | isScripted prevE && not (isLargeOpScripted prevE) &&
      isIdentifierLike curE                             = True
  | isCloseLike prevE && isIdentifierLike curE          = True
  | isIdentifierLike prevE && isWordSymbolLike curE     = True
  | isIdentifierLike prevE && isWordStyledExp curE      = True
  | isIdentifierLike prevE && isWideSpace curE          = True
  | isIdentifierLike prevE && isDelimited curE          = True
  | otherwise                                            = False

isGreekIdentifierExp :: Exp -> Bool
isGreekIdentifierExp e =
  case e of
    EIdentifier t -> greekName t /= Nothing
    _             -> False

isIdentifierLike :: Exp -> Bool
isIdentifierLike e =
  case e of
    EIdentifier{}   -> True
    ENumber{}       -> True
    EMathOperator{} -> True
    ESub{}          -> True
    ESuper{}        -> True
    ESubsup{}       -> True
    EStyled{}       -> True
    _               -> False

isMathOperatorExp :: Exp -> Bool
isMathOperatorExp e =
  case e of
    EMathOperator{} -> True
    _               -> False

isDelimited :: Exp -> Bool
isDelimited e =
  case e of
    EDelimited{} -> True
    _            -> False

isWideSpace :: Exp -> Bool
isWideSpace e =
  case e of
    ESpace w -> w >= 1
    _        -> False

isWordSymbolLike :: Exp -> Bool
isWordSymbolLike e =
  case e of
    ESymbol _ "∀" -> True
    ESymbol _ "∃" -> True
    ESymbol _ "∇" -> True
    _             -> False

isWordStyledExp :: Exp -> Bool
isWordStyledExp e =
  case e of
    EStyled TextNormal [x]     -> isWordStyledExp x
    EStyled TextItalic _       -> True
    EStyled TextBold _         -> True
    EStyled TextScript _       -> True
    EStyled TextFraktur _      -> True
    EStyled TextDoubleStruck _ -> True
    _                          -> False

isTerminatingPunctuation :: Exp -> Bool
isTerminatingPunctuation e =
  case e of
    ESymbol _ "." -> True
    ESymbol _ "," -> True
    ESymbol _ ";" -> True
    ESymbol _ ":" -> True
    _             -> False

isCloseLike :: Exp -> Bool
isCloseLike e =
  case e of
    ESymbol Close _ -> True
    EDelimited{}    -> True
    _               -> False

isScripted :: Exp -> Bool
isScripted e =
  case e of
    ESub{}    -> True
    ESuper{}  -> True
    ESubsup{} -> True
    _         -> False

isUnaryMinusSymbol :: Exp -> Bool
isUnaryMinusSymbol e =
  case e of
    ESymbol t "-" -> t /= Bin
    ESymbol t "−" -> t /= Bin
    _             -> False

isLargeOpScripted :: Exp -> Bool
isLargeOpScripted e =
  case e of
    ESub base _      -> largeOpName base /= Nothing
    ESuper base _    -> largeOpName base /= Nothing
    ESubsup base _ _ -> largeOpName base /= Nothing
    _                -> False

startsWithInfixNeedingLhs :: [Exp] -> Bool
startsWithInfixNeedingLhs exps =
  case exps of
    (e : _) -> needsNeutralLhs e
    _                 -> False

endsWithInfixNeedingRhs :: [Exp] -> Bool
endsWithInfixNeedingRhs exps =
  case reverse exps of
    (e : _) -> needsNeutralRhs e
    _       -> False

needsNeutralLhs :: Exp -> Bool
needsNeutralLhs = isInfixLikeExp

needsNeutralRhs :: Exp -> Bool
needsNeutralRhs = isInfixLikeExp

isInfixLikeExp :: Exp -> Bool
isInfixLikeExp e =
  case e of
    ESymbol t s
      | t == Bin -> True
      | t == Rel -> True
      | otherwise -> s `elem`
          [ "×", "⋅", "·", "∘"
          , "∈", "∉", "∋"
          , "∩", "∪"
          , "⊂", "⊆", "⊃", "⊇"
          , "≤", "≥", "≠", "≈", "≡", "∝"
          , "∥", "⊥"
          , "±", "∓"
          , "/", "←", "→", "↔", "⇐", "⇒", "⇔", "↦"
          ]
    _ -> False

renderExpIn :: AlignContext -> Exp -> Maybe T.Text
renderExpIn ctx e =
  case e of
    ENumber t       -> Just t
    EIdentifier t   -> Just (renderIdentifier t)
    EMathOperator t -> Just (renderMathOperator t)
    ESymbol t s     -> Just (renderSymbol t s)
    EText _ t       -> Just (quoteText t)
    ESpace w        -> Just (renderSpace w)
    EGrouped xs     -> ("{" <>) . (<> "}") <$> renderExpsIn ctx xs
    EStyled sty xs  -> renderStyled ctx sty xs

    EFraction frac num den -> do
      num' <- renderExpIn AlignDefault num
      den' <- renderExpIn AlignDefault den
      let num'' = maybeCenterFractionArg ctx num'
      let den'' = maybeCenterFractionArg ctx den'
      pure $ case frac of
        NoLineFrac -> "{" <> num'' <> " / " <> den'' <> "}"
        _          -> "{" <> num'' <> " over " <> den'' <> "}"

    ESqrt x -> ("sqrt {" <>) . (<> "}") <$> renderExpIn ctx x
    ERoot idx rad -> do
      idx' <- renderExpIn ctx idx
      rad' <- renderExpIn ctx rad
      pure $ "nroot {" <> idx' <> "} {" <> rad' <> "}"

    EDelimited op cl xs -> do
      body <- renderDelimitedBody ctx xs
      let op' = delimToken DelimLeft op
      let cl' = delimToken DelimRight cl
      pure $ "left " <> op' <> " " <> body <> " right " <> cl'

    ESub base sub -> do
      case limitOpName base of
        Just op -> do
          sub' <- renderLimitArg ctx sub
          pure $ op <> " from " <> sub' <> " "
        Nothing -> do
          base' <- renderExpIn ctx base
          sub'  <- renderScriptArg ctx sub
          pure $ renderScriptBase base base' <> "_" <> sub'

    ESuper base sup -> do
      case limitOpName base of
        Just op -> do
          sup' <- renderLimitArg ctx sup
          pure $ op <> " to " <> sup' <> " "
        Nothing -> do
          base' <- renderExpIn ctx base
          sup'  <- renderScriptArg ctx sup
          pure $ renderScriptBase base base' <> "^" <> sup'

    ESubsup base sub sup -> do
      case limitOpName base of
        Just op -> do
          sub' <- renderLimitArg ctx sub
          sup' <- renderLimitArg ctx sup
          pure $ op <> " from " <> sub' <> " to " <> sup' <> " "
        Nothing -> do
          base' <- renderExpIn ctx base
          sub'  <- renderScriptArg ctx sub
          sup'  <- renderScriptArg ctx sup
          pure $ renderScriptBase base base' <> "_" <> sub' <> "^" <> sup'

    EOver _ base over
      | Just accent <- accentName over -> do
          base' <- renderExpIn ctx base
          pure $ accent <> " " <> renderAccentArg base base'
      | otherwise -> Nothing

    EUnder _ base under ->
      case centeredScriptOpName base of
        Just op -> do
          under' <- renderScriptArg ctx under
          pure $ "{" <> op <> "} csub " <> centerScriptArg under'
        Nothing ->
          case limitOpName base of
            Just op -> do
              under' <- renderLimitArg ctx under
              pure $ op <> " from " <> under' <> " "
            Nothing -> do
              base' <- renderExpIn ctx base
              under' <- renderScriptArg ctx under
              pure $ renderScriptBase base base' <> "_" <> under'
    EUnderover _ base under over ->
      case centeredScriptOpName base of
        Just op -> do
          under' <- renderScriptArg ctx under
          over' <- renderScriptArg ctx over
          pure $ "{" <> op <> "} csub " <> centerScriptArg under'
              <> " csup " <> centerScriptArg over'
        Nothing ->
          case limitOpName base of
            Just op -> do
              under' <- renderLimitArg ctx under
              over'  <- renderLimitArg ctx over
              pure $ op <> " from " <> under' <> " to " <> over' <> " "
            Nothing -> do
              base' <- renderExpIn ctx base
              under' <- renderScriptArg ctx under
              over' <- renderScriptArg ctx over
              pure $ renderScriptBase base base' <> "_" <> under' <> "^" <> over'
    EArray aligns rows -> renderMatrix aligns rows
    EPhantom{}         -> Nothing
    _                  -> Nothing

renderDelimitedBody :: AlignContext -> [Either T.Text Exp] -> Maybe T.Text
renderDelimitedBody ctx xs = do
  chunks <- mapM (renderDelimitedChunk ctx) xs
  pure $ T.strip (mergeDelimitedChunks chunks)

data DelimitedChunk = DelimRaw T.Text | DelimExp Exp T.Text

renderDelimitedChunk :: AlignContext -> Either T.Text Exp -> Maybe DelimitedChunk
renderDelimitedChunk ctx p =
  case p of
    Left t  -> Just $ DelimRaw (" " <> delimToken DelimMiddle t <> " ")
    Right x -> DelimExp x <$> renderExpIn ctx x

mergeDelimitedChunks :: [DelimitedChunk] -> T.Text
mergeDelimitedChunks [] = ""
mergeDelimitedChunks (c0:cs) = snd $ List.foldl' step (chunkExp c0, chunkText c0) cs
 where
  step (prevExp, acc) cur
    | T.null curText = (prevExp, acc)
    | otherwise =
        case cur of
          DelimRaw _ -> (Nothing, acc <> curText)
          DelimExp curExp _ ->
            let sep = case prevExp of
                        Just pe -> if needsSeparator pe curExp then " " else ""
                        Nothing -> ""
            in (Just curExp, acc <> sep <> curText)
   where
    curText = chunkText cur

  chunkText c =
    case c of
      DelimRaw t   -> t
      DelimExp _ t -> t

  chunkExp c =
    case c of
      DelimRaw _   -> Nothing
      DelimExp e _ -> Just e

renderMatrix :: [Alignment] -> [[[Exp]]] -> Maybe T.Text
renderMatrix aligns rows = do
  rows' <- mapM (renderMatrixRow aligns) rows
  pure $ "matrix { " <> T.intercalate " ## " rows' <> " }"

renderMatrixRow :: [Alignment] -> [[Exp]] -> Maybe T.Text
renderMatrixRow aligns cells = do
  cells' <- sequence
    [ renderMatrixCellWithAlign (columnAlign aligns i) c
    | (i, c) <- zip [(0 :: Int) ..] cells
    ]
  pure $ T.intercalate " # " cells'

renderMatrixCell :: AlignContext -> [Exp] -> Maybe T.Text
renderMatrixCell _ [] = Just "{}"
renderMatrixCell ctx xs = do
  rendered <- renderExpsIn ctx xs
  let stripped = T.strip rendered
  pure $ if T.null stripped then "{}" else stripped

renderMatrixCellWithAlign :: Alignment -> [Exp] -> Maybe T.Text
renderMatrixCellWithAlign align xs = do
  cell <- renderMatrixCell (alignmentContext align) xs
  pure $ case align of
    AlignLeft  -> "alignl " <> cell
    AlignRight -> "alignr " <> cell
    _          -> cell

columnAlign :: [Alignment] -> Int -> Alignment
columnAlign aligns i =
  case drop i aligns of
    (a : _) -> a
    []      -> AlignCenter

renderStyled :: AlignContext -> TextType -> [Exp] -> Maybe T.Text
renderStyled ctx sty xs = do
  body <- renderExpsIn ctx xs
  pure $ case sty of
    TextNormal
      | Just txt <- styledText xs -> quoteText txt
      | Just txt <- renderTextNormalStyled xs
      , shouldForceUprightTextNormal xs -> "nitalic{" <> txt <> "}"
      | Just txt <- renderTextNormalStyled xs -> txt
    TextItalic       -> "ital " <> styleArg body
    TextBold         -> "bold " <> styleArg body
    TextScript       -> "ital " <> styleArg body
    TextFraktur      -> "ital " <> styleArg body
    TextDoubleStruck -> "ital " <> styleArg body
    _                -> body
 where
  styleArg t
    | T.null t       = "{}"
    | T.length t == 1 = t
    | otherwise      = "{" <> t <> "}"

styledText :: [Exp] -> Maybe T.Text
styledText = fmap T.concat . mapM styledTextExp

styledTextExp :: Exp -> Maybe T.Text
styledTextExp e =
  case e of
    ENumber t         -> Just t
    EIdentifier t     -> Just t
    EText _ t         -> Just t
    ESpace w
      | w <= 0        -> Just ""
      | w >= 2        -> Just "  "
      | otherwise     -> Just " "
    EGrouped xs       -> styledText xs
    EStyled TextNormal xs -> styledText xs
    ESub base sub     -> do
      base' <- styledTextExp base
      sub'  <- styledTextNonNumericExp sub
      pure $ base' <> "_" <> sub'
    ESuper base sup   -> do
      base' <- styledTextExp base
      sup'  <- styledTextNonNumericExp sup
      pure $ base' <> "^" <> sup'
    ESubsup base sub sup -> do
      base' <- styledTextExp base
      sub'  <- styledTextNonNumericExp sub
      sup'  <- styledTextNonNumericExp sup
      pure $ base' <> "_" <> sub' <> "^" <> sup'
    ESymbol _ s
      | isPlainTextSymbol s -> Just s
    _ -> Nothing

styledTextNonNumericExp :: Exp -> Maybe T.Text
styledTextNonNumericExp e = do
  txt <- styledTextExp e
  if T.any isAsciiDigit txt
     then Nothing
     else Just txt

isAsciiDigit :: Char -> Bool
isAsciiDigit c = c >= '0' && c <= '9'

isPlainTextSymbol :: T.Text -> Bool
isPlainTextSymbol s =
  s `elem`
    [ "_", ",", ".", ":", ";", "-", "−", "/", "(", ")", "[", "]"
    , "+", "=", "'", "′"
    ]

renderTextNormalStyled :: [Exp] -> Maybe T.Text
renderTextNormalStyled xs = do
  rendered <- mapM renderTextNormalExp xs
  pure $ mergePieces (zip xs rendered)

renderTextNormalExp :: Exp -> Maybe T.Text
renderTextNormalExp e =
  case e of
    ENumber t       -> Just t
    EIdentifier t   -> Just (renderIdentifier t)
    EText _ t       -> Just (quoteText t)
    ESpace w        -> Just (renderSpace w)
    EGrouped xs     -> renderTextNormalStyled xs
    EStyled TextNormal xs -> renderTextNormalStyled xs
    EStyled TextBold xs -> do
      body <- renderExpsIn AlignDefault xs
      pure $ "bold nitalic " <> styleArg body
    EStyled sty xs  -> renderStyled AlignDefault sty xs
    ESub base sub   -> do
      base' <- renderTextNormalExp base
      sub'  <- renderTextNormalExp sub
      pure $ base' <> "_" <> sub'
    ESuper base sup -> do
      base' <- renderTextNormalExp base
      sup'  <- renderTextNormalExp sup
      pure $ base' <> "^" <> sup'
    ESubsup base sub sup -> do
      base' <- renderTextNormalExp base
      sub'  <- renderTextNormalExp sub
      sup'  <- renderTextNormalExp sup
      pure $ base' <> "_" <> sub' <> "^" <> sup'
    ESymbol t s     -> Just (T.strip (renderSymbol t s))
    _               -> Nothing

shouldForceUprightTextNormal :: [Exp] -> Bool
shouldForceUprightTextNormal = all isUprightTextNormalExp

isUprightTextNormalExp :: Exp -> Bool
isUprightTextNormalExp e =
  case e of
    ENumber{}              -> True
    EIdentifier{}          -> True
    EText{}                -> True
    ESpace{}               -> True
    ESymbol _ s            -> isPlainTextSymbol s
    EGrouped xs            -> shouldForceUprightTextNormal xs
    EStyled TextNormal xs  -> shouldForceUprightTextNormal xs
    ESub base sub          -> isUprightTextNormalExp base && isUprightTextNormalExp sub
    ESuper base sup        -> isUprightTextNormalExp base && isUprightTextNormalExp sup
    ESubsup base sub sup   -> isUprightTextNormalExp base
                           && isUprightTextNormalExp sub
                           && isUprightTextNormalExp sup
    _                      -> False

styleArg :: T.Text -> T.Text
styleArg t
  | T.null t        = "{}"
  | T.length t == 1 = t
  | otherwise       = "{" <> t <> "}"

alignmentContext :: Alignment -> AlignContext
alignmentContext a =
  case a of
    AlignLeft  -> AlignLeftCtx
    AlignRight -> AlignRightCtx
    _          -> AlignDefault

maybeCenterFractionArg :: AlignContext -> T.Text -> T.Text
maybeCenterFractionArg ctx t
  | ctx == AlignLeftCtx || ctx == AlignRightCtx = "{alignc " <> asArg t <> "}"
  | otherwise = t
 where
  asArg x =
    let s = T.strip x
    in if T.null s
          then "{}"
          else if T.length s == 1
                  then s
                  else if T.head s == '{' && T.last s == '}'
                          then s
                          else "{" <> s <> "}"

renderSpace :: Rational -> T.Text
renderSpace w
  | w <= 0    = ""
  | w >= 2    = "~~ "
  | w >= 1    = "~ "
  | otherwise = " "

renderIdentifier :: T.Text -> T.Text
renderIdentifier ident =
  case greekName ident of
    Just name
      | shouldItalicizeGreek ident -> "%i" <> name
      | otherwise                  -> "%" <> name
    Nothing -> ident

renderMathOperator :: T.Text -> T.Text
renderMathOperator t
  | isBareMathOperator t = t
  | otherwise            = "func " <> t

isBareMathOperator :: T.Text -> Bool
isBareMathOperator t =
  t `elem`
    [ "min", "max", "log", "sin", "cos", "cosh", "sinh"
    , "cot", "ln", "exp"
    ]

shouldItalicizeGreek :: T.Text -> Bool
shouldItalicizeGreek ident =
  case ident of
    "α" -> True
    "β" -> True
    "γ" -> True
    "δ" -> True
    "ϵ" -> True
    "ε" -> True
    "ζ" -> True
    "η" -> True
    "θ" -> True
    "ϑ" -> True
    "ι" -> True
    "κ" -> True
    "λ" -> True
    "μ" -> True
    "ν" -> True
    "ξ" -> True
    "ο" -> True
    "π" -> True
    "ϖ" -> True
    "ρ" -> True
    "ϱ" -> True
    "𝜚" -> True
    "σ" -> True
    "ς" -> True
    "𝜍" -> True
    "τ" -> True
    "υ" -> True
    "ϕ" -> True
    "φ" -> True
    "χ" -> True
    "ψ" -> True
    "ω" -> True
    _   -> False

greekName :: T.Text -> Maybe T.Text
greekName ident =
  case ident of
    "α" -> Just "alpha"
    "β" -> Just "beta"
    "γ" -> Just "gamma"
    "δ" -> Just "delta"
    "ϵ" -> Just "varepsilon"
    "ε" -> Just "epsilon"
    "ζ" -> Just "zeta"
    "η" -> Just "eta"
    "θ" -> Just "theta"
    "ϑ" -> Just "vartheta"
    "ι" -> Just "iota"
    "κ" -> Just "kappa"
    "λ" -> Just "lambda"
    "μ" -> Just "mu"
    "ν" -> Just "nu"
    "ξ" -> Just "xi"
    "ο" -> Just "omicron"
    "π" -> Just "pi"
    "ϖ" -> Just "varpi"
    "ρ" -> Just "rho"
    "ϱ" -> Just "varrho"
    "𝜚" -> Just "varrho"
    "σ" -> Just "sigma"
    "ς" -> Just "varsigma"
    "𝜍" -> Just "varsigma"
    "τ" -> Just "tau"
    "υ" -> Just "upsilon"
    "ϕ" -> Just "phi"
    "φ" -> Just "varphi"
    "χ" -> Just "chi"
    "ψ" -> Just "psi"
    "ω" -> Just "omega"
    "Γ" -> Just "GAMMA"
    "Δ" -> Just "DELTA"
    "Θ" -> Just "THETA"
    "Λ" -> Just "LAMBDA"
    "Ξ" -> Just "XI"
    "Π" -> Just "PI"
    "Σ" -> Just "SIGMA"
    "Υ" -> Just "UPSILON"
    "Φ" -> Just "PHI"
    "Ψ" -> Just "PSI"
    "Ω" -> Just "OMEGA"
    _   -> Nothing

renderScriptBase :: Exp -> T.Text -> T.Text
renderScriptBase e rendered0 =
  let rendered = T.strip rendered0
  in if isEmptyScriptBase e || T.null rendered
        then "{}"
        else if isAtomic e
        then rendered
        else "{" <> rendered <> "}"

renderScriptArg :: AlignContext -> Exp -> Maybe T.Text
renderScriptArg ctx e = do
  rendered0 <- renderExpIn ctx e
  let rendered = T.strip rendered0
  pure $ if isAtomic e || isQuotedText rendered
            then rendered
            else "{" <> rendered <> "}"

renderLimitArg :: AlignContext -> Exp -> Maybe T.Text
renderLimitArg ctx e =
  case e of
    EGrouped xs -> renderExpsIn ctx xs
    _           -> T.strip <$> renderExpIn ctx e

renderAccentArg :: Exp -> T.Text -> T.Text
renderAccentArg e rendered0 =
  let rendered = T.strip rendered0
  in if isAtomic e
        then rendered
        else "{" <> rendered <> "}"

centerScriptArg :: T.Text -> T.Text
centerScriptArg rendered
  | isWrapped rendered = rendered
  | otherwise          = "{" <> rendered <> "}"
 where
  isWrapped t = T.length t >= 2 && T.head t == '{' && T.last t == '}'

isAtomic :: Exp -> Bool
isAtomic e =
  case e of
    ENumber{}       -> True
    EIdentifier{}   -> True
    EMathOperator{} -> True
    EText{}         -> True
    ESymbol{}       -> True
    _               -> False

isEmptyScriptBase :: Exp -> Bool
isEmptyScriptBase e =
  case e of
    EIdentifier t -> T.null t
    _             -> False

accentName :: Exp -> Maybe T.Text
accentName e =
  case e of
    ESymbol Accent s -> accentFromChar s
    ESymbol _ s      -> accentFromChar s
    _                -> Nothing

accentFromChar :: T.Text -> Maybe T.Text
accentFromChar s =
  case s of
    "\775"  -> Just "dot"
    "˙"     -> Just "dot"
    "\776"  -> Just "ddot"
    "¨"     -> Just "ddot"
    "\770"  -> Just "hat"
    "ˆ"     -> Just "hat"
    "\780"  -> Just "check"
    "ˇ"     -> Just "check"
    "\771"  -> Just "tilde"
    "˜"     -> Just "tilde"
    "\772"  -> Just "bar"
    "\8254" -> Just "bar"
    "¯"     -> Just "bar"
    "\8407" -> Just "vec"
    "→"     -> Just "vec"
    "\774"  -> Just "breve"
    "˘"     -> Just "breve"
    _       -> Nothing

data DelimSide = DelimLeft | DelimRight | DelimMiddle

delimToken :: DelimSide -> T.Text -> T.Text
delimToken side raw =
  case raw of
    ""  -> "none"
    "." -> "none"
    "(" -> "("
    ")" -> ")"
    "[" -> "["
    "]" -> "]"
    "{" -> case side of
      DelimLeft   -> "lbrace"
      DelimRight  -> "rbrace"
      DelimMiddle -> "{"
    "}" -> case side of
      DelimLeft   -> "lbrace"
      DelimRight  -> "rbrace"
      DelimMiddle -> "}"
    "|" -> case side of
      DelimLeft   -> "lline"
      DelimRight  -> "rline"
      DelimMiddle -> "mline"
    "∣" -> case side of
      DelimLeft   -> "lline"
      DelimRight  -> "rline"
      DelimMiddle -> "mline"
    "∥" -> case side of
      DelimLeft   -> "ldline"
      DelimRight  -> "rdline"
      DelimMiddle -> "mline"
    "⟨" -> "langle"
    "⟩" -> "rangle"
    "⌊" -> "lfloor"
    "⌋" -> "rfloor"
    "⌈" -> "lceil"
    "⌉" -> "rceil"
    "⟦" -> "ldbracket"
    "⟧" -> "rdbracket"
    _   -> raw

renderSymbol :: TeXSymbolType -> T.Text -> T.Text
renderSymbol t s =
  case s of
    "∫" -> "int "
    "∑" -> "sum "
    "←" -> " leftarrow "
    "→" -> " toward "
    "↔" -> " leftrightarrow "
    "⇐" -> " dlarrow "
    "⇒" -> " drarrow "
    "⇔" -> " dlrarrow "
    "↑" -> " uparrow "
    "↓" -> " downarrow "
    "↦" -> " mapsto "
    "… " -> " dotsaxis "
    "…" -> " dotsaxis "
    "⋯" -> " dotsaxis "
    "⋮" -> " dotsvert "
    "⋱" -> " dotsdown "
    "⋰" -> " dotsup "
    "∈" -> " in "
    "∉" -> " notin "
    "∋" -> " owns "
    "∩" -> " intersection "
    "∪" -> " union "
    "⊂" -> " subset "
    "⊆" -> " subseteq "
    "⊃" -> " supset "
    "⊇" -> " supseteq "
    "≤" -> " <= "
    "≥" -> " >= "
    "≠" -> " <> "
    "≈" -> " approx "
    "≡" -> " equiv "
    "∝" -> " prop "
    "∥" -> " parallel "
    "⊥" -> " ortho "
    "±" -> " plusminus "
    "∓" -> " minusplus "
    "×" -> " times "
    "⋅" -> " cdot "
    "·" -> " cdot "
    "∘" -> " circ "
    "/" -> " / "
    "∂" -> "partial"
    "∇" -> "nabla"
    "∀" -> "forall"
    "∃" -> "exists"
    "¬" -> "neg"
    "∧" -> "and"
    "∨" -> "or"
    "∞" -> "infinity"
    "∅" -> "emptyset"
    "+" -> " + "
    "-" | t == Bin  -> " - "
    "-"             -> "-"
    "−" | t == Bin  -> " - "
    "−"             -> "-"
    "=" -> " = "
    "," -> ", "
    ";" -> "; "
    ":" -> " : "
    "!" -> " ! "
    "'" -> "′"
    "′" -> "′"
    _   -> s

quoteText :: T.Text -> T.Text
quoteText t = "\"" <> escapeQuotes t <> "\""

escapeQuotes :: T.Text -> T.Text
escapeQuotes = T.replace "\"" "\\\""

isQuotedText :: T.Text -> Bool
isQuotedText t =
  T.length t >= 2 && T.head t == '"' && T.last t == '"'

largeOpName :: Exp -> Maybe T.Text
largeOpName e =
  case e of
    ESymbol Op "\8747" -> Just "int"
    ESymbol Op "\8721" -> Just "sum"
    ESymbol Op "\8719" -> Just "prod"
    ESymbol Op "∫"     -> Just "int"
    ESymbol Op "∑"     -> Just "sum"
    ESymbol Op "∏"     -> Just "prod"
    _                  -> Nothing

limitOpName :: Exp -> Maybe T.Text
limitOpName e =
  case largeOpName e of
    Just op -> Just op
    Nothing ->
      case e of
        EMathOperator "lim"    -> Just "lim"
        EMathOperator "liminf" -> Just "liminf"
        EMathOperator "limsup" -> Just "limsup"
        EMathOperator "min"    -> Nothing
        EMathOperator "max"    -> Nothing
        _                      -> Nothing

centeredScriptOpName :: Exp -> Maybe T.Text
centeredScriptOpName e =
  case e of
    EMathOperator "min" -> Just "func min"
    EMathOperator "max" -> Just "func max"
    _                   -> Nothing
