{-# LANGUAGE OverloadedStrings #-}
module Text.TeXMath.Writers.StarMath
  ( writeStarMath
  ) where

import Data.Char (isLetter)
import Data.Generics (everywhere, mkT)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Text.TeXMath.Shared as S
import Text.TeXMath.Unicode.ToUnicode (toUnicodeChar)
import Text.TeXMath.Types
  ( Alignment(..)
  , DisplayType(..)
  , Exp(..)
  , FractionType(..)
  , TeXSymbolType(..)
  , TextType(..)
  )
import Text.TeXMath.Writers.TeX (writeTeX)

-- | Render TeXMath expressions as StarMath syntax.
-- Falls back to TeX output for expressions that are not yet supported.
writeStarMath :: DisplayType -> [Exp] -> T.Text
writeStarMath dt exps =
  case renderExps dt (normalizeExps (everywhere (mkT $ S.handleDownup dt) exps)) of
    Just rendered -> T.strip rendered
    Nothing       -> writeTeX exps

data AlignContext = AlignDefault | AlignLeftCtx | AlignRightCtx
  deriving (Eq)

renderExps :: DisplayType -> [Exp] -> Maybe T.Text
renderExps dt = renderExpsIn dt AlignDefault

normalizeExps :: [Exp] -> [Exp]
normalizeExps =
  normalizeBareBars
  . normalizeEvaluationBars
  . mergeAdjacentUnicodeSerifStyled
  . normalizeBareBraces
  . map normalizeExp

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
      case collectBareDelimited sym [] xs of
        Just (mid, trailingScript, rest) ->
          let delimited = EDelimited sym sym (map Right mid)
              scripted = maybe delimited ($ delimited) trailingScript
          in scripted : normalizeBareBars rest
        _ ->
          x : normalizeBareBars xs
normalizeBareBars (x:xs) = x : normalizeBareBars xs

normalizeBareBraces :: [Exp] -> [Exp]
normalizeBareBraces [] = []
normalizeBareBraces (ESymbol Open "{" : xs) =
  case collectBareBraces [] xs of
    Just (mid, rest) -> EDelimited "{" "}" (map Right mid) : normalizeBareBraces rest
    Nothing          -> ESymbol Open "{" : normalizeBareBraces xs
normalizeBareBraces (x:xs) = x : normalizeBareBraces xs

mergeAdjacentUnicodeSerifStyled :: [Exp] -> [Exp]
mergeAdjacentUnicodeSerifStyled [] = []
mergeAdjacentUnicodeSerifStyled (EStyled sty xs : EStyled sty' ys : rest)
  | sty == sty' && isUnicodeSerifStyle sty =
      mergeAdjacentUnicodeSerifStyled (EStyled sty (xs <> [ESpace 1] <> ys) : rest)
mergeAdjacentUnicodeSerifStyled (x:xs) = x : mergeAdjacentUnicodeSerifStyled xs

isUnicodeSerifStyle :: TextType -> Bool
isUnicodeSerifStyle sty =
  sty `elem`
    [ TextScript
    , TextFraktur
    , TextDoubleStruck
    , TextBoldScript
    , TextBoldFraktur
    ]

normalizeEvaluationBars :: [Exp] -> [Exp]
normalizeEvaluationBars = reverse . go []
 where
  go acc [] = acc
  go acc (cur : xs)
    | Just script <- evaluationBarScript cur
    , Just (rest, target) <- takeEvaluationTarget acc =
        go (script target : rest) xs
    | otherwise =
        go (cur : acc) xs

evaluationBarScript :: Exp -> Maybe (Exp -> Exp)
evaluationBarScript cur =
  case cur of
    ESub base sub
      | isEvaluationBarBase base ->
          Just (\target -> ESub (EDelimited "." "|" [Right target]) sub)
    ESuper base sup
      | isEvaluationBarBase base ->
          Just (\target -> ESuper (EDelimited "." "|" [Right target]) sup)
    ESubsup base sub sup
      | isEvaluationBarBase base ->
          Just (\target -> ESubsup (EDelimited "." "|" [Right target]) sub sup)
    _ -> Nothing

takeEvaluationTarget :: [Exp] -> Maybe ([Exp], Exp)
takeEvaluationTarget [] = Nothing
takeEvaluationTarget (e : rest) =
  case e of
    EDelimited{} -> Just (rest, e)
    ESymbol Close c
      | Just (rest', target) <- takeDelimitedTarget c rest ->
          Just (rest', target)
    _ -> Just (rest, e)

takeDelimitedTarget :: T.Text -> [Exp] -> Maybe ([Exp], Exp)
takeDelimitedTarget closeTxt = go 0 []
 where
  openTxt =
    case closeTxt of
      ")" -> Just "("
      "]" -> Just "["
      "}" -> Just "{"
      _   -> Nothing

  go _ _ [] = Nothing
  go depth inner (e : rest) =
    case (openTxt, e) of
      (Just open, ESymbol Close c)
        | c == closeTxt ->
            go (depth + 1) (e : inner) rest
      (Just open, ESymbol Open o)
        | o == open ->
            if depth == 0
               then Just (rest, EDelimited open closeTxt (map Right inner))
               else go (depth - 1) (e : inner) rest
      _ ->
        go depth (e : inner) rest

isEvaluationBarBase :: Exp -> Bool
isEvaluationBarBase e =
  case e of
    EScaled _ (ESymbol Open "|")  -> True
    EScaled _ (ESymbol Close "|") -> True
    _                             -> False

collectBareBraces :: [Exp] -> [Exp] -> Maybe ([Exp], [Exp])
collectBareBraces _ [] = Nothing
collectBareBraces acc (ESymbol Close "}" : xs) = Just (reverse acc, xs)
collectBareBraces acc (x:xs) = collectBareBraces (x:acc) xs

collectBareDelimited :: T.Text -> [Exp] -> [Exp]
                     -> Maybe ([Exp], Maybe (Exp -> Exp), [Exp])
collectBareDelimited _ acc [] = Nothing
collectBareDelimited sym acc (y:ys)
  | matchesBareBar sym y = Just (reverse acc, Nothing, ys)
  | Just apply <- scriptedBareBar sym y = Just (reverse acc, Just apply, ys)
  | otherwise = collectBareDelimited sym (y:acc) ys

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

scriptedBareBar :: T.Text -> Exp -> Maybe (Exp -> Exp)
scriptedBareBar sym e =
  case e of
    ESub base sub
      | matchesBareBar sym base -> Just (\del -> ESub del sub)
    ESuper base sup
      | matchesBareBar sym base -> Just (\del -> ESuper del sup)
    ESubsup base sub sup
      | matchesBareBar sym base -> Just (\del -> ESubsup del sub sup)
    _ -> Nothing

renderExpsIn :: DisplayType -> AlignContext -> [Exp] -> Maybe T.Text
renderExpsIn dt ctx exps = do
  rendered <- mapM (renderExpIn dt ctx) exps
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
         (curE, appendRendered (needsSeparator prevE curE) acc curT)

appendRendered :: Bool -> T.Text -> T.Text -> T.Text
appendRendered needSep left right
  | T.null left = T.stripStart right
  | T.null right = T.stripEnd left
  | otherwise =
      let leftHadWs = endsWithAsciiSpace left
          rightHadWs = startsWithAsciiSpace right
          left' = T.dropWhileEnd isAsciiSpaceChar left
          right' = T.dropWhile isAsciiSpaceChar right
          sep = if leftHadWs || rightHadWs || needSep then " " else ""
      in left' <> sep <> right'

startsWithAsciiSpace :: T.Text -> Bool
startsWithAsciiSpace t =
  case T.uncons t of
    Just (c, _) -> isAsciiSpaceChar c
    Nothing     -> False

endsWithAsciiSpace :: T.Text -> Bool
endsWithAsciiSpace t =
  case T.unsnoc t of
    Just (_, c) -> isAsciiSpaceChar c
    Nothing     -> False

isAsciiSpaceChar :: Char -> Bool
isAsciiSpaceChar c = c == ' ' || c == '\t' || c == '\n'

needsSeparator :: Exp -> Exp -> Bool
needsSeparator prevE curE
  | isGreekIdentifierExp prevE && isIdentifierLike curE = True
  | isGreekIdentifierExp prevE && isRootLike curE        = True
  | isGreekIdentifierExp prevE && isTerminatingPunctuation curE = True
  | isWordSymbolLike prevE && isIdentifierLike curE      = True
  | isWordSymbolLike prevE && isOpenLike curE            = True
  | isWordStyledExp prevE && isIdentifierLike curE       = True
  | isAccentCommandExp prevE && isIdentifierLike curE    = True
  | isAccentCommandExp prevE && isOpenLike curE          = True
  | isAccentCommandExp prevE && isRootLike curE          = True
  | isMathOperatorExp prevE && isIdentifierLike curE     = True
  | isUnaryMinusSymbol prevE && isIdentifierLike curE    = True
  | isIdentifierLike prevE && isMathOperatorExp curE     = True
  | isIdentifierLike prevE && isRootLike curE            = True
  | isIdentifierLike prevE && isArrowScriptedExp curE    = True
  | isIdentifierLike prevE && isScriptedMathOperatorExp curE = True
  | isIdentifierLike prevE && isWordSymbolLike curE      = True
  | isLargeOpScriptedExp prevE && isIdentifierLike curE  = True
  | isLargeOpScriptedExp prevE && isFractionLike curE    = True
  | isLargeOpScriptedExp prevE && isAccentCommandExp curE = True
  | isScripted prevE && isIdentifierLike curE           = True
  | isArrowScriptedExp prevE && isIdentifierLike curE   = True
  | isScriptedMathOperatorExp prevE && isIdentifierLike curE = True
  | isDelimited prevE && isDelimited curE               = True
  | isCloseLike prevE && isIdentifierLike curE          = True
  | isCloseLike prevE && isWordSymbolLike curE          = True
  | isCloseLike prevE && isAccentCommandExp curE        = True
  | isCloseLike prevE && isArrayLike curE               = True
  | isIdentifierLike prevE && isWordStyledExp curE      = True
  | isIdentifierLike prevE && isUprightMathTextExp curE = True
  | isIdentifierLike prevE && isAccentCommandExp curE   = True
  | isIdentifierLike prevE && isWideSpace curE          = True
  | isIdentifierLike prevE && isDelimited curE          = True
  | isIdentifierLike prevE && isNonNormalTextExp curE   = True
  | isQuotedTextExp prevE && isItalicTextExp curE       = True
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

isArrayLike :: Exp -> Bool
isArrayLike e =
  case e of
    EArray{} -> True
    _        -> False

isFractionLike :: Exp -> Bool
isFractionLike e =
  case e of
    EFraction{} -> True
    _           -> False

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
    ESymbol _ "∂" -> True
    ESymbol _ "¬" -> True
    ESymbol _ "∧" -> True
    ESymbol _ "∨" -> True
    ESymbol _ "and" -> True
    ESymbol _ "or" -> True
    _             -> False

isOpenLike :: Exp -> Bool
isOpenLike e =
  case e of
    ESymbol Open _ -> True
    _              -> False

isRootLike :: Exp -> Bool
isRootLike e =
  case e of
    ESqrt{} -> True
    ERoot{} -> True
    _       -> False

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

isUprightMathTextExp :: Exp -> Bool
isUprightMathTextExp e =
  case e of
    EStyled TextNormal [EIdentifier _] -> True
    _                                  -> False

isItalicTextExp :: Exp -> Bool
isItalicTextExp e =
  case e of
    EText TextItalic _ -> True
    _                  -> False

isQuotedTextExp :: Exp -> Bool
isQuotedTextExp e =
  case e of
    EText TextNormal _ -> True
    _                  -> False

isNonNormalTextExp :: Exp -> Bool
isNonNormalTextExp e =
  case e of
    EText sty _ -> sty /= TextNormal
    _           -> False

isAccentCommandExp :: Exp -> Bool
isAccentCommandExp e =
  case e of
    EOver _ _ over -> accentName over /= Nothing
    _              -> False

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

isArrowScriptedExp :: Exp -> Bool
isArrowScriptedExp e =
  case e of
    EUnder _ base _       -> isArrowBase base
    EOver _ base _        -> isArrowBase base
    EUnderover _ base _ _ -> isArrowBase base
    ESub base _           -> isArrowBase base
    ESuper base _         -> isArrowBase base
    ESubsup base _ _      -> isArrowBase base
    _                     -> False

isScriptedMathOperatorExp :: Exp -> Bool
isScriptedMathOperatorExp e =
  case e of
    ESub base _           -> isMathOperatorExp base
    ESuper base _         -> isMathOperatorExp base
    ESubsup base _ _      -> isMathOperatorExp base
    EUnder _ base _       -> isMathOperatorExp base
    EOver _ base _        -> isMathOperatorExp base
    EUnderover _ base _ _ -> isMathOperatorExp base
    _                     -> False

isArrowBase :: Exp -> Bool
isArrowBase e =
  case e of
    ESymbol _ "←" -> True
    ESymbol _ "→" -> True
    ESymbol _ "↔" -> True
    ESymbol _ "⇐" -> True
    ESymbol _ "⇒" -> True
    ESymbol _ "⇔" -> True
    ESymbol _ "↦" -> True
    _             -> False

isUnaryMinusSymbol :: Exp -> Bool
isUnaryMinusSymbol e =
  case e of
    ESymbol t "-" -> t /= Bin
    ESymbol t "−" -> t /= Bin
    _             -> False

isLargeOpScriptedExp :: Exp -> Bool
isLargeOpScriptedExp e =
  case e of
    EUnder _ base _       -> largeOpName base /= Nothing
    EOver _ base _        -> largeOpName base /= Nothing
    EUnderover _ base _ _ -> largeOpName base /= Nothing
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

needsNeutralScriptOperands :: Exp -> Bool
needsNeutralScriptOperands e =
  isInfixLikeExp e && not (isAtomicScriptOperator e)

isAtomicScriptOperator :: Exp -> Bool
isAtomicScriptOperator e =
  case e of
    ESymbol _ "∘" -> True
    _             -> False

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

renderExpIn :: DisplayType -> AlignContext -> Exp -> Maybe T.Text
renderExpIn dt ctx e =
  case e of
    ENumber t       -> Just t
    EIdentifier t   -> Just (renderIdentifier t)
    EMathOperator t -> Just (renderMathOperator t)
    ESymbol t s     -> Just (renderSymbol t s)
    EText sty t     -> Just (renderTextAtom sty t)
    ESpace w        -> Just (renderSpace w)
    EGrouped xs     -> ("{" <>) . (<> "}") <$> renderExpsIn dt ctx xs
    EStyled sty xs  -> renderStyled dt ctx sty xs

    EFraction frac num den -> do
      num' <- renderExpIn dt AlignDefault num
      den' <- renderExpIn dt AlignDefault den
      let num'' = maybeCenterFractionArg ctx num'
      let den'' = maybeCenterFractionArg ctx den'
      pure $ case frac of
        NoLineFrac -> "binom" <> asDelimitedArg num'' <> asDelimitedArg den''
        InlineFrac -> renderInlineFraction num'' den''
        NormalFrac
          | dt == DisplayInline -> renderInlineFraction num'' den''
        _          -> "{" <> num'' <> " over " <> den'' <> "}"

    ESqrt x -> ("sqrt {" <>) . (<> "}") <$> renderExpIn dt ctx x
    ERoot idx rad -> do
      idx' <- renderExpIn dt ctx idx
      rad' <- renderExpIn dt ctx rad
      pure $ "nroot {" <> idx' <> "} {" <> rad' <> "}"

    EScaled _ (ESymbol Open "|") ->
      Just "mline"
    EScaled _ (ESymbol Close "|") ->
      Just "mline"
    EScaled _ x ->
      renderExpIn dt ctx x

    EDelimited op cl xs -> do
      body <- renderDelimitedBody dt ctx xs
      let op' = delimToken DelimLeft op
      let cl' = delimToken DelimRight cl
      pure $ "left " <> op' <> " " <> body <> " right " <> cl'

    ESub base sub -> do
      base' <- renderExpIn dt ctx base
      sub'  <- renderScriptArg dt ctx sub
      pure $ renderScriptBase base base' <> "_" <> sub'

    ESuper base sup -> do
      baseRendered <- renderExpIn dt ctx base
      case renderPrimeSuffix sup of
        Just primes ->
          pure $ renderScriptBase base baseRendered <> primes
        Nothing -> do
          supRendered <- renderScriptArg dt ctx sup
          pure $ renderScriptBase base baseRendered <> "^" <> supRendered

    ESubsup base sub sup -> do
      baseRendered <- renderExpIn dt ctx base
      subRendered <- renderScriptArg dt ctx sub
      case renderPrimeSuffix sup of
        Just primes ->
          pure $ renderScriptBase base baseRendered <> "_" <> subRendered <> primes
        Nothing -> do
          supRendered <- renderScriptArg dt ctx sup
          pure $ renderScriptBase base baseRendered <> "_" <> subRendered <> "^" <> supRendered

    EOver _ base over
      | Just arrow <- arrowScriptOpName base -> do
          if isEmptyScriptArg over
             then pure arrow
             else do
               over' <- renderScriptArg dt ctx over
               pure $ arrow <> " csup " <> centerScriptArg over'
      | Just op <- centeredScriptOpName base -> do
          over' <- renderScriptArg dt ctx over
          pure $ "{" <> op <> "} csup " <> centerScriptArg over'
      | Just brace <- braceAnnotationName over -> do
          base' <- renderExpIn dt ctx base
          pure $ renderScriptBase base base' <> " " <> brace
      | isBraceAnnotatedExp base -> do
          base' <- renderExpIn dt ctx base
          over' <- renderBraceLabel dt ctx over
          pure $ base' <> " " <> over'
      | Just accent <- accentName over -> do
          base' <- renderExpIn dt ctx base
          pure $ accent <> " " <> renderAccentArg base base'
      | Just op <- limitOpName base -> do
          over' <- renderLimitArg dt ctx over
          pure $ op <> " to " <> over' <> " "
      | otherwise -> Nothing

    EUnder _ base under ->
      case arrowScriptOpName base of
        Just arrow ->
          if isEmptyScriptArg under
             then pure arrow
             else do
               under' <- renderScriptArg dt ctx under
               pure $ arrow <> " csub " <> centerScriptArg under'
        Nothing ->
          case underlineMarkerName under of
            Just marker -> do
              base' <- renderExpIn dt ctx base
              pure $ marker <> " " <> renderAccentArg base base'
            Nothing ->
              case braceAnnotationName under of
                Just brace -> do
                  base' <- renderExpIn dt ctx base
                  pure $ renderScriptBase base base' <> " " <> brace
                Nothing ->
                  if isBraceAnnotatedExp base
                     then do
                       base' <- renderExpIn dt ctx base
                       under' <- renderBraceLabel dt ctx under
                       pure $ base' <> " " <> under'
                     else
                       case centeredScriptOpName base of
                         Just op -> do
                           under' <- renderScriptArg dt ctx under
                           pure $ "{" <> op <> "} csub " <> centerScriptArg under'
                         Nothing ->
                           case limitOpName base of
                             Just op -> do
                               under' <- renderLimitArg dt ctx under
                               pure $ op <> " from " <> under' <> " "
                             Nothing -> do
                               base' <- renderExpIn dt ctx base
                               under' <- renderScriptArg dt ctx under
                               pure $ renderScriptBase base base' <> "_" <> under'
    EUnderover _ base under over ->
      case arrowScriptOpName base of
        Just arrow ->
          case (isEmptyScriptArg under, isEmptyScriptArg over) of
            (True, True) -> pure arrow
            (False, True) -> do
              under' <- renderScriptArg dt ctx under
              pure $ arrow <> " csub " <> centerScriptArg under'
            (True, False) -> do
              over' <- renderScriptArg dt ctx over
              pure $ arrow <> " csup " <> centerScriptArg over'
            (False, False) -> do
              under' <- renderScriptArg dt ctx under
              over' <- renderScriptArg dt ctx over
              pure $ arrow <> " csub " <> centerScriptArg under'
                  <> " csup " <> centerScriptArg over'
        Nothing ->
          case centeredScriptOpName base of
            Just op -> do
              under' <- renderScriptArg dt ctx under
              over' <- renderScriptArg dt ctx over
              pure $ "{" <> op <> "} csub " <> centerScriptArg under'
                  <> " csup " <> centerScriptArg over'
            Nothing ->
              case limitOpName base of
                Just op -> do
                  under' <- renderLimitArg dt ctx under
                  over'  <- renderLimitArg dt ctx over
                  pure $ op <> " from " <> under' <> " to " <> over' <> " "
                Nothing -> do
                  base' <- renderExpIn dt ctx base
                  under' <- renderScriptArg dt ctx under
                  over' <- renderScriptArg dt ctx over
                  pure $ renderScriptBase base base' <> "_" <> under' <> "^" <> over'
    EArray aligns rows -> renderMatrix dt aligns rows
    EPhantom x         -> do
      x' <- renderExpIn dt ctx x
      pure $ "phantom " <> renderPhantomArg x x'
    _                  -> Nothing

renderDelimitedBody :: DisplayType -> AlignContext -> [Either T.Text Exp] -> Maybe T.Text
renderDelimitedBody dt ctx xs = do
  chunks <- mapM (renderDelimitedChunk dt ctx) xs
  pure $ T.strip (mergeDelimitedChunks chunks)

data DelimitedChunk = DelimRaw T.Text | DelimExp Exp T.Text

renderDelimitedChunk :: DisplayType -> AlignContext -> Either T.Text Exp -> Maybe DelimitedChunk
renderDelimitedChunk dt ctx p =
  case p of
    Left t  -> Just $ DelimRaw (" " <> delimToken DelimMiddle t <> " ")
    Right x -> DelimExp x <$> renderExpIn dt ctx x

mergeDelimitedChunks :: [DelimitedChunk] -> T.Text
mergeDelimitedChunks [] = ""
mergeDelimitedChunks (c0:cs) = snd $ List.foldl' step (chunkExp c0, chunkText c0) cs
 where
  step (prevExp, acc) cur
    | T.null curText = (prevExp, acc)
    | otherwise =
        case cur of
          DelimRaw _ -> (Nothing, appendRendered False acc curText)
          DelimExp curExp _ ->
            let needSep = case prevExp of
                            Just pe -> needsSeparator pe curExp
                            Nothing -> False
            in (Just curExp, appendRendered needSep acc curText)
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

renderMatrix :: DisplayType -> [Alignment] -> [[[Exp]]] -> Maybe T.Text
renderMatrix dt aligns rows = do
  rows' <- mapM (renderMatrixRow dt aligns) rows
  pure $ "matrix { " <> T.intercalate " ## " rows' <> " }"

renderMatrixRow :: DisplayType -> [Alignment] -> [[Exp]] -> Maybe T.Text
renderMatrixRow dt aligns cells = do
  let explicitCenter = any (/= AlignCenter) aligns
  let columnCount = max (length aligns) (length cells)
  let paddedCells = take columnCount (cells ++ repeat [])
  cells' <- sequence
    [ renderMatrixCellWithAlign dt explicitCenter (columnAlign aligns i) c
    | (i, c) <- zip [(0 :: Int) ..] paddedCells
    ]
  pure $ T.intercalate " # " cells'

renderMatrixCell :: DisplayType -> AlignContext -> [Exp] -> Maybe T.Text
renderMatrixCell _ _ [] = Just "{}"
renderMatrixCell dt ctx xs = do
  rendered <- renderExpsIn dt ctx xs
  let stripped = T.strip rendered
  pure $ if T.null stripped then "{}" else stripped

renderMatrixCellWithAlign :: DisplayType -> Bool -> Alignment -> [Exp] -> Maybe T.Text
renderMatrixCellWithAlign dt explicitCenter align xs = do
  cell <- renderMatrixCell dt (alignmentContext align) xs
  pure $ case align of
    AlignLeft  -> "alignl " <> cell
    AlignRight -> "alignr " <> cell
    AlignCenter | explicitCenter -> "alignc " <> cell
    _                            -> cell

columnAlign :: [Alignment] -> Int -> Alignment
columnAlign aligns i =
  case drop i aligns of
    (a : _) -> a
    []      -> AlignCenter

renderStyled :: DisplayType -> AlignContext -> TextType -> [Exp] -> Maybe T.Text
renderStyled dt ctx sty xs = do
  body <- renderExpsIn dt ctx xs
  pure $ case sty of
    _
      | Just unicodeBody <- renderUnicodeSerifStyled sty xs ->
          "nitalic " <> styleArg unicodeBody
    TextNormal
      | Just ident <- singleUprightIdentifier xs ->
          "nitalic " <> renderTextNormalIdentifier ident
      | Just txt <- styledText xs -> quoteText txt
      | Just txt <- renderTextNormalStyled dt xs
      , shouldForceUprightTextNormal xs -> "nitalic{" <> txt <> "}"
      | Just txt <- renderTextNormalStyled dt xs -> txt
    TextItalic       -> "ital " <> styleArg body
    TextBold
      | shouldForceUprightBold xs -> "bold nitalic " <> styleArg body
      | otherwise -> "bold " <> styleArg body
    TextBoldItalic   -> "bold " <> styleArg ("ital " <> styleArg body)
    TextMonospace    -> "font fixed nitalic " <> styleArg body
    TextSansSerif    -> "font sans nitalic " <> styleArg body
    TextSansSerifBold -> "bold " <> styleArg ("font sans nitalic " <> styleArg body)
    TextSansSerifBoldItalic -> "bold " <> styleArg ("font sans ital " <> styleArg body)
    TextSansSerifItalic -> "font sans ital " <> styleArg body
    TextScript       -> "ital " <> styleArg body
    TextFraktur      -> "bold " <> styleArg body
    TextDoubleStruck -> "bold nitalic " <> styleArg body
    _                -> body
 where
  styleArg t
    | T.null t       = "{}"
    | T.length t == 1 = t
    | otherwise      = "{" <> t <> "}"

renderUnicodeSerifStyled :: TextType -> [Exp] -> Maybe T.Text
renderUnicodeSerifStyled sty xs =
  case sty of
    TextScript       -> styledUnicodeText sty xs
    TextFraktur      -> styledUnicodeText sty xs
    TextDoubleStruck -> styledUnicodeText sty xs
    TextBoldScript   -> styledUnicodeText sty xs
    TextBoldFraktur  -> styledUnicodeText sty xs
    _                -> Nothing

styledUnicodeText :: TextType -> [Exp] -> Maybe T.Text
styledUnicodeText sty = fmap T.concat . mapM (styledUnicodeExp sty)

styledUnicodeExp :: TextType -> Exp -> Maybe T.Text
styledUnicodeExp sty e =
  case e of
    EIdentifier t        -> mapStyledUnicode sty t
    ENumber t            -> mapStyledUnicode sty t
    EGrouped xs          -> styledUnicodeText sty xs
    EStyled TextNormal xs -> styledUnicodeText sty xs
    ESpace w
      | w <= 0          -> Just ""
      | w >= 2          -> Just "  "
      | otherwise       -> Just " "
    _                   -> Nothing

mapStyledUnicode :: TextType -> T.Text -> Maybe T.Text
mapStyledUnicode sty t = T.pack <$> mapM (\c -> toUnicodeChar (sty, c)) (T.unpack t)

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

renderTextNormalStyled :: DisplayType -> [Exp] -> Maybe T.Text
renderTextNormalStyled dt xs = do
  let xs' = quoteStarMathKeywordRuns xs
  rendered <- mapM (renderTextNormalExp dt) xs'
  pure $ mergePieces (zip xs' rendered)

quoteStarMathKeywordRuns :: [Exp] -> [Exp]
quoteStarMathKeywordRuns [] = []
quoteStarMathKeywordRuns xs@(x : rest)
  | Just _ <- asciiIdentifierWord x =
      let (word, wordExps, rest') = takeIdentifierWordRun xs
      in if isStarMathReservedWord word
            then EText TextNormal word : quoteStarMathKeywordRuns rest'
            else wordExps <> quoteStarMathKeywordRuns rest'
  | otherwise = x : quoteStarMathKeywordRuns rest

takeIdentifierWordRun :: [Exp] -> (T.Text, [Exp], [Exp])
takeIdentifierWordRun = go [] []
 where
  go pieces exps (e : rest)
    | Just piece <- asciiIdentifierWord e = go (piece : pieces) (e : exps) rest
  go pieces exps rest = (T.concat (reverse pieces), reverse exps, rest)

asciiIdentifierWord :: Exp -> Maybe T.Text
asciiIdentifierWord e =
  case e of
    EIdentifier t
      | not (T.null t) && T.all isAsciiAlpha t -> Just t
    _ -> Nothing

renderTextNormalExp :: DisplayType -> Exp -> Maybe T.Text
renderTextNormalExp dt e =
  case e of
    ENumber t       -> Just t
    EIdentifier t   -> Just (renderIdentifier t)
    EText sty t     -> Just (renderTextAtom sty t)
    ESpace w        -> Just (renderSpace w)
    EGrouped xs     -> renderTextNormalStyled dt xs
    EStyled TextNormal xs -> renderTextNormalStyled dt xs
    EStyled TextBold xs -> do
      body <- renderExpsIn dt AlignDefault xs
      pure $ "bold nitalic " <> styleArg body
    EStyled sty xs  -> renderStyled dt AlignDefault sty xs
    ESub base sub   -> do
      base' <- renderTextNormalExp dt base
      sub'  <- renderTextNormalExp dt sub
      pure $ base' <> "_" <> sub'
    ESuper base sup -> do
      base' <- renderTextNormalExp dt base
      sup'  <- renderTextNormalExp dt sup
      pure $ base' <> "^" <> sup'
    ESubsup base sub sup -> do
      base' <- renderTextNormalExp dt base
      sub'  <- renderTextNormalExp dt sub
      sup'  <- renderTextNormalExp dt sup
      pure $ base' <> "_" <> sub' <> "^" <> sup'
    ESymbol t s     -> Just (T.strip (renderSymbol t s))
    _               -> Nothing

shouldForceUprightTextNormal :: [Exp] -> Bool
shouldForceUprightTextNormal = all isUprightTextNormalExp

shouldForceUprightBold :: [Exp] -> Bool
shouldForceUprightBold = all isUprightBoldExp

isUprightBoldExp :: Exp -> Bool
isUprightBoldExp e =
  case e of
    ENumber{}            -> True
    EIdentifier t        -> T.all isAsciiAlphaNum t
    EText{}              -> True
    ESpace{}             -> True
    ESymbol _ s          -> isPlainTextSymbol s
    EGrouped xs          -> shouldForceUprightBold xs
    EStyled TextNormal xs -> shouldForceUprightBold xs
    ESub base sub        -> isUprightBoldExp base && isUprightBoldExp sub
    ESuper base sup      -> isUprightBoldExp base && isUprightBoldExp sup
    ESubsup base sub sup -> isUprightBoldExp base
                         && isUprightBoldExp sub
                         && isUprightBoldExp sup
    _                    -> False

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

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c =
  (c >= 'A' && c <= 'Z') ||
  (c >= 'a' && c <= 'z')

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c =
  isAsciiAlpha c ||
  (c >= '0' && c <= '9')

styleArg :: T.Text -> T.Text
styleArg t
  | T.null t        = "{}"
  | T.length t == 1 = t
  | otherwise       = "{" <> t <> "}"

singleUprightIdentifier :: [Exp] -> Maybe T.Text
singleUprightIdentifier xs =
  case xs of
    [EIdentifier t] -> Just t
    _               -> Nothing

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

asDelimitedArg :: T.Text -> T.Text
asDelimitedArg t =
  let s = T.strip t
  in if T.null s
        then "{}"
        else if T.head s == '{' && T.last s == '}'
                then s
                else "{" <> s <> "}"

renderInlineFraction :: T.Text -> T.Text -> T.Text
renderInlineFraction num den =
  "size*0.7 {" <> num <> " over " <> den <> "}"

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

renderTextNormalIdentifier :: T.Text -> T.Text
renderTextNormalIdentifier ident =
  case greekName ident of
    Just{}  -> renderIdentifier ident
    Nothing
      | isStarMathReservedWord ident -> quoteText ident
      | otherwise                    -> ident

isStarMathReservedWord :: T.Text -> Bool
isStarMathReservedWord t =
  t `elem`
    [ "alignc", "alignl", "alignr"
    , "and", "approx"
    , "bar", "binom", "bold", "breve"
    , "cdot", "check", "circ", "cos", "cosh", "cot", "csub", "csup"
    , "dlarrow", "dlrarrow", "dot", "dotsaxis", "dotsdown", "dotslow"
    , "dotsup", "dotsvert", "downarrow", "drarrow"
    , "emptyset", "equiv", "exists", "exp"
    , "fixed", "font", "forall", "from", "func"
    , "gg"
    , "hat"
    , "in", "infinity", "int", "intersection", "ital", "iiint"
    , "langle", "lbrace", "lceil", "ldbracket", "ldline", "left"
    , "leftarrow", "leftrightarrow", "lfloor", "lim", "liminf"
    , "limsup", "ll", "lline", "ln", "log"
    , "mapsto", "matrix", "max", "min", "minusplus", "mline"
    , "nabla", "neg", "none", "notin", "nitalic", "nroot"
    , "or", "ortho", "over", "overbrace", "overline", "owns"
    , "parallel", "phantom", "plusminus", "partial", "prod", "prop"
    , "rangle", "rbrace", "rceil", "rdbracket", "rdline", "rfloor"
    , "right", "rline"
    , "sans", "sin", "sinh", "sqrt", "subset", "subseteq", "sum"
    , "supset", "supseteq"
    , "tilde", "times", "to", "toward"
    , "underbrace", "underline", "union", "uparrow"
    , "vec"
    ]

renderMathOperator :: T.Text -> T.Text
renderMathOperator t
  | t == "min" = "func min"
  | t == "max" = "func max"
  | isBareMathOperator t = t
  | shouldQuoteMathOperator t = "func " <> quoteText t
  | otherwise                 = "func " <> t

isBareMathOperator :: T.Text -> Bool
isBareMathOperator t =
  t `elem`
    [ "min", "max", "log", "sin", "cos", "cosh", "sinh"
    , "cot", "ln", "exp"
    ]

shouldQuoteMathOperator :: T.Text -> Bool
shouldQuoteMathOperator = not . T.all isLetter

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
    "ϵ" -> Just "epsilon"
    "ε" -> Just "varepsilon"
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
        else if isWrapped rendered
        then rendered
        else if isAtomic e
        then rendered
        else "{" <> rendered <> "}"

renderScriptArg :: DisplayType -> AlignContext -> Exp -> Maybe T.Text
renderScriptArg dt ctx e = do
  rendered0 <-
    if needsNeutralScriptOperands e
       then renderExpsIn dt ctx [e]
       else renderExpIn dt ctx e
  let rendered = T.strip rendered0
  pure $ if (isAtomic e && not (needsNeutralScriptOperands e)) || isQuotedText rendered
            then rendered
            else "{" <> rendered <> "}"

renderPrimeSuffix :: Exp -> Maybe T.Text
renderPrimeSuffix e =
  case e of
    ESymbol Pun "'"  -> Just "'"
    ESymbol Pun "′"  -> Just "'"
    ESymbol Pun "″"  -> Just "''"
    ESymbol Pun "‴"  -> Just "'''"
    _                -> Nothing

renderLimitArg :: DisplayType -> AlignContext -> Exp -> Maybe T.Text
renderLimitArg dt ctx e =
  case e of
    EGrouped xs -> do
      rendered <- renderExpsIn dt ctx xs
      let stripped = T.strip rendered
      pure $
        if T.null stripped
           then "{}"
           else if containsRelationLike xs
                   then stripped
           else if isQuotedText stripped || isWrapped stripped || T.length stripped == 1
                   then stripped
                   else "{" <> stripped <> "}"
    _           -> T.strip <$> renderExpIn dt ctx e

containsRelationLike :: [Exp] -> Bool
containsRelationLike = any isRelationLike
 where
  isRelationLike expn =
    case expn of
      ESymbol Rel _ -> True
      ESymbol Bin _ -> True
      EGrouped ys   -> containsRelationLike ys
      _             -> False

renderAccentArg :: Exp -> T.Text -> T.Text
renderAccentArg e rendered0 =
  let rendered = T.strip rendered0
  in if isAtomic e
        then rendered
        else "{" <> rendered <> "}"

renderPhantomArg :: Exp -> T.Text -> T.Text
renderPhantomArg e rendered0 =
  let rendered = T.strip rendered0
  in if isAtomic e
        then rendered
        else if isWrapped rendered
        then rendered
        else "{" <> rendered <> "}"

centerScriptArg :: T.Text -> T.Text
centerScriptArg rendered
  | isWrapped rendered = rendered
  | otherwise          = "{" <> rendered <> "}"

isWrapped :: T.Text -> Bool
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

isEmptyScriptArg :: Exp -> Bool
isEmptyScriptArg e =
  case e of
    EGrouped []   -> True
    EIdentifier t -> T.null t
    _             -> False

isBraceAnnotatedExp :: Exp -> Bool
isBraceAnnotatedExp e =
  case e of
    EOver _ _ over  -> braceAnnotationName over /= Nothing
    EUnder _ _ under -> braceAnnotationName under /= Nothing
    _               -> False

braceAnnotationName :: Exp -> Maybe T.Text
braceAnnotationName e =
  case e of
    ESymbol TOver "\9182"  -> Just "overbrace"
    ESymbol TOver "\9140"  -> Just "overbrace"
    ESymbol TUnder "\9183" -> Just "underbrace"
    ESymbol TUnder "\9141" -> Just "underbrace"
    _                       -> Nothing

underlineMarkerName :: Exp -> Maybe T.Text
underlineMarkerName e =
  case e of
    ESymbol TUnder "_" -> Just "underline"
    _                  -> Nothing

renderBraceLabel :: DisplayType -> AlignContext -> Exp -> Maybe T.Text
renderBraceLabel dt ctx e = do
  rendered0 <- renderExpIn dt ctx e
  let rendered = T.strip rendered0
  pure $ if isQuotedText rendered || isAtomic e
            then rendered
            else "{" <> rendered <> "}"

arrowScriptOpName :: Exp -> Maybe T.Text
arrowScriptOpName e =
  case e of
    ESymbol _ "←" -> Just "leftarrow"
    ESymbol _ "→" -> Just "toward"
    ESymbol _ "↔" -> Just "leftrightarrow"
    ESymbol _ "⇐" -> Just "dlarrow"
    ESymbol _ "⇒" -> Just "drarrow"
    ESymbol _ "⇔" -> Just "dlrarrow"
    ESymbol _ "↦" -> Just "mapsto"
    ESymbol _ "↑" -> Just "uparrow"
    ESymbol _ "↓" -> Just "downarrow"
    _             -> Nothing

accentName :: Exp -> Maybe T.Text
accentName e =
  case e of
    ESymbol Accent s -> accentFromChar s
    ESymbol TOver s  -> overAccentFromChar s
    ESymbol _ s      -> accentFromChar s
    _                -> Nothing

overAccentFromChar :: T.Text -> Maybe T.Text
overAccentFromChar s =
  case s of
    "\772"  -> Just "overline"
    "\8254" -> Just "overline"
    "¯"     -> Just "overline"
    _       -> accentFromChar s

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
    "\8230 " -> " dotslow "
    "… " -> " dotslow "
    "\8230" -> " dotslow "
    "…" -> " dotslow "
    "\8943" -> " dotsaxis "
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
    "\8810" -> " ll "
    "≪" -> " ll "
    "\8811" -> " gg "
    "≫" -> " gg "
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
    "<" -> " < "
    ">" -> " > "
    "=" -> " = "
    "," -> ", "
    ";" -> "; "
    ":" -> " : "
    "!" -> " ! "
    "'" -> "'"
    "′" -> "'"
    "″" -> "''"
    "‴" -> "'''"
    _   | isWordLiteralSymbol s -> quoteText s
        | otherwise             -> s

isWordLiteralSymbol :: T.Text -> Bool
isWordLiteralSymbol s =
  not (T.null s) && T.all isWordLiteralChar s

isWordLiteralChar :: Char -> Bool
isWordLiteralChar c =
  (c >= 'A' && c <= 'Z') ||
  (c >= 'a' && c <= 'z') ||
  c == ' ' || c == '-'

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
    ESymbol Op "\8751" -> Just "iiint"
    ESymbol Op "\8721" -> Just "sum"
    ESymbol Op "\8719" -> Just "prod"
    ESymbol Op "\8899" -> Just "oper ∪"
    ESymbol Op "\8898" -> Just "oper ∩"
    ESymbol Op "∫"     -> Just "int"
    ESymbol Op "∭"     -> Just "iiint"
    ESymbol Op "∑"     -> Just "sum"
    ESymbol Op "∏"     -> Just "prod"
    ESymbol Op "⋃"     -> Just "oper ∪"
    ESymbol Op "⋂"     -> Just "oper ∩"
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
    EMathOperator "det" -> Just "func det"
    EMathOperator "Pr"  -> Just "func Pr"
    EMathOperator "gcd" -> Just "func gcd"
    EMathOperator "lim" -> Nothing
    EMathOperator "liminf" -> Nothing
    EMathOperator "limsup" -> Nothing
    EMathOperator t     -> Just (renderMathOperator t)
    _                   -> Nothing
renderTextAtom :: TextType -> T.Text -> T.Text
renderTextAtom sty t =
  case sty of
    TextItalic    -> "ital " <> styleArg t
    TextBold      -> "bold " <> styleArg (quoteText t)
    TextMonospace -> "font fixed " <> quoteText t
    TextSansSerif -> "font sans " <> quoteText t
    _             -> quoteText t
