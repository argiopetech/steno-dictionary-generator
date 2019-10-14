module Suffixes where

import Stroke
import Keys
import qualified Keys.Left as L
import qualified Keys.Right as R

import qualified Data.Set as S


suffix k s = suffixS [k] s

suffixS ks s =
  let ender = \ss ->
        if ((\(Stroke _ _ r _) -> any (\k -> S.member k r) ks) $ last ss)
          then ss ++ [stks ks]
          else init ss ++ ([last ss <> stks ks])
  in (\n -> n ++ s, ender)

plural = suffix R.S "{^s}"
pluralPosessive = suffixS [R.S, R.Z] "s'"

contractLL = suffix R.L "'ll"
contractM = suffixS [R.P, R.L] "'m"
contractS = suffix R.Z "'s"

ing = suffix R.G "{^ing}"
ed = suffix R.D "{^ed}"



