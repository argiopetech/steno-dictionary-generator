module Suffixes where

import Stroke
import Keys
import qualified Keys.Left as L
import qualified Keys.Right as R

import qualified Data.Set as S


suffix k s = suffixS [k] s

suffixS ks s = suffixSC ks ks s

suffixSC conflicts ks s =
  let ender = \ss ->
        if ((\(Stroke _ _ r _) -> any (\k -> S.member k r) conflicts) $ last ss)
          then ss ++ [stks ks]
          else init ss ++ ([last ss <> stks ks])
  in (\n -> n ++ s, ender)

plural = suffixSC [R.S, R.D] [R.S] "{^s}"
pluralPosessive = suffixSC [R.S, R.Z, R.T, R.D] [R.S, R.Z] "s'"

contractLL = suffix R.L "'ll"
contractM = suffixS [R.P, R.L] "'m"
contractS = suffixSC [R.T, R.Z] [R.Z] "'s"
contractD = suffixSC [R.S, R.D] [R.D] "'d"

ing = suffix R.G "{^ing}"
ed = suffix R.D "{^ed}"

ly =
  let ender = \ss ->
        if ((\(Stroke _ v r _) -> S.member R.L r
                               || and [S.member A v, S.member E v]) $ last ss)
          then ss ++ [stks [stk R.L, stk A, stk E]]
          else init ss ++ ([last ss <> stks [stk R.L, stk A, stk E]])
  in (\n -> n ++ "{^ly}", ender)
