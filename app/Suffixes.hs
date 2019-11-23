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

plural = suffixSC (enumFrom R.S) [R.S] "{^s}"
pluralPosessive = suffixSC (enumFrom R.S) [R.S, R.Z] "{^s}{^}'"

contractLL = suffixSC (enumFrom R.L) [R.L]      "'ll"
contractM  = suffixSC (enumFrom R.P) [R.P, R.L] "'m"
contractS  = suffixSC [R.T, R.Z] [R.Z] "'s"
contractD  = suffixSC [R.S, R.D] [R.D] "'d"
contractT  = suffixSC (enumFrom R.T) [R.T] "'t"

finalApostrophe = suffixSC [R.T, R.Z] [R.Z] "'"

ing = (\n -> n ++ "{^ing}", \ss -> ss <> [stk R.G])
ed  = suffixSC (R.S : enumFrom R.D) [R.D] "{^ed}"

combine (nMod1, sMod1) (nMod2, sMod2) = (nMod2 . nMod1, sMod2 . sMod1)

ly =
  let ender = \ss ->
        if ((\(Stroke _ v r _) -> length ss > 1
                               && S.notMember R.L r
                               && S.null v) $ last ss)
          then init ss ++ ([last ss <> stks [stk R.L, stk A, stk E]])
          else ss ++ [stks [stk R.L, stk A, stk E]]
  in (\n -> n ++ "{^ly}", ender)
