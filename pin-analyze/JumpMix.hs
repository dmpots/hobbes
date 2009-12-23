module JumpMix where
import PinData
import Jumpcodes

type PinJumpData = GenCountData Jump


readJumpCount :: String -> (Jump, PinCounter)
readJumpCount line = read line

