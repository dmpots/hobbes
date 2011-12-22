module RegMix where
import PinData
import Regcodes

type PinRegData = GenCountData Reg

readRegCount :: String -> (Reg, PinCounter)
readRegCount line = read line

