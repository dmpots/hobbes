module BBLengthMix where
import PinData

type BBLength = Int
type PinBBLengthData = GenCountData BBLength

readBBLengthCount :: String -> (BBLength, PinCounter)
readBBLengthCount line = read line 

