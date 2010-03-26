module PapiMix where
import PinData
import Papicodes

type PapiData = GenCountData PapiEvent

readPapiCount :: String -> (PapiEvent, PinCounter)
readPapiCount = read


