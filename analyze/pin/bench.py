


def group(benchmark):
    if benchmark in spec:
        return spec[benchmark]
    elif benchmark in fibon:
        return fibon[benchmark]
    else:
        return 'Unknown'

fibon = dict([
    ("Agum"        , "Hackage"),
    ("BinaryTrees" , "Shootout"),
    ("Blur"        , "Repa"),
    ("Bzlib"       , "Hackage"),
    ("Chameneos"   , "Shootout"),
    ("Cpsa"        , "Hackage"),
    ("Crypto"      , "Hackage"),
    ("Dotp"        , "Dph"),
    ("FFT2d"       , "Repa"),
    ("FFT3d"       , "Repa"),
    ("Fannkuch"    , "Shootout"),
    ("Fgl"         , "Hackage"),
    ("Fst"         , "Hackage"),
    ("Funsat"      , "Hackage"),
    ("Gf"          , "Hackage"),
    ("HaLeX"       , "Hackage"),
    ("Happy"       , "Hackage"),
    ("Hgalib"      , "Hackage"),
    ("Laplace"     , "Repa"),
    ("MMult"       , "Repa"),
    ("Mandelbrot"  , "Shootout"),
    ("Nbody"       , "Shootout"),
    ("Palindromes" , "Hackage"),
    ("Pappy"       , "Hackage"),
    ("Pidigits"    , "Shootout"),
    ("Qsort"       , "Dph"),
    ("QuickCheck"  , "Hackage"),
    ("QuickHull"   , "Dph"),
    ("Regex"       , "Hackage"),
    ("Simgi"       , "Hackage"),
    ("SpectralNorm", "Shootout"),
    ("Sumsq"       , "Dph"),
    ("TernaryTrees", "Hackage"),
    ("Xsact"       , "Hackage")
    ])


spec = dict([
    ( "400.perlbench" , "SPECint"),
    ( "401.bzip2"     , "SPECint"),
    ( "403.gcc"       , "SPECint"),
    ( "429.mcf"       , "SPECint"),
    ( "445.gobmk"     , "SPECint"),
    ( "456.hmmer"     , "SPECint"),
    ( "458.sjeng"     , "SPECint"),
    ( "462.libquantum", "SPECint"),
    ( "464.h264ref"   , "SPECint"),
    ( "471.omnetpp"   , "SPECint"),
    ( "473.astar"     , "SPECint"),
    ( "483.xalancbmk" , "SPECint"),
    ( "999.specrand"  , "SPECfp"),
    ( "410.bwaves"    , "SPECfp"),
    ( "416.gamess"    , "SPECfp"),
    ( "433.milc"      , "SPECfp"),
    ( "434.zeusmp"    , "SPECfp"),
    ( "435.gromacs"   , "SPECfp"),
    ( "436.cactusADM" , "SPECfp"),
    ( "437.leslie3d"  , "SPECfp"),
    ( "444.namd"      , "SPECfp"),
    ( "447.dealII"    , "SPECfp"),
    ( "450.soplex"    , "SPECfp"),
    ( "453.povray"    , "SPECfp"),
    ( "454.calculix"  , "SPECfp"),
    ( "459.GemsFDTD"  , "SPECfp"),
    ( "465.tonto"     , "SPECfp"),
    ( "470.lbm"       , "SPECfp"),
    ( "481.wrf"       , "SPECfp"),
    ( "482.sphinx3"   , "SPECfp"),
    ( "998.specrand"  , "SPECfp"),
    ])
