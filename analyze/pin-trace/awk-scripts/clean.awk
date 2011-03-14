# Clean up a haskell trace
#   1. Replace the executable name by "exe"
#   2. Replace a non-haskell library by _
#   3. Replace a full libHSfoo-x.y.z.so by foo
#
# Input Record Format (one field):
# <shared-library-name>

$1 ~ /^(Dotp|QsortDph|quickhull|sumsq|agum|hsbzip|cpsa|crypto|fgl|fst|funsat|gf|halex|happy|ga|palindromes|java-parser|qc|pderiv|simgi|tdict|xsact|repa-blur|repa=fft2d|repa-fft3d|repa-laplace|repa-mmult|binarytrees|chameneosredux|fannkuch|mandelbrot|nbody|pidigits|spectralnorm)$/ {$1 = "exe"}

$1 !~ /^(libHS|exe)/ {$1 = "_"}

# replace libHSfoo-x.y.z.so by libHSfoo
# with special cases for rts and ffi libraries
$1 ~ /^libHSrts/     {$1 = "libHSrts"}
$1 ~ /^libHSffi/     {$1 = "libHSffi"}
$1 ~ /^libHS/        {sub(/-[[:digit:]].*/, "", $1);}

#get rid of the leading libHS
{sub(/libHS/, "", $1);}
  

#print out the line
{print $0}

