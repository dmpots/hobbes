######################################################
# DATA
######################################################
#x <- read.table("nofib.dat", header=TRUE, row.names=1) 
#datatitle <- "Nofib"
x <- read.table("fibon.dat", header=TRUE, row.names=1) 
datatitle <- "Fibon"


######################################################
# FUNCTIONS
######################################################
ellipse <- function(x0, a, y0, b) {
  theta <- seq(0, 2 * pi, length=100)
  xs <- x0 + a * cos(theta)
  ys <- y0 + b * sin(theta)
  lines(xs, ys, col="red")
}

#label points
#library(calibrate)
#textxy(x$UNHALTED_CORE_CYCLES, 
#       x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
#       labs=labels(x)[[1]])

######################################################
# GRAPHS
######################################################

#
# ICache vs. Correlation
#
plot(x$BR_MISSP_EXEC/x$L1I_MISSES, 
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Correlation (Branch Mispredictions / I-Cache Misses)", 
     ylab="I-Cache Stall% (of Total Execution Time)")
title(paste(datatitle, "I-Cache Stall% vs. Correlation"))

ellipse(10, 20, 0.4, 0.4)

plot(x$BR_MISSP_EXEC/x$L1I_MISSES, 
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Correlation (Branch Mispredictions / I-Cache Misses)", 
     ylab="I-Cache Stall% (of Total Execution Time)",
     xlim=c(0,10))
title(paste(datatitle, "I-Cache Stall% vs. Correlation -- Zoomed"))


#
# ICache vs. TotalCycles
#
plot(x$UNHALTED_CORE_CYCLES,
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Total Cycles", 
     ylab="I-Cache Stall% (of Total Execution Time)")
title(paste(datatitle, "I-Cache Stall% vs. Total Cycles"))

ellipse(3e8, 5e8, 0.4, 0.4)

plot(x$UNHALTED_CORE_CYCLES,
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Total Cycles", 
     ylab="I-Cache Stall% (of Total Execution Time)",
     xlim=c(0,1e9))
title(paste(datatitle, "I-Cache Stall% vs. Total Cycles -- Zoomed"))

# hpg == 40
# maillist == 51

plot(x$UNHALTED_CORE_CYCLES,
     x$BR_MISSP_EXEC/x$UNHALTED_CORE_CYCLES,
     xlab="Total Cycles", 
     ylab="Number of Branch Mispredictions",
     xlim=c(0,5e8))
title(paste(datatitle, "Branch Mispredictions vs. Total Cycles"))


