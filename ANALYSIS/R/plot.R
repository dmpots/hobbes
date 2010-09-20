######################################################
# DATA
######################################################

qH <- 4.75
qW <- 6.25
qF <- "Times"
qT <- "pdf"

#label points
#library(calibrate)
#textxy(x$UNHALTED_CORE_CYCLES, 
#       x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
#       labs=labels(x)[[1]])

######################################################
# GRAPHS
######################################################

doPlots <- function(x, datatitle){
#
# ICache vs. Correlation
#
fName     <- paste(datatitle, "-Icache-correlation-full", ".pdf", sep="")
quartz(height=qH, width=qW, family=qF, type=qT, file=fName)
plot(x$BR_MISSP_EXEC/x$L1I_MISSES, 
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Correlation (Branch Mispredictions / I-Cache Misses)", 
     ylab="I-Cache Stall% (of Total Execution Time)")
q3 <- summary(x$BR_MISSP_EXEC/x$L1I_MISSES)[5]
abline(v=q3, col="red")
text(q3, 0.4, "Q3", pos=4)
title(paste(datatitle, "I-Cache Stall% vs. Correlation"))
dev.off()

fName     <- paste(datatitle, "-Icache-correlation-zoom", ".pdf", sep="")
quartz(height=qH, width=qW, family=qF, type=qT, file=fName)
plot(x$BR_MISSP_EXEC/x$L1I_MISSES, 
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Correlation (Branch Mispredictions / I-Cache Misses)", 
     ylab="I-Cache Stall% (of Total Execution Time)",
     xlim=c(0,q3))
title(paste(datatitle, "I-Cache Stall% vs. Correlation -- Upto Q3"))
dev.off()

#
# ICache vs. TotalCycles
#
fName     <- paste(datatitle, "-Icache-cycles-full", ".pdf", sep="")
quartz(height=qH, width=qW, family=qF, type=qT, file=fName)
plot(x$UNHALTED_CORE_CYCLES,
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Total Cycles", 
     ylab="I-Cache Stall% (of Total Execution Time)")
title(paste(datatitle, "I-Cache Stall% vs. Total Cycles"))
q3 <- summary(x$UNHALTED_CORE_CYCLES)[5]
abline(v=q3, col="red")
text(q3, 0.4, "Q3", pos=4)
dev.off()


fName     <- paste(datatitle, "-Icache-cycles-zoom", ".pdf", sep="")
quartz(height=qH, width=qW, family=qF, type=qT, file=fName)
plot(x$UNHALTED_CORE_CYCLES,
     x$CYCLES_L1I_MEM_STALLED/x$UNHALTED_CORE_CYCLES, 
     xlab="Total Cycles", 
     ylab="I-Cache Stall% (of Total Execution Time)",
     xlim=c(0,summary(x$UNHALTED_CORE_CYCLES)[5]))
title(paste(datatitle, "I-Cache Stall% vs. Total Cycles -- Upto Q3"))
dev.off()
}


######################################################
# MAIN
######################################################
nofib <- read.table("nofib.dat", header=TRUE, row.names=1)
doPlots(nofib, "Nofib")
fibon <- read.table("fibon.dat", header=TRUE, row.names=1)
doPlots(fibon, "Fibon")

