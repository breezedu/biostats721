
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Topic 7 Part 1: Reproducible Research in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create Simulation Results --------------------------------------------------------

rm(list=ls())

# Name Working Directories: 
# - Point R to the directory where the simulation output 
#   files are stored 
# - You should change this to where you stored these files
# - Make sure to use \\ or / in the file path and not \
in_dir <- ''        # Set to location of .RData file from Topic 6 Part 3
out_dir <- ''       # Set to location where you want to save report doc

# Load the R Workspace with the simulation results
# - Should sit the fit objects listed in the workspace
#   after running the load() command
load(paste(in_dir,'sim_allns_fitobjects.RData',sep=''))

# Read in the script file containing the plotting function
source(paste(in_dir,'topic6_plot_func.R',sep=''))

# Exporting Figures ---------------------------------------------------------------

# Option 1: By "hand"

par(mfcol=c(2,3))

# (1) Bias for n = 50 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n50$bias.t1,
          t2.data=fit.n50$bias.t2,
          t3.data=fit.n50$bias.t3,
          main.title='For n = 50',
          y.lab='Bias',
          y.lim=c(-0.01,0.70),
          legend=TRUE,
          x.leg=0,
          y.leg=0.65)

# (2) MSE for n = 50
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n50$mse.t1,
          t2.data=fit.n50$mse.t2,
          t3.data=fit.n50$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

# (3) Bias for n = 100 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n100$bias.t1,
          t2.data=fit.n100$bias.t2,
          t3.data=fit.n100$bias.t3,
          main.title='For n = 100',
          y.lab='Bias',
          y.lim=c(-0.01,0.70))

# (4) MSE for n = 100
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n100$mse.t1,
          t2.data=fit.n100$mse.t2,
          t3.data=fit.n100$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

# (5) Bias for n = 150 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n150$bias.t1,
          t2.data=fit.n150$bias.t2,
          t3.data=fit.n150$bias.t3,
          main.title='For n = 150',
          y.lab='Bias',
          y.lim=c(-0.01,0.70))

# (6) MSE for n = 150
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n150$mse.t1,
          t2.data=fit.n150$mse.t2,
          t3.data=fit.n150$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

# Option 2: Export to an image file 

png(file=paste(out_dir,"Figure.png",sep=''),width=9,height=7,units="in",res=300)

par(mfcol=c(2,3))

# (1) Bias for n = 50 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n50$bias.t1,
          t2.data=fit.n50$bias.t2,
          t3.data=fit.n50$bias.t3,
          main.title='For n = 50',
          y.lab='Bias',
          y.lim=c(-0.01,0.70),
          legend=TRUE,
          x.leg=0,
          y.leg=0.65)

# (2) MSE for n = 50
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n50$mse.t1,
          t2.data=fit.n50$mse.t2,
          t3.data=fit.n50$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

# (3) Bias for n = 100 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n100$bias.t1,
          t2.data=fit.n100$bias.t2,
          t3.data=fit.n100$bias.t3,
          main.title='For n = 100',
          y.lab='Bias',
          y.lim=c(-0.01,0.70))

# (4) MSE for n = 100
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n100$mse.t1,
          t2.data=fit.n100$mse.t2,
          t3.data=fit.n100$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

# (5) Bias for n = 150 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n150$bias.t1,
          t2.data=fit.n150$bias.t2,
          t3.data=fit.n150$bias.t3,
          main.title='For n = 150',
          y.lab='Bias',
          y.lim=c(-0.01,0.70))

# (6) MSE for n = 150
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n150$mse.t1,
          t2.data=fit.n150$mse.t2,
          t3.data=fit.n150$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

dev.off()


# Option 3a: Export to a word file 
# install.packages('rtf')
library(rtf)

make.plot <- function(){
par(mfcol=c(2,3))

# (1) Bias for n = 50 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n50$bias.t1,
          t2.data=fit.n50$bias.t2,
          t3.data=fit.n50$bias.t3,
          main.title='For n = 50',
          y.lab='Bias',
          y.lim=c(-0.01,0.70),
          legend=TRUE,
          x.leg=0,
          y.leg=0.65)

# (2) MSE for n = 50
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n50$mse.t1,
          t2.data=fit.n50$mse.t2,
          t3.data=fit.n50$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

# (3) Bias for n = 100 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n100$bias.t1,
          t2.data=fit.n100$bias.t2,
          t3.data=fit.n100$bias.t3,
          main.title='For n = 100',
          y.lab='Bias',
          y.lim=c(-0.01,0.70))

# (4) MSE for n = 100
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n100$mse.t1,
          t2.data=fit.n100$mse.t2,
          t3.data=fit.n100$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70))

# (5) Bias for n = 150 
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n150$bias.t1,
          t2.data=fit.n150$bias.t2,
          t3.data=fit.n150$bias.t3,
          main.title='For n = 150',
          y.lab='Bias',
          y.lim=c(-0.01,0.70))

# (6) MSE for n = 150
plot.func(prop.out=seq(0.0,0.20,by=0.01),
          t1.data=fit.n150$mse.t1,
          t2.data=fit.n150$mse.t2,
          t3.data=fit.n150$mse.t3,
          main.title='',
          y.lab='MSE',
          y.lim=c(-0.01,0.70)) }

make.plot()

# - Create Word doc file: 
output<-paste(out_dir,"figure_option3a.doc",sep='')

rtf<-RTF(output,width=8.5,height=11,font.size=12,omi=rep(0.5,4))

addHeader(rtf,title="Simulation Study Summary Report")

addText(rtf,"Background| ",bold=TRUE)
addParagraph(rtf,"Performed a simulation study to compare the performance of three estimators (mean, trimmed mean, and median) of the population mean by comparing their BIAS and MSE.\n\n")

addText(rtf,"Results| ",bold=TRUE)
addParagraph(rtf,"See figure below for BIAS and MSE results. \n\n")

addPlot(rtf,plot.fun=make.plot,width=7,height=5,res=300)
addText(rtf,'\n\n')

addText(rtf,'Conclusions| ',bold=TRUE)
addParagraph(rtf,'Mean is superior when proportion of outliers is low (< 5%), but the median becomes superior as the proportion increases. However, for higher proportions the performance of all three estimators suffers. \n')

done(rtf)

# Option 3b: Weave R code/output with LaTeX to create pdf file
# - Compile code in figure_option3b.rnw

# Option 3c: Export to a word or pdf or HTML or .. file 
# - Compile code in figure_option3c.Rmd

# --------------------------------------------------------------------------------
# End of Program 


