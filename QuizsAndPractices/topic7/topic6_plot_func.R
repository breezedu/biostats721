
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ BIOS 721| Topic 6 Part 3: Simulation Studies in R ~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Result Plot Function Script ------------------------------

plot.func <- function(prop.out,t1.data,t2.data,t3.data,
                      main.title,y.lab,y.lim=NULL,
                      legend=FALSE,x.leg=NULL,y.leg=NULL) {
     
  if (is.null(y.lim)) {y.lim <- range(t1.data,t2.data,t3.data)}
     
     plot(prop.out,t1.data,
          ylim=y.lim,
          ylab=y.lab,main=main.title,
          xlab='Proportion of Outliers in Sample',
          type='l',col='blue',lwd=2)
     lines(prop.out,t2.data,col='magenta',lwd=2)
     lines(prop.out,t3.data,col='orange',lwd=2)
     if(legend==TRUE) {
          legend(x.leg,y.leg,
                 legend=c('Mean','20% Trim Mean','Median'),
                 lty=rep(1,3),
                 col=c('blue','magenta','orange'),cex=0.7)
     }
}

# --------------------------------------------------------
# End of Program

