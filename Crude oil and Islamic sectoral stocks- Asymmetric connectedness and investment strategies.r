## References
## https://sites.google.com/view/davidgabauer/econometric-code?authuser=0
## https://sites.google.com/site/fk83research/code?authuser=0

#-------------------------------------
# clear memory
rm(list=ls())    
#----------------------------------

## Packages ##
#----------------------------------
# Source the package setup script
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Compliance-Carbon-Markets"
setwd(Git)
source("Packages.R")

data("aaacgo2022")
Y = Yp = Yn = aaacgo2022[-1,]
k = ncol(Y)
for (i in 1:k) {
  x = embed(as.numeric(aaacgo2022[,i]),2)
  Y[,i] = Yp[,i] = Yn[,i] = 100*(x[,1]-x[,2])/x[,2]
  Yp[which(Y[,i]<0),i] = 0
  Yn[which(Y[,i]>0),i] = 0
}
Y_list = list(Y, Yp, Yn)


DCA = list()
spec = c("all", "positive", "negative")
for (i in 1:length(Y_list)) {
  DCA[[i]] = suppressMessages(ConnectednessApproach(Y_list[[i]], 
                              model="TVP-VAR",
                              connectedness="Time",
                              nlag=1,
                              nfore=10,
                              window.size=200,
                              VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1))))
  kable(DCA[[i]]$TABLE)
}

PlotTCI(DCA[[1]], ca=list(DCA[[2]], DCA[[3]]), ylim=c(50,100))

kable(DCA[[1]]$TABLE)
kable(DCA[[2]]$TABLE)
kable(DCA[[3]]$TABLE)
