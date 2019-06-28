library(xtable)
library(foreign)
library(fields)
require(devtools)
set.seed(854604)

install_version("rdrobust", version = "0.93", repos = "http://cran.us.r-project.org")
library(rdrobust)

source("wbwp_fl_distance-functions.R")

pointsALL <- read.csv("wbwp_fl_boundary_points.csv")
pointsALL$latitude= pointsALL$Y
pointsALL$longitude= pointsALL$X

# define outcomes
outcomes <- c("v2000","v2004","v2008","v2012")

# define points
points = pointsALL

# fixed bandwidth
hfix = 3
hlist =  vector("list", length(outcomes))
for(j in 1:length(outcomes)) hlist[[j]] = rep(hfix, 7)

## Set up parameters for estimation
dislimit10per = 1.00
dislimitmin   = 0.50 
minobs = 10
np = nrow(points)

colnm2 = c("Point", "Estimate", "Conv-CIl", "Conv-CIu", "hfix", "Ntr-hfix", 
           "Nco-hfix", "Rob-CIl",  "Rob-CIu","hCCT", "Ntr-hCCT","Nco-hCCT")

finaltab = as.data.frame(matrix(data=NA, ncol = length(colnm2), nrow = np , 
                                dimnames = list(NULL, colnm2)))

data <- read.csv("wbwp_fl_voting_data.csv")
results <- list()

for(j in 1:length(outcomes)) {
  
  t0 = proc.time()[3]
  cat("/******************************************************************************\n")
  cat("Starting estimation of variable", outcomes[j], "\n")
  cat("/******************************************************************************\n")    
  ## Set up data
  datause = data[complete.cases(data$latitude, data$longitude, data$treat,data[,c(outcomes[j])]),]
  
  ## Get covariates: latitude and longitude
  x1 <- datause$latitude
  x2 <- datause$longitude
  # Get outcome
  y <- datause[,c(outcomes[j])]
  treat <- datause$treat
  
  ## Set up treated data
  indx = (treat == 1)
  sum(indx)
  x1.Tr <- x1[indx]
  x2.Tr <- x2[indx]
  y.Tr  <- y[indx]
  rm(indx)
  gc()
  
  ## Set up control data
  indx = (treat == 0)
  sum(indx)
  x1.Co <- x1[indx]
  x2.Co <- x2[indx]
  y.Co  <- y[indx]
  rm(indx)
  gc()
  
  for(i in 1:np) {
    cat("Point", i, "of", np, "\n")
    b1 = points$latitude[i]
    b2 = points$longitude[i]
    h = hlist[[j]][i]
    
    
    ####################################
    #### Calculate score in treated area
    ####################################
    
    ## Calculate chordal distance between each (x1,x2) latitude-longitude pair 
    # in the data and the point of the boundary (b1,b2) where we are evaluating
    outdis = disvec(lat1=b1, lon1=b2, lat2 = x1.Tr, lon2 = x2.Tr)
    dis= as.vector(outdis$chord)
    summary(dis)  
    disTr = dis
    cat("Minimum treated distance is ",min(dis), "\n")
    
    ## Create weights using triangular kernel and fixed bandwidth 
    w = as.vector((1/h) * Kt((dis-0)/h))     #  w = 1 * (dis>=-h & dis <= h)
    wTr=w
    npTr = sum(w>0)
    cat("There are", npTr, "Tr observations with positive weights out of", length(y.Tr), "Tr obs \n")  
    
    ####################################
    #### Calculate score in control area
    ####################################
    outdis = disvec(lat1=b1, lon1=b2, lat2 = x1.Co, lon2 = x2.Co)
    dis= as.vector(outdis$chord)
    summary(dis)  
    disCo = dis
    cat("Minimum control distance is ",min(dis), "\n")   
    
    ## Create weights using triangular kernel and fixed bandwidth of 0.5 kilometers
    w = as.vector((1/h) * Kt((dis-0)/h))     #  w = 1 * (dis>=-h & dis <= h)
    wCo=w
    npCo = sum(w>0)
    cat("There are", npCo, "Co observations with positive weights out of", length(y.Co), "Co obs \n")  
    
    score = c(disTr, -disCo)
    y = c(y.Tr, y.Co)
    
    if(npTr >= minobs & npCo >= minobs) {
      rdout = rdrobust(y = y , x = score , c = 0 , h = h)
      print(rdout)
      rdout.Coef = summary(rdout)[[2]]
      
      rdoutCCT = rdrobust(y = y ,x = score , c = 0 , bwselect = "mserd")
      print(rdoutCCT)
      hCCT = rdoutCCT$bws[1]          
      rdoutCCT.Coef = summary(rdoutCCT)[[2]]
      
      rdoutIK  = rdrobust(y = y ,x = score , c = 0 , bwselect = "cerrd")
      print(rdoutIK)
      hIK = rdoutIK$bws[1]
      rdoutIK.Coef = summary(rdoutIK)[[2]]
    }
    
    ################################
    # Calculate and save results
    #################################
    finaltab[i,"Point"]         = i 
    finaltab[i, "Estimate"]     = round(rdoutCCT.Coef["Conventional",1], 2)
    finaltab[i, "Conv-CIl"]     = round(rdout.Coef["Conventional",5], 2)
    finaltab[i,  "Conv-CIu"]    = round(rdout.Coef["Conventional",6], 2)
    finaltab[i, "hfix"]         = hfix
    finaltab[i, "Ntr-hfix"]     = npTr
    finaltab[i, "Nco-hfix"]     = npCo
    finaltab[i, "Rob-CIl"]      = round(rdoutCCT.Coef["Robust",5], 2)
    finaltab[i, "Rob-CIu"]      = round(rdoutCCT.Coef["Robust",6], 2)
    finaltab[i,"hCCT"]          = round(hCCT, 2)
  }
  
  cat("\n\n\n")    
  cat("-------------------------------------------------------\n")
  cat("Final results for outcome", outcomes[j], "\n")
  print(finaltab)
  cat("-------------------------------------------------------\n")    
  
  results[[outcomes[j]]] <- finaltab
  
  write.csv(finaltab, paste("wbwp_fl_estimates_", outcomes[j], ".csv", sep = ""))
  
  
}  
