#library(matrixcalc)
library(mvtnorm)

# set level of diagnostic output (0=none,1=brief,2=all)
diag_flag <- 3
prior_output <- NA
item_crf_output <- NA
posterior_output <- NA

# number of dimensions
ndim <- 4

# levels of theta to evaluate for each dimension
#theta <- seq(-4,4,by=.5)
theta <- seq(-2,2,by=1)
nt <- length(theta)

# Read item parameters from data file
#setwd("C:/Users/scott/Downloads")
setwd("/Users/shengzhang/Downloads/Dissertations/Results From Mike/Verification/2. First verification/Comparison Results")
item_par <- read.csv("Dissertation parameters.csv")

# number of items
nitems <- nrow(item_par)
#nitems <- 1

# create arrays of item parameters
apar <- array(c(item_par$a1,item_par$a2,item_par$a3,item_par$a4),dim=c(nitems,4)) # must be hard coded for number of dimensions
cpar <- as.matrix(item_par[, grep("c[0-9]+", names(item_par))])  # Dynamically extract all c-parameters

# Compute the maximum number of categories per item
maxCat <- rowSums(!is.na(cpar)) + 1
#maxCat <- apply(cpar, 1, function(x) sum(!is.na(x)) + 1) 

# set correlations among latent variables
# these are obtained from the MIRT calibration
R_prior <- diag(ndim)
#R_prior <- array(c(1,.88,.78,.88,1,.79,.78,.79,1),dim=c(ndim,ndim))
# set the number of persons to simulate

# Setup to compute posterior-weighted information for each item
quad_min <- -2
quad_max <- 2
nquad <- 5  # note nquad = 9 for 1.0 interval or 81 for .1 interval
quad_inc <- (quad_max - quad_min)/(nquad-1)
theta_value <- seq(quad_min,quad_max,by=quad_inc)


#Initialize prior distribution, posterior distrbution, and results
posterior <- array(0,dim=c(rep(nquad,ndim)))
prior <- array(0,dim=c(rep(nquad,ndim)))
results <- data.frame()

itembank<-(c(25,17,21,22,20))
Responseop<-(c(4,1,1,1,1))
# FUNCTIONS
#############
# Function to compute  the BRFs and CRFs for all items and categories, but for a single theta value
# This function uses the item intercept parameters c
GetIRF <- function (ap,cp,th)
{
  # ap: nitems x ndim array of discrimination parameters 
  # cp: nitems x ncat-1 array of category parameters
  # Parameters in slope/intercept form: logit = ap * th + cp
  # th: vector of current theta estimates

  # print(ap)
  # print(cp)
  # print(th)
  #  For debugging purposes
  #  ap <- apar[item,]
  #  cp <- cpar[item,]
  ndim <- ncol(th)
  maxCat <- ncol(cp)+1
  nItm <- max(nrow(ap),1)
  ncat <- maxCat - apply(cpar, 1, function(x) sum(as.numeric(is.na(x))))
  
  BndProb <- array(0, dim=c(nItm,max(ncat)+1))
  CatProb <- array(0,dim=c(nItm,max(ncat)))
  
  # Compute vector of Boundary Response Functions
  logit <- ap %*% th
  for (j in 1:nItm)
  {
    BndProb[j,1] <- 1
    for (k in 1:(ncat[j]-1))
    {
      BndProb[j,k+1] <- 1/(1+exp(-logit[j] - cp[j,k]))
    }
    BndProb [j,(ncat[j]+1)] <- 0.
    
    for (k in 1:ncat[j])
    {
      CatProb[j,k] <- BndProb[j,k] - BndProb[j,k+1]
    }
    
  }  # and of j loop
  
  # combine BRF and CRF into a list
  irf <- list(brf=BndProb, crf=CatProb)
  irf
  
}
# End of GetIRF function


# compute prior and use to initialize posterior
L <- array (1, dim=c(nt,nt,nt,nt))
sum_posterior <- 0
eap <- array (0, dim=ndim)

npersons <- 1
maxitems <- 1
response <- array(0, dim=c(rep(nt,ndim),npersons,maxitems))

#First Loop: Compute Prior Distribution  
for (th1 in 1:nquad)
{
  for (th2 in 1:nquad)
  {
    for (th3 in 1:nquad)
    {
      for (th4 in 1:nquad) 
      {
        th <- c(theta_value[th1],theta_value[th2],theta_value[th3],theta_value[th4])
        prior[th1,th2,th3,th4] <- dmvnorm(th,mean=rep(0, length=ndim),R_prior)
        
        
        # local_irf <- GetIRF(apar,cpar,th)
        
        if (diag_flag > 2)
        {
          txt <- sprintf("Th: %.1f %.1f %.1f %.1f Prior: %14.11f",theta_value[th1],theta_value[th2],theta_value[th3],theta_value[th4],posterior[th1,th2,th3,th4])
          #print(txt)
          prior_output <- rbind(prior_output,txt)
        }
      }
    }
  }
}

prior <- prior/sum(prior) 

# For the second round 
for (round in 1:5){
  
#Create a loop for all the items
for (item in 1:nitems) {
V <- vector("list", maxCat[item])
  for (i in 1:maxCat[item])
  {
    V[[i]] <- matrix(0,ndim,ndim)
  }
P_ChooseX <- rep (0, length(maxCat[item]))


## Create a loop for all the reponses
for (response in 1:maxCat[item]) {
    sum_marginal <- 0
  	sum_posterior <- 0 
  	posterior <- array(0,dim=c(rep(nquad,ndim)))
  	L <- array (1, dim=c(nt,nt,nt,nt))
  	eap <- array (0, dim=ndim)
# Second Loop: Compute Posterior with Local IRF
for (th1 in 1:nt) 
  {for (th2 in 1:nt) 
    {for (th3 in 1:nt) 
      {for (th4 in 1:nt) 
        {
        th <- c(theta_value[th1],theta_value[th2],theta_value[th3],theta_value[th4])
        local_irf <- GetIRF(apar,cpar,th)
        
        L[th1,th2,th3,th4] <- local_irf$crf[item,response]

        posterior [th1,th2,th3,th4] <- L[th1,th2,th3,th4] * prior[th1,th2,th3,th4]
        sum_posterior <- sum_posterior + posterior [th1,th2,th3,th4]
          
            
        }
      }
    }
  }
posterior <- posterior/sum_posterior

# Compute Marginal Probability of Response P_ChooseX

#Store the marginal probability for this response category
P_ChooseX[response] <- sum_posterior

sum_posterior <- 0

#Third Loop: Compute Expected A Posteriori (EAP)
for (th1 in 1:nt) 
  {for (th2 in 1:nt) 
    {for (th3 in 1:nt) 
      {for (th4 in 1:nt) 
        {th <- c(theta_value[th1],theta_value[th2],theta_value[th3],theta_value[th4])
        eap <- eap + th * posterior[th1,th2,th3,th4]
        sum_posterior <- sum_posterior + posterior [th1,th2,th3,th4]
        }
      }
    }
  }
eap <- eap/sum_posterior

# Fourth Loop: Compute Variance for Response Option


for (th1 in 1:nt) 
  {for (th2 in 1:nt) 
    {for (th3 in 1:nt) 
      {for (th4 in 1:nt) 
        {th <- c(theta_value[th1],theta_value[th2],theta_value[th3],theta_value[th4])
        V[[response]] <- V[[response]] + ((th - eap) %*% t(th - eap))* posterior[th1,th2,th3,th4]
        }
      }
    }
  }

V[[response]] <- V[[response]]/sum_posterior
} #End of response loop

# Normalize P(X = u) across all responses
P_ChooseX <- P_ChooseX / sum(P_ChooseX)

E_V <- array (0, dim=c(ndim,ndim))
for (response in 1:maxCat[item]){
	E_V <- E_V + P_ChooseX[response] * V[[response]]
}

Var_1 <- E_V[1,1]
Var_2 <- E_V[2,2]
Var_3 <- E_V[3,3]
Var_4 <- E_V[4,4]
Cov12 <- E_V[1,2]
Cov13 <- E_V[1,3]
Cov14 <- E_V[1,4]
Cov23 <- E_V[2,3]
Cov24 <- E_V[2,4]
Cov34 <- E_V[3,4]
Overall <- 0.48 * E_V[1,1] + 0.14 * E_V[2,2] + 0.13 * E_V[3,3] + 0.26 * E_V[4,4]
Domain1 <- 0.40^2 * E_V[1,1] + 0.60^2 * E_V[2,2] + 0.40 * 0.60 * E_V[1,2]
Domain2 <- 0.59^2 * E_V[1,1] + 0.41^2 * E_V[3,3] + 0.59 * 0.41 * E_V[1,3]
Domain3 <- 0.43^2 * E_V[1,1] + 0.57^2 * E_V[4,4] + 0.43 * 0.57 * E_V[1,4]
Total <- sum(Domain1,Domain2,Domain3)
results <- rbind(results, c(item, Overall,Domain1,Domain2,Domain3,Total,Var_1,Var_2,Var_3,Var_4,Cov12,Cov13,Cov14,Cov23,Cov24,Cov34))
}# End of item loop

write.csv(results, "composite Results2.csv", row.names = FALSE)
itm <- itembank[round]
resp <- Responseop[round]


sum_posterior <- 0

for (th1 in 1:nt) 
  {for (th2 in 1:nt) 
    {for (th3 in 1:nt) 
      {for (th4 in 1:nt) 
        {
  th <- c(theta_value[th1],theta_value[th2],theta_value[th3],theta_value[th4])
  local_irf <- GetIRF(apar,cpar,th)
  
  L[th1,th2,th3,th4] <- local_irf$crf[itm,resp]
  
  posterior [th1,th2,th3,th4] <- L[th1,th2,th3,th4] * prior[th1,th2,th3,th4]
  sum_posterior <- sum_posterior + posterior [th1,th2,th3,th4]
  
  
  if (diag_flag > 2)
  {
    txt <- sprintf("Th: %.1f %.1f %.1f %.1f posterior: %14.11f",theta_value[th1],theta_value[th2],theta_value[th3],theta_value[th4],posterior[th1,th2,th3,th4])
    #print(txt)
    posterior_output <- rbind(posterior_output,txt)
    
    txt <- sprintf ("Th: %.1f %.1f %.1f %.1f, %8s, CRF: %10.8f  %10.8f %10.8f %10.8f %10.8f",
                    theta_value[th1],theta_value[th2],theta_value[th3],theta_value [th4],item_par$Item[item],
                    local_irf$crf[item,1],local_irf$crf[item,2],local_irf$crf[item,3],local_irf$crf[item,4],local_irf$crf[item,5])
    #print(txt)
    item_crf_output <- rbind(item_crf_output,txt)
  }
        }
      }
    }
  }

prior <- posterior/sum_posterior
}# end of round loop
