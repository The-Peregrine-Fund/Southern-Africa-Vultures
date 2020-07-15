model{  
  #### Priors ###### 
  mean.r ~ dnorm(0, 0.001)
  mean.lambda <- exp(mean.r)
  tau.proc <-  1/(sig.proc*sig.proc)
  sig.proc ~ dunif(0,10)
  tau.obs <-  1/(sig.obs*sig.obs)
  sig.obs ~ dunif(0,10)
  
  #### data model ###### 
  for(k in 1 : ncounts) { 	
    lN.est[k] <- lmu.N[yr[k],str[k]]
    logtheta[k] ~ dnorm (lN.est[k], tau.obs)
    log(theta[k]) <- logtheta[k]
    count[k] ~ dpois(theta[k]) 
  }  # k
  
  for(s in 1 : nstrata) { 
    #### baseline year and strata expectations ######
    lmn1st[s] <- log(mn1st[s]+0.1) # add a small constant to prevent log(zero) which will result in failure to run
    lmu.N[1,s] ~ dnorm(lmn1st[s], 0.1) # priors for 1st yr abundance are roughly near observed values
    #### dynamics ######
    for( y in startyear[s] : (endyear[s]-1)) { 
      lmu.N[y+1,s] <- lmu.N[y,s]+ r[y,s] 
      r[y,s] ~ dnorm(mean.r, tau.proc)
      lambda[y,s] <- exp(r[y,s])
    } # y
  } # s
  
} #model end

