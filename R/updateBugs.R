# if a previously obtained JAGS model is not converged, or needs a longer chain, then use this funciton to update it

updateBugs <- function(jagsmodel,burnMore=1000,sampleMore=1000,thin=1){
  if( class(jagsmodel) == "bugs"){
    stop("WINBUGS model is not supported, please provide a JAGS model.")
  }
  
  oldModel <- jagsmodel$model

  update(oldModel, n.iter= burnMore)

  jags_samples <- rjags::coda.samples(model=oldModel,
                                       variable.names=jagsmodel$parameters.to.save,
                                       n.iter=sampleMore,
                                       thin=thin)

  png(paste0(jagsmodel$folder,"/summary_plot_update_",as.numeric(Sys.time()),".png"),width=60,height=60,units="cm",res=600)
  par(mfrow=c(ncol(jags_samples[[1]])/3+1,6))
  plot(jags_samples,auto.layout=FALSE)
  dev.off()
  
  fit <- mcmc2bugs(jags_samples, model.file = jagsmodel$model.file, program = "jags", 
                   DIC = jagsmodel$DIC, DICOutput = NULL, 
                   n.iter = sampleMore, n.burnin = 0, 
                   n.thin = thin)
  
  out <- list(model = oldModel, BUGSoutput = fit,mcmc.list = jags_samples,
              parameters.to.save = jagsmodel$parameters.to.save, 
              model.file = jagsmodel$model.file, 
              n.iter = sampleMore, DIC = jagsmodel$DIC)
  class(out) <- "rjags"
  
  return(out)
}


