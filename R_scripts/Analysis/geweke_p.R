results_path <- "../Results/MCMC/FvH/"
mcmc_results <- list.files(results_path)
mcmc_results <- mcmc_results[grep(".RData",mcmc_results)]
geweke_p_mat <- matrix(nrow=6,ncol=156)
burnin <- 100000
for(j in 1:length(mcmc_results)){
    print(mcmc_results[j])
    load(file=paste(results_path,mcmc_results[j],sep=""))
    geweke_p_upper <- geweke.diag(samples$samples[burnin:dim(samples$samples)[1],])
    geweke_p_lower <- geweke.diag(samples$samples2[burnin:dim(samples$samples2)[1],])
    geweke_p_mat[j,] <- t(c(round(geweke_p_upper$z,2),round(geweke_p_lower$z,2)))
}

suspect_chains <- which(abs(geweke_p_mat) > 3,arr.ind=TRUE)
suspect_chains <- suspect_chains[order(suspect_chains[,1]),]
