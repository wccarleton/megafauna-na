nbCode <- nimbleCode({
   B0 ~ dnorm(mean=0,sd=100)
   B_1 ~ dnorm(mean=0,sd=100)
   B_2 ~ dnorm(mean=0,sd=100)
   Delta ~ dunif(1,N)
   ind[1:N] <- nimbleIndVector(N,Delta)
   for (n in 1:N) {
      lambda[n] <- exp(B0 + X[n] * (B_1 + (B_2 * ind[n]) ) )
      alpha[n] ~ dunif(1e-10,1-1e-10)
      Y[n] ~ dnegbin(size=lambda[n],prob=alpha[n])
      #y[n] ~ dnegbin(size=lambda[n],prob=alpha[n])
   }
})

#DATA
#sp <- "mfauna"
load(paste("../Data/rects_sample_",sp,".RData",sep=""))
rece_mfauna <- get(paste("rects_sample_",sp,sep=""))

##temporal overlap
trange <- c(11000,20000)#range(rece_mfauna[,1])

##RECTS

rece_sample_mfauna <- merge(data.frame(Date=trange[1]:trange[2]),rece_mfauna,all.x=T,by=1)
Y <- as.matrix(rece_sample_mfauna[,20])
Y[which(is.na(Y))] <- 0
X <- seq(0,diff(trange))
N <- length(Y)

#subsample for faster computing
Nsub <- seq(1,N,10)

nbData <- list(Y=as.vector(Y[Nsub]),
                X=X[Nsub])

nbConsts <- list(N=length(Nsub))

nbInits <- list(B_1=0,
               B_2=0,
                B0=0)

nbModel <- nimbleModel(code=nbCode,
                        data=nbData,
                        inits=nbInits,
                        constants=nbConsts)

#compile nimble model to C++ code—much faster runtime
C_nbModel <- compileNimble(nbModel, showCompilerOutput = FALSE)

#configure the MCMC
nbModel_conf <- configureMCMC(nbModel)

nbModel_conf$monitors <- c("B_1","B_2","B0","Delta")

#build MCMC
nbModelMCMC <- buildMCMC(nbModel_conf)

#compile MCMC to C++—much faster
C_nbModelMCMC <- compileNimble(nbModelMCMC,project=nbModel)

#number of MCMC iterations
niter=100000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_nbModelMCMC, niter=niter)

#save samples
#save(samples,file="../Results/MCMC/Exp/mcmc_samples_exp_neg_nochrono.RData")
