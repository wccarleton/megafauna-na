nbCode <- nimbleCode({
   B0 ~ dnorm(mean=0,sd=100)
   B ~ dnorm(mean=0,sd=100)
   alpha ~ dunif(1e-10,1-1e-10)
   for (n in 1:N) {
      lambda[n] <- exp(B0 + X[n] * B)
      Y[n] ~ dnegbin(size=lambda[n],prob=alpha)
      #y[n] ~ dnegbin(size=lambda[n],prob=alpha)
   }
})

#DATA
#sp <- "mfauna"
load(paste("../Data/rects_sample_",sp,".RData",sep=""))
rece_mfauna <- get(paste("rects_sample_",sp,sep=""))

load("../Data/rects_sample_arch.RData")
#rece_arch <- rects_sample_arch

##temporal interval
#trange <- range(rects_arch[rects_arch[,1] %in% rects_mammoth[,1],1])
#trange <- c(11236,15000)
trange <- c(10000,15000)

##RECE interval
rece_sample_mfauna <- merge(data.frame(Date=trange[1]:trange[2]),rece_mfauna,all.x=T,by=1)
rece_sample_arch <- merge(data.frame(Date=trange[1]:trange[2]),rects_sample_arch,all.x=T,by=1)

##RECTS
Y <- as.matrix(rece_sample_mfauna[,20])#sample(2:dim(rece_sample_mfauna)[2],size=1)])
Y[which(is.na(Y))] <- 0
Y <- as.vector(Y)
X <- as.matrix(rece_sample_arch[,20])#sample(2:dim(rece_sample_arch)[2],size=1)])
X[which(is.na(X))] <- 0
X <- as.vector(X)
N <- length(Y)

#subsample for faster computing
Nsub <- seq(1,N,2)

nbData <- list(Y=Y[Nsub],
                X=X[Nsub])

nbConsts <- list(N=length(Nsub))

nbInits <- list(B=0,
                B0=0)

nbModel <- nimbleModel(code=nbCode,
                        data=nbData,
                        inits=nbInits,
                        constants=nbConsts)

#compile nimble model to C++ code—much faster runtime
C_nbModel <- compileNimble(nbModel, showCompilerOutput = FALSE)

#configure the MCMC
nbModel_conf <- configureMCMC(nbModel)

nbModel_conf$monitors <- c("B","B0","alpha")

#nbModel_conf$addMonitors2(c("y"))

nbModel_conf$removeSamplers(c("B","B0"))
nbModel_conf$addSampler(target=c("B","B0"),type="AF_slice")

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
