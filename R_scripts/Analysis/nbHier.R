nbCode <- nimbleCode({
   ###top-level regression
   B ~ dnorm(0,100)
   B0 ~ dnorm(0,100)
   sigB ~ dunif(1e-10,100)
   sigB0 ~ dunif(1e-10,100)
   for (k in 1:K) {
      ###low-level regression
      b[k] ~ dnorm(mean=B,sd=sigB)
      b0[k] ~ dnorm(mean=B0,sd=sigB0)
      for (n in 1:N){
        alpha[n,k] ~ dunif(1e-10,1)
        lambda[n,k] <- exp(b0[k] + X[n,k] * b[k])
        Y[n,k] ~ dnegbin(size=lambda[n,k],prob=alpha[n,k])
      }
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
trange <- c(11700,15000)

##RECE interval
rece_sample_mfauna <- merge(data.frame(Date=trange[1]:trange[2]),rece_mfauna,all.x=T,by=1)
rece_sample_arch <- merge(data.frame(Date=trange[1]:trange[2]),rects_sample_arch,all.x=T,by=1)

##RECTS
Y <- as.matrix(rece_sample_mfauna[,sample(2:dim(rece_sample_mfauna)[2],size=25)])
Y[which(is.na(Y))] <- 0
X <- as.matrix(rece_sample_arch[,sample(2:dim(rece_sample_arch)[2],size=25)])
X[which(is.na(X))] <- 0
N <- dim(Y)[1]
K <- dim(Y)[2]

#subsample for faster computing
Nsub <- seq(1,N,10)

nbData <- list(Y=Y[Nsub,],
                X=X[Nsub,])

nbConsts <- list(N=length(Nsub),
                    K=K)
nbInits <- list(B=0,
                B0=0,
                b=rep(0,K),
                b0=rep(0,K),
                sigB=0.0001,
                sigB0=0.0001)

nbModel <- nimbleModel(code=nbCode,
                        data=nbData,
                        inits=nbInits,
                        constants=nbConsts)

#compile nimble model to C++ code—much faster runtime
C_nbModel <- compileNimble(nbModel, showCompilerOutput = FALSE)

#configure the MCMC
nbModel_conf <- configureMCMC(nbModel)

nbModel_conf$monitors <- c("B","B0","sigB","sigB0")
nbModel_conf$addMonitors2(c("b","b0"))

#samplers
nbModel_conf$removeSamplers(c("B","B0","sigB","sigB0","b","b0"))
nbModel_conf$addSampler(target=c("B","B0","sigB","sigB0"),type="AF_slice")
for(k in 1:K){
   nbModel_conf$addSampler(target=c(paste("b[",k,"]",sep=""),paste("b0[",k,"]",sep="")),type="AF_slice")
}


#thinning to conserve memory when the samples are saved below
nbModel_conf$setThin(1)
nbModel_conf$setThin2(1)

#build MCMC
nbModelMCMC <- buildMCMC(nbModel_conf)

#compile MCMC to C++—much faster
C_nbModelMCMC <- compileNimble(nbModelMCMC,project=nbModel)

#number of MCMC iterations
niter=2000000

#set seed for replicability
set.seed(1)

#save the MCMC chain (monitored variables) as a matrix
samples <- runMCMC(C_nbModelMCMC, niter=niter)

#save samples
save(samples,file=paste("../Results/MCMC/",sp,"_sub.RData",sep=""))
