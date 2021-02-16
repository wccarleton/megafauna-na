png(filename="../Images/mfauna_chvmf_mcmc_chains_cooper_ClovisP.png",
      height = 10,
      width = 20,
      units = "cm",
      res = 300)

plist <- list()

mfauna_sp <- c("mfauna",
               "equus",
               "mammoth",
               "mastodon",
               "sloth",
               "sabertooth")

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/mcmc_samples_",mfauna_sp[n],"_ta_hc_BroughtonNGRIP_Clovis.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[n]] <- ggplot(data=mcmc_samps,
                  mapping=aes(y=B1,x=20000:niter)) +
           geom_path() +
           labs(y=mfauna_sp[n]) +
           theme_minimal() +
           theme(text = element_text(family="Times", size=12),
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank())
}

len_plist <- length(plist)

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/mcmc_samples_",mfauna_sp[n],"_ta_hc_BroughtonNGRIP_Clovis.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[n + len_plist]] <- ggplot(data=mcmc_samps,
                           mapping=aes(y=B2,x=20000:niter)) +
                    geom_path() +
                    labs(y=mfauna_sp[n]) +
                    theme_minimal() +
                    theme(text = element_text(family="Times", size=12),
                    axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())
}

###
fig <- ggarrange(plotlist=plist,
         ncol=2,
         nrow=6,
         align="v")
fig

dev.off()

#ggsave(filename="../Images/mfauna_trends.png",
#      device = "png",
#      height = 10,
#      width = 20,
#      units = "cm",
#      scale = 1.5,
#      dpi = 2000)
