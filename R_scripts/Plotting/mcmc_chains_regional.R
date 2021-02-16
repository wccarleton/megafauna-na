png(filename="../Images/Temp/mcmc_chains_BroughtonReplication_c_regional.png",
      height = 10,
      width = 20,
      units = "cm",
      res = 600)

plist <- list()

mfauna_sp <- c("mammoth_GL",
               "mastodon_GL",
               "mammoth_SW",
               "sloth_SW")

len_plist <- 0

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/Regional/mcmc_samples_",mfauna_sp[n],"_ta_climate_BroughtonNGRIP.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   ind <- length(plist) + 1
   print(ind)

   plist[[ind]] <- ggplot(data=mcmc_samps,
                  mapping=aes(y=B0,x=20000:niter)) +
           geom_path() +
           labs(y=mfauna_sp[n]) +
           theme_minimal() +
           theme(text = element_text(family="Times", size=12),
              plot.margin=unit(c(0.1,0,0,0),"cm"),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank())

   ind <- ind + 1
   print(ind)

   plist[[ind]] <- ggplot(data=mcmc_samps,
                  mapping=aes(y=B,x=20000:niter)) +
           geom_path() +
           labs(y=mfauna_sp[n]) +
           theme_minimal() +
           theme(text = element_text(family="Times", size=12),
           plot.margin=unit(c(0.1,0,0,0),"cm"),
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y=element_blank())

   #ind <- ind + 1
   #print(ind)

   #plist[[ind]] <- ggplot(data=mcmc_samps,
   #               mapping=aes(y=B2,x=20000:niter)) +
   #        geom_path() +
   #        labs(y=mfauna_sp[n]) +
   #        theme_minimal() +
   #        theme(text = element_text(family="Times", size=12),
   #        plot.margin=unit(c(0.1,0,0,0),"cm"),
   #        axis.title.x=element_blank(),
   #        axis.text.x=element_blank(),
   #        axis.ticks.x=element_blank(),
   #        axis.title.y=element_blank())

   ind <- ind + 1
   print(ind)

   plist[[ind]] <- ggplot(data=mcmc_samps,
                  mapping=aes(y=B_ta,x=20000:niter)) +
           geom_path() +
           labs(y=mfauna_sp[n]) +
           theme_minimal() +
           theme(text = element_text(family="Times", size=12),
           plot.margin=unit(c(0.1,0,0,0),"cm"),
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y=element_blank())
   #
   ind <- ind + 1
   print(ind)

   plist[[ind]] <- ggplot(data=mcmc_samps,
                  mapping=aes(y=sigB0,x=20000:niter)) +
           geom_path() +
           labs(y=mfauna_sp[n]) +
           theme_minimal() +
           theme(text = element_text(family="Times", size=12),
           plot.margin=unit(c(0.1,0,0,0),"cm"),
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y=element_blank())

   ind <- ind + 1
   print(ind)

   plist[[ind]] <- ggplot(data=mcmc_samps,
                  mapping=aes(y=sigB,x=20000:niter)) +
           geom_path() +
           labs(y=mfauna_sp[n]) +
           theme_minimal() +
           theme(text = element_text(family="Times", size=12),
           plot.margin=unit(c(0.1,0,0,0),"cm"),
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y=element_blank())

   #ind <- ind + 1
   #print(ind)

   #plist[[ind]] <- ggplot(data=mcmc_samps,
   #               mapping=aes(y=sigB2,x=20000:niter)) +
   #        geom_path() +
   #        labs(y=mfauna_sp[n]) +
   #        theme_minimal() +
   #        theme(text = element_text(family="Times", size=12),
   #        plot.margin=unit(c(0.1,0,0,0),"cm"),
   #        axis.title.x=element_blank(),
   #        axis.text.x=element_blank(),
   #        axis.ticks.x=element_blank(),
   #        axis.title.y=element_blank())

   ind <- ind + 1
   print(ind)

   plist[[ind]] <- ggplot(data=mcmc_samps,
                  mapping=aes(y=sigB_ta,x=20000:niter)) +
           geom_path() +
           labs(y=mfauna_sp[n]) +
           theme_minimal() +
           theme(text = element_text(family="Times", size=12),
           plot.margin=unit(c(0.1,0,0,0),"cm"),
           axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank(),
           axis.title.y=element_blank())
}

###
fig <- ggarrange(plotlist=plist,
         ncol=6,
         nrow=4,
         align="v")

annotate_figure(fig,
                bottom=text_grob("Iteration",face="bold",family="Times"),
                left=text_grob("Value",face="bold",family="Times",rot=90),
                right=text_grob(" ",family="Times"),
                top=text_grob("MCMC Chains (Climate only)",face="bold",family="Times"))

dev.off()

#ggsave(filename="../Images/mfauna_trends.png",
#      device = "png",
#      height = 10,
#      width = 20,
#      units = "cm",
#      scale = 1.5,
#      dpi = 2000)
