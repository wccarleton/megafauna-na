mfauna_sp <- c("mammoth_GL",
               "mastodon_GL",
               "mammoth_SW",
               "sloth_SW")

namen <- c("Mammoth (GL)",
           "Mastodon (GL)",
           "Mammoth (SW)",
           "Sloth (SW)")

plist <- list()
plist_position <- 1

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/NewGround/Regional/mcmc_samples_",mfauna_sp[n],"_hc_ta_NGRIP_ensemble.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[plist_position]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                    geom_density(mapping=aes(x=B0,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                    #labs(y=namen[n]) +
                    xlim(-0.4,0.4) +
                    theme_minimal() +
                    theme(text = element_text(family="Times", size=12),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.x=element_blank())
    #
    plist[[plist_position + 1]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B1,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(0,0.25) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=12),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist[[plist_position + 2]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B2,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=12),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist[[plist_position + 3]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B_ta,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=12),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist_position <- plist_position + 4
}

###
fig <- ggarrange(plotlist=plist,
         ncol=4,
         nrow=4,
         align="v")

annotate_figure(fig,
                bottom=text_grob("Value",face="bold",family="Times"),
                left=text_grob("Density",face="bold",family="Times",rot=90),
                right=text_grob(" ",family="Times"),
                top=text_grob("Posterior Estimates (Climate and Humans)",face="bold",family="Times"))

ggsave(filename="../Images/posteriors_NGRIP_ensemble_reg_ch.png",
      device = "png",
      height = 20,
      width = 20,
      units = "cm",
      scale = 1,
      dpi = 300)
