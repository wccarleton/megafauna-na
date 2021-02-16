#
library(clam)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(abind)
#

mfauna_sp <- c("mfauna",
               "equus",
               "mammoth",
               "mastodon",
               "sloth",
               "sabertooth")

namen <- c("All Species",
           "Equus",
           "Mammoth",
           "Mastodon",
           "Sloth",
           "Sabertooth")

plist <- list()
plist_position <- 1

for( n in 1:length(mfauna_sp)){
   load(paste("../Results/MCMC/FvH/mcmc_samples_",mfauna_sp[n],"_ta_sub.RData",sep=""))
   niter <- dim(samples$samples)[1]
   mcmc_samps <- as.data.frame(samples$samples[20000:niter,])

   plist[[plist_position]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                    geom_density(mapping=aes(x=B0,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                    #labs(y=namen[n]) +
                    xlim(-0.5,0.5) +
                    theme_minimal() +
                    theme(text = element_text(family="Times", size=9),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.x=element_blank())
    #
    plist[[plist_position + 1]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=9),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist[[plist_position + 2]] <- ggplot(data=mcmc_samps) +
                    geom_vline(xintercept=0,size=0.75,colour="darkgrey",alpha=0.75) +
                     geom_density(mapping=aes(x=B_ta,y=..scaled..),fill="steelblue",colour=NA,alpha=0.75) +
                     #labs(y=mfauna_sp[n]) +
                     xlim(-0.4,0.4) +
                     #geom_text(label=namen[n],x=0.4,y=0.9,hjust=1) +
                     theme_minimal() +
                     theme(text = element_text(family="Times", size=9),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks.x=element_blank())
    #
    plist_position <- plist_position + 3
}

###
fig <- ggarrange(plotlist=plist,
         ncol=3,
         nrow=6,
         align="v")

annotate_figure(fig,
                bottom=text_grob("Value",face="bold",family="Times",size=9),
                left=text_grob("Density",face="bold",family="Times",rot=90,size=9),
                right=text_grob(" ",family="Times"),
                top=text_grob("Posterior Estimates (Humans only)",face="bold",family="Times",size=9))

ggsave(filename="../Images/posteriors_Broughton_h.svg",
      device = "svg",
      height = 175,
      width = 175,
      units = "mm",
      scale = 1)
