### Socialist Threat: RDD Estimates (Tables and Figures)
### Author: André Walter
### andre.walter[at]unisg.ch

### Install rdrobust package version 0.99.1 to replicate the results (package "devtools" required)

devtools::install_version("rdrobust", version = "0.99.1", repos = "http://cran.us.r-project.org")

### Load packages

library(xtable)
library(rdrobust)
library(rdlocrand)
library(rddensity)
library(ggplot2)
library(cowplot)
library(dplyr)
library(texreg)

setwd("C:/Work/Dropbox/Papers/Soc_Threat/Estimation/")

rm(list=ls())

runoff <- read.csv("data.csv")

### Manipulation/sorting around the threshold

# Figure A1

rdd <- rddensity(runoff$socmarg)

png(filename="C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/selfselect.png")
rdplotdensity(rdd, runoff$socmarg, xlabel = "Socialist/Second Non-Socialist Vote Share Difference", ylabel = "Density")
dev.off()


### RDPlots

# Figure A2

png(filename="C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/rdplot3.png")
rdplot(y = runoff$diffvotensoc, x = runoff$socmarg, p = 3, binselect = "qs", scale = 25, 
       x.label = "Socialist/Second Non-Socialist Vote Share Difference",
       y.label = "Established Party Support")
dev.off()

png(filename="C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/rdplot1.png")
rdplot(y = runoff$difcro, x = runoff$socmarg, p = 3, binselect = "es", scale = 20, 
       x.label = "Socialist/Second Non-Socialist Vote Share Difference",
       y.label = "Liberal/Conservative Alliance Probability")
dev.off()

png(filename="C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/rdplot2.png")
rdplot(y = runoff$rightmar2, x = runoff$socmarg, p = 3, binselect = "es", scale = 20, 
       x.label = "Socialist/Second Non-Socialist Vote Share Difference",
       y.label = "Alliance Probability Among Strongest Non-Socialists")
dev.off()

### Main Analysis

## Loop for different bandwidths

# Liberal-conservative alliances (Figure 1a)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$difcro2, x = runoff$socmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$difcro2, x = runoff$socmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Socialist Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/lcgraf.eps", height=7, width=10, units='in',device=cairo_ps)


# Right Margin (Figure 1b)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$rightmar2, x = runoff$socmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$rightmar2, x = runoff$socmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Socialist Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/rigraf.eps", height=7, width=10, units='in',device=cairo_ps)

## Vote Share Changes

# W & w/o Alliances (Figure 2a)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$socmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
regra$all <- c("Yes")
colnames(regra) <- c("est","low","upp","bw","obs","all")

libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$socmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)

bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Socialist Candidate in Runoff")+ 
  geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/voteall.eps", height=7, width=10, units='in',device=cairo_ps)

# Right Margin (Figure 2c)

worigmar <- subset(runoff, !(rightmar2 ==0 & socmarg>0))

rigmar <- subset(runoff, !(rightmar2 ==1 & socmarg>0))

resu <- list()
resu2 <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg, 
                     covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
                     all = T, cluster = rigmar$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
  libcon2 <- rdrobust(y = worigmar$diffvotensoc, x = worigmar$socmarg, 
                     covs = cbind(worigmar$lcomp,worigmar$Year3, worigmar$id, worigmar$marg,worigmar$lnonsocsup,worigmar$clea,worigmar$votensoc),
                     all = T, cluster = worigmar$Electoral_District_Number, h = i/100)
  
  resu2[[i]] <- c(libcon2[[3]][[1]],libcon2[[8]][[3]],libcon2[[8]][[6]],i/100, sum(libcon2[[22]]))
  
  
}

regra <- do.call(rbind.data.frame, resu)
regra$all <- c("Yes")
colnames(regra) <- c("est","low","upp","bw","obs","all")

libcon <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg, 
                   covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
                   all = T, cluster = rigmar$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


regra2 <- do.call(rbind.data.frame, resu2)
regra2$all <- c("No")
colnames(regra2) <- c("est","low","upp","bw","obs","all")

libcon2 <- rdrobust(y = worigmar$diffvotensoc, x = worigmar$socmarg, 
                    covs = cbind(worigmar$lcomp,worigmar$Year3, worigmar$id,worigmar$marg,worigmar$lnonsocsup,worigmar$clea,worigmar$votensoc),
                    all = T, cluster = worigmar$Electoral_District_Number)
bw <- libcon2[[2]]
bw <- round(bw[1,1],2)
regra2$optbw <- bw
regra2$bwo <- ifelse(regra2$optbw == regra2$bw,1,0 )
regra <- rbind(regra,regra2)
regra$all <- as.factor(regra$all)


rigra <- ggplot(data = regra, aes(y = est, x = bw, group = all, linetype = all, color = as.factor(bwo)))+geom_point(aes(),position=position_dodge(0.01))+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.01))+scale_color_manual(values = c("black","red"),guide = FALSE)+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "", group = "Electoral Alliance",
                                             y = "Marginal Effect of Socialist Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)+
  scale_linetype_manual(name = "Electoral Alliances", values = c(1,2))

riobs <- ggplot(data = regra, aes(y = obs, x = bw, group = all, linetype = all))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)+
  scale_linetype_manual(name = "Electoral Alliances", values = c(1,2))

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/rigvote.eps", height=7, width=10, units='in',device=cairo_ps)

# Cleavage (Figure 2b)

worigmar <- subset(runoff, !(difcro2 ==0 & socmarg>0))

rigmar <- subset(runoff, !(difcro2 ==1 & socmarg>0))

resu <- list()
resu2 <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg, 
                     covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
                     all = T, cluster = rigmar$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
  libcon2 <- rdrobust(y = worigmar$diffvotensoc, x = worigmar$socmarg, 
                      covs = cbind(worigmar$lcomp,worigmar$Year3, worigmar$id, worigmar$marg,worigmar$lnonsocsup,worigmar$clea,worigmar$votensoc),
                      all = T, cluster = worigmar$Electoral_District_Number, h = i/100)
  
  resu2[[i]] <- c(libcon2[[3]][[1]],libcon2[[8]][[3]],libcon2[[8]][[6]],i/100, sum(libcon2[[22]]))
  
  
}

regra <- do.call(rbind.data.frame, resu)
regra$all <- c("Yes")
colnames(regra) <- c("est","low","upp","bw","obs","all")

libcon <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg, 
                   covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
                   all = T, cluster = rigmar$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

regra2 <- do.call(rbind.data.frame, resu2)
regra2$all <- c("No")
colnames(regra2) <- c("est","low","upp","bw","obs","all")

libcon2 <- rdrobust(y = worigmar$diffvotensoc, x = worigmar$socmarg, 
                    covs = cbind(worigmar$lcomp,worigmar$Year3,worigmar$id, worigmar$marg,worigmar$lnonsocsup,worigmar$clea,worigmar$votensoc),
                    all = T, cluster = worigmar$Electoral_District_Number)
bw <- libcon2[[2]]
bw <- round(bw[1,1],2)
regra2$optbw <- bw
regra2$bwo <- ifelse(regra2$optbw == regra2$bw,1,0 )
regra <- rbind(regra,regra2)
regra$all <- as.factor(regra$all)


rigra <- ggplot(data = regra, aes(y = est, x = bw, group = all, linetype = all, color = as.factor(bwo)))+geom_point(aes(),position=position_dodge(0.01))+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.01))+scale_color_manual(values = c("black","red"),guide = FALSE)+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "", group = "Electoral Alliance",
                                             y = "Marginal Effect of Socialist Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)+
  scale_linetype_manual(name = "Electoral Alliances", values = c(1,2))

riobs <- ggplot(data = regra, aes(y = obs, x = bw, group = all, linetype = all))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)+
  scale_linetype_manual(name = "Electoral Alliances", values = c(1,2))

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/cleavote.eps", height=7, width=10, units='in',device=cairo_ps)

### Custom RDD extraction functions for texreg

extract.rdd <- function(model){
  names <- rownames(model$coef)[3]
  co <- model[[3]][[1]]
  se <- model[[5]][[3]]
  pval <- model[[7]][[3]]
  ci.low <- model[[8]][[3]]
  ci.up <- model[[8]][[6]]
  
  bw <- model[[2]][[1]]
  obs.b <- as.integer(model[[13]][[1]])
  obs.a <- as.integer(model[[14]][[1]])
  obs <- as.integer(sum(model[[21]]))
  gof <- c(bw,obs.b,obs.a,obs)
  gof.names <- c("Bandwidth","Obs. below cutoff","Obs. above cutoff","Total Obs.")
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    ci.low <- ci.low,
    ci.up <- ci.up,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = c(TRUE,FALSE,FALSE,FALSE)
  )
  return(tr)
}

setMethod("extract", signature = className("rdrobust","rdrobust"),
          definition = extract.rdd)

##### Robustness tests electoral alliances

# Quadratic regression
pvals <- c()

m1 <- rdrobust(y = runoff$difcro2, x = runoff$socmarg, 
               covs = cbind(runoff$lcomp,runoff$Year3,runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
               all = T, cluster = runoff$Electoral_District_Number, p = 2)
pvals <- c(pvals, m1$pv[[3]] )

m2 <- rdrobust(y = runoff$rightmar2, x = runoff$socmarg, 
               covs = cbind(runoff$lcomp,runoff$Year3, runoff$id,runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
               all = T, cluster = runoff$Electoral_District_Number,  p = 2)
pvals <- c(pvals, m2$pv[[3]])

## Placebo tests

# Lagged outcome/running variable

runoffpl <- runoff %>% group_by(Electoral_District_Number) %>% mutate(ldifcro2 = lag(difcro2))

runoffpl <- runoffpl %>% group_by(Electoral_District_Number) %>% mutate(lrightmar2 = lag(rightmar2))

m3 <- rdrobust(y = runoffpl$ldifcro2, x = runoffpl$socmarg, 
               covs = cbind(runoffpl$lcomp,runoffpl$Year3,runoffpl$id, runoffpl$marg,runoffpl$lnonsocsup,runoffpl$clea,runoffpl$votensoc),
               all = T, cluster = runoffpl$Electoral_District_Number)
pvals <- c(pvals, m3$pv[[3]] )

m4 <- rdrobust(y = runoffpl$lrightmar2, x = runoffpl$socmarg, 
               covs = cbind(runoffpl$lcomp,runoffpl$Year3, runoffpl$id,runoffpl$marg,runoffpl$lnonsocsup,runoffpl$clea,runoffpl$votensoc),
               all = T, cluster = runoffpl$Electoral_District_Number)
pvals <- c(pvals, m4$pv[[3]])


runoffpl <- runoff %>% group_by(Electoral_District_Number) %>% mutate(leadsoc = lag(socmarg))

m5 <- rdrobust(y = runoffpl$difcro2, x = runoffpl$leadsoc, 
               covs = cbind(runoffpl$lcomp,runoffpl$Year3, runoffpl$id, runoffpl$marg,runoffpl$lnonsocsup,runoffpl$clea,runoffpl$votensoc),
               all = T, cluster = runoffpl$Electoral_District_Number)
pvals <- c(pvals, m5$pv[[3]] )

m6 <- rdrobust(y = runoffpl$rightmar2, x = runoffpl$leadsoc, 
               covs = cbind(runoffpl$lcomp,runoffpl$Year3, runoffpl$id, runoffpl$marg,runoffpl$lnonsocsup,runoffpl$clea,runoffpl$votensoc),
               all = T, cluster = runoffpl$Electoral_District_Number)
pvals <- c(pvals, m6$pv[[3]] )

# Fake cutoff

runoffcf <- subset(runoff,socmarg>0)
runoffc <- subset(runoff,socmarg<0)

m7 <- rdrobust(y = runoffcf$difcro2, x = runoffcf$socmarg, 
               covs = cbind(runoffcf$lcomp,runoffcf$Year3, runoffcf$marg,runoffcf$lnonsocsup,runoffcf$clea,runoffcf$votensoc),
               all = T, cluster = runoffcf$Electoral_District_Number, c =.06)
pvals <- c(pvals, m7$pv[[3]] )

m8 <- rdrobust(y = runoffcf$difcro2, x = runoffcf$socmarg, 
               covs = cbind(runoffcf$lcomp,runoffcf$Year3, runoffcf$marg,runoffcf$lnonsocsup,runoffcf$clea,runoffcf$votensoc),
               all = T, cluster = runoffcf$Electoral_District_Number, c =.12)
pvals <- c(pvals, m8$pv[[3]] )

m9 <- rdrobust(y = runoffc$difcro2, x = runoffc$socmarg, 
               covs = cbind(runoffc$lcomp,runoffc$Year3, runoffc$marg,runoffc$lnonsocsup,runoffc$clea,runoffc$votensoc),
               all = T, cluster = runoffc$Electoral_District_Number, c =-.06)
pvals <- c(pvals, m9$pv[[3]] )

m10 <- rdrobust(y = runoffc$difcro2, x = runoffc$socmarg, 
                covs = cbind(runoffc$lcomp,runoffc$Year3, runoffc$marg,runoffc$lnonsocsup,runoffc$clea,runoffc$votensoc),
                all = T, cluster = runoffc$Electoral_District_Number, c =-.12)
pvals <- c(pvals, m10$pv[[3]] )

m11 <- rdrobust(y = runoffcf$rightmar2, x = runoffcf$socmarg, 
               covs = cbind(runoffcf$lcomp,runoffcf$Year3, runoffcf$marg,runoffcf$lnonsocsup,runoffcf$clea,runoffcf$votensoc),
               all = T, cluster = runoffcf$Electoral_District_Number, c =.06)
pvals <- c(pvals, m11$pv[[3]] )

m12 <- rdrobust(y = runoffcf$rightmar2, x = runoffcf$socmarg, 
               covs = cbind(runoffcf$lcomp,runoffcf$Year3, runoffcf$marg,runoffcf$lnonsocsup,runoffcf$clea,runoffcf$votensoc),
               all = T, cluster = runoffcf$Electoral_District_Number, c =.12)
pvals <- c(pvals, m12$pv[[3]] )

m13 <- rdrobust(y = runoffc$rightmar2, x = runoffc$socmarg, 
               covs = cbind(runoffc$lcomp,runoffc$Year3, runoffc$marg,runoffc$lnonsocsup,runoffc$clea,runoffc$votensoc),
               all = T, cluster = runoffc$Electoral_District_Number, c =-.06)
pvals <- c(pvals, m13$pv[[3]] )

m14 <- rdrobust(y = runoffc$rightmar2, x = runoffc$socmarg, 
                covs = cbind(runoffc$lcomp,runoffc$Year3, runoffc$marg,runoffc$lnonsocsup,runoffc$clea,runoffc$votensoc),
                all = T, cluster = runoffc$Electoral_District_Number, c =-.12)
pvals <- c(pvals, m14$pv[[3]] )


# False discovery histgrams

faldis <- data.frame(cbind(pvals, adpvals = p.adjust(pvals, method = "fdr")))

# Quadratic regression table (Table A5)

texreg(list(m1,m2), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/quad.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[1,2],faldis[2,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est1", 
       caption = "Electoral Alliances - Second-Degree Polynomials", custom.header = list("Cleavage" = 1, "Right Margin" = 2))

### Lagged 0utcome/running Variable (Table A6)

texreg(list(m3,m4,m5,m6), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/placebo.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[3,2],faldis[4,2],faldis[5,2],faldis[6,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est2", 
       caption = "Placebo Test Lagged Outcome/Running Variable", 
       custom.header = list("Cleavage" = 1, "Right Margin" = 2,"Cleavage" = 3, "Right Margin" = 4))

### Fake cutoff (Table A7)

texreg(list(m7,m8,m9,m10,m11,m12,m13,m14), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/placebo3.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .6, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."), 
       custom.gof.rows = list("Adjust. p-value" = c(faldis[7,2],faldis[8,2],faldis[9,2],faldis[10,2],
                                                    faldis[11,2],faldis[12,2],faldis[13,2],faldis[14,2])),
       custom.model.names = c("6\\%","12\\%","-6\\%","-12\\%","6\\%","12\\%","-6\\%","-12\\%"),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est4", caption = "Placebo Test Fake Cutoff",
       custom.header = list("Cleavage" = 1:4, "Right Margin" = 5:8))

### Local randomization (Table A8)

runoff$clea <- as.factor(runoff$clea)

fintab1 <- rdrandinf(runoff$difcro2,runoff$socmarg, seed = 1234,
                     covariates = cbind(runoff$lcomp,runoff$Year3, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc) )

fintab2 <- rdrandinf(runoff$rightmar2,runoff$socmarg, seed = 1234, 
                     covariates = cbind(runoff$lcomp,runoff$Year3, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc))

cofsoc <- c(NA,fintab1[[2]],fintab2[[2]])

pval <- c(NA,(fintab1[[3]]),(fintab2[[3]])) 

bw <- c(NA,abs(fintab1[[1]][[5]]),abs(fintab2[[1]][[5]])) 

lobs <- c(NA,fintab1[[1]][[2]],fintab2[[1]][[2]]) 

robs <- c(NA,fintab1[[1]][[7]],fintab2[[1]][[7]]) 

obs <- c(NA,fintab1[[1]][[1]]+fintab1[[1]][[6]],fintab2[[1]][[1]]+fintab2[[1]][[6]])

res <- round(data.frame(do.call("rbind", list(cofsoc,pval,bw,lobs,robs,obs))), 3)
res[,1] <- c("Socialist in Runoff","p-value","Bandwidth","Obs. above cutoff","Obs. below cutoff","Total obs.")
colnames(res) <- c("","Cross-Cleavage","Right Margin")


tabx <- xtable(res, caption = "Electoral Alliances: Randomized Inference", label = "ran", 
               align = c("l","l","c","c"))
print(tabx, booktabs = TRUE, hline.after = c(0,2,6), include.rownames = F,
      file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/ran.tex")

# Donut hole regression (Table A9 & A10)

runoff2 <- runoff

lengg <- c(0:5)
resu <- list()
resu2 <- list()

for (i in lengg) {
  test <- runoff[abs(runoff2$socmarg) < i/1000,] 
  runoff3 <- runoff[abs(runoff$socmarg) >=i/1000,]  

  i <- i+1
  exobsup <- sum(test$socmarg>0)
  exobsdown <- sum(test$socmarg<0)
  
  cleared <-  rdrobust(y = runoff3$difcro2, x = runoff3$socmarg, 
                       covs = cbind(runoff3$lcomp,runoff3$Year3, runoff3$id,runoff3$marg,runoff3$lnonsocsup,runoff3$clea,runoff3$votensoc),
                       all = T, cluster = runoff3$Electoral_District_Number)
  
  resu[[i]] <- c(cleared[[3]][[1]],cleared[[8]][[3]],cleared[[8]][[6]],(i-1)/1000, sum(cleared[[22]]),exobsup,exobsdown)
  
  rightred <- rdrobust(y = runoff3$rightmar2, x = runoff3$socmarg, 
                       covs = cbind(runoff3$lcomp,runoff3$Year3, runoff3$id,runoff3$marg,runoff3$lnonsocsup,runoff3$clea,runoff3$votensoc),
                       all = T, cluster = runoff3$Electoral_District_Number)
  resu2[[i]] <- c(rightred[[3]][[1]],rightred[[8]][[3]],rightred[[8]][[6]],(i-1)/1000, sum(rightred[[22]]),exobsup,exobsdown)   
  i <- i-1
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("RDD Est.","Lower CI","Upper CI","Donut-Hole Radius","Num. Obs.",
                     "Excl. Obs. Right","Excl. Obs. Right")

tabx <- xtable(regra, caption = "Donut regression (Cleavages)", label = "donclea", 
               align = c("l","l","c","c","c","c","c","c"),digits=c(0,2,2,2,3,0,0,0))
print(tabx, booktabs = TRUE, hline.after = c(-1,0,6), include.rownames = F,
      file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/donclea.tex", floating.environment = "sidewaystable")

regra2 <- do.call(rbind.data.frame, resu2)
colnames(regra2) <- c("RDD Est.","Lower CI","Upper CI","Donut-Hole Radius","Num. Obs.",
                      "Excl. Obs. Right","Excl. Obs. Right")

tabx <- xtable(regra2, caption = "Donut regression (Right Margin)", label = "donrig", 
               align = c("l","l","c","c","c","c","c","c"),digits=c(0,2,2,2,3,0,0,0))
print(tabx, booktabs = TRUE, hline.after = c(-1,0,6), include.rownames = F,
      file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/donrig.tex", floating.environment = "sidewaystable")


##### Robustness tests vote share

### Quadratic regression 
pvals <- c()
m1 <- rdrobust(y = runoff$diffvotensoc, x = runoff$socmarg,
               covs = cbind(runoff$lcomp,runoff$Year3,runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
               all = T, cluster = runoff$Electoral_District_Number, p = 2)
pvals <- c(pvals,m1$pv[[3]])

rigmar <- subset(runoff, !(difcro2 ==1 & socmarg>0))

m2 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3,rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, p = 2)
pvals <- c(pvals,m2$pv[[3]])

rigmar <- subset(runoff, !(difcro2 ==0 & socmarg>0))

m3 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3,rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, p = 2)
pvals <- c(pvals,m3$pv[[3]])

rigmar <- subset(runoff, !(rightmar2 ==1 & socmarg>0))

m4 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, p = 2)
pvals <- c(pvals,m4$pv[[3]])

rigmar <- subset(runoff, !(rightmar2 ==0 & socmarg>0))

m5 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, p = 2)
pvals <- c(pvals,m5$pv[[3]])

### Placebo tests

# Lagged outcome

runoffpl <- runoff %>% group_by(Electoral_District_Number) %>% mutate(ldiffvotensoc = lag(diffvotensoc))

m6 <- rdrobust(y = runoffpl$ldiffvotensoc, x = runoffpl$socmarg,
               covs = cbind(runoffpl$lcomp,runoffpl$Year3,runoffpl$id, runoffpl$marg,runoffpl$lnonsocsup,runoffpl$clea,runoffpl$votensoc),
               all = T, cluster = runoffpl$Electoral_District_Number)
pvals <- c(pvals,m6$pv[[3]])
rigmar <- subset(runoffpl, !(difcro2 ==1 & socmarg>0))

m7 <- rdrobust(y = rigmar$ldiffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m7$pv[[3]])
rigmar <- subset(runoffpl, !(difcro2 ==0 & socmarg>0))

m8 <- rdrobust(y = rigmar$ldiffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3,rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m8$pv[[3]])

rigmar <- subset(runoffpl, !(rightmar2 ==1 & socmarg>0))

m9 <- rdrobust(y = rigmar$ldiffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m9$pv[[3]])
rigmar <- subset(runoffpl, !(rightmar2 ==0 & socmarg>0))

m10 <- rdrobust(y = rigmar$ldiffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m10$pv[[3]])

# Lagged running variable

runoffpl <- runoff %>% group_by(Electoral_District_Number) %>% mutate(leadsoc = lag(socmarg))


m11 <- rdrobust(y = runoffpl$diffvotensoc, x = runoffpl$leadsoc,
               covs = cbind(runoffpl$lcomp,runoffpl$Year3, runoffpl$id,runoffpl$marg,runoffpl$lnonsocsup,runoffpl$clea,runoffpl$votensoc),
               all = T, cluster = runoffpl$Electoral_District_Number)
pvals <- c(pvals,m11$pv[[3]])
rigmar <- subset(runoffpl, !(difcro2 ==1 & socmarg>0))

m12 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$leadsoc,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m12$pv[[3]])
rigmar <- subset(runoffpl, !(difcro2 ==0 & socmarg>0))

m13 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$leadsoc,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m13$pv[[3]])
rigmar <- subset(runoffpl, !(rightmar2 ==1 & socmarg>0))

m14 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$leadsoc,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m14$pv[[3]])
rigmar <- subset(runoffpl, !(rightmar2 ==0 & socmarg>0))

m15 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$leadsoc,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number)
pvals <- c(pvals,m15$pv[[3]])

### Fake cutoff

# 6%

runofft <- subset(runoff, socmarg>0)
cut <- .06
m16 <- rdrobust(y = runofft$diffvotensoc, x = runofft$socmarg,
               covs = cbind(runofft$lcomp,runofft$Year3, runofft$marg,runofft$lnonsocsup,runofft$clea,runofft$votensoc),
               all = T, cluster = runofft$Electoral_District_Number, c = cut)
pvals <- c(pvals,m16$pv[[3]])
rigmar <- subset(runofft, difcro2 ==0)

m17 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m17$pv[[3]])
rigmar <- subset(runofft, difcro2 ==1)

m18 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m18$pv[[3]])
rigmar <- subset(runofft, rightmar2 ==0)

m19 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m19$pv[[3]])
rigmar <- subset(runofft, rightmar2 ==1)

m20 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m20$pv[[3]])

# 12%
cut <- .12

m21 <- rdrobust(y = runofft$diffvotensoc, x = runofft$socmarg,
               covs = cbind(runofft$lcomp,runofft$Year3, runofft$marg,runofft$lnonsocsup,runofft$clea,runofft$votensoc),
               all = T, cluster = runofft$Electoral_District_Number, c = cut)
pvals <- c(pvals,m21$pv[[3]])
rigmar <- subset(runofft, difcro2 ==0)

m22 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m22$pv[[3]])
rigmar <- subset(runofft, difcro2 ==1)

m23 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m23$pv[[3]])
rigmar <- subset(runofft, rightmar2 ==0)

m24 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m24$pv[[3]])
rigmar <- subset(runofft, rightmar2 ==1)

m25 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m25$pv[[3]])

# -6%
cut <- -.06
runofft <- subset(runoff, socmarg<0)

m26 <- rdrobust(y = runofft$diffvotensoc, x = runofft$socmarg,
               covs = cbind(runofft$lcomp,runofft$Year3, runofft$marg,runofft$lnonsocsup,runofft$clea,runofft$votensoc),
               all = T, cluster = runofft$Electoral_District_Number, c = cut)
pvals <- c(pvals,m26$pv[[3]])
rigmar <- subset(runofft, difcro2 ==0)

m27 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m27$pv[[3]])
rigmar <- subset(runofft, difcro2 ==1)

m28 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m28$pv[[3]])

rigmar <- subset(runofft, rightmar2 ==0)

m29 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m29$pv[[3]])

rigmar <- subset(runofft, rightmar2 ==1)

m30 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m30$pv[[3]])

# -12%
cut <- -.12
m31 <- rdrobust(y = runofft$diffvotensoc, x = runofft$socmarg,
               covs = cbind(runofft$lcomp,runofft$Year3, runofft$marg,runofft$lnonsocsup,runofft$clea,runofft$votensoc),
               all = T, cluster = runofft$Electoral_District_Number, c = cut)
pvals <- c(pvals,m31$pv[[3]])

rigmar <- subset(runoff, difcro2 ==0)

m32 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m32$pv[[3]])

rigmar <- subset(runoff, difcro2 ==1)

m33 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m33$pv[[3]])

rigmar <- subset(runoff, rightmar2 ==0)

m34 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m34$pv[[3]])

rigmar <- subset(runoff, rightmar2 ==1)

m35 <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg,
               covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
               all = T, cluster = rigmar$Electoral_District_Number, c = cut)
pvals <- c(pvals,m35$pv[[3]])

### False discovery

faldis <- round(data.frame(cbind(pvals, adpvals = p.adjust(pvals, method = "fdr"))),3)

### Quadratic regression table (A11)
texreg(list(m1,m2,m3,m4,m5), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/quadvot.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .7, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.model.names = c("Yes","No","Yes","No","Yes"),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[1,2],faldis[2,2],faldis[3,2],faldis[4,2],faldis[5,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est5", 
       caption = "Established Party Support - Second-Degree Polynomials", 
       custom.header = list("Runoff" = 1, "Cross-Cleavage" = 2:3 ,"Right Margin" = 4:5))

### Lagged outcome (A12)

texreg(list(m6,m7,m8,m9,m10), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/placebopar.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .7, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.model.names = c("Yes","No","Yes","No","Yes"),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[6,2],faldis[7,2],faldis[8,2],faldis[9,2],faldis[10,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est6", 
       caption = "Established Party Support - Placebo Lagged Outcome", 
       custom.header = list("Runoff" = 1, "Cross-Cleavage" = 2:3 ,"Right Margin" = 4:5))

### Lagged running variable (A13)

texreg(list(m11,m12,m13,m14,m15), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/placebopar2.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .7, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.model.names = c("Yes","No","Yes","No","Yes"),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[11,2],faldis[12,2],faldis[13,2],faldis[14,2],faldis[15,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est7", 
       caption = "Established Party Support - Lagged Running Variable", 
       custom.header = list("Runoff" = 1, "Cross-Cleavage" = 2:3 ,"Right Margin" = 4:5))

### Established Party Support - Fake Cutoff 6% (A14)

texreg(list(m16,m17,m18,m19,m20), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/votfc2.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .7, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.model.names = c("Yes","No","Yes","No","Yes"),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[16,2],faldis[17,2],faldis[18,2],faldis[19,2],faldis[20,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est8", 
       caption = "Established Party Support - Fake Cutoff 6\\%", 
       custom.header = list("Runoff" = 1, "Cross-Cleavage" = 2:3 ,"Right Margin" = 4:5))

### Established Party Support - Fake Cutoff 12% (A15)
texreg(list(m21,m22,m23,m24,m25), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/votfc4.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .7, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.model.names = c("Yes","No","Yes","No","Yes"),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[21,2],faldis[22,2],faldis[23,2],faldis[24,2],faldis[25,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est9", 
       caption = "Established Party Support - Fake Cutoff 12\\%", 
       custom.header = list("Runoff" = 1, "Cross-Cleavage" = 2:3 ,"Right Margin" = 4:5))

### Established Party Support - Fake Cutoff -6% (A16)

texreg(list(m26,m27,m28,m29,m30), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/votfcne2.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .7, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.model.names = c("Yes","No","Yes","No","Yes"),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[26,2],faldis[27,2],faldis[28,2],faldis[29,2],faldis[30,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est10", 
       caption = "Established Party Support - Fake Cutoff -6\\%", 
       custom.header = list("Runoff" = 1, "Cross-Cleavage" = 2:3 ,"Right Margin" = 4:5))

### Established Party Support - Fake Cutoff -12% (A17)

texreg(list(m31,m32,m33,m34,m35), file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/votfcne4.tex", 
       stars = numeric(0), booktabs = T, dcolumn = T, use.packages = F, scalebox = .7, ci.force = T,
       custom.note = c("95\\% Cluster-Robust Confidence Intervals reported."),
       custom.model.names = c("Yes","No","Yes","No","Yes"),
       custom.gof.rows = list("Adjust. p-value" = c(faldis[31,2],faldis[32,2],faldis[33,2],faldis[34,2],faldis[35,2])),
       custom.coef.names = c( "Socialist in Run-Off"), label = "est11", 
       caption = "Established Party Support - Fake Cutoff -12\\%", 
       custom.header = list("Runoff" = 1, "Cross-Cleavage" = 2:3 ,"Right Margin" = 4:5))


### Local randomization (A18)

runoff$clea <- as.factor(runoff$clea)

fintab1 <- rdrandinf(runoff$diffvotensoc,runoff$socmarg, seed = 1234,
                     covariates = cbind(runoff$lcomp,runoff$Year3, runoff$marg,runoff$lnonsocsup,runoff$clea) )

rigmar <- subset(runoff, !(difcro2 ==1 & socmarg>0))

fintab2 <- rdrandinf(rigmar$diffvotensoc,rigmar$socmarg, seed = 1234, 
                     covariates = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea))

rigmar <- subset(runoff, !(difcro2 ==0 & socmarg>0))

fintab3 <- rdrandinf(rigmar$diffvotensoc,rigmar$socmarg, seed = 1234, 
                     covariates = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea))

rigmar <- subset(runoff, !(rightmar2 ==1 & socmarg>0))

fintab4 <- rdrandinf(rigmar$diffvotensoc,rigmar$socmarg, seed = 1234, 
                     covariates = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea))

rigmar <- subset(runoff, !(rightmar2 ==0 & socmarg>0))

fintab5 <- rdrandinf(rigmar$diffvotensoc,rigmar$socmarg, seed = 1234, 
                     covariates = cbind(rigmar$lcomp,rigmar$Year3, rigmar$marg,rigmar$lnonsocsup,rigmar$clea))

cofsoc <- c(NA,fintab1[[2]],fintab2[[2]],fintab3[[2]],fintab4[[2]],fintab5[[2]])

pval <- c(NA,(fintab1[[3]]),(fintab2[[3]]),(fintab3[[3]]),(fintab4[[3]]),(fintab5[[3]])) 

bw <- c(NA,abs(fintab1[[1]][[5]]),abs(fintab2[[1]][[5]]),abs(fintab3[[1]][[5]]),
        abs(fintab4[[1]][[5]]),abs(fintab5[[1]][[5]])) 

lobs <- c(NA,fintab1[[1]][[2]],fintab2[[1]][[2]],fintab3[[1]][[2]],fintab4[[1]][[2]],fintab5[[1]][[2]]) 

robs <- c(NA,fintab1[[1]][[7]],fintab2[[1]][[7]],fintab3[[1]][[7]],fintab4[[1]][[7]],fintab5[[1]][[7]]) 

obs <- c(NA,fintab1[[1]][[1]]+fintab1[[1]][[6]],fintab2[[1]][[1]]+fintab2[[1]][[6]],
         fintab3[[1]][[1]]+fintab3[[1]][[6]],fintab4[[1]][[1]]+fintab4[[1]][[6]],
         fintab5[[1]][[1]]+fintab5[[1]][[6]])

res <- round(data.frame(do.call("rbind", list(cofsoc,pval,bw,lobs,robs,obs))), 3)
res[,1] <- c("Socialist in Runoff","p-value","Bandwidth","Obs. above cutoff","Obs. below cutoff","Total obs.")
colnames(res) <- c("","Yes","No","Yes","No","Yes")

construct_header <- function(df, grp_names, span, align = "c", draw_line = T) {
  if (length(align) == 1) align <- rep(align, length(grp_names))
  if (!all.equal(length(grp_names), length(span), length(align)))
    stop("grp_names and span have to have the same length!")
  
  if (ncol(df) < sum(span)) stop("Span has to be less or equal to the number of columns of df") 
  
  header <- mapply(function(s, a, grp) sprintf("\\multicolumn{%i}{%s}{%s}", s, a, grp),
                   span, align, grp_names)
  header <- paste(header, collapse = " & ")
  header <- paste0(header, " \\\\")
  
  if (draw_line) {
    # where do we span the lines:
    min_vals <- c(1, 1 + cumsum(span)[1:(length(span) - 1)])
    max_vals <- cumsum(span)
    line <- ifelse(grp_names == "", "", 
                   sprintf("\\cmidrule(lr){%i-%i}", min_vals, max_vals))
    line <- paste(line[line != ""], collapse = " ")
    
    header <- paste0(header, "  ", line, "\n  ")
  }
  
  addtorow <- list(pos = list(-1, -1, nrow(df)),
                   command = c("\\toprule\n  ", header, "\\bottomrule\n  "))
  return(addtorow)
}

a_header <- construct_header(res,
                             grp_names = c("","Runoff Comb.", "Cross-Cleavage", "Right Margin"), 
                             span = c(1,1,2,2), 
                             align = "c")


tabx <- xtable(res, caption = "Established Party Support: Randomized Inference", label = "estran", 
               align = c("l","l","c","c","c","c","c"))
print(tabx, booktabs = TRUE, hline.after = c(0,2), add.to.row = a_header, include.rownames = F,
      file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/estran.tex")

print(tabx, booktabs = TRUE, hline.after = c(0,2), add.to.row = a_header, include.rownames = F)

# Donut hole regression (Table A19, A20 & A21)

runoff2 <- runoff

lengg <- c(0:5)
resu <- list()
resu2 <- list()
resu3 <- list()

for (i in lengg) {
  test <- runoff[abs(runoff2$socmarg) < i/1000,] 
  runoff3 <- runoff[abs(runoff$socmarg) >=i/1000,]  
  
  i <- i+1
  exobsup <- sum(test$socmarg>0)
  exobsdown <- sum(test$socmarg<0)
  
  cleared <-  rdrobust(y = runoff3$diffvotensoc, x = runoff3$socmarg, 
                       covs = cbind(runoff3$lcomp,runoff3$Year3,runoff3$id, runoff3$marg,runoff3$lnonsocsup,runoff3$clea,runoff3$votensoc),
                       all = T, cluster = runoff3$Electoral_District_Number)
  
  resu[[i]] <- c(cleared[[3]][[1]],cleared[[8]][[3]],cleared[[8]][[6]],(i-1)/1000, sum(cleared[[22]]),exobsup,exobsdown)
  
  rigmar <- subset(runoff3, !(difcro2 ==0 & socmarg>0))
  
  rightred <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg, 
                       covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id,rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
                       all = T, cluster = rigmar$Electoral_District_Number)
  resu2[[i]] <- c(rightred[[3]][[1]],rightred[[8]][[3]],rightred[[8]][[6]],(i-1)/1000, sum(rightred[[22]]),exobsup,exobsdown) 
  
  rigmar <- subset(runoff3, !(rightmar2 ==0 & socmarg>0))
  
  leftred <- rdrobust(y = rigmar$diffvotensoc, x = rigmar$socmarg, 
                       covs = cbind(rigmar$lcomp,rigmar$Year3, rigmar$id, rigmar$marg,rigmar$lnonsocsup,rigmar$clea,rigmar$votensoc),
                       all = T, cluster = rigmar$Electoral_District_Number)
  resu3[[i]] <- c(leftred[[3]][[1]],leftred[[8]][[3]],leftred[[8]][[6]],(i-1)/1000, sum(leftred[[22]]),exobsup,exobsdown) 
  i <- i-1
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("RDD Est.","Lower CI","Upper CI","Donut-Hole Radius","Num. Obs.",
                     "Excl. Obs. Right","Excl. Obs. Right")
regra <- regra[c(3:nrow(regra)),]

tabx <- xtable(regra, caption = "Donut regression", label = "donvotall", 
               align = c("l","l","c","c","c","c","c","c"),digits=c(0,2,2,2,3,0,0,0))
print(tabx, booktabs = TRUE, hline.after = c(-1,0,4), include.rownames = F,
      file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/donvotall.tex", floating.environment = "sidewaystable")

regra2 <- do.call(rbind.data.frame, resu2)
colnames(regra2) <- c("RDD Est.","Lower CI","Upper CI","Donut-Hole Radius","Num. Obs.",
                      "Excl. Obs. Right","Excl. Obs. Right")
regra2 <- regra2[c(3:nrow(regra2)),]
tabx <- xtable(regra2, caption = "Donut regression (Cleavage)", label = "doncleavot", 
               align = c("l","l","c","c","c","c","c","c"),digits=c(0,2,2,2,3,0,0,0))
print(tabx, booktabs = TRUE, hline.after = c(-1,0,4), include.rownames = F,
      file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/doncleavot.tex", floating.environment = "sidewaystable")

regra3 <- do.call(rbind.data.frame, resu3)
colnames(regra3) <- c("RDD Est.","Lower CI","Upper CI","Donut-Hole Radius","Num. Obs.",
                      "Excl. Obs. Right","Excl. Obs. Right")
regra3 <- regra3[c(3:nrow(regra3)),]
tabx <- xtable(regra3, caption = "Donut regression (Right Margin)", label = "donrigvot", 
               align = c("l","l","c","c","c","c","c","c"),digits=c(0,2,2,2,3,0,0,0))
print(tabx, booktabs = TRUE, hline.after = c(-1,0,4), include.rownames = F,
      file = "C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/donrigvot.tex", floating.environment = "sidewaystable")


### Placebo: Non-socialist candidate entry (conservative/catholic/liberal/minority)

## Catholic Parties (Figure A5a)

runoff <- read.csv("cath.csv")

# Vote share

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$cathmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
regra$all <- c("Yes")
colnames(regra) <- c("est","low","upp","bw","obs","all")

libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$cathmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)

bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Catholic Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/cathvoteall.eps", height=7, width=10, units='in',device=cairo_ps)


# Cross-cleavage alliances (Figure A3a)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$difcro2, x = runoff$cathmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$difcro2, x = runoff$cathmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Catholic Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/cathlcgraf.eps", height=7, width=10, units='in',device=cairo_ps)


# Right Margin (Figure A3b)


resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$rightmar2, x = runoff$cathmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$rightmar2, x = runoff$cathmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Catholic Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/cathrigraf.eps", height=7, width=10, units='in',device=cairo_ps)

## Conservative Parties (Figure A5b)

runoff <- read.csv("cons.csv")

# Vote share

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$conmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
regra$all <- c("Yes")
colnames(regra) <- c("est","low","upp","bw","obs","all")

libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$conmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)

bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Conservative Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/convoteall.eps", height=7, width=10, units='in',device=cairo_ps)


# Cross-cleavage alliances (Figure A3c)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$difcro2, x = runoff$conmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$difcro2, x = runoff$conmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Conservative Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/conlcgraf.eps", height=7, width=10, units='in',device=cairo_ps)


# Right Margin (Figure A3d)


resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$rightmar2, x = runoff$conmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$rightmar2, x = runoff$conmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Conservative Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/conrigraf.eps", height=7, width=10, units='in',device=cairo_ps)


## Liberal Parties

runoff <- read.csv("lib.csv")

# Vote share (Figure A5c)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$libmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
regra$all <- c("Yes")
colnames(regra) <- c("est","low","upp","bw","obs","all")

libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$libmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)

bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Liberal Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/libvoteall.eps", height=7, width=10, units='in',device=cairo_ps)


# Cross-cleavage alliances (Figure A4a)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$difcro2, x = runoff$libmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$difcro2, x = runoff$libmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Liberal Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/liblcgraf.eps", height=7, width=10, units='in',device=cairo_ps)


# Right Margin (Figure A4b)


resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$rightmar2, x = runoff$libmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$rightmar2, x = runoff$libmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Liberal Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/librigraf.eps", height=7, width=10, units='in',device=cairo_ps)

## Minority Parties

runoff <- read.csv("mino.csv")

# Vote share (Figure A5d)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$minmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
regra$all <- c("Yes")
colnames(regra) <- c("est","low","upp","bw","obs","all")

libcon <- rdrobust(y = runoff$diffvotensoc, x = runoff$minmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)

bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Minority Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/minvoteall.eps", height=7, width=10, units='in',device=cairo_ps)


# Cross-cleavage alliances (Figure A4c)

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$difcro2, x = runoff$minmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$difcro2, x = runoff$minmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )

rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Minority Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(3, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/minlcgraf.eps", height=7, width=10, units='in',device=cairo_ps)


# Right Margin (Figure A4d)


resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$rightmar2, x = runoff$minmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$rightmar2, x = runoff$minmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Minority Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/minrigraf.eps", height=7, width=10, units='in',device=cairo_ps)


##### Turnout as alternative causal mechanism

### Turnout rates (Figure A6)

runoff <- read.csv("data.csv")

resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$diffturn, x = runoff$socmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc,runoff$turnfir),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$diffturn, x = runoff$socmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc,runoff$turnfir),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Minority Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/turnout.eps", height=7, width=10, units='in',device=cairo_ps)

### Raw turnout (Figure A7)


resu <- list()

for (i in 5:30){
  
  libcon <- rdrobust(y = runoff$diffturnraw, x = runoff$socmarg, 
                     covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc,
                                  runoff$turnrawfir),
                     all = T, cluster = runoff$Electoral_District_Number, h = i/100)
  
  
  resu[[i]] <- c(libcon[[3]][[1]],libcon[[8]][[3]],libcon[[8]][[6]],i/100, sum(libcon[[22]]))
  
}

regra <- do.call(rbind.data.frame, resu)
colnames(regra) <- c("est","low","upp","bw","obs")

libcon <- rdrobust(y = runoff$diffturnraw, x = runoff$socmarg, 
                   covs = cbind(runoff$lcomp,runoff$Year3, runoff$id, runoff$marg,runoff$lnonsocsup,runoff$clea,runoff$votensoc,
                                runoff$turnrawfir),
                   all = T, cluster = runoff$Electoral_District_Number)
bw <- libcon[[2]]
bw <- round(bw[1,1],2)
regra$optbw <- bw
regra$bwo <- ifelse(regra$optbw == regra$bw,1,0 )


rigra <- ggplot(data = regra, aes(y = est, x = bw, color = as.factor(bwo)))+geom_point(aes())+theme_bw()+
  geom_errorbar(aes(ymin = low, ymax = upp),position=position_dodge(0.05))+scale_color_manual(values = c("black","red"))+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(), legend.position="none")+labs(title = "", x = "",
                                                                     y = "Marginal Effect of Minority Candidate in Runoff")+ geom_hline(yintercept = 0, size = 0.2)

riobs <- ggplot(data = regra, aes(y = obs, x = bw))+geom_line(aes())+theme_bw()+
  theme(axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"),
        panel.border = element_blank())+labs(title = "", x = "Bandwidth",
                                             y = "Number of Observations")+ geom_hline(yintercept = 0, size = 0.2)

plotrig <- plot_grid(rigra,riobs, nrow = 2, align = "v", rel_heights = c(2, 1))

ggsave("C:/Work/Dropbox/Papers/Soc_Threat/Paper/Final/turnoutraw.eps", height=7, width=10, units='in',device=cairo_ps)

