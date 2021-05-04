### Descriptives for Socialist Threat (Tables 1, A2, A4, A22, A23)
### Author: André Walter
### andre.walter[at]unisg.ch

### Install rdrobust package version 0.99.1 to replicate the results (package "devtools" required)

devtools::install_version("rdrobust", version = "0.99.1", repos = "http://cran.us.r-project.org")

### Load packages

library(texreg)
library(ggplot2)
library(cowplot)
library(rdrobust)
library(rddensity)
library(rdlocrand)
library(dplyr)
library(tables)
library(xtable) 
library(openxlsx)


setwd("C:/Work/Dropbox/Papers/Soc_Threat/Estimation/")

rm(list=ls())

load("est.RData")

runoff$strnonpar <- as.factor(runoff$strnonpar)
runoff$clea <- ifelse(runoff$strnonpar %in% c("K","RP","A","DS","M","DSR","CS","DR","BdL","WVg","AVP","HRP","ELL","ub"), "Conservative",
                      ifelse(runoff$strnonpar %in% c("FVP","NL","FoVP","DF","DBB","FVg","NS","DVg","DVP","BB","ELV"), "Liberal",
                             ifelse(runoff$strnonpar %in% c("Z","ELZ"), "Catholic",
                                    ifelse(runoff$strnonpar %in% c("Minority"), "Minority","Socialist"))))
table(runoff$clea)
runoff <- cbind(runoff,model.matrix(~runoff$clea-1))
colnames(runoff)[c(57:60)] <- c("Catholic","Conservative","Liberal","Minority")


# Party table (Table A2)

parties <- data.frame(table(runoff$strnonpar))

con <- parties[c(17,21,7,3,8,9,4,23,1,18),]
con[,1] <- c("German Conservatives","Free Conservatives","German Reform Party","German Agrarian League",
             "German Social Party","German Social Reform Party","Christian Social Party","Economic Union",
             "Anti-Semits","Middle Class Party")
lib <- parties[c(20,16,14,6,15,11,2,5,10),]
lib[nrow(lib)+1,] <- NA
lib[,1] <- c("National Liberal Party","Free-minded People's Party","Progressive People's Party",
             "German Free-minded Party","Free-minded Union","German People's Party","Bavarian Peasants' League",
             "German Peasants' League","Democratic Union",NA)
cat <- parties[c(24,12,13),]
cat[nrow(cat)+7,] <- NA
cat[,1] <- c("Centre Party","State Party of Alsace-Lorraine","Centre Party of Alsace-Lorraine",rep(NA,7))
min <- parties[c(19),]
min[nrow(min)+9,] <- NA
min[,1] <- c(paste('Minorities','footnote1'), rep(NA,9))

par <- cbind(con,lib,cat,min)

colnames(par) <- c(rep(c("Party","Entry"),4))

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
                   command = c("\\hline\n  ", header, "\\hline\n  "))
  return(addtorow)
}

a_header <- construct_header(par,
                             grp_names = c("Conservatives", "Liberals", "Catholics","Minorities"), 
                             span = c(2, 2, 2,2), 
                             align = "c")


tabx <- xtable(par, caption = "Individual Parties and Runoff Entry", label = "parties", 
               align = c("c","c","c","c","c","c","c","c","c"))
print(tabx, booktabs = TRUE, hline.after = F, add.to.row = a_header, include.rownames = F,
      file = "D:/Sync/Dropbox/Papers/Soc_Threat/Paper/Final/par.tex", floating.environment = "sidewaystable", 
      scalebox = 0.7, sanitize.text.function = function(str){
        str <- gsub("footnote1","\\footnote{Poles, Danes, Alsace-Lorraines, and Hanoverian Federalists.}", str, fixed = TRUE)
        
      })
print(tabx, booktabs = TRUE, hline.after = F, add.to.row = a_header, include.rownames = F )

### Second round entry table (Table 1)
ger <- read.xlsx("C:/Work/Dropbox/Papers/Soc_Threat/Data/data.xlsx")

ger$Country <- as.factor(ger$Country)
ger$Round_of_Voting <- as.factor(ger$Round_of_Voting)
ger$fYear <- as.factor(ger$Year)
ger$Surname <- as.factor(ger$Surname)
ger$Party <- as.factor(ger$Party)

# Create runoff data

secpar <- ger %>% group_by(Electoral_District_Number,Year2,by_elect) %>% filter(any(Round_of_Voting == 2))

secpar <- ger %>% group_by(Electoral_District_Number,Year2,by_elect) %>% filter(Round_of_Voting == 2)

winpar <- secpar %>% group_by(Party) %>% dplyr::summarize(count = n(),win = sum(Elected))

winpar$clea <- ifelse(winpar$Party %in% c("K","RP","A","DS","M","DSR","CS","DR","BdL","WVg","AVP","HRP","ELL","ub"), "Conservative",
                      ifelse(winpar$Party %in% c("FVP","NL","FoVP","DF","DBB","FVg","NS","DVg","DVP","BB","ELV"), "Liberal",
                             ifelse(winpar$Party %in% c("Z","ELZ"), "Catholic",
                                    ifelse(winpar$Party %in% c("Minority"), "Minority","Socialist"))))

winpar <- winpar %>% group_by(clea) %>% summarize(count = sum(count),win = sum(win)) %>% mutate(win_sh = win/count)

names(winpar) <- c("Party","Entries","Electoral Victories","Share Victories")

print(xtable(winpar, caption = "Runoff Entries and Results by Party",label = "runres",digits=c(0,0,0,0,2)), include.rownames =F,booktabs = TRUE, 
      file = "D:/Sync/Dropbox/Papers/Soc_Threat/Paper/Final/parwin.tex")



detach("package:xtable", unload=TRUE)
rm(construct_header)

# Summary statistics (Table A4)

runoff$clea <- as.factor(runoff$clea)

sumst <- runoff %>% dplyr::select(lcomp,marg,lnonsocsup,votensoc,Catholic,Conservative,Liberal,Minority,difcro2,rightmar2,diffvotensoc,leftIIround)

fintab <- tabular((Heading("Number of Parties")*lcomp + Heading("Competition")*marg+Heading("Percent Alliances I. Round" )*lnonsocsup+
                     Heading("Vote Fractionalization")*votensoc+Heading("Catholic Candidate")*Catholic+Heading("Conservative Candidate")*Conservative+
                   Heading("Liberal Candidate")*Liberal+Heading("Minority Candidate")*Minority+Heading("Cross-Cleavage Alliance")*difcro2+
                     Heading("Right Margin")*rightmar2)+Heading("Vote Share Difference Non-Socialist")*diffvotensoc ~ Justify(c)*Heading("Socialist in Runoff")*(factor(leftIIround))*(mean + sd + min + max)+(est=1)+(pval=1), data=sumst )
fintab

### Coefficients

rddcoef <- c()

test <- rdrobust(y = runoff$lcomp, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$marg, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$lnonsocsup, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$votensoc, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$Catholic, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$Conservative, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$Liberal, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$Minority, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$difcro2, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$rightmar2, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

test <- rdrobust(y = runoff$diffvotensoc, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[3]][[1]]
rddcoef <- c(rddcoef,test)

fintab[,9] <- rddcoef
# Adjust p-values for multiple comparison

pvals <- c()

test <- rdrobust(y = runoff$lcomp, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$marg, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$lnonsocsup, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$votensoc, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$Catholic, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$Conservative, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$Liberal, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$Minority, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$difcro2, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$rightmar2, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

test <- rdrobust(y = runoff$diffvotensoc, x = runoff$socmarg, 
                 all = T, cluster = runoff$Electoral_District_Number)[[7]][[3]]
pvals <- c(pvals,test)

fintab[,10] <- p.adjust(pvals, method = "fdr")

# Create table

labs <- colLabels(fintab)
labs[2, 1] <- "No"
labs[2, 5] <- "Yes"
colLabels(fintab) <- labs
latex(fintab)

save <- booktabs()
latex(fintab)
table_options(save)
latex(fintab, file = "D:/Sync/Dropbox/Papers/Soc_Threat/Paper/sumsta.tex")


### Table by Cross-Cleavage Alliance (Table A22)
croal <- tabular((Heading("Number of Parties")*lcomp + Heading("Competition")*marg+Heading("Percent Alliances I. Round" )*lnonsocsup+
                     Heading("Vote Fractionalization")*votensoc+Heading("Catholic Candidate")*Catholic+Heading("Conservative Candidate")*Conservative+
                     Heading("Liberal Candidate")*Liberal+Heading("Minority Candidate")*Minority+Heading("Socialist Candidate")*leftIIround) ~ Justify(c)*Heading("Cross-Cleavage Alliance")*(factor(difcro2))*(mean + sd + min + max)+(est=1)+(pval=1), data=sumst )

# Point estimates

est <- c()

test <- t.test(lcomp~difcro2, data = sumst)[[5]][[2]]-t.test(lcomp~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(marg~difcro2, data = sumst)[[5]][[2]]-t.test(marg~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(lnonsocsup~difcro2, data = sumst)[[5]][[2]]-t.test(lnonsocsup~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(votensoc~difcro2, data = sumst)[[5]][[2]]-t.test(votensoc~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Catholic~difcro2, data = sumst)[[5]][[2]]-t.test(Catholic~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Conservative~difcro2, data = sumst)[[5]][[2]]-t.test(Conservative~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Liberal~difcro2, data = sumst)[[5]][[2]]-t.test(Liberal~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Minority~difcro2, data = sumst)[[5]][[2]]-t.test(Minority~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(leftIIround~difcro2, data = sumst)[[5]][[2]]-t.test(leftIIround~difcro2, data = sumst)[[5]][[1]]
est <- c(est,test)


croal[,9] <- round(est, 3)

# Adjust p-values for multiple comparison

pvals <- c()

test <- t.test(lcomp~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(marg~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(lnonsocsup~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(votensoc~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Catholic~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Conservative~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Liberal~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Minority~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(leftIIround~difcro2, data = sumst)[[3]]
pvals <- c(pvals,test)


croal[,10] <- round(p.adjust(pvals, method = "fdr"),3)


labs <- colLabels(croal)
labs[2, 1] <- "No"
labs[2, 5] <- "Yes"
colLabels(croal) <- labs

save <- booktabs()
table_options(save)
latexTable(croal)

### Table by Right Margin (Table A23)
croal <- tabular((Heading("Number of Parties")*lcomp + Heading("Competition")*marg+Heading("Percent Alliances I. Round" )*lnonsocsup+
                    Heading("Vote Fractionalization")*votensoc+Heading("Catholic Candidate")*Catholic+Heading("Conservative Candidate")*Conservative+
                    Heading("Liberal Candidate")*Liberal+Heading("Minority Candidate")*Minority+Heading("Socialist Candidate")*leftIIround) ~ Justify(c)*Heading("Right Margin")*(factor(rightmar2))*(mean + sd + min + max)+(est=1)+(pval=1), data=sumst )


# Point estimates


est <- c()

test <- t.test(lcomp~rightmar2, data = sumst)[[5]][[2]]-t.test(lcomp~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(marg~rightmar2, data = sumst)[[5]][[2]]-t.test(marg~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(lnonsocsup~rightmar2, data = sumst)[[5]][[2]]-t.test(lnonsocsup~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(votensoc~rightmar2, data = sumst)[[5]][[2]]-t.test(votensoc~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Catholic~rightmar2, data = sumst)[[5]][[2]]-t.test(Catholic~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Conservative~rightmar2, data = sumst)[[5]][[2]]-t.test(Conservative~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Liberal~rightmar2, data = sumst)[[5]][[2]]-t.test(Liberal~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(Minority~rightmar2, data = sumst)[[5]][[2]]-t.test(Minority~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)

test <- t.test(leftIIround~rightmar2, data = sumst)[[5]][[2]]-t.test(leftIIround~rightmar2, data = sumst)[[5]][[1]]
est <- c(est,test)


croal[,9] <- round(est, 3)

# Adjust p-values for multiple comparison

pvals <- c()

test <- t.test(lcomp~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(marg~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(lnonsocsup~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(votensoc~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Catholic~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Conservative~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Liberal~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(Minority~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)

test <- t.test(leftIIround~rightmar2, data = sumst)[[3]]
pvals <- c(pvals,test)


croal[,10] <- round(p.adjust(pvals, method = "fdr"),3)




labs <- colLabels(croal)
labs[2, 1] <- "No"
labs[2, 5] <- "Yes"
colLabels(croal) <- labs

save <- booktabs()
table_options(save)
latexTable(croal)