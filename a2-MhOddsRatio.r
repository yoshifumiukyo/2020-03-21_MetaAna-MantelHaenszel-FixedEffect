############################################################
# R-project                                                #
# Program      : a2-MhOddsRatio.r                          #
# Protocol     :                                           #
# Date         :                                           #
# Last         :                                           #
# Programmer   : yoshifumi ukyo                            #
#                                                          #
############################################################
# [Ver.0000]                                               #
# Memorandom   :                                           #
#                                                          #
############################################################


#----- clean-up working directory 
rm(list = (ls(all = TRUE)))
#----- library assignment 
base_dir <- ""
setwd(base_dir)


############################################################
# MH odds ratio                                            #  
############################################################

#----- Frequent handwashing
study <- c("Chen2009", "Lau2004a", "Nishiura2005", "Seto2003", "Teleman2004", "Wu2004", "Yin2004")
a <- c(45, 61, 15, 10, 27, 73, 28)
b <- c(46, 269, 10, 3, 9, 21, 49)
c <- c(323, 222, 56, 227, 46, 253, 97)
d <- c(334, 438, 34, 14, 4, 28, 83)

ds <- data.frame(study = study, a = a, b = b, c = c, d = d)
ds <- subset(ds, study != "Lau2004a")


#----- Wearing mask
study <- c("Chen2009", "Lau2004a", "Liu2009", "Nishiura2005", "Seto2003", "Wu2004", "Yin2004")
a <- c(59, 93, 15, 8, 0, 25, 68)
b <- c(32, 237, 36, 17, 13, 69, 9)
c <- c(541, 388, 259, 35, 51, 121, 178)
d <- c(116, 272, 167, 55, 190, 160, 2)

ds <- data.frame(study = study, a = a, b = b, c = c, d = d)
ds <- subset(ds, study != "Lau2004a")



ai <- ds$a 
bi <- ds$b
ci <- ds$c 
di <- ds$d 
tn <- ds$a + ds$b + ds$c + ds$d
n1 <- ds$a + ds$b 
n0 <- ds$c + ds$d
m1 <- ds$a + ds$c 
m0 <- ds$b + ds$d

or <- (ai / bi) / (ci / di)
se <- sqrt(tn / bi / ci)
# vlog <- 1/ai + 1/bi + 1/ci + 1/di
log_or <- log(or)
or_low <- exp(log_or - qnorm(p = 0.975) * se)
or_upp <- exp(log_or + qnorm(p = 0.975) * se) 
w <- 1 / se / se

ds$log_or <- log_or 
ds$or     <- or 
ds$or_low <- or_low 
ds$or_upp <- or_upp 
ds$w      <- w / sum(w)

mh_or <- sum(ai * di / tn) / sum(bi * ci / tn)

P <- (ai + di)/tn 
Q <- (bi + ci)/tn 
R <- (ai * di)/tn 
S <- (bi * ci)/tn


mh_var <- (sum(P*R)/sum(R)^2 + sum(P*S+Q*R)/sum(R)/sum(S) + sum(Q*S)/sum(S)^2)/2

mh_or_low <- exp(log(mh_or) - qnorm(p = 0.975) * sqrt(mh_var))
mh_or_upp <- exp(log(mh_or) + qnorm(p = 0.975) * sqrt(mh_var))


#----- mantelhaen.test 
tab <- array(dim = c(2, 2, length(study))) 
for (s in 1:length(study)){
  tab[1, 1, s] <- a[s]
  tab[1, 2, s] <- b[s]
  tab[2, 1, s] <- c[s]
  tab[2, 2, s] <- d[s]
}

mantelhaen.test(x = tab, alternative = "two.sided", conf.level = 0.95)



