library(magrittr)
library(dplyr)

data <- read.csv("wuvc_nc_balance_data.csv")

# Figure C1

votes.tr <- data %>% filter(district == 1 & m500 == 1 & treatment == 1) %>% select(voted.2008) %>% sum()
votes.co <- data %>% filter(district == 1 & m500 == 1 & treatment == 0) %>% select(voted.2008) %>% sum()

regis.tr <- data %>% filter(district == 1 & m500 == 1 & treatment == 1) %>% select(voted.2008) %>% nrow()
regis.co <- data %>% filter(district == 1 & m500 == 1 & treatment == 0) %>% select(voted.2008) %>% nrow()

sens = function(votes.tr, regis.tr, votes.co, regis.co, p.tr,k) {
  St = (votes.tr/regis.tr)
  Sc = (votes.co/regis.co)
  turnREG.ratio = St/Sc
  pr.co = p.tr * turnREG.ratio    
  delta = p.tr *(St - Sc/k)
  return(delta)
}

St = (votes.tr/regis.tr)
Sc = (votes.co/regis.co)

Rtc = (regis.tr/regis.co)

k = 1

plot(x = seq(0, 1, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, 1, by=0.01),k=k ),
     type="l", xlim = c(0,1), ylim = c(-0.4, 0.4), axes=FALSE,
     xlab = '', ylab=expression("Treatment-control difference in true turnout ("*Delta*")"), col = "black", lty=1, lwd=2, main = "2008 Election")

axis(side=1, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"), tick = TRUE)

axis(side=2, at = c(-0.4, -0.2, 0, 0.2, 0.4), labels = c("-0.4", "-0.2", "0", "0.2", "0.4"), tick = TRUE, las=2)

axis(side=2, at = c(St-Sc), labels = expression(T[T]^Reg-T[C]^Reg), tick = TRUE, las=2, cex.axis = .5)

mtext(expression("Registration rate in treatment group ("*r[T]*")"), side=1,line=3)

for(k in c(2, 3, 5)) 
  lines(x = seq(0, 1, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, 1, by=0.01),k=k ),
        col = "black", lty=1, lwd=2)


for(k in c(Sc/St)) 
  lines(x = seq(0, 1, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, 1, by=0.01),k=k ),
        col = "blue", lty=1, lwd=2.5)
kstar = k

i = 5

for(k in c(0.8, 0.5) ) {
  lines(x = seq(0, k, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, k, by=0.01),k=k),
        col = "black", lty=1, lwd=2)
  i = i + 1
}

abline(h=St-Sc, lty=2, col="red", lwd=1)
legend("topleft", legend=c(expression('Differential registration factor: k = '*r[T]/r[C]),
                           bquote(T[T]^Reg-T[C]^Reg == .(round(St-Sc, 2))), 
                           bquote(T[C]^Reg/T[T]^Reg == .(round(Rtc, 2))),
                           bquote(k^"*" == .(round(kstar, 2)))))

abline(v=1, lty=2, col="red", lwd=1)
for(k in c(1, 2, 3, 5)) text(0.79,  sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=0.8,k=k)+0.05, paste("k = ", k, sep=""))
for(k in c(0.8)) text(0.807,  sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=0.8,k=k)+0.03, paste("k = ", k, sep="")) 
for(k in c(0.5)) text(0.42,  sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=0.4,k=k)+0.04, paste("k = ", k, sep="")) 
text(0.85,  0  - 0.05, expression(paste(k,"*"==T[C]^Reg/T[T]^Reg)))
abline(h = 0)

dev.off()

# Figure C2

votes.tr <- data %>% filter(district == 1 & m500 == 1 & treatment == 1) %>% select(voted.2008) %>% sum()
votes.co <- data %>% filter(district == 1 & m500 == 1 & treatment == 0) %>% select(voted.2008) %>% sum()

regis.tr <- data %>% filter(district == 1 & m500 == 1 & treatment == 1) %>% select(voted.2008) %>% nrow()
regis.co <- data %>% filter(district == 1 & m500 == 1 & treatment == 0) %>% select(voted.2008) %>% nrow()

sens = function(votes.tr, regis.tr, votes.co, regis.co, p.tr,k) {
  St = (votes.tr/regis.tr)
  Sc = (votes.co/regis.co)
  turnREG.ratio = St/Sc
  pr.co = p.tr * turnREG.ratio    
  delta = p.tr *(St - Sc/k)
  return(delta)
}

St = (votes.tr/regis.tr)
Sc = (votes.co/regis.co)

Rtc = (regis.tr/regis.co)

k = 1

plot(x = seq(0, 1, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, 1, by=0.01),k=k ),
     type="l", xlim = c(0,1), ylim = c(-0.4, 0.4), axes=FALSE,
     xlab = '', ylab=expression("Treatment-control difference in true turnout ("*Delta*")"), col = "black", lty=1, lwd=2, main = "2012 Election")

axis(side=1, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"), tick = TRUE)

axis(side=2, at = c(-0.4, -0.2, 0, 0.2, 0.4), labels = c("-0.4", "-0.2", "0", "0.2", "0.4"), tick = TRUE, las=2)

axis(side=2, at = c(St-Sc), labels = expression(T[T]^Reg-T[C]^Reg), tick = TRUE, las=2, cex.axis = .5)

mtext(expression("Registration rate in treatment group ("*r[T]*")"), side=1,line=3)

for(k in c(2, 3, 5)) 
  lines(x = seq(0, 1, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, 1, by=0.01),k=k ),
        col = "black", lty=1, lwd=2)


for(k in c(Sc/St)) 
  lines(x = seq(0, 1, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, 1, by=0.01),k=k ),
        col = "blue", lty=1, lwd=2.5)
kstar = k

i = 5

for(k in c(0.8, 0.5) ) {
  lines(x = seq(0, k, by=0.01), sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=seq(0, k, by=0.01),k=k),
        col = "black", lty=1, lwd=2)
  i = i + 1
}

abline(h=St-Sc, lty=2, col="red", lwd=1)
legend("topleft", legend=c(expression('Differential registration factor: k = '*r[T]/r[C]),
                           bquote(T[T]^Reg-T[C]^Reg == .(round(St-Sc, 2))), 
                           bquote(T[C]^Reg/T[T]^Reg == .(round(Rtc, 2))),
                           bquote(k^"*" == .(round(kstar, 2)))))

abline(v=1, lty=2, col="red", lwd=1)
for(k in c(1, 2, 3, 5)) text(0.79,  sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=0.8,k=k)+0.05, paste("k = ", k, sep=""))
for(k in c(0.8)) text(0.807,  sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=0.8,k=k)+0.03, paste("k = ", k, sep="")) 
for(k in c(0.5)) text(0.42,  sens(votes.tr=votes.tr, regis.tr=regis.tr,votes.co=votes.co, regis.co=regis.co, p.tr=0.4,k=k)+0.04, paste("k = ", k, sep="")) 
text(0.85,  0  - 0.05, expression(paste(k,"*"==T[C]^Reg/T[T]^Reg)))
abline(h = 0)



