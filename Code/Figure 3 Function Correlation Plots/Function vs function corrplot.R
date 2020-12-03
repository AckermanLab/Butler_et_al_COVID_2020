rm(list = ls())

library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(gtools)

data <- read.csv("Function_SERUMv1.csv")

rownames(data)<-data[,1]
data[,1]<-NULL
data <- na.omit(data)

panel.cor <- function(x,y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  p <- cor.test(x,y)$p.value
  r <- cor(x,y,use="pairwise.complete.obs")
  txt <- stars.pval(p)
  cex.cor <- 0.8/strwidth(txt)
  fill=brewer.pal(n=8, name="RdYlBu")
  if(r < -0.75){fill=fill[1]}
  else if (r > -0.75 & r <= -0.5){fill=fill[2]}
  else if (r > -0.5 & r <= -0.25){fill=fill[3]}
  else if (r > -0.25 & r <= 0){fill=fill[4]}
  else if (r > 0 & r <= 0.25){fill=fill[5]}
  else if (r > 0.25 & r <= 0.5){fill=fill[6]}
  else if (r > 0.5 & r <= 0.75){fill=fill[7]}
  else (fill=fill[8])
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = fill)
  text(0.5, 0.5, txt)
}

upper.panel<-function(x,y){
  points(x,y, pch = 5)
}

M<-rcorr(as.matrix(data))   

#just the lower cor plot

#both
pairs(data,lower.panel = panel.cor, upper.panel = upper.panel)

#splice from legend below


corrplot(M$r,p.mat=M$P,insig="label_sig",sig.level =c(0.001,.01,.05),pch.cex=0.8,pch.col="white",
         method="shade",add=F, col=colorRampPalette(c("dark orange", "white", "blue"))(50),type="upper",
         diag = F, tl.col = "black",tl.cex=0.75)




data <- read.csv("Function_NASALv1.csv")

rownames(data)<-data[,1]
data[,1]<-NULL
data <- na.omit(data)

panel.cor <- function(x,y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  p <- cor.test(x,y)$p.value
  r <- cor(x,y,use="pairwise.complete.obs")
  txt <- stars.pval(p)
  cex.cor <- 0.8/strwidth(txt)
  fill=brewer.pal(n=8, name="RdYlBu")
  if(r < -0.75){fill=fill[1]}
  else if (r > -0.75 & r <= -0.5){fill=fill[2]}
  else if (r > -0.5 & r <= -0.25){fill=fill[3]}
  else if (r > -0.25 & r <= 0){fill=fill[4]}
  else if (r > 0 & r <= 0.25){fill=fill[5]}
  else if (r > 0.25 & r <= 0.5){fill=fill[6]}
  else if (r > 0.5 & r <= 0.75){fill=fill[7]}
  else (fill=fill[8])
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = fill)
  text(0.5, 0.5, txt)
}

upper.panel<-function(x,y){
  points(x,y, pch = 5)
}

M<-rcorr(as.matrix(data))   

#just the lower cor plot

#both
pairs(data,lower.panel = panel.cor, upper.panel = upper.panel)

#splice from legend below


corrplot(M$r,p.mat=M$P,insig="label_sig",sig.level =c(0.001,.01,.05),pch.cex=0.8,pch.col="white",
         method="shade",add=F, col=colorRampPalette(c("dark orange", "white", "blue"))(50),type="upper",
         diag = F, tl.col = "black",tl.cex=0.75)
