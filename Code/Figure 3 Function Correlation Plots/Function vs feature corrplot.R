rm(list = ls())

#correlation for feature vs function

library(corrplot)


data <- read.csv("Function_Array_SERUMv1.csv")

is.na(data)
data <- na.omit(data)
featCols = c(2:6)
fxnCols = 7:20

corrMat = matrix(0, nrow = length(fxnCols), ncol = length(featCols))
colnames(corrMat) = colnames(data)[featCols]
row.names(corrMat) = colnames(data)[fxnCols]
pMat = corrMat

for (i in 1:nrow(corrMat)){
  for (j in 1:ncol(corrMat)){
    fxn = row.names(corrMat)[i]
    feat = colnames(corrMat)[j]
    corrMat[fxn,feat] = cor(x = data[,feat], y = data[,fxn])
    pMat[fxn,feat] = cor.test(x = data[,feat], y = data[,fxn])$p.value
  }
}

corrplot(corrMat, p.mat = pMat, method = "shade", tl.cex = 0.75, sig.level = c(.001, .01, .05), pch.cex =0.8,
         insig = "label_sig", pch.col = "white", is.corr=FALSE, 
         col=colorRampPalette(c("dark orange", "white", "blue"))(50), tl.col = "black", cl.lim=c(-1,1))

                                              
data <- read.csv("Function_Array_NASALv1.csv")

is.na(data)
data <- na.omit(data)
featCols <- c(2:6)
fxnCols <- 7:20

corrMat <- matrix(0, nrow = length(fxnCols), ncol = length(featCols))
colnames(corrMat) = colnames(data)[featCols]
row.names(corrMat) = colnames(data)[fxnCols]
pMat = corrMat

for (i in 1:nrow(corrMat)){
  for (j in 1:ncol(corrMat)){
    fxn = row.names(corrMat)[i]
    feat = colnames(corrMat)[j]
    corrMat[fxn,feat] = cor(x = data[,feat], y = data[,fxn])
    pMat[fxn,feat] = cor.test(x = data[,feat], y = data[,fxn])$p.value
  }
}

corrplot(corrMat, p.mat = pMat, method = "shade", tl.cex = 0.75, sig.level = c(.001, .01, .05), pch.cex =0.8,
         insig = "label_sig", pch.col = "white", is.corr=FALSE, 
         col=colorRampPalette(c("dark orange", "white", "blue"))(50), tl.col = "black", cl.lim=c(-1,1))

