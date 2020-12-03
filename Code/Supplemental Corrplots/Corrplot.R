library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(gtools)
library(grid)
library(pheatmap)

#load in data
data <- read.csv("Fc_Array.csv", header=FALSE)
dim(data)

#remove all non-CoV2 columns

data<-data[,-which(data[2,]=="non-CoV-2")]
dim(data)

#break into our column categories

Categories<-data[1:4,5:ncol(data)]
Categories<-as.data.frame(t(Categories))
Categories<-Categories[,c(1,4)]
colnames(Categories)<-c("Detection", "Viral Antigen")

#now bin viral antigens

Categories$`Viral Antigen`<-as.character(Categories$`Viral Antigen`)


summary(Categories)

data<-data[-c(1:4),]

#make first column row names

data[,1]<-as.character(data[,1])
data[nrow(data),1]<-"blank2"
rownames(data)<-data[,1]
data[,1]<-NULL

#make the row categories data frame

RowCategories<-data[-c(nrow(data)-1,nrow(data)),1:3]
rownames(RowCategories)<-rownames(data)[-c(nrow(data)-1,nrow(data))]
colnames(RowCategories)<-c("Sex","Disease Severity", "Age")
RowCategories<-as.data.frame(RowCategories)
RowCategories$`Sample Type`<-as.factor(c(rep("Serum",20),rep("Serum",15),rep("Nasal Wash",19),rep("Nasal Wash",15)))
RowCategories$`CoV-2 Status`<-as.factor(c(rep("Convalescent",20),rep("Naive",15),rep("Convalescent",19),rep("Naive",15)))

#rebinning into less age group
RowCategories$Age<-as.character(RowCategories$Age)
RowCategories$Age[which(RowCategories$Age%in%c("20-29","<20", "30-39"))]<-"<40" 
RowCategories$Age[which(RowCategories$Age%in%c("40-49", "50-59"))]<-"40-59"
RowCategories$Age[which(RowCategories$Age%in%c("60-69","70-79"))]<-">60"
RowCategories$Age<-as.factor(RowCategories$Age)

summary(RowCategories)

#reorder columns
RowCategories<-RowCategories[,rev(c("CoV-2 Status","Sample Type","Disease Severity","Age","Sex"))]
RowCategories$Age <- NULL
RowCategories$Sex <- NULL

data<-data[,4:ncol(data)]

#Remove antigens that are less than 10 std. dev above blanks

(cutoff<-apply(data[c(nrow(data)-1,nrow(data)),],2,sd))
cutoff<-cutoff*10

data<-data[-c(nrow(data)-1,nrow(data)),]

data<-apply(data,2,as.numeric)

#Remove columns where <25% of samples are 10SD above blank

removeThese<-c()

for(i in 1:ncol(data)){
  p<-sum(na.omit(data[,i])>cutoff[i])/length(na.omit(data[,i]))
  if(p<=0.25){
    removeThese<-append(removeThese,i)
    cat(paste(p,"\n"))
  }
}

Categories[removeThese,]

Categories<-Categories[-removeThese,]
data<-data[,-removeThese]

colnames(data)<-paste(Categories$`Viral Antigen`,1:ncol(data))
rownames(Categories)<-paste(Categories$`Viral Antigen`,1:ncol(data))


#Scale and Center 

logdata<-log10(data)

#now check which columns are different between pos and negative, discard ones that are not. 

keepColumnsS<-c()
keepColumnsNW<-c()

for(i in 1:ncol(logdata)){
  serum<-logdata[grepl("serum",RowCategories$`Sample Type`,ignore.case = T),i]
  nasal<-logdata[grepl("nasal",RowCategories$`Sample Type`,ignore.case = T),i]
  
  ps<-t.test(serum[1:20],serum[21:35])$p.value
  pn<-t.test(nasal[1:19],nasal[20:34])$p.value
  
  if(ps<0.05|pn<0.05) {keepColumnsS<-append(keepColumnsS,i)
  keepColumnsNW<-append(keepColumnsNW,i)}
  
}

CategoriesSerum<-Categories[keepColumnsS,]
logdataS<-logdata[grepl("serum",RowCategories$`Sample Type`,ignore.case = T)&RowCategories$`CoV-2 Status`=="Convalescent",keepColumnsS]
RowCategoriesSerum <- RowCategories[grepl("serum",RowCategories$`Sample Type`,ignore.case = T)&RowCategories$`CoV-2 Status`=="Convalescent",]

SerumSamples <- logdataS

CategoriesNW<-Categories[keepColumnsNW,]
logdataNW<-logdata[grepl("nasal",RowCategories$`Sample Type`,ignore.case = T)&RowCategories$`CoV-2 Status`=="Convalescent",keepColumnsNW]
RowCategoriesNW <- RowCategories[grepl("nasal",RowCategories$`Sample Type`,ignore.case = T)&RowCategories$`CoV-2 Status`=="Convalescent",]

NasalSamples <- logdataNW

RowCategoriesSerum$`Sample Type` <- NULL
RowCategoriesNW$`Sample Type` <- NULL

#Serum Corrplot 

M<-rcorr(as.matrix(logdataS))

IGGColors<-c("dodgerblue","#C6E2FF","#CC3232", "#4F94CD","orangered")
names(IGGColors)<-c("a-IgG","a-IgG1","a-IgG2","a-IgG3","a-IgG4")

FCColors<-c("#8fbc8f", "#458B74", "#CDCD00", "#526830", "#bcbddc")
names(FCColors)<- c("FcgR2A", "FcgR2B", "FcgR3A", "FcgR3B", "FcaR")

IGAColors<-c("#3f007d","#6a51a3","#9e9ac8")
names(IGAColors)<-c("a-IgA", "a-IgA1", "a-IgA2")

otherColors<-c("#662506","peachpuff4")
names(otherColors)<-c("a-IgD","a-IgM")

(detectColours<-unlist(list(IGGColors,FCColors,IGAColors,otherColors)))


colors<- list(
  Detection = detectColours,
  `Disease Severity`= c(mild="#00C000", moderate="#FF8000", severe= "#FF0000", `N/A`="#808080"),
  #Strain= c(`CoV-2`="sienna4", `non-CoV-2`="skyblue"),
  `Viral Antigen` = c(S="#006d2c",S1="#2ca25f",S2="#66c2a4", RBD="#99d8c9", N="#fdc086", FP= "#ffff99" ),
  `CoV-2 Status` = c(Convalescent="orangered3", Naive="black"))

rownames(M$r)<-rownames(CategoriesSerum)
colnames(M$r)<-rownames(CategoriesSerum)


pdf("SerumvsSerumcorrplot.pdf")
setHook("grid.newpage", function() pushViewport(viewport(x=1,y=1,width=0.9, height=0.9, name="vp", just=c("right","top"))), action="prepend")

SerumCorrplot<-pheatmap(M$r,scale = "none",cluster_rows = T,cluster_cols = T, annotation_colors = colors,
                        clustering_distance_cols = "manhattan",clustering_distance_rows="manhattan", annotation_legend = T,
                        show_rownames = F, show_colnames = F, fontsize =6,  annotation_col = Categories, annotation_row = Categories,
                        color = colorRampPalette(c("dark orange", "white", "blue"))(50),breaks = seq(from=-1,to=1,length.out = 50)
)
setHook("grid.newpage", NULL, "replace")
grid.text("Serum Detection/Antigen Pairs", y=0.05, gp=gpar(fontsize=10))
grid.text("Serum Detection/Antigen Pairs", x=-0.02, rot=90, gp=gpar(fontsize=10))
dev.off()


corrplot(M$r,p.mat=M$P,insig="label_sig",sig.level =c(0.001,.01,.05),pch.cex=0.5,pch.col="white",
         method="shade",add=F,col=brewer.pal(n=8, name="RdYlBu"),type="lower",diag = T,tl.col = "black",tl.cex=0.75)

maxes<-c(sort(unlist(M$r),decreasing = T)[1:200])

cols<-c()
rows<-c()
topCor<-c()
for(i in 1:200){
  rows<-append(rows,which(M$r==maxes[i], arr.ind = T)[,1])
  cols<-append(cols,which(M$r==maxes[i],arr.ind = T)[,2])
  topCor<-append(topCor,rep(maxes[i],length(which(M$r==maxes[i],arr.ind = T)[,2])))
}

View(cbind(CategoriesSerum[rows,],CategoriesNW[cols,],topCor,rows,cols))

plot(SerumSamples[,52],NasalSamples[,51],xlab=paste(CategoriesSerum[52,1],CategoriesSerum[52,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[51,1],CategoriesNW[51,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,51]~SerumSamples[,50]),col="red")
text(locator(1),paste("R =",round(CorrMatrix[52,51],2),sep=" "))

textLoc<-locator(1)

#for writing out

tiff("scatterPlotExamplePositive",height=6, width=6, units="in",res=400)
plot(SerumSamples[,52],NasalSamples[,51],xlab=paste(CategoriesSerum[52,1],CategoriesSerum[52,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[51,1],CategoriesNW[51,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,51]~SerumSamples[,52]),col="red")
text(textLoc,paste("R =",round(CorrMatrix[52,51],2),sep=" "))
dev.off()


mins<-c(sort(unlist(M$r),decreasing = F)[1:200])

cols<-c()
rows<-c()
bottomCor<-c()
for(i in 1:200){
  rows<-append(rows,which(M$r==mins[i], arr.ind = T)[,1])
  cols<-append(cols,which(M$r==mins[i],arr.ind = T)[,2])
  bottomCor<-append(bottomCor,rep(mins[i],length(which(M$r==mins[i],arr.ind = T)[,2])))
}

View(cbind(CategoriesSerum[rows,],CategoriesNW[cols,],bottomCor,rows,cols))


plot(SerumSamples[,33],NasalSamples[,73],xlab=paste(CategoriesSerum[33,1],CategoriesSerum[33,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[73,1],CategoriesNW[73,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,73]~SerumSamples[,33]),col="red")
text(locator(1),paste("R =",round(CorrMatrix[33,73],2),sep=" "))

textLoc<-locator(1)

#for writing out

tiff("scatterPlotExampleNegative",height=6, width=6, units="in",res=400)
plot(SerumSamples[,33],NasalSamples[,73],xlab=paste(CategoriesSerum[33,1],CategoriesSerum[33,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[73,1],CategoriesNW[73,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,73]~SerumSamples[,33]),col="red")
text(textLoc,paste("R =",round(CorrMatrix[33,73],2),sep=" "))
dev.off()


#Nasal Wash Corrplot


N<-rcorr(as.matrix(logdataNW))


rownames(N$r)<-rownames(CategoriesNW)
colnames(N$r)<-rownames(CategoriesNW)

pdf("NWvsNWcorrplot.pdf")
setHook("grid.newpage", function() pushViewport(viewport(x=1,y=1,width=0.9, height=0.9, name="vp", just=c("right","top"))), action="prepend")

NasalCorrplot<-pheatmap(N$r,scale = "none",cluster_rows = T,cluster_cols = T, annotation_colors = colors,
                        clustering_distance_cols = "manhattan",clustering_distance_rows="manhattan", annotation_legend = T,
                        show_rownames = F, show_colnames = F, fontsize =6,  annotation_col = Categories, annotation_row = Categories,
                        color = colorRampPalette(c("dark orange", "white", "blue"))(50),breaks = seq(from=-1,to=1,length.out = 50)
)
setHook("grid.newpage", NULL, "replace")
grid.text("Nasal Detection/Antigen Pairs", y=0.05, gp=gpar(fontsize=10))
grid.text("Nasal Detection/Antigen Pairs", x=-0.02, rot=90, gp=gpar(fontsize=10))
dev.off()


maxes<-c(sort(unlist(N$r),decreasing = T)[1:200])

cols<-c()
rows<-c()
topCor<-c()
for(i in 1:200){
  rows<-append(rows,which(N$r==maxes[i], arr.ind = T)[,1])
  cols<-append(cols,which(N$r==maxes[i],arr.ind = T)[,2])
  topCor<-append(topCor,rep(maxes[i],length(which(N$r==maxes[i],arr.ind = T)[,2])))
}

View(cbind(CategoriesSerum[rows,],CategoriesNW[cols,],topCor,rows,cols))

plot(SerumSamples[,52],NasalSamples[,51],xlab=paste(CategoriesSerum[52,1],CategoriesSerum[52,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[51,1],CategoriesNW[51,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,51]~SerumSamples[,50]),col="red")
text(locator(1),paste("R =",round(CorrMatrix[52,51],2),sep=" "))

textLoc<-locator(1)

#for writing out

tiff("scatterPlotExamplePositive",height=6, width=6, units="in",res=400)
plot(SerumSamples[,52],NasalSamples[,51],xlab=paste(CategoriesSerum[52,1],CategoriesSerum[52,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[51,1],CategoriesNW[51,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,51]~SerumSamples[,52]),col="red")
text(textLoc,paste("R =",round(CorrMatrix[52,51],2),sep=" "))
dev.off()


mins<-c(sort(unlist(N$r),decreasing = F)[1:300])

cols<-c()
rows<-c()
bottomCor<-c()
for(i in 1:300){
  rows<-append(rows,which(N$r==mins[i], arr.ind = T)[,1])
  cols<-append(cols,which(N$r==mins[i],arr.ind = T)[,2])
  bottomCor<-append(bottomCor,rep(mins[i],length(which(N$r==mins[i],arr.ind = T)[,2])))
}

View(cbind(CategoriesSerum[rows,],CategoriesNW[cols,],bottomCor,rows,cols))

plot(SerumSamples[,33],NasalSamples[,73],xlab=paste(CategoriesSerum[33,1],CategoriesSerum[33,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[73,1],CategoriesNW[73,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,73]~SerumSamples[,33]),col="red")
text(locator(1),paste("R =",round(CorrMatrix[33,73],2),sep=" "))

textLoc<-locator(1)

#for writing out

tiff("scatterPlotExampleNegative",height=6, width=6, units="in",res=400)
plot(SerumSamples[,33],NasalSamples[,73],xlab=paste(CategoriesSerum[33,1],CategoriesSerum[33,3],"in Serum",sep=" "),ylab=paste(CategoriesNW[73,1],CategoriesNW[73,3],"in Nasal Wash", sep=" "),pch=19)
abline(lm(NasalSamples[,73]~SerumSamples[,33]),col="red")
text(textLoc,paste("R =",round(CorrMatrix[33,73],2),sep=" "))
dev.off()


corrplot(N$r,p.mat=N$P,insig="label_sig",sig.level =c(0.001,.01,.05),pch.cex=0.5,pch.col="white",
         method="shade",add=F,col=brewer.pal(n=8, name="RdYlBu"),type="lower",diag = T,tl.col = "black",tl.cex=0.75)
