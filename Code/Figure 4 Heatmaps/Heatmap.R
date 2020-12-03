#################################################
# Analysis of the CoV-2 Data for DHMC
#################################################
#
# Create heatmaps to visualize differences across
# detections, antigens, and virus
#
# Written by Harini Natarajan
# June 12, 2020
#
##################################################

#install.packages("pheatmap")
library(pheatmap)

#read in the data

data <- read.csv("Fc_Array.csv", header=FALSE)
dim(data)

#break into our column categories

Categories<-data[1:4,5:ncol(data)]
Categories<-as.data.frame(t(Categories))
Categories<-Categories[,c(1,4)]
colnames(Categories)<-c("Detection", "Viral Antigen")

cov2<-c("S", "S1", "S2", "RBD", "N", "FP")
oc43 <- c("OC43 S-2P", "OC43 S")
other <- c("229E S", "229E S1", "HKU1 S", "HKU1 S1", "MERS S", "MERS S1", "NL63")
othernonCoV <- c("gE", "HA")

#now bin viral antigens

Categories$`Viral Antigen`<-as.character(Categories$`Viral Antigen`)
Categories$`Viral Antigen`[which(Categories$`Viral Antigen`%in%cov2)]<-"CoV-2"
Categories$`Viral Antigen`[which(Categories$`Viral Antigen`%in%other)]<- "Other CoV"
Categories$`Viral Antigen`[which(Categories$`Viral Antigen`%in%othernonCoV)] <- "Non CoV"
Categories$`Viral Antigen`[which(Categories$`Viral Antigen`%in%oc43)] <- "OC43"
Categories$`Viral Antigen` <- as.factor(Categories$`Viral Antigen`) 

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
RowCategories$`Sample Type`<-as.factor(c(rep("Serum",20),rep("Serum",15),rep("Nasal Wash",20),rep("Nasal Wash",15)))
RowCategories$`CoV-2 Status`<-as.factor(c(rep("Convalescent",20),rep("Naive",15),rep("Convalescent",20),rep("Naive",15)))

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


#setting our colors


IGGColors<-c("dodgerblue","#C6E2FF","#CC3232", "#4F94CD","orangered")
names(IGGColors)<-c("a-IgG","a-IgG1","a-IgG2","a-IgG3","a-IgG4")

FCColors<-c("#8fbc8f", "#458B74", "#CDCD00", "#526830", "#bcbddc")
names(FCColors)<-c("FcgR2A", "FcgR2B", "FcgR3A", "FcgR3B", "FcaR")
FCColors <- na.omit(FCColors)

IGAColors<-c("#3f007d","#6a51a3","#9e9ac8")
names(IGAColors)<-unique(Categories$Detection)[2:4]


otherColors<-c("#662506","peachpuff4")
names(otherColors)<-c("a-IgD","a-IgM")

(detectColours<-unlist(list(IGGColors,FCColors,IGAColors,otherColors)))

#install.packages("RColorBrewer")
library(RColorBrewer)
ageBlues<-brewer.pal(7,"Greys")


colors<- list(
  Detection = detectColours,
  #Antigen = c(S="#006d2c",N="#2ca25f",E="#66c2a4", FP="#99d8c9", HA="#beaed4", gE="#fdc086" ),
  #Sex = c(`F`= "chocolate1", `M`= "dimgray"),
  `Disease Severity`= c(mild="#00C000", moderate="#FF8000", severe= "#FF0000", `N/A`="#808080"),
  #Age= c(`<20`=ageBlues[1], `20-29`=ageBlues[2], `30-39`= ageBlues[3], `40-49`= ageBlues[4], `50-59`=ageBlues[7], `60-69`=ageBlues[7], `70-79`=ageBlues[7]),
  #Age= c(`<40`=ageBlues[1], `40-59`=ageBlues[4], `>60`= ageBlues[7]),
  Strain= c(`CoV-2`="sienna4", `non-CoV-2`="skyblue"),
  `Viral Antigen` = c(`CoV-2`="sienna4", `OC43`= "yellow", `WIV1`= "darkorange1", `SARS CoV-1`= "turquoise3", `Other CoV`= "skyblue4", `Non CoV`= "gray87"),
  `CoV-2 Status` = c(Convalescent="orangered3", Naive="black"))

#Scale and Center 

logdata<-log10(data)

#now check which columns are different between pos and negative, discard ones that are not. 

keepColumnsS<-c()
keepColumnsNW<-c()

for(i in 1:ncol(logdata)){
  serum<-logdata[grepl("serum",RowCategories$`Sample Type`,ignore.case = T),i]
  nasal<-na.omit(logdata[grepl("nasal",RowCategories$`Sample Type`,ignore.case = T),i])
  
  ps<-t.test(serum[1:20],serum[21:35])$p.value
  pn<-t.test(nasal[1:19],nasal[20:34])$p.value
  
  if(ps<0.05) {keepColumnsS<-append(keepColumnsS,i)}
  if(pn<0.05) {keepColumnsNW<-append(keepColumnsNW,i)}
  
}

CategoriesSerum<-Categories[keepColumnsS,]
logdataS<-logdata[grepl("serum",RowCategories$`Sample Type`,ignore.case = T),keepColumnsS]
RowCategoriesSerum <- RowCategories[grepl("serum",RowCategories$`Sample Type`,ignore.case = T),]

scaledandCenteredDataS<-scale(logdataS)
scaledandCenteredDataS[scaledandCenteredDataS>3] = 3; scaledandCenteredDataS[scaledandCenteredDataS < -3] = -3
rownames(scaledandCenteredDataS)<-rownames(RowCategoriesSerum)

CategoriesNW<-Categories[keepColumnsNW,]
logdataNW<-logdata[grepl("nasal",RowCategories$`Sample Type`,ignore.case = T),keepColumnsNW]
RowCategoriesNW <- RowCategories[grepl("nasal",RowCategories$`Sample Type`,ignore.case = T),]
scaledandCenteredDataNW<-scale(logdataNW)
scaledandCenteredDataNW[scaledandCenteredDataNW>3] = 3; scaledandCenteredDataNW[scaledandCenteredDataNW < -3] = -3
rownames(scaledandCenteredDataNW)<-rownames(RowCategoriesNW)

RowCategoriesSerum$`Sample Type` <- NULL
RowCategoriesNW$`Sample Type` <- NULL


#Write out .csv files of t-test data for classification

#colnames(scaledandCenteredDataS)<-paste(CategoriesSerum$Detection,CategoriesSerum$`Viral Antigen`,sep=".")
#write.csv(scaledandCenteredDataS,"WrightJustCOV2withTtestDataSerum.csv")

#colnames(scaledandCenteredDataNW)<-paste(CategoriesNW$Detection,CategoriesNW$`Viral Antigen`,sep=".")
#write.csv(scaledandCenteredDataNW,"WrightJustCOV2withTtestDataNW.csv")


tiff("WrightAllAgsSerumClusteringTtest.tiff",height=10, width=10, units="in",res=100)
pheatmap(scaledandCenteredDataS,scale="none",cluster_cols=T,clustering_distance_cols = "manhattan",
         cluster_rows = F, clustering_distance_rows = "manhattan", annotation_col = CategoriesSerum, annotation_row = RowCategoriesSerum,
         show_rownames=F, show_colnames=F, fontsize=7, cellheight= 12, cellwidth = 4, border_color = NA,
         annotation_colors = colors, color = colorRampPalette(c("blue", "white", "red"))(50))
dev.off()


tiff("WrightAllAgsNWClusteringTtest29added.tiff",height=10, width=10, units="in",res=100)
pheatmap(scaledandCenteredDataNW,scale="none",cluster_cols=T,clustering_distance_cols = "manhattan",
         cluster_rows = F, clustering_distance_rows = "manhattan", annotation_col = CategoriesNW, annotation_row = RowCategoriesNW,
         show_rownames=F, show_colnames=F, fontsize=7, border_color= NA, cellheight=12, cellwidth = 4,
         annotation_colors = colors, na_col="grey69", color = colorRampPalette(c("blue", "white", "red"))(50))
dev.off()