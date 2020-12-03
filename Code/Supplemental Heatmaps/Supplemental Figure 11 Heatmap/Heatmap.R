
rm(list = ls())

source('funcsToImport.R')

plot_font = 'Helvetica'

dir_res = paste('results_heatmap/',sep="")
dir.create(dir_res)

# -------------------------------------------
# Sec 02: Data
# -------------------------------------------

subjects = read.csv('in/subjects.csv', header=TRUE, row.names=1)

# biophysical data
luminex = read.csv('in/Fc_Array_ttest_log.csv',header=T,row.names=1)

# Prepare subject and column colors
scolors = createSubjectColors(subjects,group_colors,challenge_colors)
scolors =scolors[,-c(3)]
lcolors = createColumnColors(colnames(luminex),reagent_names,reagent_colors,antigen_names,antigen_colors)



ldata = scale(luminex)
ldata[ldata>2] = 2; ldata[ldata < -1] = -1
lr = ceiling(max(abs(min(ldata,na.rm=TRUE)), max(ldata,na.rm=TRUE)))
lbreaks = seq(-lr,lr,0.1)

pdf(paste(dir_res,'nasal ttest.pdf',sep=""))
heatmap.4(ldata, col=bluered, scale='none', trace='none', cexRow=0.5, cexCol=0.6, margin=c(8,5), breaks=lbreaks, symkey=FALSE, dendrogram='none',Rowv=TRUE, Colv=TRUE, na.color='black',NumRowSideColors = 1,RowSideColors = scolors, ColSideColors = lcolors)
dev.off()


