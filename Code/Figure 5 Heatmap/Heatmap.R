
# -------------------------------------------
# Sec 01: Setup
# -------------------------------------------

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
luminex_tp5 = read.csv('in/Fc_Array_ttest_log_CoV2.csv',header=T,row.names=1)

# Prepare subject and column colors

lcolors_tp5 = createColumnColors(colnames(luminex_tp5),reagent_names,reagent_colors)

scolors <-c(rep("#4daf4a",4),rep("#ff7f00",11),rep("#e41a1c",4))



ldata = scale(luminex_tp5)
ldata[ldata>2] = 2; ldata[ldata < -1] = -1
lr = ceiling(max(abs(min(ldata,na.rm=TRUE)), max(ldata,na.rm=TRUE)))
lbreaks = seq(-lr,lr,0.1)

pdf(paste(dir_res,'heatmap.pdf',sep=""))
heatmap.2(ldata, col=bluered, scale='none', trace='none', cexRow=0.5, cexCol=0.6, margin=c(8,5), breaks=lbreaks, symkey=FALSE, dendrogram='none',Rowv=TRUE, Colv=TRUE, na.color='black',RowSideColors = scolors, ColSideColors = lcolors_tp5)
dev.off()


