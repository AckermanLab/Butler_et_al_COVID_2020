library(gplots)
library(ggplot2)


# -----------------------------------------------------

source('funcs/createColumnColors.R')
source('funcs/createSubjectColors.R')
source('funcs/heatmap4.R')

# -----------------------------------------------------
# Define colors

# group type

group_id = c(1,2,3)
names(group_id) = c('1','2','3')

group_colors = c("green","orange","red")

names(group_colors) = group_id

reagent_names = c('a.IgG',
                  'a.IgA',
                  'a.IgA1',
                  'a.IgA2',
                  'a.IgD',
                  'a.IgM',
                  'a.IgG1',
                  'a.IgG2',
                  'a.IgG3',
                  'a.IgG4',
                  'FcgR2AR',
                  'FcgR2B',
                  'FcgR3AV',
                  'FcgR3BNA2',
                  'FcaR',
                  'Func')

reagent_colors = c(a.IgG = "#1e90ff",
                   a.IgA = "#3f007d",
                   a.IgA1 = "#6a51a3",
                   a.IgA2 = "#9e9ac8",
                   a.IgD = "#662506",
                   a.IgM = "#8b7765",
                   a.IgG1 = "#C6E2FF",
                   a.IgG2 = "#CC3232",
                   a.IgG3 = "#4F94CD",
                   a.IgG4 = "#CD3700",
                   FcgR2AR = "#458B74",
                   FcgR2B = "#CDCD00",
                   FcgR3AV = "#526830",
                   FcgR3BNA2 = "#526830",
                   FcaR = "#bcbddc",
                   Func="Black")

antigen_names = c('S.S',
                  'S.S1',
                  'S.S2',
                  'RBD',
                  'N',
                  'FP'
)

antigen_colors = c(S.S="#006D2C",
                   S.S1="#2CA25F",
                   S.S2="#66C2A4",
                   RBD="#99D8C9",
                   N="#FDC086",
                   FP="#ffff99"
)

#challenge_colors = colorRampPalette(c('beige','bisque4'))(17);
challenge_colors = colorRampPalette(c('grey34','honeydew2'))(17);
names(challenge_colors) = c(1:17)
