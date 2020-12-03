
library(gplots)
library(ggplot2)


# -----------------------------------------------------



source('funcs/createColumnColors.R')
source('funcs/createColumnShapes.R')

# -----------------------------------------------------
# Define colors


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
                   Func="#000000")
                   
antigen_names = c('S.S',
                  'RBD',
                  'N',
                  'FP',
                  'non.CoV.2',
                  'Func')

antigen_colors = c(S.S="#006d2c",
                   RBD="#99d8c9",
                   N="#beaed4",
                   FP="#ffff99",
                   non.CoV.2="#386cb0",
                   Func="#000000")

antigen_shapes = c(S.S=19,
                  RBD=15,
                  N=17,
                  
                 FP=18,
                 non.CoV.2=1,
                 Func=19
  )

