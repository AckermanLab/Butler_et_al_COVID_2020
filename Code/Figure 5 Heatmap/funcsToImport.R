
library(gplots)
library(ggplot2)


# -----------------------------------------------------

source('funcs/createColumnColors.R')


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
                  'Comp')



reagent_colors = c(a.IgG = "dodgerblue",
                   a.IgA = "#3f007d",
                   a.IgA1 = "#6a51a3",
                   a.IgA2 = "#9e9ac8",
                   a.IgD = "#662506",
                   a.IgM = "peachpuff4",
                   a.IgG1 = "#C6E2FF",
                   a.IgG2 = "#CC3232",
                   a.IgG3 = "#4F94CD",
                   a.IgG4 = "orangered",
                   FcgR2AR = "#8fbc8f",
                   FcgR2B = "#458B74",
                   FcgR3AV = "#CDCD00",
                   FcgR3BNA2 = "#526830",
                   FcaR = "#bcbddc",
                   Comp="Black")
