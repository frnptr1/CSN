# Clean Environment
rm(list = ls())

# Load and install necessary packages
requiredPackages <- c("igraph", "ggplot2", "Matrix", "ggthemes", "gridExtra",
                      "minpack.lm", "stats4", "VGAM")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)


# Set WD and load data
wd = getwd()
if(grepl("nora", wd)) {
    setwd("~/Documents/18-19/CSN/LABS/05/src")
} else {
    # Set Piero Working directory
    setwd("~/Uni/csn/CSN-Lab-05/src")
}
rm(wd)

#################
####  Flags  ####
#################

APPLY_PROFILING = FALSE

LOAD_EXISTING_RUN_BA = TRUE
LOAD_EXISTING_RUN_BA_RAND = TRUE
LOAD_EXISTING_RUN_BA_NO_GROWTH = TRUE



TESTING = TRUE

#################
### Variables ###
#################

t.max = 10000
timestamps <- c(1, 10, 100, 1000) # 4 vertices to track

#################
### Sourcing  ###
#################
source("commonFunctions.R")
source("01.BA_original.R")
source("02.BA_growth_rand_att.R")
source("03.BA_no_growth.R")


m.0 = 2
n.0 = 3



# Vertex Growth -----------------------------------------------------------

# sequence of values
t = seq(t.max+1)


suppressWarnings(do.call(rbind, apply(table.BA[,-1],2, model_selection_vertex_growth)))
plot_ki(table.BA, "right", "gp") # growth preferential

suppressWarnings(do.call(rbind, apply(table.BA.Rand[,-1],2, model_selection_vertex_growth)))
plot_ki(table.BA.Rand, "bottomright", "gr") # growth random

suppressWarnings(do.call(rbind, apply(table.BA.no.growth[,-1],2, model_selection_vertex_growth)))
plot_ki(table.BA.no.growth, "bottomright", "ngp") # no growth preferential



# Degree Distribution -----------------------------------------------------


## Final degrees sequence
final.BA.seq <- read.csv2(filenameBAFinal); final.BA.seq <- final.BA.seq$x
final.BA.Rand.Att.seq <- read.csv2(filenameBA.Rand.Att.Final); final.BA.Rand.Att.seq <- final.BA.Rand.Att.seq$x
final.BA.no.growth.seq <- read.csv2(filename.final.BA.no.growth); final.BA.no.growth.seq <- final.BA.no.growth.seq$x



# Fitting ----------------------------------
# Growth and Preferential Attachment 
df1 = suppressWarnings(model_selection_degree_distribution(final.BA.seq))
# parameter of best model
minAIC.1 <- which.min(df1$AIC)
cat("Best fit. distrib; B.A. Growth + preferential:", as.character(df1[minAIC.1,]$Model), "\n")
opt_param1 = df1[minAIC.1,]$Param1
# geometric distribution
geom_param1 = df1[minAIC.1,"Param1"]


# Growth and Random attachment    
# models with relative values
df2 = suppressWarnings(model_selection_degree_distribution(final.BA.Rand.Att.seq))
# parameter of best model
minAIC.2 <- which.min(df2$AIC)
cat("Best fit. distrib; B.A. Growth + Random:", as.character(df2[minAIC.2,]$Model), "\n")
opt_param = df2[minAIC.2,]$Param1
# geometric distribution
poisson_param = df2[minAIC.2,"Param1"]


# No growth and Preferential attachment
# models with relative values
df3 = suppressWarnings(model_selection_degree_distribution(final.BA.no.growth.seq + 1))
df3
# parameter of best model
minAIC.3 <- which.min(df3$AIC)
cat("Best fit. distrib; B.A. NO Growth + Random:", as.character(df3[minAIC.3,]$Model), "\n")
opt_param = df3[which.min(df3$AIC),]$Param1
# geometric distribution
geom_param2 = df3[minAIC.3,"Param1"]
