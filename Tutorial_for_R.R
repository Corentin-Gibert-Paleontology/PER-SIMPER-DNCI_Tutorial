##############################################################
###### EXAMPLE FILE for PerSIMPER and DNCI function usage ####
##############################################################


#### WARNING : DNCI and PERSIMPER ARE SENSITIVE TO SAMPLING BIAS
#### PLEASE ALWAYS SUBSAMPLE YOUR DATA TO HAVE SIMILAR NUMBER OF LINES/SITES/SAMPLE
#### OTHERWISE YOUR COMPARAISON BETWEEN YEARS/REGION ETC... WILL BE SPURIOUS
#### USE symmetrize = TRUE ARGUMENT WHEN NEEDED AND REPEAT THE ANALYSIS

### Loading library, functions, datas & working directory
library(devtools)
devtools::install_github("Corentin-Gibert-Paleontology/DNCImper") #DNCImper installation from GitHub repository

library(vegan)
library(plyr)
library(ggplot2)
library(DNCImper)

setwd("Your working directory")

Matrix <- DNCImper::Matrix #Data already loaded with DNCImper package. Can be call like that: DNCImper::Matrix 
Group <- DNCImper::Group  #Can be call like that: DNCImper::Group

################################################################################
##### PerSIMPER usage for 2 groups #############################################
################################################################################

### PerSIMPER is the function at the root of any DNCI calculation.
### It compare empirical SIMPER profile with 3 null hypothesis based on 
### matrix permutation while keeping (niche) rows sums, (dispersal) columns sums and row + column sums.

?PerSIMPER #Explanation for arguments, descriptions and examples
X1 <- DNCImper:::PerSIMPER(Matrix, Group, Nperm = 100) #Default = 1000 permutations, Plot = TRUE

#Produce 2 plots and a list() with plots data. A simper plot + PerSIMPER profiles, and the E index plot.

################################################################################
##### DNCI - Dispersal-Niche Continuum Index usage for 2 groups ################
################################################################################
?DNCI.ses #Explanation for arguments, descriptions and examples
?DNCI_multigroup #Explanation for arguments, descriptions and examples

### In DNCI, the deviation between empirical SIMPER profile and the three nulls models 
### are used to compute the Dispersal Niche Continuum Index
### The more negative the more the DISPERSAL POTENTIAL of taxa is driving composition structure
### The more positive the more the NICHE/ENVIRONMENT is driving composition structure

X2 <- DNCImper:::DNCI.ses(Matrix, Group, Nperm = 100, count = FALSE, plotSIMPER = FALSE) #Original DNCI function, limited to 2 groups
#or
X2b <- DNCImper:::DNCI_multigroup(Matrix, Group, plotSIMPER = TRUE, Nperm = 100) #Wraper, 2 or more groups
#Produce 2 plots -if plotSIMPER = TRUE- and a data.frame() with the DNCI result, 
#                                       associated with its confidence interval.

######
###### Results can be made more robust by making group even ## REPEAT X times ## 
X2c <- DNCImper:::DNCI_multigroup(Matrix, Group, plotSIMPER = TRUE, symmetrize = TRUE, Nperm = 100)

#Comparing previous results (X2b) with this last one (X2c) shows how PerSIMPER is sensitive to sampling bias
#As symmetrize == TRUE is a subsampling method, please repeat it multiples time to obtain a mean/median results
#Here an example below
X2d <- data.frame()
for(i in 1:10){
  X2dtemp <- DNCImper:::DNCI_multigroup(Matrix, Group, plotSIMPER = FALSE, 
                                        count = FALSE, symmetrize = TRUE, 
                                        Nperm = 100, parallelComputing = TRUE)
  X2d <- rbind(X2d, X2dtemp)
  }
median(X2d$DNCI) # ~~ - 4.3 DNCI after subsampling to make both group have similar size 
X2b$DNCI         # ~~ - 6.1 DNCI without subsampling (grp 1 = 12 sites, grp 2 = 34 sites)

### WARNING : Please use parallelComputing = TRUE as an argument to make permutation fasters

################################################################################
###### DNCI usage for more than 2 groups #######################################
################################################################################
Matrix_4groups <- DNCImper::Matrix_4groups
Group4 <- DNCImper::Group4

X3 <- DNCImper:::DNCI_multigroup(Matrix_4groups, Group4, Nperm = 100, plotSIMPER = TRUE)
#Produce 2 plots by pairs of groups -if plotSIMPER = TRUE- and a data.frame() 
#                                           with the DNCI result for each pairs, 
#                                           associated with their respective confidence interval.
######
###### Results can be made more robust by making pairs of groups even ## REPEAT X times ## 
X3b <- DNCImper:::DNCI_multigroup(Matrix_4groups, Group4, Nperm = 100, count = FALSE,
                                  plotSIMPER = TRUE, symmetrize = TRUE)

### Fast example of subsampling procedure with 10 subsampling + parrallel coding
X3c <- data.frame()
for(i in 1:10){
  X3ctemp <- DNCImper:::DNCI_multigroup(Matrix_4groups, Group4, plotSIMPER = FALSE, 
                                        count = FALSE, symmetrize = TRUE, 
                                        Nperm = 100, parallelComputing = TRUE)
  X3c <- rbind(X3c, X3ctemp)
}
X3c[-which(is.na(X3c$DNCI) == TRUE), ] #If NA are included
X3       

######################## OVERALL DNCI - PerSIMPER ##############################
############## COMPUTATION ON THE OVERALL DISSIMILARITY ########################
##################### BETWEEN MORE THAN 2 GROUPS ###############################
################################################################################

##### PerSIMPER Overall #### QUALITATIVE #######################################
X4 <- DNCImper:::PerSIMPER_overall(Matrix_4groups, Group4, Nperm = 100, count = TRUE)
## Result would be read on the PerSIMPER plot()

##### DNCI Overall ##### QUANTITATIVE ##########################################
X5 <- DNCImper:::DNCI.ses_overall(Matrix_4groups, Group4, Nperm = 100,
                                  count = FALSE, parallelComputing = TRUE)
#Produce 2 plots -if plotSIMPER = TRUE- and a data.frame() with the overall DNCI result,
#                                       associated with its confidence interval.

######
###### Results can be made more robust by making group even ## REPEAT X times ## 
X6 <- DNCImper:::DNCI.ses_overall_symmetrized(Matrix_4groups, Group4, Nperm = 100,
                                              NbrReRun = 10, count = FALSE, parallelComputing = TRUE)
###### NbrReRun = argument to choose the number of iterations for resampling ###


##### CAUTION : THE PACKAGE WILL BE ADDED TO CRAN (R) in fall 2025 or early 2026.
##### CAUTION : PLEASE ALWAYS USE THE MOST UP TO DATE VERSION OF THE PACKAGE (now 1.7)