## ALL FUNCTIONS IN ONE FILE

#
#############################################################
# Original Persimper function
# require packages: vegan, ggplot2
# by Corentin Gibert 
# original idea by Gilles Escarguel (LEHNA lab)
# revised by Jianjun Wang, 2019-02-01
# revised by Aurelien, 2019-07-19
#############################################################
#

PerSIMPER <- function(matrixSIMP, 
                      Groups, 
                      leg = FALSE, 
                      count = TRUE,   # default is true
                      dataTYPE = "prab", 
                      Nperm=1000, 
                      plotSIMPER = TRUE){    # add the possibility to change the number of permutations
  
  
  ################################################################################
  ## for every problems or questions, please contact me by mail or ResearchGate: ##
  ##### at corentingibert@gmail.com | corentin.gibert@univ-poitiers.fr ############
  ########### https://www.researchgate.net/profile/Corentin_Gibert ################
  #################################################################################
  
  #### GitHub : https://github.com/Corentin-Gibert-Paleontology
  
  # The PerSIMPER function requests exactly the same required arguments (matrixSIMP & Groups) 
  # as the functions used to compute SIMPER method in R, i.e. a presence/absence matrix and
  # a vector encoding cluster information.
  
  #Arguments :
  
  #matrixSIMP <- Stores the matrix to use in SIMPER analysis (i.e. the result 
  #of the presence/absence or the abundance distribution of taxa in at least 2 clusters of assemblies)
  # LOCALITIES in LINES
  # TAXA in COLUMNS
  
  #Groups <- A vector allowing to assign to the lines of the SIMPER matrix (matrixSIMP) a cluster of 
  #localities ; for example in a matrix of 10 lines built from 2 sets of localities 
  #of the same size : 1, 1, 1, 1, 1, 2, 2, 2, 2, 2. Strings are accepted, e.g., RegionA, Region, A, Region B, etc...
  
  #log <- the "log" argument allows to log the y-axis (i.e. the percentage contribution to the OAD of the species)
  
  #leg <- the "leg" argument allows a legend to be displayed on the Per-SIMPER profile
  
  #count <- the "count" argument allows a Screen output of the number of iterations performed.
  #This option is used to indicate if the permutation function is unable to swap the matrix cells. 
  #This incapacity is usually the result of a matrix too sparse in data (too many cells at 0).
  
  #dataTYPE <- the "dataTYPE" argument allows to choose between presence/absence data or abundance data. By default
  #the algorithm use presence/absence permutation for presence/absence data, if you use abundance dataset, you need
  #to write "count" in the dataTYPE argument.
  #e.g. dataTYPE = "count"
  
  #Nperm <- number of matrix permutation
  
  AnaSimp <- simper(matrixSIMP, Groups)   # summary(AnaSimp)
  #Classical SIMPER analysis computed on the compared groups
  
  Contribution <- sort(AnaSimp[[1]]$average, decreasing = TRUE)
  #Replication in a vector (named 'Contribution') of the sorting 
  #of species by their contribution to overall dissimilarity (OAD)
  
  Pourcent_Contribution <- ((Contribution)/sum(Contribution))*100 
  #Conversion as a percentage of each species' contribution to the OAD
  
  if(plotSIMPER == TRUE)
  {
    # if(logSIMPER == TRUE) { 
    # plot(Pourcent_Contribution, col = "brown2", log="y", type="p",lwd = 1.5, 
    #      ylab ="%  contribution to dissimilarity", xlab="Species") 
    # } else {
    plot(Pourcent_Contribution, col = "brown2", type="p",lwd = 1, 
         ylab ="% contribution to dissimilarity", xlab="Species")
    #Ploting SIMPER results in percentage (in Log or not)
  }
  
  
  # set.seed(123456)  # add by jjwang
  dp2 <- permatfull(matrixSIMP, fixedmar = "both", mtype = dataTYPE,  times = Nperm) #prab
  # set.seed(123456)  # add by jjwang
  dp3 <- permatfull(matrixSIMP, fixedmar ="rows" , mtype = dataTYPE, times = Nperm)  #prab
  
  #Randomization of the matrixSIMP matrix ; permatfull need to be used in order to swap cells under
  #various conditions. permatswap only allow permutation with both fixed rows and fixed columns count.
  #mtype = "prab" is used for presence/absence data, this setting must be changed with "count" if abundance
  #analysis are performed.
  
  df2 <- matrix(nrow = Nperm, ncol = length(matrixSIMP[2,]))
  df3 <- matrix(nrow = Nperm, ncol = length(matrixSIMP[2,]))
  df4 <- matrix(nrow = Nperm, ncol = length(matrixSIMP[2,]))
  #Generating matrices that will store the results (the ranked contribution of species to the OAD) 
  #of the 1000 permutations of the original matrix
  
  jj = 0
  
  for (i in 1:Nperm)  {
    if(count == TRUE && i < 100 || count == TRUE && i > round(Nperm*0.90) ){print(i)} 
    #Screen output of the number of iterations performed. 
    #This option is used to indicate if the permutation function is unable to swap the matrix cells. 
    #This incapacity is usually the result of a matrix too sparse in data (too many cells at 0).
    repeat {
      v <- T
      dp4 <- permatfull(matrixSIMP, fixedmar = "columns", mtype = dataTYPE, times = 1)  #prab
      for(j in 1:length(dp4$perm[[1]][,2])) {
        if(sum(dp4$perm[[1]][j,]) == 0){v <- FALSE}      
      }
      if(v == TRUE) break 
    }
    
    simp2 <- simper(dp2$perm[[i]], Groups)  
    simp3 <- simper(dp3$perm[[i]], Groups)
    simp4 <- simper(dp4$perm[[1]], Groups)
    #SIMPER analysis performed on each permutated matrix
    
    df2[i,] <- sort(simp2[[1]]$average, decreasing = TRUE)         
    df3[i,] <- sort(simp3[[1]]$average, decreasing = TRUE)
    df4[i,] <- sort(simp4[[1]]$average, decreasing = TRUE)
    #Storage of SIMPER results (ranked contribution to OAD)
    
    df2[i,] <- (df2[i,]/sum(df2[i,]))*100
    df3[i,] <- (df3[i,]/sum(df3[i,]))*100
    df4[i,] <- (df4[i,]/sum(df4[i,]))*100
    #Conversion to percentage of SIMPER results
  }   # 
  
  dn2 <- apply(df2, 2, sort)
  dn3 <- apply(df3, 2, sort)
  dn4 <- apply(df4, 2, sort)
  
  if(plotSIMPER == TRUE){
    lines(dn3[0.975*Nperm,], lty="dotted", lwd=2, col="cornflowerblue")
    lines(dn3[0.025*Nperm,], lty="dotted", lwd=2, col="cornflowerblue")
    lines(dn4[0.975*Nperm,], lty="dotted", lwd=2, col="orange2")
    lines(dn4[0.025*Nperm,], lty="dotted", lwd=2, col="orange2")
    #Plot of the upper and lower limit of the confidence intervals for fixed rows, 
    #fixed columns and fixed rows and columns
    title("SIMPER (in red) and PER-SIMPER profiles")
    
    legend(x="topright", bty="n",legend=c("SIMPER profil", "Rows fixed", "Col fixed"), 
           col=c("brown2", "cornflowerblue","orange2"), pch=c(15,15,15))
  }
  
  up <- 0.975*Nperm # ex for 100 permutations it will used 97
  lo <- 0.025*Nperm # ex for 100 permutations it will used 2
  med <- 0.5*Nperm 
  
  ###########################################################################
  #### The following is the calculation and the illustration of E index ####
  ####  E = Log of the sum of square deviations with empirical profile  ####
  ###########################################################################
  
  obs <- Pourcent_Contribution
  Orange <- dn4
  Blue <- dn2
  Green <- dn3
  # Ranked % of contribution to OAD of empirical and simulated profiles
  
  VectorEcartCarreOrangeLog <- vector(mode = "numeric", Nperm)
  VectorEcartCarreGreenLog <- vector(mode = "numeric", Nperm)
  VectorEcartCarreBlueLog <- vector(mode = "numeric", Nperm)
  
  for(i in 1:Nperm)  {
    SommeEcartCarreOrange <- vector(mode = "numeric", length = length(Orange[1,]))
    SommeEcartCarreGreen <- vector(mode = "numeric", length = length(Green[1,]))
    SommeEcartCarreBlue <- vector(mode = "numeric", length = length(Blue[1,]))
    
    for(j in 1:length(obs))    {
      SommeEcartCarreOrange[j] <-  (Orange[i,j] - obs[j])^2
      SommeEcartCarreGreen[j] <-   (Green[i,j] - obs[j])^2
      SommeEcartCarreBlue[j] <-    (Blue[i,j] - obs[j])^2
    }
    # Computation of square deviations with empirical profile (obs)
    
    #Mise en log des carre des ecarts pour symetriser la distribution
    VectorEcartCarreOrangeLog[i] <- log10(sum(SommeEcartCarreOrange))
    VectorEcartCarreGreenLog[i] <- log10(sum(SommeEcartCarreGreen))
    VectorEcartCarreBlueLog[i] <- log10(sum(SommeEcartCarreBlue))
  }
  # Log conversion of the sum of square deviations
  
  meanCarreOrangeLog <- mean(VectorEcartCarreOrangeLog)
  meanCarreGreenLog <- mean(VectorEcartCarreGreenLog)
  meanCarreBlueLog <- mean(VectorEcartCarreBlueLog)
  
  DataMeanCarreLog <- data.frame(Orange=VectorEcartCarreOrangeLog, 
                                 Blue=VectorEcartCarreBlueLog, 
                                 Green=VectorEcartCarreGreenLog)
  
  if(plotSIMPER == TRUE)
  {
    
    ## BOXPLOT with 95 % intervals of E (Log of the sum of square deviations with empirical profile) index
    
    Ax <- c("Fixed columns", "Both fixed", "Fixed rows")
    y <- DataMeanCarreLog
    df <- data.frame(
      Permutation_model = Ax,
      y0 = quantile(y$Orange, 0.025),
      y25 = quantile(y$Orange, 0.25),
      y50 = median(y$Orange),
      y75 = quantile(y$Orange, 0.75),
      y100 = quantile(y$Orange, 0.975)
    )
    df[2,2] = quantile(y$Blue, 0.025)
    df[2,3] = quantile(y$Blue, 0.25)
    df[2,4] = median(y$Blue)
    df[2,5] = quantile(y$Blue, 0.75)
    df[2,6] = quantile(y$Blue, 0.975)
    
    df[3,2] = quantile(y$Green, 0.025)
    df[3,3] = quantile(y$Green, 0.25)
    df[3,4] = median(y$Green)
    df[3,5] = quantile(y$Green, 0.75)
    df[3,6] = quantile(y$Green, 0.975)
    # Extraction of quantiles of interest
    
    # comment out by jjwang
    print(df)   # comment out by jjwang, 2019-01-30
    
    E <- ggplot(df, aes(Permutation_model, fill = Permutation_model)) +
      geom_boxplot(
        aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
        stat = "identity") + scale_fill_manual(values=c("#CCCCCC", "#FF6600", "#00CCFF")) +
      ggtitle("The lower E, the closer the simulated profile to empirical SIMPER profile") +
      theme(plot.title = element_text(lineheight= 2)) +
      scale_y_continuous(name=" E (Log of the sum of square deviations with empirical profile)")+
      labs(fill="Permutation models")+
      scale_x_discrete(name = "Permutation models")
    
    print(E)
    
    #Computation of ggplots2 boxplot for E index
    
    # outputs
  }
  
  ListResults <- list(EcartCarreLog = DataMeanCarreLog,  
                      mat = matrixSIMP, ContriPercentage = Pourcent_Contribution, 
                      UpOrange = dn4[up,], DownOrange = dn4[lo,], MedOrange = dn4[med,], 
                      UpBlue = dn2[up,], DownBlue = dn2[lo,], MedBlue = dn2[med,], 
                      UpGreen = dn3[lo,], DownGreen = dn3[up,], MedGreen = dn3[med,], 
                      dnOrange = dn4, dnBlue = dn2, dnGreen = dn3)
  
  return(ListResults)
}


#############################################################
###### DNCI Function : Dispersal-Niche Continuum Index ######
######### idea by Gibert, Escarguel, Vilmi and Wang #########
#############################################################
###### This function calls/needs PerSIMPER function #########

##### DIFFERENCES WITH PerSIMPER : QUANTITATIVE Results #####

## x = matrix with taxa in columns and localities in rows
## grouping = grouping vector for localities e.g. group <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
## grouping need to have the same length as X number of rows
## id = Name of your dataset
## Nperm and count arguments are for PerSIMPER calling, same argument as PerSIMPER fun()

DNCI.ses <- function(x, grouping,id = "no_name", Nperm = 1000, count = TRUE, plotSIMPER = TRUE) { #this calculates the metric using PERSIMPER - now the output included DELTAd-n, sd of DELTA.d-n and confidence interval
  groups <- sort(unique(grouping))
  stopifnot(length(groups) == 2)
  results = PerSIMPER(x, grouping,  count = count, Nperm = Nperm, plotSIMPER = plotSIMPER)
  E = results[["EcartCarreLog"]]
  
  #first calculate SES.d and SES.n based on E values from PERSIMPER
  SES.d = (E$Orange-mean(E$Blue))/sd(E$Blue)
  SES.n = (E$Green-mean(E$Blue))/sd(E$Blue)
  
  #then calculate DNCI
  DNCI = mean(SES.d)-mean(SES.n)
  #then calculate sd related to DNCI
  S.DNCI = sqrt(sd(SES.d)^2+sd(SES.n)^2)
  #get the confidence interval based on S.DNCI
  CI.DNCI = 2*S.DNCI
  # ---> then you have both the DNCI and the 95% confidence interval wich is 2 * S.DNCI
  metric = data.frame(id=id, group1= groups[1], group2 = groups[2], DNCI, CI.DNCI, S.DNCI)
  
  
  return(metric)
}

### return : DNCI = Dispersal-Niche continuum index value. Negative values = Dispersal ; Positive values = Niche
###          CI.DNCI = Confidence interval associated with DNCI; mandatory to read DNCI analysis
###          S.DNCI = Variance associated with DNCI

#############################################################
######### DNCI Function for 2 groups AND more  ##############
################# ANALYSIS ON PAIRS #########################
###### This function calls PerSIMPER and DNCI functions #####

## x = matrix with taxa in columns and localities in rows
## grouping = grouping vector for localities e.g. group <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
## grouping need to have the same length as X number of rows
## id = Name of your dataset
## Nperm and count arguments are for PerSIMPER calling, same argument as PerSIMPER fun()

## SYMMETRIZE : [IMPORTANT] : this argument make group even by subsampling the largest
##                            group to reduce it (or them) to the smallest group size
##              [IMPORTANT] : Repeat computation X (e.g. 1000) times to obtain mean values
##                            Effect can be strong if groups are strongly uneven

DNCI_multigroup <- function(x, grouping,id = "no_name", Nperm = 1000, count = TRUE, symmetrize = FALSE, plotSIMPER = TRUE) {
  group.combinations <- combn(unique(sort(grouping)),2)
  
  ddelta <- NULL
  
  if(NCOL(group.combinations) == 1) #If only 2 groups are compared
  {
    ddelta <- DNCI.ses(x , grouping,id=id, Nperm = Nperm, count = count, plotSIMPER = plotSIMPER)
  }
  if(NCOL(group.combinations) > 1)
  {
    
    for(i in 1:NCOL(group.combinations)) {
      splitx <- split(x,grouping)
      
      #Ici symmetrize:
      if(symmetrize == TRUE)
      {
        Add <- which(c(NROW(splitx[[group.combinations[1,i]]]), 
                       NROW(splitx[[group.combinations[2,i]]])) == max(c(NROW(splitx[[group.combinations[1,i]]]), 
                                                                         NROW(splitx[[group.combinations[2,i]]]))))
        if(Add == 1)
        {
          
          sampled_lines <- sample(1:length(splitx[[group.combinations[1,i]]][,1]), 
                                  length(splitx[[group.combinations[2,i]]][,1]))
          splitx[[group.combinations[1,i]]] <- splitx[[group.combinations[1,i]]][sampled_lines,]
        }
        
        if(Add == 2)
        {
          
          sampled_lines <- sample(1:length(splitx[[group.combinations[2,i]]][,1]), 
                                  length(splitx[[group.combinations[1,i]]][,1]))
          splitx[[group.combinations[2,i]]] <- splitx[[group.combinations[2,i]]][sampled_lines,]
        }
        
      }
      
      paired.x <- rbind(splitx[[group.combinations[1,i]]],
                        splitx[[group.combinations[2,i]]])
      
      # remove empty species
      ifzero <- which(apply(paired.x, 2, sum) == 0)
      if(length(ifzero > 0)){
        paired.x <- paired.x[,-which(colSums(paired.x)==0)]}
      if(length(which(rowSums(paired.x) == 0)) != 0){stop("ERROR : A row/sample is empty")}
      group.pair <- c(rep(group.combinations[1,i], NROW(splitx[[group.combinations[1,i]]])),
                      rep(group.combinations[2,i], NROW(splitx[[group.combinations[2,i]]])))
      ddelta <- rbind(ddelta, DNCI.ses(x=paired.x,grouping=group.pair,id=id, Nperm = Nperm, count = count, plotSIMPER = plotSIMPER)) #here is the part that calculates the index based on PERSIMPER
    }
  }
  return(ddelta)
}

### return : similar to DNCI results with the exception that returned variable is a multiples rows dataframe().


#
#############################################################
# persimper function modified to compute OVERALL PerSIMPER
#
# OVERALL ? Taxa contribution and PerSIMPER computation on
# the overall between-group dissimilarity of more than 2 groups
#
# require packages: vegan, ggplot2
# by Corentin Gibert
# revised by Jianjun Wang, 2019-02-01
# revised by Aurelien, 2019-07-19
#############################################################
#

PerSIMPER_overall <- function(matrixSIMP, 
                              Groups, 
                              leg = FALSE, 
                              count = TRUE,   # default is true
                              dataTYPE = "prab", 
                              Nperm=1000, 
                              plotSIMPER = TRUE){    # add the possibility to change the number of permutations
  
  
  ################################################################################
  ## for every problems or questions, please contact me by mail or ResearchGate: ##
  ##### at corentingibert@gmail.com | corentin.gibert@univ-poitiers.fr ############
  ########### https://www.researchgate.net/profile/Corentin_Gibert ################
  #################################################################################
  
  # The PerSIMPER function requests exactly the same required arguments (matrixSIMP & Groups) 
  # as the functions used to compute SIMPER method in R, i.e. a presence/absence matrix and
  # a vector encoding cluster information.
  
  #Arguments :
  
  #matrixSIMP <- Stores the matrix to use in SIMPER analysis (i.e. the result 
  #of the presence/absence or the abundance distribution of taxa in at least 2 clusters of assemblies)
  # LOCALITIES in LINES
  # TAXA in COLUMNS
  
  #Groups <- A vector allowing to assign to the lines of the SIMPER matrix (matrixSIMP) a cluster of 
  #localities ; for example in a matrix of 10 lines built from 2 sets of localities 
  #of the same size : 1, 1, 1, 1, 1, 2, 2, 2, 2, 2. Strings are accepted, e.g., RegionA, Region, A, Region B, etc...
  
  #log <- the "log" argument allows to log the y-axis (i.e. the percentage contribution to the OAD of the species)
  
  #leg <- the "leg" argument allows a legend to be displayed on the Per-SIMPER profile
  
  #count <- the "count" argument allows a Screen output of the number of iterations performed.
  #This option is used to indicate if the permutation function is unable to swap the matrix cells. 
  #This incapacity is usually the result of a matrix too sparse in data (too many cells at 0).
  
  #dataTYPE <- the "dataTYPE" argument allows to choose between presence/absence data or abundance data. By default
  #the algorithm use presence/absence permutation for presence/absence data, if you use abundance dataset, you need
  #to write "count" in the dataTYPE argument.
  #e.g. dataTYPE = "count"
  
  #Nperm <- number of matrix permutation
  
  AnaSimp <- simper(matrixSIMP, Groups)   # summary(AnaSimp)
  #Classical SIMPER analysis computed on all compared groups
  
  #Number of possible pairs of groups
  Nbr_Pairs <- seq(1:length(AnaSimp))
  Contribution <- rep(0, length(matrixSIMP[1,]))
  for(i in 1:length(AnaSimp))
  {
    Contribution <- Contribution + AnaSimp[[i]]$average
  }
  #Replication in a vector (named 'Contribution') of the sorting 
  #of species by their contribution to overall dissimilarity (OAD) in all pairs of groups
  
  Pourcent_Contribution <- sort(((Contribution)/sum(Contribution))*100, decreasing = TRUE)
  #Conversion as a percentage of each species' contribution to the OAD
  
  if(plotSIMPER == TRUE)
  {
    # if(logSIMPER == TRUE) { 
    # plot(Pourcent_Contribution, col = "brown2", log="y", type="p",lwd = 1.5, 
    #      ylab ="%  contribution to dissimilarity", xlab="Species") 
    # } else {
    plot(Pourcent_Contribution, col = "brown2", type="p",lwd = 1, 
         ylab ="% contribution to dissimilarity", xlab="Species")
    #Ploting SIMPER results in percentage (in Log or not)
  }
  
  
  # set.seed(123456)  # add by jjwang
  dp2 <- permatfull(matrixSIMP, fixedmar = "both", mtype = dataTYPE,  times = Nperm) #prab
  # set.seed(123456)  # add by jjwang
  dp3 <- permatfull(matrixSIMP, fixedmar ="rows" , mtype = dataTYPE, times = Nperm)  #prab
  
  #Randomization of the matrixSIMP matrix ; permatfull need to be used in order to swap cells under
  #various conditions. permatswap only allow permutation with both fixed rows and fixed columns count.
  #mtype = "prab" is used for presence/absence data, this setting must be changed with "count" if abundance
  #analysis are performed.
  
  df2 <- matrix(nrow = Nperm, ncol = length(matrixSIMP[2,]))
  df3 <- matrix(nrow = Nperm, ncol = length(matrixSIMP[2,]))
  df4 <- matrix(nrow = Nperm, ncol = length(matrixSIMP[2,]))
  #Generating matrices that will store the results (the ranked contribution of species to the OAD) 
  #of the 1000 permutations of the original matrix
  
  jj = 0
  
  for (i in 1:Nperm)  {
    if(count == TRUE && i < 100 || count == TRUE && i > round(Nperm*0.90) ){print(i)} 
    #Screen output of the number of iterations performed. 
    #This option is used to indicate if the permutation function is unable to swap the matrix cells. 
    #This incapacity is usually the result of a matrix too sparse in data (too many cells at 0).
    repeat {
      v <- T
      dp4 <- permatfull(matrixSIMP, fixedmar = "columns", mtype = dataTYPE, times = 1)  #prab
      for(j in 1:length(dp4$perm[[1]][,2])) {
        if(sum(dp4$perm[[1]][j,]) == 0){v <- FALSE}      
      }
      if(v == TRUE) break 
    }
    
    simp2 <- simper(dp2$perm[[i]], Groups)  
    simp3 <- simper(dp3$perm[[i]], Groups)
    simp4 <- simper(dp4$perm[[1]], Groups)
    #SIMPER analysis performed on each permutated matrix
    
    df2[i,] <- simp2[[1]]$average
    df3[i,] <- simp3[[1]]$average
    df4[i,] <- simp4[[1]]$average
    
    for(m in 2:length(simp2))
    {
      df2[i,] <- df2[i,] + simp2[[m]]$average
      df3[i,] <- df3[i,] + simp3[[m]]$average
      df4[i,] <- df4[i,] + simp4[[m]]$average
    }
    
  }
  #Storage of SIMPER results (ranked contribution to OAD)
  for(i in 1:Nperm)
  {
    df2[i,] <- sort((df2[i,]/sum(df2[i,]))*100, decreasing = TRUE)
    df3[i,] <- sort((df3[i,]/sum(df3[i,]))*100, decreasing = TRUE)
    df4[i,] <- sort((df4[i,]/sum(df4[i,]))*100, decreasing = TRUE)
    #Conversion to percentage of SIMPER results
  }
  
  dn2 <- apply(df2, 2, sort)
  dn3 <- apply(df3, 2, sort)
  dn4 <- apply(df4, 2, sort)
  
  if(plotSIMPER == TRUE){
    lines(dn3[0.975*Nperm,], lty="dotted", lwd=2, col="cornflowerblue")
    lines(dn3[0.025*Nperm,], lty="dotted", lwd=2, col="cornflowerblue")
    lines(dn4[0.975*Nperm,], lty="dotted", lwd=2, col="orange2")
    lines(dn4[0.025*Nperm,], lty="dotted", lwd=2, col="orange2")
    #Plot of the upper and lower limit of the confidence intervals for fixed rows, 
    #fixed columns and fixed rows and columns
    title("SIMPER (in red) and PER-SIMPER profiles")
    
    legend(x="topright", bty="n",legend=c("SIMPER profil", "Rows fixed", "Col fixed"), 
           col=c("brown2", "cornflowerblue","orange2"), pch=c(15,15,15))
  }
  
  up <- 0.975*Nperm # ex for 100 permutations it will used 97
  lo <- 0.025*Nperm # ex for 100 permutations it will used 2
  med <- 0.5*Nperm 
  
  ###########################################################################
  #### The following is the calculation and the illustration of E index ####
  ####  E = Log of the sum of square deviations with empirical profile  ####
  ###########################################################################
  
  obs <- Pourcent_Contribution
  Orange <- dn4
  Blue <- dn2
  Green <- dn3
  # Ranked % of contribution to OAD of empirical and simulated profiles
  
  VectorEcartCarreOrangeLog <- vector(mode = "numeric", Nperm)
  VectorEcartCarreGreenLog <- vector(mode = "numeric", Nperm)
  VectorEcartCarreBlueLog <- vector(mode = "numeric", Nperm)
  
  for(i in 1:Nperm)  {
    SommeEcartCarreOrange <- vector(mode = "numeric", length = length(Orange[1,]))
    SommeEcartCarreGreen <- vector(mode = "numeric", length = length(Green[1,]))
    SommeEcartCarreBlue <- vector(mode = "numeric", length = length(Blue[1,]))
    
    for(j in 1:length(obs))    {
      SommeEcartCarreOrange[j] <-  (Orange[i,j] - obs[j])^2
      SommeEcartCarreGreen[j] <-   (Green[i,j] - obs[j])^2
      SommeEcartCarreBlue[j] <-    (Blue[i,j] - obs[j])^2
    }
    # Computation of square deviations with empirical profile (obs)
    
    #Mise en log des carre des ecarts pour symetriser la distribution
    VectorEcartCarreOrangeLog[i] <- log10(sum(SommeEcartCarreOrange))
    VectorEcartCarreGreenLog[i] <- log10(sum(SommeEcartCarreGreen))
    VectorEcartCarreBlueLog[i] <- log10(sum(SommeEcartCarreBlue))
  }
  # Log conversion of the sum of square deviations
  
  meanCarreOrangeLog <- mean(VectorEcartCarreOrangeLog)
  meanCarreGreenLog <- mean(VectorEcartCarreGreenLog)
  meanCarreBlueLog <- mean(VectorEcartCarreBlueLog)
  
  DataMeanCarreLog <- data.frame(Orange=VectorEcartCarreOrangeLog, 
                                 Blue=VectorEcartCarreBlueLog, 
                                 Green=VectorEcartCarreGreenLog)
  
  if(plotSIMPER == TRUE)
  {
    
    ## BOXPLOT with 95 % intervals of E (Log of the sum of square deviations with empirical profile) index
    
    Ax <- c("Fixed columns", "Both fixed", "Fixed rows")
    y <- DataMeanCarreLog
    df <- data.frame(
      Permutation_model = Ax,
      y0 = quantile(y$Orange, 0.025),
      y25 = quantile(y$Orange, 0.25),
      y50 = median(y$Orange),
      y75 = quantile(y$Orange, 0.75),
      y100 = quantile(y$Orange, 0.975)
    )
    df[2,2] = quantile(y$Blue, 0.025)
    df[2,3] = quantile(y$Blue, 0.25)
    df[2,4] = median(y$Blue)
    df[2,5] = quantile(y$Blue, 0.75)
    df[2,6] = quantile(y$Blue, 0.975)
    
    df[3,2] = quantile(y$Green, 0.025)
    df[3,3] = quantile(y$Green, 0.25)
    df[3,4] = median(y$Green)
    df[3,5] = quantile(y$Green, 0.75)
    df[3,6] = quantile(y$Green, 0.975)
    # Extraction of quantiles of interest
    
    # comment out by jjwang
    print(df)   # comment out by jjwang, 2019-01-30
    
    E <- ggplot(df, aes(Permutation_model, fill = Permutation_model)) +
      geom_boxplot(
        aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
        stat = "identity") + scale_fill_manual(values=c("#CCCCCC", "#FF6600", "#00CCFF")) +
      ggtitle("The lower E, the closer the simulated profile to empirical SIMPER profile") +
      theme(plot.title = element_text(lineheight= 2)) +
      scale_y_continuous(name=" E (Log of the sum of square deviations with empirical profile)")+
      labs(fill="Permutation models")+
      scale_x_discrete(name = "Permutation models")
    
    print(E)
    
    #Computation of ggplots2 boxplot for E index
    
    # outputs
  }
  
  ListResults <- list(EcartCarreLog = DataMeanCarreLog,  
                      mat = matrixSIMP, ContriPercentage = Pourcent_Contribution, 
                      UpOrange = dn4[up,], DownOrange = dn4[lo,], MedOrange = dn4[med,], 
                      UpBlue = dn2[up,], DownBlue = dn2[lo,], MedBlue = dn2[med,], 
                      UpGreen = dn3[lo,], DownGreen = dn3[up,], MedGreen = dn3[med,], 
                      dnOrange = dn4, dnBlue = dn2, dnGreen = dn3)
  
  return(ListResults)
}


################ UNDER DEVELOPMENT ######################
## To contact me : corentingibert@gmail.com (feel free)
################ EN DEVELOPPEMENT #######################
# This function calls/needs PerSIMPER_Overall function #


################ UNDER DEVELOPMENT ##################
####### DNCI.ses function modified to be used #######
####### on multiples groups matrix (more than 2) ####
###### to compute OVERALL PerSIMPER/DNCI analysis ###
###### OVERALL = contribution of taxa to the ########
###### overall between-group dissimilarity ##########
###### and not to individual pair of groups #########
#####################################################

##### Same arguments and results as in DNCI_ses.R

DNCI.ses_overall <- function(x, grouping, id = "no_name", Nperm = 1000, count = TRUE, plotSIMPER = TRUE) { #this calculates the metric using PERSIMPER - now the output included DELTAd-n, sd of DELTA.d-n and confidence interval
  groups <- sort(unique(grouping))
  #results = PerSIMPER(x, grouping, log = TRUE, count = count)
  results = PerSIMPER_overall(x, grouping, count = count, Nperm = Nperm, plotSIMPER = plotSIMPER)
  E = results[["EcartCarreLog"]]
  
  #first calculate SES.d and SES.n based on E values from PERSIMPER
  SES.d = (E$Orange-mean(E$Blue))/sd(E$Blue)
  SES.n = (E$Green-mean(E$Blue))/sd(E$Blue)
  
  #then calculate DELTA.d-n
  DELTA.dn = mean(SES.d)-mean(SES.n)
  #then calculate sd related to DELTA.d-n
  S.DELTA.dn = sqrt(sd(SES.d)^2+sd(SES.n)^2)
  #get the confidence interval based on S.DELTA.dn
  CI.DELTA.dn = 2*S.DELTA.dn
  # ---> then you have both the DELTA.d-n and the 95% confidence interval wich is 2 * S.DELTA.d-n
  metric = data.frame(id = id, group1= groups[1], group2 = groups[2],DELTA.dn, CI.DELTA.dn, S.DELTA.dn)
  #,id dans le function() a ?t? supprim? aussi
  
  return(metric)
}


################ UNDER DEVELOPMENT ######################
## To contact me : corentingibert@gmail.com (feel free)
################ EN DEVELOPPEMENT #######################
# This function calls/needs DNCI_ses_overall function ###
# This function calls/needs PerSIMPER_Overall function ##

######################### CAUTION #######################
#### This function is a wraper for DNCI_ses_overall to ##
### compute robust overall DNCI values by making all ####
### groups even. ########################################
##### By making all groups even, this function can ######
##### change massively the original DNCI_overall ########
###### IF (!) one group is way smaller than the others ##
############## Consequently you should (A) run a lot ####
########### of iterations or (B) remove the smallest ####
######## group ##########################################
#########################################################

### Arguments are similar to DNCI_ses_overall.R 
##### BUT (!) you can choose the number of iteration to
######## compute mean robust overall DNCI value : NbrRerun

DNCI.ses_overall_symmetrized <- function(Mat, Group, NbrReRun = 100, count = FALSE) 
{
  Number_ofpairs <- combn(1:length(unique(Group)), 2)
  Unik_group <- unique(Group)
  
  List_result <- c()
  
  for(x in 1:NbrReRun)
  {
    SampleGroup <- c()
    for(y in 1:length(Unik_group)) #CHANGER PAR LA
    {
      SampleGroup <- c(SampleGroup, sample(which(Group == y), min(table(Group))))
    }
    Mat_Sampled <- Mat[SampleGroup,]
    Group_Sampled <- Group[SampleGroup]
    if(length(which(apply(Mat_Sampled, 2, sum) == 0)) != 0)
    {
      Mat_Sampled <- Mat_Sampled[,-which(apply(Mat_Sampled, 2, sum) == 0)]
    }
    
    if(length(Number_ofpairs[1,]) > 2){
      #Analyse_Pairs <- PerSIMPER_onMatrix(Mat_Sampled, Group_Sampled, NomCluster = LETTERS[Unik_group], NS = FALSE, 
      #                                    overall = FALSE)
      Analyse_Overall <- DNCI.ses_overall(Mat_Sampled, Group_Sampled, count = count)
      
      if(x == 1)
      {
        List_result <- Analyse_Overall
      }
      if(x != 1)
      {
        List_result <- rbind(List_result, Analyse_Overall)
      }
    }
  }
  return(List_result)
}

