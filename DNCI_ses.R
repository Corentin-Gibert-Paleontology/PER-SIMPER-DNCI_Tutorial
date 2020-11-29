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