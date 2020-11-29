
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