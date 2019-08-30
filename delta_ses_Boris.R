delta.ses_boris <- function(x, grouping, count = FALSE) { #this calculates the metric using PERSIMPER - now the output included DELTAd-n, sd of DELTA.d-n and confidence interval
  groups <- sort(unique(grouping))
  results = PerSIMPER(x, grouping, log = TRUE, count = count)
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
  metric = data.frame(group1= groups[1], group2 = groups[2],DELTA.dn, CI.DELTA.dn, S.DELTA.dn)
  #,id dans le function() a été supprimé aussi
  
  return(metric)
}