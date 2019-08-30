PerSIMPER_onMatrix <- function(mat, group, pair = 0, overall = FALSE, NS = TRUE, Info = "NA", sortiePDF = TRUE, count = FALSE, NomCluster)
{
  #If Names of clusters are empty
  if(length(NomCluster) <= 1)
  {
    NomCluster <- group
  }
    
  #data.frame for stocking results
  resultsSIMPER <- data.frame()
  
  #Overall analysis
  if(overall == TRUE)
  {
    anos <- anosim(mat, group)
    SIMP <- delta.ses_boris(mat, group, count = count)
    resultsSIMPER <- rbind(resultsSIMPER, c(SIMP$DELTA.dn, SIMP$CI.DELTA.dn, SIMP$S.DELTA.dn, anos$signif, anos$statistic, table(group)))
    row.names(resultsSIMPER)[length(resultsSIMPER[,1])] <- "overall"
  }
  
  #mat is an occurrence matrix with taxa in columns and localities/samples/sites in rows
  mat <- as.data.frame(mat)
  mat <- cbind(group, mat)
  
  if(length(pair) == 1) #if no pattern of analysis (which pairs should be analysed and which shouldn't)
  {
    #all possibles pairs are analysed
    for(i in 1:(length(unique(group))-1))
    {
      for(j in (1+i):length(unique(group)))
      {
        mat_op <- subset(mat, mat[,"group"] == unique(group)[i] | mat[,"group"] == unique(group)[j])
        want <- which(apply(mat_op, 2, sum) == 0)
        mat_op <- mat_op[,-want]
        mat_group <- mat_op[,"group"]
        NombreLoc <- table(mat_group)
        mat_op <- mat_op[,-1]
        anos <- anosim(mat_op, mat_group)
        if(NS == TRUE){
        if(anos$signif <= 0.05)
        {
          SIMP <- delta.ses_boris(mat_op, mat_group, count = count)
          if(sortiePDF == TRUE)
          {
            ExtractTitle <- paste(Info, NombreLoc[[1]], NomCluster[i],  NombreLoc[[2]], NomCluster[j], ".pdf",sep = "_")
            dev.print(device = pdf, file = ExtractTitle, width = 10)
          }
          resultsSIMPER <- rbind(resultsSIMPER, c(SIMP$DELTA.dn, SIMP$CI.DELTA.dn, SIMP$S.DELTA.dn, anos$signif, anos$statistic, table(mat_group)))
          row.names(resultsSIMPER)[length(resultsSIMPER[,1])] <- paste(NombreLoc[[1]], NomCluster[i], NombreLoc[[2]],  NomCluster[j], sep = "_")
        }  
        else
        {
          resultsSIMPER <- rbind(resultsSIMPER, c(NA,NA,NA, anos$signif, anos$statistic, table(mat_group)))
          row.names(resultsSIMPER)[length(resultsSIMPER[,1])] <- paste(NombreLoc[[1]], NomCluster[i], NombreLoc[[2]],  NomCluster[j], sep = "_")
        }
        }
        if(NS == FALSE)
        {
          SIMP <- delta.ses_boris(mat_op, mat_group, count = count)
          if(sortiePDF == TRUE)
          {
            ExtractTitle <- paste(Info, NombreLoc[[1]], NomCluster[i],  NombreLoc[[2]], NomCluster[j], ".pdf",sep = "_")
            dev.print(device = pdf, file = ExtractTitle, width = 10)
          }
          resultsSIMPER <- rbind(resultsSIMPER, c(SIMP$DELTA.dn, SIMP$CI.DELTA.dn, SIMP$S.DELTA.dn, anos$signif, anos$statistic, table(mat_group)))
          row.names(resultsSIMPER)[length(resultsSIMPER[,1])] <- paste(NombreLoc[[1]], NomCluster[i], NombreLoc[[2]],  NomCluster[j], sep = "_")
        }
      }
    }
    #Only pre-selectionned pairs are analyzed
    #pair need to be a list of vector of length 2
  }
    if(length(pair) > 1)
    {
      for(i in 1:length(pair))
      {
      mat_op <- subset(mat, mat[,"group"] == pair[[i]][1] | mat[,"group"] == pair[[i]][2])
      want <- which(apply(mat_op, 2, sum) == 0)
      mat_op <- mat_op[,-want]
      mat_group <- mat_op[,"group"] 
      mat_op <- mat_op[,-1] 
      anos <- anosim(mat_op, mat_group)
      if(NS == TRUE){
        if(anos$signif <= 0.05)
        {
          SIMP <- delta.ses_boris(mat_op, mat_group, count = count)
          if(sortiePDF == TRUE)
          {
            ExtractTitle <- paste(Info, NomCluster[i],  NomCluster[j], ".pdf",sep = "_")
            dev.print(device = pdf, file = ExtractTitle, width = 10)
          }
          resultsSIMPER <- rbind(resultsSIMPER, c(SIMP$DELTA.dn, SIMP$CI.DELTA.dn, SIMP$S.DELTA.dn, anos$signif, anos$statistic, table(mat_group)))
          row.names(resultsSIMPER)[length(resultsSIMPER[,1])] <- paste(pair[[i]][1],  pair[[i]][2], sep = "_")
        }  
        else
        {
          resultsSIMPER <- rbind(resultsSIMPER, c(NA,NA,NA, anos$signif, anos$statistic, table(mat_group)))
          row.names(resultsSIMPER)[length(resultsSIMPER[,1])] <- paste(pair[[i]][1],  pair[[i]][2], sep = "_")
        }
      }
      if(NS == FALSE)
      {
        SIMP <- delta.ses_boris(mat_op, mat_group, count = count)
        if(sortiePDF == TRUE)
        {
          ExtractTitle <- paste(Info, unique(group)[i],  unique(group)[j], ".pdf",sep = "_")
          dev.print(device = pdf, file = ExtractTitle, width = 10)
        }
        resultsSIMPER <- rbind(resultsSIMPER, c(SIMP$DELTA.dn, SIMP$CI.DELTA.dn, SIMP$S.DELTA.dn, anos$signif, anos$statistic, table(mat_group)))
        row.names(resultsSIMPER)[length(resultsSIMPER[,1])] <- paste(pair[[i]][1],  pair[[i]][2], sep = "_")
      }
      }
    }
    
    colnames(resultsSIMPER) <- c("delta","IC", "S", "ANO(p)", "ANO(R)")
    return(resultsSIMPER)
  }
