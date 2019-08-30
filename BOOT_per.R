BOOT_per <- function(FullFauna, Info)
{
  
  Decroiss <- c()
  Suivi <- c()
  for(i in 1:length(FullFauna))
  {
    Decroiss <- c(Decroiss, length(Presence_Absence_matrix(FullFauna[[i]], type = "Species", min5 = 2, singletons = TRUE)[,1]))
    Suivi <- c(Suivi, i)
  }
  
  OLD <- data.frame(Suivi = Suivi, Decroiss = Decroiss)
  OLD <- OLD[order(Decroiss, decreasing = TRUE),]
  TransiFauna <- list()
  for(i in 1:length(FullFauna))
  {
  TransiFauna[[i]] <- FullFauna[[OLD$Suivi[i]]]
  }
  names(TransiFauna) <- names(FullFauna[OLD$Suivi])
  
  FullFauna <- TransiFauna
  
  #Ordonner le ClustersMNX par ordre décroissant
MN_boot <- data.frame()
A <- -1
B <- 0
for(k in 1:(length(FullFauna) - 1))
{
  for(j in 2:(length(FullFauna)))
  {
    if(length(Presence_Absence_matrix(FullFauna[[k]], type = "Species", singletons = TRUE, min5 = 2)[,1]) >  0.38 * length(Presence_Absence_matrix(FullFauna[[j]], type = "Species", singletons = TRUE, min5 = 2)[,1])
       + length(Presence_Absence_matrix(FullFauna[[j]], type = "Species", singletons = TRUE, min5 = 2)[,1]))
    {
      A <- A + 1  
      for(i in 1:10)
      {
        LocAsup <- unique(FullFauna[[k]]$NAME)
        ## 40 % de plus que length Italie
        LocBsup <- unique(FullFauna[[j]]$NAME)
        Asup <- length(LocAsup) - (length(LocBsup) + round(0.38*length(LocBsup)))
        if(Asup <= 0){Asup <- 1}
        LocAsup <- sample(LocAsup, Asup) ##dixit 37 %
        LocMatch <- match(FullFauna[[k]]$NAME, LocAsup)
        LocTRUE <- is.na(LocMatch)
        ASUP <- c()
        for(y in 1:length(LocTRUE))
        {
          if(LocTRUE[y] == FALSE)
          {
            ASUP <- c(ASUP, y) 
          }
        }
        
        FullFauna_temp <- FullFauna[[k]][-ASUP,]
        
        length_temp <- length(Presence_Absence_matrix(FullFauna_temp, type = "Species", singletons = TRUE, min5 = FALSE)[,1])
        
        Clusters_cont <- join(FullFauna_temp, FullFauna[[j]], match="all", type="full")
        MatP.A.Faune <- Presence_Absence_matrix(Clusters_cont, type = "Species", singletons = TRUE, min5 = FALSE)
        Group <- c(rep("A", length_temp), rep("B", length(Presence_Absence_matrix(FullFauna[[j]], type = "Species", singletons = TRUE, min5 = 2)[,1])))
        if(A == 0 && i == 1){MN_boot <- delta.ses_boris(MatP.A.Faune, Group, count = FALSE)}
        else{MN_boot[i+B,] <- delta.ses_boris(MatP.A.Faune, Group, count = FALSE)}
        titre <- paste(Info ,"_" ,names(FullFauna)[k],"_" ,names(FullFauna)[j],"_" ,i, ".pdf", sep = "") #NOM MN
        nom <- paste(Info ,names(FullFauna)[k],"_" ,names(FullFauna)[j],"_" ,i, sep = "")
        dimnames(MN_boot)[[1]][i+B] <- nom
        dev.print(device = pdf, file =titre)
      }
      B <- B + 10
    }
  }
}

return(MN_boot)
}