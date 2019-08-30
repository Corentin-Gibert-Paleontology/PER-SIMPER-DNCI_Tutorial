Tri_SIMPER <- function(FullFauna, NS = FALSE, Info = "NA", LimSIMP = 4, Robustesse = TRUE)
{
  A <- list()
  b <- vector()
  d <- vector()
  e <- 
  for(i in 1:length(FullFauna))
  {
    if(length(FullFauna[[i]][,1]) > 8 & length(unique(FullFauna[[i]]$SPECIES)) >= 3)
    {
    #print(i)
    Pre_Count <- Presence_Absence_matrix(FullFauna[[i]], type = "Species") #pb ici
    Sum_Count <- length(which(apply(Pre_Count, 1, sum) >= 2))
    Sum_Count

    if(Sum_Count >= LimSIMP)
    {
   #print("in")
    Mat_longueur <- Presence_Absence_matrix(FullFauna[[i]], type = "Species", min5 = 2, singletons = TRUE)
    if(length(Mat_longueur[,1]) >= LimSIMP)
    {
      A[[length(A)+1]] <- FullFauna[[i]]
      names(A)[[length(A)]] <- names(FullFauna)[[i]]
      b <- c(b, i)
      d <- c(d,length(Mat_longueur[,1]))
      #print(paste("b : ", b))
      #print(paste("d : ", d))
    }
    }
    }
  }
  
  if(length(b) > 1){
  
  #Utiliser A pour (si argument TRUE) la charger dans la fonction(robustesse)
  if(Robustesse == TRUE)
  {
    BOOT_per(A, Info)
  }

  Group_full <- c(rep(1, d[1]))
  Data_full <- data.frame(A[[1]])
  for(i in 2:length(b))
  {
  Data_full <- rbind(Data_full, A[[i]])
  Group_full <- c(Group_full, rep(i, d[i]))
  }
  
  Mat_full <- Presence_Absence_matrix(Data_full, type = "Species", min5 = 2, singletons = TRUE)
  if(length(Mat_full[,1]) != sum(d)) {
    print(paste("Matrice : " , length(Mat_full[,1]), "Groupe : ", sum(d), "b :", b))
    stop("Taille vecteur_group != taille matrice")
  }
  
  if(length(b) > 2){
  FauneSIMPER <- PerSIMPER_onMatrix(Mat_full, Group_full, NS = NS, Info = Info, sortiePDF = TRUE, NomCluster = names(FullFauna[b]))
  }
  if(length(b) == 2)
  {
  FauneSIMPER <- delta.ses_boris(Mat_full, Group_full, count = FALSE)
  ExtractTitle <- paste(Info, table(Group_full)[[1]] ,names(FullFauna[b[1]]), table(Group_full)[[2]], names(FullFauna[b[2]]), ".pdf",sep = "_")
  dev.print(device = pdf, file = ExtractTitle, width = 10)
  }
  return(FauneSIMPER)
  }
}