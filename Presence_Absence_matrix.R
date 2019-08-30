#Test avec la liste MN, EU, Aquitanien puis par millions d'annees

Presence_Absence_matrix <- function(ListeFaunique, type = "Genus", singletons = TRUE, min5 = FALSE){
#A_indet <- subset(A, A$GENUS != "indet." & A$GENUS != "_") 
library(stringr)
  
if(type == "Species")
{
  ch <- str_detect(ListeFaunique$SPECIES, "_")
  for(i in 1:length(ch))
  {
    if(ch[i] == TRUE)
    {
      TempSplit <- str_split(ListeFaunique$SPECIES[i], "_")
      if(length(TempSplit[[1]]) < 3){
        ListeFaunique$SPECIES[i] <- TempSplit[[1]][2]}
      if(length(TempSplit[[1]]) > 2){
        ListeFaunique$SPECIES[i] <- TempSplit[[1]][length(TempSplit[[1]])]}
    }
  }
Species <- subset(ListeFaunique, ListeFaunique$SPECIES != "indet." & ListeFaunique$SPECIES != "_" & ListeFaunique$SPECIES != "sp." & ListeFaunique$SPECIES != "ssp.") #ajouter _sp
SpeciesConcat <- paste(Species$GENUS, Species$SPECIES, sep = "_") #reflechir
AllGenera <- c(as.character(unique(SpeciesConcat)))
AllLocality <- c(as.character(unique(Species$NAME))) #vecteur des noms de localites
}
else if(type == "Genus")
{
AllGenera <- c(as.character(unique(ListeFaunique$GENUS))) #vecteur des noms de Genre unique
AllLocality <- c(as.character(unique(ListeFaunique$NAME))) #vecteur des noms de localites
}
#AllLocality <- c(as.character(unique(ListeFaunique$NAME))) #vecteur des noms de localites
matrix.PreAbs <- matrix(ncol=length(AllGenera), nrow=length(AllLocality), data = 0)# Matrice a remplir
dimnames(matrix.PreAbs) <- list(AllLocality, AllGenera) #Recopie les noms des especes et localités dans la matrice

for(i in 1:length(matrix.PreAbs[1,])) #On va aller chercher les noms 
{
  if(type == "Species")
  {
  LocalityOfGenera <- subset(ListeFaunique, AllGenera[i] == paste(ListeFaunique$GENUS, ListeFaunique$SPECIES, sep = "_"))
  }
  else if(type == "Genus")
  {
  LocalityOfGenera <- subset(ListeFaunique, AllGenera[i] == ListeFaunique$GENUS) 
  }
  vecTemp <- as.character(LocalityOfGenera$NAME)
  ## INSCRIPTION DANS LA MATRICE DES PRESENCES ##
  for(j in 1:length(matrix.PreAbs[,1]))                 #On va maintenant parcourir la liste des localités de la matrice (toutes les localités du JDD)
  {
    for(k in 1:length(vecTemp))                       #On va maintenant parcourir les localités ou l'espece [i] est connue.
      {
      if(vecTemp[k] == AllLocality[j]){matrix.PreAbs[j,i] <- 1}  #On transforme la case de la localité de 0 -> 1
      }
    }
}

  if(singletons == FALSE)
  {
    tempMat <- apply(matrix.PreAbs, 2, sum)
    want <- which(tempMat == 1)
    #print(want)
    tempMat <- matrix.PreAbs[,-want]
    #print(apply(tempMat, 2, sum))
    #print(apply(tempMat, 1, sum))
    matrix.PreAbs <- tempMat
    #print(matrix.PreAbs)
    tempMat <- apply(matrix.PreAbs, 1, sum)
    #print(tempMat)
    want <- which(tempMat == 0)
    #print(want)
    if(length(want) != 0)
    {
    tempMat <- matrix.PreAbs[-want,]
    matrix.PreAbs <- tempMat
    }
    tempMat <- apply(matrix.PreAbs, 2, sum)
    want <- which(tempMat == 0)
    if(length(want) != 0)
    {
      tempMat <- matrix.PreAbs[,-want]
      matrix.PreAbs <- tempMat
    }
  }

  if(min5 != FALSE)
  {
    tempMat <- apply(matrix.PreAbs, 1, sum)
    want <- which(tempMat < min5)
    if(length(want) > 0){
    tempMat <- matrix.PreAbs[-want,]
    matrix.PreAbs <- tempMat
    tempMat <- apply(matrix.PreAbs, 2, sum)
    want <- which(tempMat == 0)
    if(length(want) != 0)
    {
      tempMat <- matrix.PreAbs[,-want]
      matrix.PreAbs <- tempMat
    }
    }
  }


return(matrix.PreAbs)
}

#sort(apply(matrix.PreAbs, 2, sum))
#sort(apply(matrix.PreAbs, 1, sum))

