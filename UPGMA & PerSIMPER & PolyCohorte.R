##############################################################################################################
####################################### APRES CE TEST FONCTIONNEL ############################################
####################################### GENERALISATION A TOUTE L'EUROPE ######################################
####################################### PUIS A TOUS LES DATASETS #############################################
##############################################################################################################

setwd("C:/Users/cgibert01/Desktop/Biblio post doc Poitiers/Data téléchargés")
source("IllustrationCluster.R")

EUROPEpbdb <- read.table("pbdb_data_EUROPE_25_0.txt", h = T) 
f <- EUROPEpbdb$lat
f2 <- EUROPEpbdb$lng
EUROPEpbdb$lat <- as.numeric(levels(f))[f]
EUROPEpbdb$lng <- as.numeric(levels(f2))[f2]

EuropeNOW <- read.table("now_export_EUROPE_25_0.txt", h = T)

A <- EuropeNOW
B <- EUROPEpbdb

str(A)
#On conserve les colonnes les plus utiles (a premiere vue).
#Selection des occurrences au moins definies au niveau du GENRE

A_indet <- subset(A, A$GENUS != "indet." & A$GENUS != "_" & A$GENUS != "gen." & A$LAT != 0 & A$LONG != 0) 
A_Expurg <- A_indet[, c("LIDNUM", "NAME", "ORDER" ,"FAMILY", "GENUS", "SPECIES", "LAT", "LONG", "MAX_AGE", "MIN_AGE")]

B_indet <- subset(B, B$genus != "_" & B$genus != "indet")
B_Expurg <- B_indet[, c("occurrence_no", "collection_name", "order", "family", "genus", "accepted_name","lat", "lng", "max_ma", "min_ma")]

#Remplacer les GENRES dans B_Expurg par indet.
B_Expurg$accepted_name <- as.character(B_Expurg$accepted_name)
str(B_Expurg$accepted_name)

for(i in 1:length(B_Expurg[,1]))
{
  if(B_Expurg[i ,("accepted_name")] == B_Expurg[i ,("genus")])
  {
    B_Expurg[i, ("accepted_name")] <- "indet."
  }
}

#Joindre dans un seul dataset : sur le modele de la base de donnes NOW
names(B_Expurg)
names(A_Expurg)
names(B_Expurg) <- names(A_Expurg)

library(plyr)
FULLeurope <- join(A_Expurg, B_Expurg , match="all", type="full")

#write.table(FULLeurope, file = "FULLEurope.txt", sep = " ", row.names = TRUE, col.names = TRUE)

##################################################################################################################
################## POUR NE PAS MELANGER DES CHOUX ET DES CAROTTES, ON ENLEVE LES LOCALITES QUI NE SONT ###########
################## PAS DEFINIS AU MOINS AU NIVEAU D'UN SEUL ETAGE (age max = 4.43 Ma -Burdigalien-) ##############
##################################################################################################################

wantTime <- which((FULLeurope$MAX_AGE - FULLeurope$MIN_AGE) > 5)
FULLeurope <- FULLeurope[-wantTime,]
if(length(which((FULLeurope$MAX_AGE - FULLeurope$MIN_AGE) > 5)) == 0)
{
  print("OK")
}


##################################################################################################################
##################################################################################################################
########### AFIN D'EVITER DE FABRIQUER UN UPGMA ILLISIBLE ON VA REGROUPER LES LOCALITES ##########################
##################################################################################################################

#Maintenant il faut le faire par taille de grain.
Min_LAT.EU <- floor(min(FULLeurope$LAT, na.rm = TRUE))
Max_LAT.EU <- ceiling(max(FULLeurope$LAT, na.rm = TRUE))
Min_LNG.EU <- floor(min(FULLeurope$LONG, na.rm = TRUE))
Max_LNG.EU <- ceiling(max(FULLeurope$LONG, na.rm = TRUE))

#Pour un grain de 1 degres
Grain1.EU.LAT <- c(Min_LAT.EU:Max_LAT.EU)
Grain1.EU.LNG <- c(Min_LNG.EU:Max_LNG.EU)

#Pour un grain de 0.5 degres
Grain05.EU.LAT <- seq(from = Min_LAT.EU, to = Max_LAT.EU, 0.5)
Grain05.EU.LNG <- seq(from = Min_LNG.EU, to = Max_LNG.EU, 0.5)

#Pour un grain de 0.1 degres
Grain01.EU.LAT <- seq(from = Min_LAT.EU, to = Max_LAT.EU, 0.1)
Grain01.EU.LNG <- seq(from = Min_LNG.EU, to = Max_LNG.EU, 0.1)

#Pour un grain de 1 degres
Liste.GRAIN1.EU <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain1.EU.LAT) - 1))
{
  for(k in 1:(length(Grain1.EU.LNG) - 1))
  {
    want <- which(FULLeurope$LAT >= Grain1.EU.LAT[j] & FULLeurope$LAT < Grain1.EU.LAT[j+1] 
                  & FULLeurope$LONG >= Grain1.EU.LNG[k] & FULLeurope$LONG < Grain1.EU.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain1.dataframe.eu <- paste("EU", Grain1.EU.LAT[j], Grain1.EU.LNG[k], sep = "_")
      N <- assign(names.Grain1.dataframe.eu, FULLeurope[want,])
      Liste.GRAIN1.EU[[(length(Liste.GRAIN1.EU)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm(names.Grain1.dataframe.eu)
    }
  }
  print(paste(ceiling((j/(length(Grain1.EU.LAT) - 1) *100)), " % "))
}

#Pour un grain de 0.5 degres
Liste.GRAIN05.EU <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain05.EU.LAT) - 1))
{
  for(k in 1:(length(Grain05.EU.LNG) - 1))
  {
    want <- which(FULLeurope$LAT >= Grain05.EU.LAT[j] & FULLeurope$LAT < Grain05.EU.LAT[j+1] 
                  & FULLeurope$LONG >= Grain05.EU.LNG[k] & FULLeurope$LONG < Grain05.EU.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain05.dataframe.eu <- paste("EU", Grain05.EU.LAT[j], Grain05.EU.LNG[k], sep = "_")
      N <- assign(names.Grain05.dataframe.eu, FULLeurope[want,])
      Liste.GRAIN05.EU[[(length(Liste.GRAIN05.EU)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm(names.Grain05.dataframe.eu)
    }
  }
  print(paste(ceiling((j/(length(Grain05.EU.LAT) - 1) *100)), " % "))
}

#Pour un grain de 0.1 degres
Liste.GRAIN01.EU <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain01.EU.LAT) - 1))
{
  for(k in 1:(length(Grain01.EU.LNG) - 1))
  {
    want <- which(FULLeurope$LAT >= Grain01.EU.LAT[j] & FULLeurope$LAT < Grain01.EU.LAT[j+1] 
                  & FULLeurope$LONG >= Grain01.EU.LNG[k] & FULLeurope$LONG < Grain01.EU.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain01.dataframe.eu <- paste("EU", Grain01.EU.LAT[j], Grain01.EU.LNG[k], sep = "_")
      N <- assign(names.Grain01.dataframe.eu, FULLeurope[want,])
      Liste.GRAIN01.EU[[(length(Liste.GRAIN01.EU)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm(names.Grain01.dataframe.eu)
    }
  }
  print(paste(ceiling((j/(length(Grain01.EU.LAT) - 1) *100)), " % "))
}

#DONNER UN MEME NOM DE LOCALITE POUR CHAQUE LOCALITE REGROUPEES#

for(i in 1:length(Liste.GRAIN1.EU))
{
  titre <- paste(floor(Liste.GRAIN1.EU[[i]][1,]$LAT), floor(Liste.GRAIN1.EU[[i]][1,]$LONG), sep = "_")
  for(j in 1:length(Liste.GRAIN1.EU[[i]]))
    {
    Liste.GRAIN1.EU[[i]][j]$NAME <- titre
    }
}

for(i in 1:length(Liste.GRAIN05.EU))
{
  titre <- paste(round(Liste.GRAIN05.EU[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN05.EU[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN05.EU[[i]]))
  {
    Liste.GRAIN05.EU[[i]][j]$NAME <- titre
  }
}

for(i in 1:length(Liste.GRAIN01.EU))
{
  titre <- paste(round(Liste.GRAIN01.EU[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN01.EU[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN01.EU[[i]]))
  {
    Liste.GRAIN01.EU[[i]][j]$NAME <- titre
  }
}

#JOINDRE TOUTES LES "LOCALITES" DANS UN SEUL DATAFRAME ???? ET FAUT IL ??? IGNORER LES LOCALITE < 5 SP ???? NON 
### LA FONCTION PRESENCE ABSENCE MATRIX LE FAIT

FULLeuropeGRAIN1 <- data.frame()
  for(i in 1:length(Liste.GRAIN1.EU))
  {
    FULLeuropeGRAIN1 <- join(FULLeuropeGRAIN1, Liste.GRAIN1.EU[[i]], match="all", type="full")  
  }

FULLeuropeGRAIN05 <- data.frame()
for(i in 1:length(Liste.GRAIN05.EU))
{
    FULLeuropeGRAIN05 <- join(FULLeuropeGRAIN05, Liste.GRAIN05.EU[[i]], match="all", type="full")  
}

FULLeuropeGRAIN01 <- data.frame()
for(i in 1:length(Liste.GRAIN01.EU))
{
    FULLeuropeGRAIN01 <- join(FULLeuropeGRAIN01, Liste.GRAIN01.EU[[i]], match="all", type="full")  
}

#Reste -t-il des NA dans le JDD ?
ISNA <- is.na(FULLeuropeGRAIN1)
summary(ISNA) #AUCUN NA
ISNA <- is.na(FULLeuropeGRAIN05)
summary(ISNA) #AUCUN NA
ISNA <- is.na(FULLeuropeGRAIN01)
summary(ISNA) #AUCUN NA



####################################################################################################################################################
######################################## CHOISIR RESOLUTION POUR NE PAS TOUT RE ECRIRE #####################################
####################################################################################################################################################
FULLeuropeGRAIN1 <- FULLeuropeGRAIN05
FULLeuropeGRAIN1 <- FULLeuropeGRAIN01

#Suppression des Cetaces et des chauves pouris
str(FULLeuropeGRAIN1)
sort(summary(FULLeuropeGRAIN1$FAMILY, maxsum = 157))
wantOUT <- which(FULLeuropeGRAIN1$ORDER == "Chiroptera" | FULLeuropeGRAIN1$ORDER == "Cetacea" | FULLeuropeGRAIN1$FAMILY == "Phocidae" | FULLeuropeGRAIN1$FAMILY == "Vespertilionidae" |
                   FULLeuropeGRAIN1$ORDER == "Indet" |  FULLeuropeGRAIN1$GENUS == "indet." | 
                   FULLeuropeGRAIN1$FAMILY == "Balaenopteridae" | FULLeuropeGRAIN1$FAMILY == "Rhinolophidae" |
                      FULLeuropeGRAIN1$FAMILY == "Dugongidae" | FULLeuropeGRAIN1$FAMILY == "Ziphiidae" |
                      FULLeuropeGRAIN1$FAMILY == "Balaenidae" | FULLeuropeGRAIN1$FAMILY == "Eurhinodelphinidae" |
                      FULLeuropeGRAIN1$FAMILY == "Odobenidae" | FULLeuropeGRAIN1$FAMILY == "Cetotheriidae" |
                      FULLeuropeGRAIN1$FAMILY == "Squalodontidae" | FULLeuropeGRAIN1$FAMILY == "Physeteridae" |
                      FULLeuropeGRAIN1$FAMILY == "Hipposideridae" | FULLeuropeGRAIN1$FAMILY == "Monodontidae" |
                      FULLeuropeGRAIN1$FAMILY == "Megadermatidae" | FULLeuropeGRAIN1$FAMILY == "Tranatocetidae" |
                      FULLeuropeGRAIN1$FAMILY == "Platanistidae" | FULLeuropeGRAIN1$FAMILY == "Kentriodontidae" |
                      FULLeuropeGRAIN1$FAMILY == "Molossidae" | FULLeuropeGRAIN1$FAMILY == "Eschrichtiidae" |
                      FULLeuropeGRAIN1$FAMILY == "Desmostylidae" | FULLeuropeGRAIN1$FAMILY == "Eoplatanistidae" |
                      FULLeuropeGRAIN1$FAMILY == "Kogiidae" | FULLeuropeGRAIN1$FAMILY == "Phocoenidae" | 
                      FULLeuropeGRAIN1$FAMILY == "Pontoporiidae" | FULLeuropeGRAIN1$FAMILY == "Squalodelphinidae" |
                      FULLeuropeGRAIN1$FAMILY == "Emballonuridae" | FULLeuropeGRAIN1$FAMILY == "Trichechidae" |
                      FULLeuropeGRAIN1$FAMILY == "Dalpiazinidae" | FULLeuropeGRAIN1$FAMILY == "Acrodelphidae" |
                      FULLeuropeGRAIN1$FAMILY == "Patriocetidae" | FULLeuropeGRAIN1$FAMILY == "Hyperoodontidae" |
                      FULLeuropeGRAIN1$FAMILY == "Bohlininae" | FULLeuropeGRAIN1$GENUS == "Pinocetus" | FULLeuropeGRAIN1$GENUS == "Isocetus" |
                      FULLeuropeGRAIN1$GENUS == "Scaldicetus" | FULLeuropeGRAIN1$GENUS == "Tagicetus" | FULLeuropeGRAIN1$GENUS == "Uranocetus" |
                      FULLeuropeGRAIN1$GENUS == "Hoplocetus" | FULLeuropeGRAIN1$GENUS == "Aglaocetus" | FULLeuropeGRAIN1$GENUS == "Diorocetus" |
                      FULLeuropeGRAIN1$GENUS == "Graamocetus" | FULLeuropeGRAIN1$GENUS == "Pelocetus" | FULLeuropeGRAIN1$GENUS == "Phococetus")


EU.Grain1.terre <- FULLeuropeGRAIN1[-wantOUT,]
str(EU.Grain1.terre)
summary(EU.Grain1.terre$FAMILY)
summary(EU.Grain1.terre$ORDER)


########################################################
##### ON ENLEVE LES LOCALITES RUSSES ET ASIATIQUES #####
########################################################

EU.Grain1.terre <- subset(EU.Grain1.terre, EU.Grain1.terre$LONG < 45 & EU.Grain1.terre$LONG > -10 & EU.Grain1.terre$LAT > 35 & EU.Grain1.terre$LAT < 70)

######################################
##### SI ON NE VEUT QUE LES GROS #####
######################################

#1 on garde les insectivores
wantBIGeuropa <- which(EU.Grain1.terre$ORDER == "Rodentia" | EU.Grain1.terre$ORDER == "Lagomorpha")
EU.Grain1.terre <- EU.Grain1.terre[-wantBIGeuropa,]
summary(EU.Grain1.terre$ORDER)
#2 on garde pas les insectivores
wantBIGeuropa <- which(EU.Grain1.terre$ORDER == "Rodentia" | EU.Grain1.terre$ORDER == "Lagomorpha" | EU.Grain1.terre$ORDER == "Eulipotyphla"
                       | EU.Grain1.terre$ORDER == "Hyracoidea" | EU.Grain1.terre$ORDER == "Lipotyphla" | EU.Grain1.terre$ORDER == "_"
                       | EU.Grain1.terre$ORDER == "Eutriconodonta")
EU.Grain1.terre <- EU.Grain1.terre[-wantBIGeuropa,]
summary(EU.Grain1.terre$ORDER)


################################################################################################################################
################################# REGROUPEMENT DES FAUNES PAR CHRONOFAUNES #####################################################
################################################################################################################################
### ATTENTION, CHOISIR A QUELLE RESOLUTION SPATIALE NOUS VOULONS TRAVAILLER
### SI ON VEUT CHANGER LA RESOLUTION (100 km par default) IL FAUT STOCKER DANS EU.Grain1.terre 
### LA LISTE A LA RESOLUTION DESIREE
source("Presence_Absence_matrix.R")

#Modele 1 de selection, permissif : on garde tout ce qui est existe pendant un temps donne durant la periode cible
### Faune 1 : 
Faune1.EU <- data.frame()

wantFaune1 <- which(11.2 >= EU.Grain1.terre$MIN_AGE & 11.2 < EU.Grain1.terre$MAX_AGE & 23.8 >= EU.Grain1.terre$MAX_AGE
                    | 11.2 <= EU.Grain1.terre$MIN_AGE & 23.8 > EU.Grain1.terre$MIN_AGE & 23.8 <= EU.Grain1.terre$MAX_AGE
                    | 11.2 > EU.Grain1.terre$MIN_AGE & 23.8 < EU.Grain1.terre$MAX_AGE
                    | 11.2 < EU.Grain1.terre$MIN_AGE & 23.8 > EU.Grain1.terre$MAX_AGE)

Faune1.EU <- EU.Grain1.terre[wantFaune1,]  
#Matrice de P/A # TESTER LA 1 et la 4:
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Species")
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Species", singletons = FALSE)
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Species", singletons = FALSE, min5 = 3) #new Pre/Abs function
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Genus")
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Genus", singletons = FALSE)
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Genus", singletons = FALSE, min5 = TRUE)

## Faune 2 : 
Faune2.EU <- data.frame()

wantFaune2 <- which(5.3 >= EU.Grain1.terre$MIN_AGE & 5.3 < EU.Grain1.terre$MAX_AGE & 12.5 >= EU.Grain1.terre$MAX_AGE
                    | 5.3 <= EU.Grain1.terre$MIN_AGE & 12.5 > EU.Grain1.terre$MIN_AGE & 12.5 <= EU.Grain1.terre$MAX_AGE
                    | 5.3 > EU.Grain1.terre$MIN_AGE & 12.5 < EU.Grain1.terre$MAX_AGE
                    | 5.3 < EU.Grain1.terre$MIN_AGE & 12.5 > EU.Grain1.terre$MAX_AGE)

Faune2.EU <- EU.Grain1.terre[wantFaune2,] 
#Matrice de P/A
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Species")
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Species", singletons = FALSE)
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Genus")
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Genus", singletons = FALSE)
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Genus", singletons = TRUE, min5 = TRUE)


## Faune 3 :
Faune3.EU <- data.frame()

wantFaune3 <- which(0 >= EU.Grain1.terre$MIN_AGE & 0 < EU.Grain1.terre$MAX_AGE & 7.1 >= EU.Grain1.terre$MAX_AGE
                    | 0 <= EU.Grain1.terre$MIN_AGE & 7.1 > EU.Grain1.terre$MIN_AGE & 7.1 <= EU.Grain1.terre$MAX_AGE
                    | 0 > EU.Grain1.terre$MIN_AGE & 7.1 < EU.Grain1.terre$MAX_AGE
                    | 0 < EU.Grain1.terre$MIN_AGE & 7.1 > EU.Grain1.terre$MAX_AGE)

Faune3.EU <- EU.Grain1.terre[wantFaune3,] 
#Matrice de P/A
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Species")
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Species", singletons = FALSE)
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Genus")
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Genus", singletons = FALSE)
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Genus", singletons = TRUE, min5 = TRUE)


#Modele 2 de selection, exclusif : on ne garde que les taxons disparaissant avant la borne sup et avant la borne inf
## Faune 1
Faune1B.EU <- data.frame()

wantFaune1B <- which(11.2 <= EU.Grain1.terre$MIN_AGE & 11.2 < EU.Grain1.terre$MAX_AGE & 23.8 >= EU.Grain1.terre$MAX_AGE)

Faune1B.EU <- EU.Grain1.terre[wantFaune1B,] 
#Matrice de P/A
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Species")
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Species", singletons = FALSE)
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Genus")
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Genus", singletons = FALSE)
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune1B.EU <- Presence_Absence_matrix(Faune1B.EU, type = "Genus", singletons = TRUE, min5 = TRUE)

# Faune 2
Faune2B.EU <- data.frame()

wantFaune2B <- which(5.3 <= EU.Grain1.terre$MIN_AGE & 5.3 < EU.Grain1.terre$MAX_AGE & 12.5 >= EU.Grain1.terre$MAX_AGE)

Faune2B.EU <- EU.Grain1.terre[wantFaune2B,] 
#Matrice de P/A
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Species")
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Species", singletons = FALSE)
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Genus")
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Genus", singletons = FALSE)
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2B.EU <- Presence_Absence_matrix(Faune2B.EU, type = "Genus", singletons = TRUE, min5 = TRUE)

# Faune 3
Faune3B.EU <- data.frame()

wantFaune3B <- which(0 <= EU.Grain1.terre$MIN_AGE & 0 < EU.Grain1.terre$MAX_AGE & 7.1 >= EU.Grain1.terre$MAX_AGE)

Faune3B.EU <- EU.Grain1.terre[wantFaune3B,]
#Matrice de P/A
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Species")
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Species", singletons = FALSE)
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Genus")
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Genus", singletons = FALSE)
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3B.EU <- Presence_Absence_matrix(Faune3B.EU, type = "Genus", singletons = TRUE, min5 = TRUE)


##################################################################################################################
############# ON VA DESORMAIS DECOUPER LE JDD PAR MN #############################################################
##################################################################################################################
#AGE_MN <- c(0, 1.95, 2.6, 3.4, 4.2, 5.3, 7.1, 8.2, 9, 9.5, 11.2, 12.5, 15.2, 17, 18, 20, 22.8, 23.8)
#NOM_MN <- c("MN17","MN16","MN15","MN14","MN13","MN12","MN11","MN10","MN9","MN8","MN7","MN6","MN5","MN4","MN3","MN2","MN1")
  
### AVEC WHICH : 
Liste.MN.EU <- list() #Liste pour stocker tous ces dataframes
for(i in 1:(length(AGE_MN) - 1))
{
  wantMN <- which(AGE_MN[i] >= EU.Grain1.terre$MIN_AGE & AGE_MN[i] < EU.Grain1.terre$MAX_AGE & AGE_MN[i+1] >= EU.Grain1.terre$MAX_AGE
                  | AGE_MN[i] <= EU.Grain1.terre$MIN_AGE & AGE_MN[i+1] > EU.Grain1.terre$MIN_AGE & AGE_MN[i+1] <= EU.Grain1.terre$MAX_AGE
                  | AGE_MN[i] > EU.Grain1.terre$MIN_AGE & AGE_MN[i+1] < EU.Grain1.terre$MAX_AGE
                  | AGE_MN[i] < EU.Grain1.terre$MIN_AGE & AGE_MN[i+1] > EU.Grain1.terre$MAX_AGE)
  #print(wantMN)
  if(length(wantMN) != 0)
  {
    names.dataframe.eu <- paste("AGE.MN", AGE_MN[i], sep = "_")
    N <- assign(names.dataframe.eu, EU.Grain1.terre[wantMN,])
    Liste.MN.EU[[length(Liste.MN.EU)+ 1]] <- N
  }
}

summary(Liste.MN.EU[[1]])
str(Liste.MN.EU[[1]])

##################################################################################################################
######################################## LE NMDS VA ETRE UTILISE EN AMONT AFIN DE ################################
######################################## TRANSFORMER NOS DONNES QUALITATIVES (DISCRETES) #########################
######################################## EN DONNES CONTINUES, CES NOUVELLES DONNES ###############################
######################################## SERONT UTILISES POUR TROUVER LE BON NOMBRE DE ###########################
######################################## CLUSTERS : HCPC (HIERARCHICAL CLASSI ON PRINCIPAL COMPONANT) ############                                ##############################
##################################################################################################################
#MATRICE EUROPE ENTIERE
#PUIS ANALYSE EN POLYCOHORTE
source("Presence_Absence_matrix.R")
library(cluster)
library(ggplot2)
library(vegan)
library(FactoMineR)
library(factoextra)
library(fossil)
source("IllustrationCluster.R")
## Contrairement à une PCA ou les valeurs initiales, quantitatives et continues sont utilisées pour réduire le nombre
## de variable, ici pour un MDS/PCoA il s'agit d'abord de transformer les données binaires en données de dissimilarités.
#Bray-Curtis
Dist.Faune1.EU <- vegdist(MatP.A.Faune1.EU, method = "bray")
mds.Faune1.EU.BRAY <- cmdscale(Dist.Faune1.EU, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune1.EU.BRAY <- HCPC(as.data.frame(mds.Faune1.EU.BRAY$point))
x11()
IllustrationCluster(hcpc.Faune1.EU.BRAY)

MatP.A.Faune1.EU <- Presence_Absence_matrix(Faune1.EU, type = "Species", singletons = TRUE, min5 = 2) #new Pre/Abs function

#SIMPSON
MatP.A.Faune1.EU_Inv <- t(MatP.A.Faune1.EU)
Dist.Faune1.EU <- ecol.dist(MatP.A.Faune1.EU_Inv, method = simpson, type = "dis")
mds.Faune1.EU.SIMP <- cmdscale(Dist.Faune1.EU, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune1.EU.SIMP <- HCPC(as.data.frame(mds.Faune1.EU.SIMP$point))
x11()
IllustrationCluster(hcpc.Faune1.EU.SIMP, etik = TRUE)

#RAUP & CRICK
Dist.Faune1.EU <- raupcrick(MatP.A.Faune1.EU)
mds.Faune1.EU.RC <- cmdscale(Dist.Faune1.EU, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune1.EU.RC <- HCPC(as.data.frame(mds.Faune1.EU.RC$point))
x11()
IllustrationCluster(hcpc.Faune1.EU.RC, etik = TRUE, cexEtik = 0.1)

#Faune 2
#Bray-Curtis
Dist.Faune2.EU <- vegdist(MatP.A.Faune2.EU, method = "bray")
mds.Faune2.EU <- cmdscale(Dist.Faune2.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune2.EU.BRAY <- HCPC(as.data.frame(mds.Faune2.EU$points))
x11()
IllustrationCluster(hcpc.Faune2.EU.BRAY)

MatP.A.Faune2.EU <- Presence_Absence_matrix(Faune2.EU, type = "Species", singletons = TRUE, min5 = 2)

#SIMPSON
MatP.A.Faune2.EU_Inv <- t(MatP.A.Faune2.EU)
Dist.Faune2.EU <- ecol.dist(MatP.A.Faune2.EU_Inv, method = simpson, type = "dis")
mds.Faune2.EU <- cmdscale(Dist.Faune2.EU, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune2.EU.SIMP <- HCPC(as.data.frame(mds.Faune2.EU$points))
x11()
IllustrationCluster(hcpc.Faune2.EU.SIMP, etik = TRUE)

#RAUP & CRICK
Dist.Faune2.EU <- raupcrick(MatP.A.Faune2.EU)
mds.Faune2.EU <- cmdscale(Dist.Faune2.EU, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune2.EU.RC <- HCPC(as.data.frame(mds.Faune2.EU$point))
x11()
IllustrationCluster(hcpc.Faune2.EU.RC, etik = TRUE, cexEtik = 0.1)
dimnames(hcpc.Faune2.EU.RC$data.clust)[[1]]

#Faune 3

MatP.A.Faune3.EU <- Presence_Absence_matrix(Faune3.EU, type = "Species", singletons = TRUE, min5 = 2)

#Bray-Curtis
Dist.Faune3.EU <- vegdist(MatP.A.Faune3.EU, method = "bray")
mds.Faune3.EU.BRAY <- cmdscale(Dist.Faune3.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3.EU.BRAY <- HCPC(as.data.frame(mds.Faune3.EU.BRAY$points))
x11()
IllustrationCluster(hcpc.Faune3.EU.BRAY)

#SIMPSON
MatP.A.Faune3.EU_Inv <- t(MatP.A.Faune3.EU)
Dist.Faune3.EU <- ecol.dist(MatP.A.Faune3.EU_Inv, method = simpson, type = "dis")
mds.Faune3.EU.SIMP <- cmdscale(Dist.Faune3.EU, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune3.EU.SIMP <- HCPC(as.data.frame(mds.Faune3.EU.SIMP$points))
x11()
IllustrationCluster(hcpc.Faune3.EU.SIMP, etik = TRUE)

#RAUP & CRICK
Dist.Faune3.EU <- raupcrick(MatP.A.Faune3.EU)
mds.Faune3.EU.RC <- cmdscale(Dist.Faune3.EU, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune3.EU.RC <- HCPC(as.data.frame(mds.Faune3.EU.RC$point))
x11()
IllustrationCluster(hcpc.Faune3.EU.RC, etik = TRUE, cexEtik = 0.1)
dimnames(hcpc.Faune3.EU.RC$data.clust)[[1]]

## VERSION B : EXCLUSIVE
#Faune 1B
#Bray-Curtis
Dist.Faune1B.EU <- vegdist(MatP.A.Faune1B.EU, method = "bray")
mds.Faune1B.EU.BRAY <- cmdscale(Dist.Faune1B.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune1B.EU.BRAY <- HCPC(as.data.frame(mds.Faune1B.EU.BRAY$points))
x11()
IllustrationCluster(hcpc.Faune1B.EU.BRAY)

#SIMPSON
MatP.A.Faune1B.EU_Inv <- t(MatP.A.Faune1B.EU)
Dist.Faune1B.EU <- ecol.dist(MatP.A.Faune1B.EU_Inv, method = simpson, type = "dis")
mds.Faune1B.EU.SIMP <- cmdscale(Dist.Faune1B.EU, eig = TRUE, x.ret = TRUE)

hcpc.Faune1B.EU.SIMP <- HCPC(as.data.frame(mds.Faune1B.EU.SIMP$points))
x11()
IllustrationCluster(hcpc.Faune1B.EU.SIMP)

#RAUP & CRICK
Dist.Faune1B.EU <- raupcrick(MatP.A.Faune1B.EU)
mds.Faune1B.EU.RC <- cmdscale(Dist.Faune1B.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune1B.EU.RC <- HCPC(as.data.frame(mds.Faune1B.EU.RC$points))
x11()
IllustrationCluster(hcpc.Faune1B.EU.RC)

#Faune 2B
#Bray-Curtis
Dist.Faune2B.EU <- vegdist(MatP.A.Faune2B.EU, method = "bray")
mds.Faune2B.EU <- cmdscale(Dist.Faune2B.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune2B.EU.BRAY <- HCPC(as.data.frame(mds.Faune2B.EU$points))
x11()
IllustrationCluster(hcpc.Faune2B.EU.BRAY)

#SIMPSON
MatP.A.Faune2B.EU_Inv <- t(MatP.A.Faune2B.EU)
Dist.Faune2B.EU <- ecol.dist(MatP.A.Faune2B.EU_Inv, method = simpson, type = "dis")
mds.Faune2B.EU <- cmdscale(Dist.Faune2B.EU, eig = TRUE, x.ret = TRUE)

hcpc.Faune2B.EU.SIMP <- HCPC(as.data.frame(mds.Faune2B.EU$points))
x11()

IllustrationCluster(hcpc.Faune2B.EU.SIMP)

#RAUP & CRICK
Dist.Faune2B.EU <- raupcrick(MatP.A.Faune2B.EU)
mds.Faune2B.EU <- cmdscale(Dist.Faune2B.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune2B.EU.RC <- HCPC(as.data.frame(mds.Faune2B.EU$points))
x11()

IllustrationCluster(hcpc.Faune2B.EU.RC)

#Faune 3B
#Bray-Curtis
Dist.Faune3B.EU <- vegdist(MatP.A.Faune3B.EU, method = "bray")
mds.Faune3B.EU.BR <- cmdscale(Dist.Faune3B.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3B.EU.BR <- HCPC(as.data.frame(mds.Faune3B.EU.BR$points))
x11()
IllustrationCluster(hcpc.Faune3B.EU.BR)

#SIMPSON
MatP.A.Faune3B.EU_Inv <- t(MatP.A.Faune3B.EU)
Dist.Faune3B.EU <- ecol.dist(MatP.A.Faune3B.EU_Inv, method = simpson, type = "dis")
mds.Faune3B.EU.SIMP <- cmdscale(Dist.Faune3B.EU, eig = TRUE, x.ret = TRUE)

hcpc.Faune3B.EU.SIMP <- HCPC(as.data.frame(mds.Faune3B.EU.SIMP$points))
x11()
IllustrationCluster(hcpc.Faune3B.EU.SIMP)

#RAUP & CRICK
Dist.Faune3B.EU <- raupcrick(MatP.A.Faune3B.EU)
mds.Faune3B.EU.RC <- cmdscale(Dist.Faune3B.EU, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3B.EU.RC <- HCPC(as.data.frame(mds.Faune3B.EU.RC$points))
x11()
IllustrationCluster(hcpc.Faune3B.EU.RC)


##################################################################################################################
####################################      SORTIE CLUSTERS POUR ANOSIM         ####################################
##################################################################################################################


Anos <- anosim(MatP.A.Faune1B.EU, grouping = hcpc.Faune1B.EU$data.clust$clust, distance = "bray")
summary(Anos)

ttt <- names(MatP.A.Faune1.EU[,1])
GroupInTTT <- c()
for(i in 1:length(ttt))
{
  wantGROUP <- which(ttt == Cluster)
}
Cluster1 <- hcpc.Faune1.EU$data.clust$clust
Cluster <- dimnames(hcpc.Faune1.EU$data.clust)[[1]]
MatP.A.Faune1.EU
#write.table(FULLeurope, file = "FULLEurope.txt", sep = " ", row.names = TRUE, col.names = TRUE)

dimnames(hcpc.Faune1.EU$data.clust)
##################################################################################################################
##################################################################################################################
##################################################################################################################
################################################## ASIE ##########################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
setwd("C:/Users/cgibert01/Desktop/Biblio post doc Poitiers/Data téléchargés")
ASIApbdb <- read.table("pbdb_data_ASIA_25_0.txt", h = T) 
f <- ASIApbdb$lat
f2 <- ASIApbdb$lng
ASIApbdb$lat <- as.numeric(levels(f))[f]
ASIApbdb$lng <- as.numeric(levels(f2))[f2]

AsiaNOW <- read.table("now_export_ASIA_25_0.txt", h = T)

A <- AsiaNOW
B <- ASIApbdb

str(A)
#On conserve les colonnes les plus utiles (a premiere vue).
#Selection des occurrences au moins definies au niveau du GENRE

A_indet <- subset(A, A$GENUS != "indet." & A$GENUS != "_" & A$GENUS != "gen." & A$LAT != 0) 
A_Expurg <- A_indet[, c("LIDNUM", "NAME", "ORDER" , "FAMILY", "GENUS", "SPECIES", "LAT", "LONG", "MAX_AGE", "MIN_AGE")]

B_indet <- subset(B, B$genus != "_" & B$genus != "indet" & B$genus != "indet.")
B_Expurg <- B_indet[, c("occurrence_no", "collection_name", "order" , "family", "genus", "accepted_name","lat", "lng", "max_ma", "min_ma")]

#Remplacer les GENRES dans B_Expurg par indet.
B_Expurg$accepted_name <- as.character(B_Expurg$accepted_name)
str(B_Expurg$accepted_name)

for(i in 1:length(B_Expurg[,1]))
{
  if(B_Expurg[i ,("accepted_name")] == B_Expurg[i ,("genus")])
  {
    B_Expurg[i, ("accepted_name")] <- "indet."
  }
}

#Joindre dans un seul dataset : sur le modele de la base de donnes NOW
names(B_Expurg)
names(A_Expurg)
names(B_Expurg) <- names(A_Expurg)

library(plyr)
FULLasia <- join(A_Expurg, B_Expurg , match="all", type="full")

#write.table(FULLeurope, file = "FULLEurope.txt", sep = " ", row.names = TRUE, col.names = TRUE)

##################################################################################################################
################## POUR NE PAS MELANGER DES CHOUX ET DES CAROTTES, ON ENLEVE LES LOCALITES QUI NE SONT ###########
################## PAS DEFINIS AU MOINS AU NIVEAU D'UN SEUL ETAGE (age max = 4.43 Ma -Burdigalien-) ##############
##################################################################################################################

wantTime <- which((FULLasia$MAX_AGE - FULLasia$MIN_AGE) > 5)
FULLasia <- FULLasia[-wantTime,]
if(length(which((FULLasia$MAX_AGE - FULLasia$MIN_AGE) > 5)) == 0)
{
  print("OK")
}


##################################################################################################################
##################################################################################################################
########### AFIN D'EVITER DE FABRIQUER UN UPGMA ILLISIBLE ON VA REGROUPER LES LOCALITES ##########################
##################################################################################################################

#Maintenant il faut le faire par taille de grain.
Min_LAT.ASIA <- floor(min(FULLasia$LAT, na.rm = TRUE))
Max_LAT.ASIA <- ceiling(max(FULLasia$LAT, na.rm = TRUE))
Min_LNG.ASIA <- floor(min(FULLasia$LONG, na.rm = TRUE))
Max_LNG.ASIA <- ceiling(max(FULLasia$LONG, na.rm = TRUE))

#Pour un grain de 1 degres
Grain1.ASIA.LAT <- c(Min_LAT.ASIA:Max_LAT.ASIA)
Grain1.ASIA.LNG <- c(Min_LNG.ASIA:Max_LNG.ASIA)

#Pour un grain de 0.5 degres
Grain05.ASIA.LAT <- seq(from = Min_LAT.ASIA, to = Max_LAT.ASIA, 0.5)
Grain05.ASIA.LNG <- seq(from = Min_LNG.ASIA, to = Max_LNG.ASIA, 0.5)

#Pour un grain de 0.1 degres
Grain01.ASIA.LAT <- seq(from = Min_LAT.ASIA, to = Max_LAT.ASIA, 0.1)
Grain01.ASIA.LNG <- seq(from = Min_LNG.ASIA, to = Max_LNG.ASIA, 0.1)

#Pour un grain de 1 degres
Liste.GRAIN1.ASIA <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain1.ASIA.LAT) - 1))
{
  for(k in 1:(length(Grain1.ASIA.LNG) - 1))
  {
    want <- which(FULLasia$LAT >= Grain1.ASIA.LAT[j] & FULLasia$LAT < Grain1.ASIA.LAT[j+1] 
                  & FULLasia$LONG >= Grain1.ASIA.LNG[k] & FULLasia$LONG < Grain1.ASIA.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain1.dataframe.ASIA <- paste("ASIA", Grain1.ASIA.LAT[j], Grain1.ASIA.LNG[k], sep = "_")
      N <- assign(names.Grain1.dataframe.ASIA, FULLasia[want,])
      Liste.GRAIN1.ASIA[[(length(Liste.GRAIN1.ASIA)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm("names.Grain1.dataframe.ASIA")
      }
  }
  print(paste(ceiling((j/(length(Grain1.ASIA.LAT) - 1) *100)), " % "))
}

#Pour un grain de 0.5 degres
Liste.GRAIN05.ASIA <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain05.ASIA.LAT) - 1))
{
  for(k in 1:(length(Grain05.ASIA.LNG) - 1))
  {
    want <- which(FULLasia$LAT >= Grain05.ASIA.LAT[j] & FULLasia$LAT < Grain05.ASIA.LAT[j+1] 
                  & FULLasia$LONG >= Grain05.ASIA.LNG[k] & FULLasia$LONG < Grain05.ASIA.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain05.dataframe.ASIA <- paste("ASIA", Grain05.ASIA.LAT[j], Grain05.ASIA.LNG[k], sep = "_")
      N <- assign(names.Grain05.dataframe.ASIA, FULLasia[want,])
      Liste.GRAIN05.ASIA[[(length(Liste.GRAIN05.ASIA)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm(names.Grain05.dataframe.ASIA)
    }
  }
  print(paste(ceiling((j/(length(Grain05.ASIA.LAT) - 1) *100)), " % "))
}

#Pour un grain de 0.1 degres
Liste.GRAIN01.ASIA <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain01.ASIA.LAT) - 1))
{
  for(k in 1:(length(Grain01.ASIA.LNG) - 1))
  {
    want <- which(FULLasia$LAT >= Grain01.ASIA.LAT[j] & FULLasia$LAT < Grain01.ASIA.LAT[j+1] 
                  & FULLasia$LONG >= Grain01.ASIA.LNG[k] & FULLasia$LONG < Grain01.ASIA.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain01.dataframe.ASIA <- paste("ASIA", Grain01.ASIA.LAT[j], Grain01.ASIA.LNG[k], sep = "_")
      N <- assign(names.Grain01.dataframe.ASIA, FULLasia[want,])
      Liste.GRAIN01.ASIA[[(length(Liste.GRAIN01.ASIA)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm(names.Grain01.dataframe.ASIA)
    }
  }
  print(paste(ceiling((j/(length(Grain01.ASIA.LAT) - 1) *100)), " % "))
}

#DONNER UN MEME NOM DE LOCALITE POUR CHAQUE LOCALITE REGROUPEES#

for(i in 1:length(Liste.GRAIN1.ASIA))
{
  titre <- paste(floor(Liste.GRAIN1.ASIA[[i]][1,]$LAT), floor(Liste.GRAIN1.ASIA[[i]][1,]$LONG), sep = "_")
  for(j in 1:length(Liste.GRAIN1.ASIA[[i]]))
  {
    Liste.GRAIN1.ASIA[[i]][j]$NAME <- titre
  }
}

for(i in 1:length(Liste.GRAIN05.ASIA))
{
  titre <- paste(round(Liste.GRAIN05.ASIA[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN05.ASIA[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN05.ASIA[[i]]))
  {
    Liste.GRAIN05.ASIA[[i]][j]$NAME <- titre
  }
}

for(i in 1:length(Liste.GRAIN01.ASIA))
{
  titre <- paste(round(Liste.GRAIN01.ASIA[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN01.ASIA[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN01.ASIA[[i]]))
  {
    Liste.GRAIN01.ASIA[[i]][j]$NAME <- titre
  }
}

#JOINDRE TOUTES LES "LOCALITES" DANS UN SEUL DATAFRAME ???? ET FAUT IL ??? IGNORER LES LOCALITE < 5 SP ????

FULLasiaGRAIN1 <- data.frame()
for(i in 1:length(Liste.GRAIN1.ASIA))
{
    FULLasiaGRAIN1 <- join(FULLasiaGRAIN1, Liste.GRAIN1.ASIA[[i]], match="all", type="full")  
}

FULLasiaGRAIN05 <- data.frame()
for(i in 1:length(Liste.GRAIN05.ASIA))
{
  FULLasiaGRAIN05 <- join(FULLasiaGRAIN05, Liste.GRAIN05.ASIA[[i]], match="all", type="full")  
}

FULLasiaGRAIN01 <- data.frame()
for(i in 1:length(Liste.GRAIN01.ASIA))
{
  FULLasiaGRAIN01 <- join(FULLasiaGRAIN01, Liste.GRAIN01.ASIA[[i]], match="all", type="full")  
}


#Reste -t-il des NA dans le JDD ?
ISNA <- is.na(FULLasiaGRAIN1)
summary(ISNA) #AUCUN NA
ISNA <- is.na(FULLasiaGRAIN05)
summary(ISNA) #AUCUN NA
ISNA <- is.na(FULLasiaGRAIN01)
summary(ISNA) #AUCUN NA

####################################################################################################################################################
######################################## CHOISIR RESOLUTION POUR NE PAS TOUT RE ECRIRE #####################################
####################################################################################################################################################
FULLasiaGRAIN1 <- FULLasiaGRAIN05
FULLasiaGRAIN1 <- FULLasiaGRAIN01

#Suppression des Cetaces et des chauves pouris
str(FULLasiaGRAIN1)
sort(summary(FULLasiaGRAIN1$FAMILY, maxsum = 157))
wantOUT <- which(FULLasiaGRAIN1$ORDER == "Chiroptera" | FULLasiaGRAIN1$ORDER == "Cetacea" | FULLasiaGRAIN1$FAMILY == "Phocidae" | FULLasiaGRAIN1$FAMILY == "Vespertilionidae" |
                   FULLasiaGRAIN1$ORDER == "Indet" |  FULLasiaGRAIN1$GENUS == "indet." | 
                   FULLasiaGRAIN1$FAMILY == "Balaenopteridae" | FULLasiaGRAIN1$FAMILY == "Rhinolophidae" |
                   FULLasiaGRAIN1$FAMILY == "Dugongidae" | FULLasiaGRAIN1$FAMILY == "Ziphiidae" |
                   FULLasiaGRAIN1$FAMILY == "Balaenidae" | FULLasiaGRAIN1$FAMILY == "Eurhinodelphinidae" |
                   FULLasiaGRAIN1$FAMILY == "Odobenidae" | FULLasiaGRAIN1$FAMILY == "Cetotheriidae" |
                   FULLasiaGRAIN1$FAMILY == "Squalodontidae" | FULLasiaGRAIN1$FAMILY == "Physeteridae" |
                   FULLasiaGRAIN1$FAMILY == "Hipposideridae" | FULLasiaGRAIN1$FAMILY == "Monodontidae" |
                   FULLasiaGRAIN1$FAMILY == "Megadermatidae" | FULLasiaGRAIN1$FAMILY == "Tranatocetidae" |
                   FULLasiaGRAIN1$FAMILY == "Platanistidae" | FULLasiaGRAIN1$FAMILY == "Kentriodontidae" |
                   FULLasiaGRAIN1$FAMILY == "Molossidae" | FULLasiaGRAIN1$FAMILY == "Eschrichtiidae" |
                   FULLasiaGRAIN1$FAMILY == "Desmostylidae" | FULLasiaGRAIN1$FAMILY == "Eoplatanistidae" |
                   FULLasiaGRAIN1$FAMILY == "Kogiidae" | FULLasiaGRAIN1$FAMILY == "Phocoenidae" | 
                   FULLasiaGRAIN1$FAMILY == "Pontoporiidae" | FULLasiaGRAIN1$FAMILY == "Squalodelphinidae" |
                   FULLasiaGRAIN1$FAMILY == "Emballonuridae" | FULLasiaGRAIN1$FAMILY == "Trichechidae" |
                   FULLasiaGRAIN1$FAMILY == "Dalpiazinidae" | FULLasiaGRAIN1$FAMILY == "Acrodelphidae" |
                   FULLasiaGRAIN1$FAMILY == "Patriocetidae" | FULLasiaGRAIN1$FAMILY == "Hyperoodontidae" |
                   FULLasiaGRAIN1$FAMILY == "Bohlininae" | FULLasiaGRAIN1$GENUS == "Pinocetus" | FULLasiaGRAIN1$GENUS == "Isocetus" |
                   FULLasiaGRAIN1$GENUS == "Scaldicetus" | FULLasiaGRAIN1$GENUS == "Tagicetus" | FULLasiaGRAIN1$GENUS == "Uranocetus" |
                   FULLasiaGRAIN1$GENUS == "Hoplocetus" | FULLasiaGRAIN1$GENUS == "Aglaocetus" | FULLasiaGRAIN1$GENUS == "Diorocetus" |
                   FULLasiaGRAIN1$GENUS == "Graamocetus" | FULLasiaGRAIN1$GENUS == "Pelocetus" | FULLasiaGRAIN1$GENUS == "Phococetus")


ASIA.Grain1.terre <- FULLasiaGRAIN1[-wantOUT,]
summary(ASIA.Grain1.terre$FAMILY, maxsum = 157)
summary(ASIA.Grain1.terre$ORDER, maxsum = 157)

######################################
##### SI ON NE VEUT QUE LES GROS #####
######################################

#1 on garde les insectivores
wantBIGASIA <- which(ASIA.Grain1.terre$ORDER == "Rodentia" | ASIA.Grain1.terre$ORDER == "Lagomorpha")
ASIA.Grain1.terre <- ASIA.Grain1.terre[-wantBIGASIA,]
summary(ASIA.Grain1.terre$ORDER)
#2 on garde pas les insectivores
wantBIGASIA <- which(ASIA.Grain1.terre$ORDER == "Rodentia" | ASIA.Grain1.terre$ORDER == "Lagomorpha" | ASIA.Grain1.terre$ORDER == "Eulipotyphla"
                       | ASIA.Grain1.terre$ORDER == "Hyracoidea" | ASIA.Grain1.terre$ORDER == "Lipotyphla" | ASIA.Grain1.terre$ORDER == "_")
ASIA.Grain1.terre <- ASIA.Grain1.terre[-wantBIGASIA,]
summary(ASIA.Grain1.terre$ORDER)

##################################################################################################################
############# ON VA DESORMAIS DECOUPER LE JDD PAR MN #############################################################
##################################################################################################################
AGE_MN <- c(0, 1.95, 2.6, 3.4, 4.2, 5.3, 7.1, 8.2, 9, 9.5, 11.2, 12.5, 15.2, 17, 18, 20, 22.8, 23.8)
NOM_MN <- c("MN17","MN16","MN15","MN14","MN13","MN12","MN11","MN10","MN9","MN8","MN7","MN6","MN5","MN4","MN3","MN2","MN1")

### AVEC WHICH : 
Liste.MN.ASIA <- list() #Liste pour stocker tous ces dataframes
for(i in 1:(length(AGE_MN) - 1))
{
  wantMN <- which(AGE_MN[i] >= ASIA.Grain1.terre$MIN_AGE & AGE_MN[i] < ASIA.Grain1.terre$MAX_AGE & AGE_MN[i+1] >= ASIA.Grain1.terre$MAX_AGE
                  | AGE_MN[i] <= ASIA.Grain1.terre$MIN_AGE & AGE_MN[i+1] > ASIA.Grain1.terre$MIN_AGE & AGE_MN[i+1] <= ASIA.Grain1.terre$MAX_AGE
                  | AGE_MN[i] > ASIA.Grain1.terre$MIN_AGE & AGE_MN[i+1] < ASIA.Grain1.terre$MAX_AGE
                  | AGE_MN[i] < ASIA.Grain1.terre$MIN_AGE & AGE_MN[i+1] > ASIA.Grain1.terre$MAX_AGE)
  #print(wantMN)
  if(length(wantMN) != 0)
  {
    names.dataframe.ASIA <- paste("AGE.MN", AGE_MN[i], sep = "_")
    N <- assign(names.dataframe.ASIA, ASIA.Grain1.terre[wantMN,])
    Liste.MN.ASIA[[length(Liste.MN.ASIA)+ 1]] <- N
  }
}

summary(Liste.MN.ASIA[[1]])
str(Liste.MN.ASIA[[1]])

##################################################################################################################
######################## FABRICATION DE LA MATRICE DE PRESENCE / ABSENCE #########################################
##################################################################################################################
source("Presence_Absence_matrix.R")
library(cluster)
MatP.A <- Presence_Absence_matrix(Liste.MN.ASI[[1]])
str(MatP.A)
apply(MatP.A, 2, sum)
apply(MatP.A, 1, sum)


#Modele 1 de selection, permissif : on garde tout ce qui est existe pendant un temps donne durant la periode cible
### Faune 1 : 
Faune1.ASI <- data.frame()

wantFaune1 <- which(11.2 >= ASIA.Grain1.terre$MIN_AGE & 11.2 < ASIA.Grain1.terre$MAX_AGE & 23.8 >= ASIA.Grain1.terre$MAX_AGE
                    | 11.2 <= ASIA.Grain1.terre$MIN_AGE & 23.8 > ASIA.Grain1.terre$MIN_AGE & 23.8 <= ASIA.Grain1.terre$MAX_AGE
                    | 11.2 > ASIA.Grain1.terre$MIN_AGE & 23.8 < ASIA.Grain1.terre$MAX_AGE
                    | 11.2 < ASIA.Grain1.terre$MIN_AGE & 23.8 > ASIA.Grain1.terre$MAX_AGE)

Faune1.ASI <- ASIA.Grain1.terre[wantFaune1,]  
#Matrice de P/A # TESTER LA 1 et la 4:
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Species")
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Species", singletons = FALSE)
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Genus")
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Genus", singletons = FALSE)
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Genus", singletons = FALSE, min5 = TRUE)

## Faune 2 : 
Faune2.ASI <- data.frame()

wantFaune2 <- which(5.3 >= ASIA.Grain1.terre$MIN_AGE & 5.3 < ASIA.Grain1.terre$MAX_AGE & 12.5 >= ASIA.Grain1.terre$MAX_AGE
                    | 5.3 <= ASIA.Grain1.terre$MIN_AGE & 12.5 > ASIA.Grain1.terre$MIN_AGE & 12.5 <= ASIA.Grain1.terre$MAX_AGE
                    | 5.3 > ASIA.Grain1.terre$MIN_AGE & 12.5 < ASIA.Grain1.terre$MAX_AGE
                    | 5.3 < ASIA.Grain1.terre$MIN_AGE & 12.5 > ASIA.Grain1.terre$MAX_AGE)

Faune2.ASI <- ASIA.Grain1.terre[wantFaune2,] 
#Matrice de P/A
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Species")
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Species", singletons = FALSE)
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Genus")
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Genus", singletons = FALSE)
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Genus", singletons = TRUE, min5 = TRUE)


## Faune 3 :
Faune3.ASI <- data.frame()

wantFaune3 <- which(0 >= ASIA.Grain1.terre$MIN_AGE & 0 < ASIA.Grain1.terre$MAX_AGE & 7.1 >= ASIA.Grain1.terre$MAX_AGE
                    | 0 <= ASIA.Grain1.terre$MIN_AGE & 7.1 > ASIA.Grain1.terre$MIN_AGE & 7.1 <= ASIA.Grain1.terre$MAX_AGE
                    | 0 > ASIA.Grain1.terre$MIN_AGE & 7.1 < ASIA.Grain1.terre$MAX_AGE
                    | 0 < ASIA.Grain1.terre$MIN_AGE & 7.1 > ASIA.Grain1.terre$MAX_AGE)

Faune3.ASI <- ASIA.Grain1.terre[wantFaune3,] 

## On retire un point en Australie
wantFaune3 <- which(Faune3.ASI$LAT < -20)
Faune3.ASI <- Faune3.ASI[-wantFaune3,]

#Matrice de P/A
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Species")
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Species", singletons = FALSE)
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Genus")
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Genus", singletons = FALSE)
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Genus", singletons = TRUE, min5 = TRUE)


#Modele 2 de selection, exclusif : on ne garde que les taxons disparaissant avant la borne sup et avant la borne inf
## Faune 1
Faune1B.ASI <- data.frame()

wantFaune1B <- which(11.2 <= ASIA.Grain1.terre$MIN_AGE & 11.2 < ASIA.Grain1.terre$MAX_AGE & 23.8 >= ASIA.Grain1.terre$MAX_AGE)

Faune1B.ASI <- ASIA.Grain1.terre[wantFaune1B,] 
#Matrice de P/A
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Species")
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Species", singletons = FALSE)
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Genus")
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Genus", singletons = FALSE)
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune1B.ASI <- Presence_Absence_matrix(Faune1B.ASI, type = "Genus", singletons = TRUE, min5 = TRUE)

# Faune 2
Faune2B.ASI <- data.frame()

wantFaune2B <- which(5.3 <= ASIA.Grain1.terre$MIN_AGE & 5.3 < ASIA.Grain1.terre$MAX_AGE & 12.5 >= ASIA.Grain1.terre$MAX_AGE)

Faune2B.ASI <- ASIA.Grain1.terre[wantFaune2B,] 
#Matrice de P/A
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Species")
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Species", singletons = FALSE)
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Genus")
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Genus", singletons = FALSE)
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune2B.ASI <- Presence_Absence_matrix(Faune2B.ASI, type = "Genus", singletons = TRUE, min5 = TRUE)

# Faune 3
Faune3B.ASI <- data.frame()

wantFaune3B <- which(0 <= ASIA.Grain1.terre$MIN_AGE & 0 < ASIA.Grain1.terre$MAX_AGE & 7.1 >= ASIA.Grain1.terre$MAX_AGE)

Faune3B.ASI <- ASIA.Grain1.terre[wantFaune3B,]

## On retire un point en Australie
wantFaune3B <- which(Faune3B.ASI$LAT < -20)
Faune3B.ASI <- Faune3B.ASI[-wantFaune3B,]

#Matrice de P/A
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Species")
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Species", singletons = FALSE)
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Genus")
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Genus", singletons = FALSE)
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3B.ASI <- Presence_Absence_matrix(Faune3B.ASI, type = "Genus", singletons = TRUE, min5 = TRUE)




##################################################################################################################
######################################## LE NMDS VA ETRE UTILISE EN AMONT AFIN DE ################################
######################################## TRANSFORMER NOS DONNES QUALITATIVES (DISCRETES) #########################
######################################## EN DONNES CONTINUES, CES NOUVELLES DONNES ###############################
######################################## SERONT UTILISES POUR TROUVER LE BON NOMBRE DE ###########################
######################################## CLUSTERS : HCPC (HIERARCHICAL CLASSI ON PRINCIPAL COMPONANT) ############                                ##############################
##################################################################################################################
source("Presence_Absence_matrix.R")
library(cluster)
library(FactoMineR)
source("IllustrationCluster.R")

## Contrairement à une PCA ou les valeurs initiales, quantitatives et continues sont utilisées pour réduire le nombre
## de variable, ici pour un MDS/PCoA il s'agit d'abord de transformer les données binaires en données de dissimilarités.

MatP.A.Faune1.ASI <- Presence_Absence_matrix(Faune1.ASI, type = "Species", singletons = FALSE, min5 = 2)

#Tester avec un NMDS (travail avec des rangs)
#Bray-Curtis
Dist.Faune1.ASI <- vegdist(MatP.A.Faune1.ASI, method = "bray")
mds.Faune1.ASI.BRAY <- cmdscale(Dist.Faune1.ASI, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune1.ASI.BRAY <- HCPC(as.data.frame(mds.Faune1.ASI.BRAY$points))
x11()
IllustrationCluster(hcpc.Faune1.ASI.BRAY)

#SIMPSON
MatP.A.Faune1.ASI_Inv <- t(MatP.A.Faune1.ASI)
Dist.Faune1.ASI <- ecol.dist(MatP.A.Faune1.ASI_Inv, method = simpson, type = "dis")
mds.Faune1.ASI.SIMP <- cmdscale(Dist.Faune1.ASI, eig = TRUE, x.ret = TRUE)

hcpc.Faune1.ASI.SIMP <- HCPC(as.data.frame(mds.Faune1.ASI.SIMP$points))
x11()
IllustrationCluster(hcpc.Faune1.ASI.SIMP, etik = TRUE)

#RAUP & CRICK
Dist.Faune1.ASIA <- raupcrick(MatP.A.Faune1.ASI)
mds.Faune1.ASIA.RC <- cmdscale(Dist.Faune1.ASIA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune1.ASIA.RC <- HCPC(as.data.frame(mds.Faune1.ASIA.RC$point))
x11()
IllustrationCluster(hcpc.Faune1.ASIA.RC, etik = TRUE, cexEtik = 0.1)

MatP.A.Faune2.ASI <- Presence_Absence_matrix(Faune2.ASI, type = "Species", singletons = FALSE, min5 = 2)

#Faune 2
#Bray-Curtis
Dist.Faune2.ASI <- vegdist(MatP.A.Faune2.ASI, method = "bray")
mds.Faune2.ASI.BRAY <- cmdscale(Dist.Faune2.ASI, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune2.ASI.BRAY <- HCPC(as.data.frame(mds.Faune2.ASI.BRAY$points))
x11()
IllustrationCluster(hcpc.Faune2.ASI.BRAY)

#SIMPSON
MatP.A.Faune2.ASI_Inv <- t(MatP.A.Faune2.ASI)
Dist.Faune2.ASI <- ecol.dist(MatP.A.Faune2.ASI_Inv, method = simpson, type = "dis")
mds.Faune2.ASI.SIMP <- cmdscale(Dist.Faune2.ASI, eig = TRUE, x.ret = TRUE)

hcpc.Faune2.ASI.SIMP <- HCPC(as.data.frame(mds.Faune2.ASI.SIMP$points))
x11()
IllustrationCluster(hcpc.Faune2.ASI.SIMP, etik = TRUE)

#RAUP & CRICK
Dist.Faune2.ASIA <- raupcrick(MatP.A.Faune2.ASI)
mds.Faune2.ASIA.RC <- cmdscale(Dist.Faune2.ASIA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune2.ASIA.RC <- HCPC(as.data.frame(mds.Faune2.ASIA.RC$point))
x11()
IllustrationCluster(hcpc.Faune2.ASIA.RC, etik = TRUE, cexEtik = 0.1)

MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Species", singletons = TRUE, min5 = 4)

#Faune 3
#Bray-Curtis
Dist.Faune3.ASI <- vegdist(MatP.A.Faune3.ASI, method = "bray")
mds.Faune3.ASI.BRAY <- cmdscale(Dist.Faune3.ASI, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3.ASI.BRAY <- HCPC(as.data.frame(mds.Faune3.ASI.BRAY$points))
x11()
IllustrationCluster(hcpc.Faune3.ASI.BRAY)

#SIMPSON
MatP.A.Faune3.ASI_Inv <- t(MatP.A.Faune3.ASI)
Dist.Faune3.ASI <- ecol.dist(MatP.A.Faune3.ASI_Inv, method = simpson, type = "dis")
mds.Faune3.ASI.SIMP <- cmdscale(Dist.Faune3.ASI, eig = TRUE, x.ret = TRUE)

hcpc.Faune3.ASI.SIMP <- HCPC(as.data.frame(mds.Faune3.ASI.SIMP$points))
x11()
IllustrationCluster(hcpc.Faune3.ASI.SIMP, etik = TRUE)

MatP.A.Faune3.ASI <- Presence_Absence_matrix(Faune3.ASI, type = "Species", singletons = FALSE, min5 = 2)

#RAUP & CRICK
Dist.Faune3.ASIA <- raupcrick(MatP.A.Faune3.ASI)
mds.Faune3.ASIA.RC <- cmdscale(Dist.Faune3.ASIA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune3.ASIA.RC <- HCPC(as.data.frame(mds.Faune3.ASIA.RC$point))
x11()
IllustrationCluster(hcpc.Faune3.ASIA.RC, etik = TRUE, cexEtik = 0.1)


## VERSION B : EXCLUSIVE
#Faune 1B
#Bray-Curtis
Dist.Faune1B.ASI <- vegdist(MatP.A.Faune1B.ASI, method = "bray")
mds.Faune1B.ASI.BRAY <- cmdscale(Dist.Faune1B.ASI, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune1B.ASI.BRAY <- HCPC(as.data.frame(mds.Faune1B.ASI.BRAY$points))
x11()
IllustrationCluster(hcpc.Faune1B.ASI.BRAY)

#SIMPSON
MatP.A.Faune1B.ASI_Inv <- t(MatP.A.Faune1B.ASI)
Dist.Faune1B.ASI <- ecol.dist(MatP.A.Faune1B.ASI_Inv, method = simpson, type = "dis")
mds.Faune1B.ASI.SIMP <- cmdscale(Dist.Faune1B.ASI, eig = TRUE, x.ret = TRUE)

hcpc.Faune1B.ASI.SIMP <- HCPC(as.data.frame(mds.Faune1B.ASI.SIMP$points))
x11()
IllustrationCluster(hcpc.Faune1B.ASI.SIMP)

#RAUP & CRICK
Dist.Faune1B.ASIA <- raupcrick(MatP.A.Faune1B.ASI)
mds.Faune1B.ASIA.RC <- cmdscale(Dist.Faune1B.ASIA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune1B.ASIA.RC <- HCPC(as.data.frame(mds.Faune1B.ASIA.RC$point))
x11()
IllustrationCluster(hcpc.Faune1B.ASIA.RC)


#Faune 2B
#Bray-Curtis
Dist.Faune2B.ASI <- vegdist(MatP.A.Faune2B.ASI, method = "bray")
mds.Faune2B.ASI.BRAY <- cmdscale(Dist.Faune2B.ASI, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune2B.ASI.BRAY <- HCPC(as.data.frame(mds.Faune2B.ASI.BRAY$points)) 
x11()
IllustrationCluster(hcpc.Faune2B.ASI.BRAY)

#SIMPSON
MatP.A.Faune2B.ASI_Inv <- t(MatP.A.Faune2B.ASI)
Dist.Faune2B.ASI <- ecol.dist(MatP.A.Faune2B.ASI_Inv, method = simpson, type = "dis")
mds.Faune2B.ASI.SIMP <- cmdscale(Dist.Faune2B.ASI, eig = TRUE, x.ret = TRUE)

hcpc.Faune2B.ASI.SIMP <- HCPC(as.data.frame(mds.Faune2B.ASI.SIMP$points)) 
x11()
IllustrationCluster(hcpc.Faune2B.ASI.SIMP)

#RAUP & CRICK
Dist.Faune2B.ASIA <- raupcrick(MatP.A.Faune2B.ASI)
mds.Faune2B.ASIA.RC <- cmdscale(Dist.Faune2B.ASIA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune2B.ASIA.RC <- HCPC(as.data.frame(mds.Faune2B.ASIA.RC$point))
x11()
IllustrationCluster(hcpc.Faune2B.ASIA.RC)


#Faune 3B
#Bray-Curtis
Dist.Faune3B.ASI <- vegdist(MatP.A.Faune3B.ASI, method = "bray")
mds.Faune3B.ASI.BRAY <- cmdscale(Dist.Faune3B.ASI, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3B.ASI.BRAY <- HCPC(as.data.frame(mds.Faune3B.ASI.BRAY$points)) 
x11()
IllustrationCluster(hcpc.Faune3B.ASI.BRAY)

#SIMPSON
MatP.A.Faune3B.ASI_Inv <- t(MatP.A.Faune3B.ASI)
Dist.Faune3B.ASI <- ecol.dist(MatP.A.Faune3B.ASI_Inv, method = simpson, type = "dis")
mds.Faune3B.ASI.SIMP <- cmdscale(Dist.Faune3B.ASI, eig = TRUE, x.ret = TRUE)

hcpc.Faune3B.ASI.SIMP <- HCPC(as.data.frame(mds.Faune3B.ASI.SIMP$points)) 
x11()
IllustrationCluster(hcpc.Faune3B.ASI.SIMP)

#RAUP & CRICK
Dist.Faune3B.ASIA <- raupcrick(MatP.A.Faune3B.ASI)
mds.Faune3B.ASIA.RC <- cmdscale(Dist.Faune3B.ASIA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune3B.ASIA.RC <- HCPC(as.data.frame(mds.Faune3B.ASIA.RC$point))
x11()
IllustrationCluster(hcpc.Faune3B.ASIA.RC)


##################################################################################################################
##################################################################################################################
##################################################################################################################
################################################## AFRIQUE #######################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
setwd("C:/Users/cgibert01/Desktop/Biblio post doc Poitiers/Data téléchargés")
AFRICApbdb <- read.table("pbdb_data_AFRICA_25_0.txt", h = T) 
f <- AFRICApbdb$lat
f2 <- AFRICApbdb$lng
AFRICApbdb$lat <- as.numeric(levels(f))[f]
AFRICApbdb$lng <- as.numeric(levels(f2))[f2]

AfriqueNOW <- read.table("now_export_AFRICA_25_0.txt", h = T)

AfriqueEthiopie <- read.table("ExportAllEthiopieWithDates.txt", h = T)

A <- AfriqueNOW
B <- AFRICApbdb
C <- AfriqueEthiopie

str(A)
str(C)
#On conserve les colonnes les plus utiles (a premiere vue).
#Selection des occurrences au moins definies au niveau du GENRE

A_indet <- subset(A, A$GENUS != "indet." & A$GENUS != "_" & A$GENUS != "gen.") 
A_Expurg <- A_indet[, c("LIDNUM", "NAME",  "ORDER", "FAMILY", "GENUS", "SPECIES", "LAT", "LONG", "MAX_AGE", "MIN_AGE")]

B_indet <- subset(B, B$genus != "_" & B$genus != "indet")
B_Expurg <- B_indet[, c("occurrence_no", "collection_name",  "order","family", "genus", "accepted_name","lat", "lng", "max_ma", "min_ma")]

C_indet <- subset(C, C$Genre != "_" & C$Genre != "indet")
C_Expurg <- C_indet[, c("Inventaire_specimen", "Classe", "Ordre", "Famille", "Genre", "Espece", "Coordonnees_localites..Latitude_decimale", "Coordonnees_localites..Longitude_decimale", "Localites..Age_maximum", "Localites..Age_minimum")]

#Remplacer les GENRES dans B_Expurg par indet.
B_Expurg$accepted_name <- as.character(B_Expurg$accepted_name)
str(B_Expurg$accepted_name)

for(i in 1:length(B_Expurg[,1]))
{
  if(B_Expurg[i ,("accepted_name")] == B_Expurg[i ,("genus")])
  {
    B_Expurg[i, ("accepted_name")] <- "indet."
  }
}

#Joindre dans un seul dataset : sur le modele de la base de donnes NOW
names(B_Expurg)
names(A_Expurg)
names(B_Expurg) <- names(A_Expurg)
names(C_Expurg) <- names(A_Expurg)

library(plyr)
FULLAFRICA <- join(A_Expurg, B_Expurg , match="all", type="full")
FULLAFRICA <- join(FULLAFRICA, C_Expurg , match="all", type="full")

#write.table(FULLeurope, file = "FULLEurope.txt", sep = " ", row.names = TRUE, col.names = TRUE)

##################################################################################################################
################## POUR NE PAS MELANGER DES CHOUX ET DES CAROTTES, ON ENLEVE LES LOCALITES QUI NE SONT ###########
################## PAS DEFINIS AU MOINS AU NIVEAU D'UN SEUL ETAGE (age max = 4.43 Ma -Burdigalien-) ##############
##################################################################################################################

wantTime <- which((FULLAFRICA$MAX_AGE - FULLAFRICA$MIN_AGE) > 5)
FULLAFRICA <- FULLAFRICA[-wantTime,]
if(length(which((FULLAFRICA$MAX_AGE - FULLAFRICA$MIN_AGE) > 5)) == 0)
{
  print("OK")
}

##################################################################################################################
##################################################################################################################
########### AFIN D'EVITER DE FABRIQUER UN UPGMA ILLISIBLE ON VA REGROUPER LES LOCALITES ##########################
##################################################################################################################

#Maintenant il faut le faire par taille de grain.
Min_LAT.AFRICA <- floor(min(FULLAFRICA$LAT, na.rm = TRUE))
Max_LAT.AFRICA <- ceiling(max(FULLAFRICA$LAT, na.rm = TRUE))
Min_LNG.AFRICA <- floor(min(FULLAFRICA$LONG, na.rm = TRUE))
Max_LNG.AFRICA <- ceiling(max(FULLAFRICA$LONG, na.rm = TRUE))

#Pour un grain de 1 degres
Grain1.AFRICA.LAT <- c(Min_LAT.AFRICA:Max_LAT.AFRICA)
Grain1.AFRICA.LNG <- c(Min_LNG.AFRICA:Max_LNG.AFRICA)

#Pour un grain de 0.5 degres
Grain05.AFRICA.LAT <- seq(from = Min_LAT.AFRICA, to = Max_LAT.AFRICA, 0.5)
Grain05.AFRICA.LNG <- seq(from = Min_LNG.AFRICA, to = Max_LNG.AFRICA, 0.5)

#Pour un grain de 0.1 degres
Grain01.AFRICA.LAT <- seq(from = Min_LAT.AFRICA, to = Max_LAT.AFRICA, 0.1)
Grain01.AFRICA.LNG <- seq(from = Min_LNG.AFRICA, to = Max_LNG.AFRICA, 0.1)


#Pour un grain de 1 degres
Liste.GRAIN1.AFRICA <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain1.AFRICA.LAT) - 1))
{
  for(k in 1:(length(Grain1.AFRICA.LNG) - 1))
  {
    want <- which(FULLAFRICA$LAT >= Grain1.AFRICA.LAT[j] & FULLAFRICA$LAT < Grain1.AFRICA.LAT[j+1] 
                  & FULLAFRICA$LONG >= Grain1.AFRICA.LNG[k] & FULLAFRICA$LONG < Grain1.AFRICA.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain1.dataframe.AFRICA <- paste("EU", Grain1.AFRICA.LAT[j], Grain1.AFRICA.LNG[k], sep = "_")
      N <- assign(names.Grain1.dataframe.AFRICA, FULLAFRICA[want,])
      Liste.GRAIN1.AFRICA[[(length(Liste.GRAIN1.AFRICA)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
    }
  }
  print(paste(ceiling((j/(length(Grain1.AFRICA.LAT) - 1) *100)), " % "))
}

#Pour un grain de 0.5 degres
Liste.GRAIN05.AFRICA <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain05.AFRICA.LAT) - 1))
{
  for(k in 1:(length(Grain05.AFRICA.LNG) - 1))
  {
    want <- which(FULLAFRICA$LAT >= Grain05.AFRICA.LAT[j] & FULLAFRICA$LAT < Grain05.AFRICA.LAT[j+1] 
                  & FULLAFRICA$LONG >= Grain05.AFRICA.LNG[k] & FULLAFRICA$LONG < Grain05.AFRICA.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain05.dataframe.AFRICA <- paste("AFRICA", Grain05.AFRICA.LAT[j], Grain05.AFRICA.LNG[k], sep = "_")
      N <- assign(names.Grain05.dataframe.AFRICA, FULLAFRICA[want,])
      Liste.GRAIN05.AFRICA[[(length(Liste.GRAIN05.AFRICA)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm(names.Grain05.dataframe.AFRICA)
    }
  }
  print(paste(ceiling((j/(length(Grain05.AFRICA.LAT) - 1) *100)), " % "))
}

#Pour un grain de 0.1 degres
Liste.GRAIN01.AFRICA <- list() #Liste pour stocker tous ces dataframes

for(j in 1:(length(Grain01.AFRICA.LAT) - 1))
{
  for(k in 1:(length(Grain01.AFRICA.LNG) - 1))
  {
    want <- which(FULLAFRICA$LAT >= Grain01.AFRICA.LAT[j] & FULLAFRICA$LAT < Grain01.AFRICA.LAT[j+1] 
                  & FULLAFRICA$LONG >= Grain01.AFRICA.LNG[k] & FULLAFRICA$LONG < Grain01.AFRICA.LNG[k+1])
    if(length(want) != 0)
    {
      names.Grain01.dataframe.AFRICA <- paste("AFRICA", Grain01.AFRICA.LAT[j], Grain01.AFRICA.LNG[k], sep = "_")
      N <- assign(names.Grain01.dataframe.AFRICA, FULLAFRICA[want,])
      Liste.GRAIN01.AFRICA[[(length(Liste.GRAIN01.AFRICA)+1)]] <- N # Une erreur été générée par l'adresse d'écriture dans la Liste
      rm(names.Grain01.dataframe.AFRICA)
    }
  }
  print(paste(ceiling((j/(length(Grain01.AFRICA.LAT) - 1) *100)), " % "))
}


#DONNER UN MEME NOM DE LOCALITE POUR CHAQUE LOCALITE REGROUPEES#

for(i in 1:length(Liste.GRAIN1.AFRICA))
{
  titre <- paste(floor(Liste.GRAIN1.AFRICA[[i]][1,]$LAT), floor(Liste.GRAIN1.AFRICA[[i]][1,]$LONG), sep = "_")
  for(j in 1:length(Liste.GRAIN1.AFRICA[[i]]))
  {
    Liste.GRAIN1.AFRICA[[i]][j]$NAME <- titre
  }
}

for(i in 1:length(Liste.GRAIN05.AFRICA))
{
  titre <- paste(round(Liste.GRAIN05.AFRICA[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN05.AFRICA[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN05.AFRICA[[i]]))
  {
    Liste.GRAIN05.AFRICA[[i]][j]$NAME <- titre
  }
}

for(i in 1:length(Liste.GRAIN01.AFRICA))
{
  titre <- paste(round(Liste.GRAIN01.AFRICA[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN01.AFRICA[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN01.AFRICA[[i]]))
  {
    Liste.GRAIN01.AFRICA[[i]][j]$NAME <- titre
  }
}

#JOINDRE TOUTES LES "LOCALITES" DANS UN SEUL DATAFRAME ???? ET FAUT IL ??? IGNORER LES LOCALITE < 5 SP ????

FULLAFRICAGRAIN1 <- data.frame()
for(i in 1:length(Liste.GRAIN1.AFRICA))
{
  if(length(Liste.GRAIN1.AFRICA[[i]][,1]) > 4)
  {
    FULLAFRICAGRAIN1 <- join(FULLAFRICAGRAIN1, Liste.GRAIN1.AFRICA[[i]], match="all", type="full")  
  }
}

FULLAFRICAGRAIN05 <- data.frame()
for(i in 1:length(Liste.GRAIN05.AFRICA))
{
  FULLAFRICAGRAIN05 <- join(FULLAFRICAGRAIN05, Liste.GRAIN05.AFRICA[[i]], match="all", type="full")  
}

FULLAFRICAGRAIN01 <- data.frame()
for(i in 1:length(Liste.GRAIN01.AFRICA))
{
  FULLAFRICAGRAIN01 <- join(FULLAFRICAGRAIN01, Liste.GRAIN01.AFRICA[[i]], match="all", type="full")  
}

#Reste -t-il des NA dans le JDD ?
ISNA <- is.na(FULLAFRICAGRAIN1)
summary(ISNA) #AUCUN NA
ISNA <- is.na(FULLAFRICAGRAIN05)
summary(ISNA) #AUCUN NA
ISNA <- is.na(FULLAFRICAGRAIN01)
summary(ISNA) #AUCUN NA

#Suppression des Cetaces et des chauves pouris
FULLAFRICAGRAIN1 <- FULLAFRICAGRAIN05
FULLAFRICAGRAIN1 <- FULLAFRICAGRAIN01

str(FULLAFRICAGRAIN1)
sort(summary(FULLAFRICAGRAIN1$FAMILY, maxsum = 157))
wantOUT <- which(FULLAFRICAGRAIN1$ORDER == "Chiroptera" | FULLAFRICAGRAIN1$ORDER == "Cetacea" | FULLAFRICAGRAIN1$FAMILY == "Phocidae" | FULLAFRICAGRAIN1$FAMILY == "Vespertilionidae" |
                   FULLAFRICAGRAIN1$ORDER == "Indet" |  FULLAFRICAGRAIN1$GENUS == "indet." |
                   FULLAFRICAGRAIN1$ORDER == "Indet." |  FULLAFRICAGRAIN1$FAMILY == "Otariidae" |
                   FULLAFRICAGRAIN1$FAMILY == "incertae_sedis" | FULLAFRICAGRAIN1$FAMILY == "indet." |
                   FULLAFRICAGRAIN1$FAMILY == "Balaenopteridae" | FULLAFRICAGRAIN1$FAMILY == "Rhinolophidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Dugongidae" | FULLAFRICAGRAIN1$FAMILY == "Ziphiidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Balaenidae" | FULLAFRICAGRAIN1$FAMILY == "Eurhinodelphinidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Odobenidae" | FULLAFRICAGRAIN1$FAMILY == "Cetotheriidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Squalodontidae" | FULLAFRICAGRAIN1$FAMILY == "Physeteridae" |
                   FULLAFRICAGRAIN1$FAMILY == "Hipposideridae" | FULLAFRICAGRAIN1$FAMILY == "Monodontidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Megadermatidae" | FULLAFRICAGRAIN1$FAMILY == "Tranatocetidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Platanistidae" | FULLAFRICAGRAIN1$FAMILY == "Kentriodontidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Molossidae" | FULLAFRICAGRAIN1$FAMILY == "Eschrichtiidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Desmostylidae" | FULLAFRICAGRAIN1$FAMILY == "Eoplatanistidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Kogiidae" | FULLAFRICAGRAIN1$FAMILY == "Phocoenidae" | 
                   FULLAFRICAGRAIN1$FAMILY == "Pontoporiidae" | FULLAFRICAGRAIN1$FAMILY == "Squalodelphinidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Emballonuridae" | FULLAFRICAGRAIN1$FAMILY == "Trichechidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Dalpiazinidae" | FULLAFRICAGRAIN1$FAMILY == "Acrodelphidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Patriocetidae" | FULLAFRICAGRAIN1$FAMILY == "Hyperoodontidae" |
                   FULLAFRICAGRAIN1$FAMILY == "Bohlininae" | FULLAFRICAGRAIN1$GENUS == "Pinocetus" | FULLAFRICAGRAIN1$GENUS == "Isocetus" |
                   FULLAFRICAGRAIN1$GENUS == "Scaldicetus" | FULLAFRICAGRAIN1$GENUS == "Tagicetus" | FULLAFRICAGRAIN1$GENUS == "Uranocetus" |
                   FULLAFRICAGRAIN1$GENUS == "Hoplocetus" | FULLAFRICAGRAIN1$GENUS == "Aglaocetus" | FULLAFRICAGRAIN1$GENUS == "Diorocetus" |
                   FULLAFRICAGRAIN1$GENUS == "Graamocetus" | FULLAFRICAGRAIN1$GENUS == "Pelocetus" | FULLAFRICAGRAIN1$GENUS == "Phococetus")


AFRICA.Grain1.terre <- FULLAFRICAGRAIN1[-wantOUT,]
summary(AFRICA.Grain1.terre$FAMILY, maxsum = 157)
summary(AFRICA.Grain1.terre$ORDER, maxsum = 157)

######################################
##### SI ON NE VEUT QUE LES GROS #####
######################################

#1 on garde les insectivores
wantBIGASIA <- which(AFRICA.Grain1.terre$ORDER == "Rodentia" | AFRICA.Grain1.terre$ORDER == "Lagomorpha")
AFRICA.Grain1.terre <- AFRICA.Grain1.terre[-wantBIGASIA,]
summary(AFRICA.Grain1.terre$ORDER)
#2 on garde pas les insectivores
wantBIGASIA <- which(AFRICA.Grain1.terre$ORDER == "Rodentia" | AFRICA.Grain1.terre$ORDER == "Lagomorpha" | AFRICA.Grain1.terre$ORDER == "Eulipotyphla"
                     | AFRICA.Grain1.terre$ORDER == "Hyracoidea" | AFRICA.Grain1.terre$ORDER == "Lipotyphla" | AFRICA.Grain1.terre$ORDER == "_"
                     | AFRICA.Grain1.terre$ORDER == "Glires")
AFRICA.Grain1.terre <- AFRICA.Grain1.terre[-wantBIGASIA,]
summary(AFRICA.Grain1.terre$ORDER)


##################################################################################################################
############# ON VA DESORMAIS DECOUPER LE JDD PAR MN #############################################################
##################################################################################################################
#AGE_MN <- c(0, 1.95, 2.6, 3.4, 4.2, 5.3, 7.1, 8.2, 9, 9.5, 11.2, 12.5, 15.2, 17, 18, 20, 22.8, 23.8)
#NOM_MN <- c("MN17","MN16","MN15","MN14","MN13","MN12","MN11","MN10","MN9","MN8","MN7","MN6","MN5","MN4","MN3","MN2","MN1")

### AVEC WHICH : 
Liste.MN.AFRICA <- list() #Liste pour stocker tous ces dataframes
for(i in 1:(length(AGE_MN) - 1))
{
  wantMN <- which(AGE_MN[i] >= AFRICA.Grain1.terre$MIN_AGE & AGE_MN[i] < AFRICA.Grain1.terre$MAX_AGE & AGE_MN[i+1] >= AFRICA.Grain1.terre$MAX_AGE
                  | AGE_MN[i] <= AFRICA.Grain1.terre$MIN_AGE & AGE_MN[i+1] > AFRICA.Grain1.terre$MIN_AGE & AGE_MN[i+1] <= AFRICA.Grain1.terre$MAX_AGE
                  | AGE_MN[i] > AFRICA.Grain1.terre$MIN_AGE & AGE_MN[i+1] < AFRICA.Grain1.terre$MAX_AGE
                  | AGE_MN[i] < AFRICA.Grain1.terre$MIN_AGE & AGE_MN[i+1] > AFRICA.Grain1.terre$MAX_AGE)
  #print(wantMN)
  if(length(wantMN) != 0)
  {
    names.dataframe.AFRICA <- paste("AGE.MN", AGE_MN[i], sep = "_")
    N <- assign(names.dataframe.AFRICA, AFRICA.Grain1.terre[wantMN,])
    Liste.MN.AFRICA[[length(Liste.MN.AFRICA)+ 1]] <- N
  }
}

summary(Liste.MN.AFRICA[[1]])
str(Liste.MN.AFRICA[[1]])
quantile(Liste.MN.AFRICA[[1]]$MAX_AGE, probs = seq(0,1,0.05))

##################################################################################################################
######################## FABRICATION DE LA MATRICE DE PRESENCE / ABSENCE #########################################
##################################################################################################################
source("Presence_Absence_matrix.R")
library(cluster)
MatP.A <- Presence_Absence_matrix(Liste.MN.AFRICA[[1]])
str(MatP.A)
apply(MatP.A, 2, sum)
apply(MatP.A, 1, sum)


## Faune 3 :
Faune3.AFRICA <- data.frame()

wantFaune3 <- which(0 >= AFRICA.Grain1.terre$MIN_AGE & 0 < AFRICA.Grain1.terre$MAX_AGE & 7.1 >= AFRICA.Grain1.terre$MAX_AGE
                    | 0 <= AFRICA.Grain1.terre$MIN_AGE & 7.1 > AFRICA.Grain1.terre$MIN_AGE & 7.1 <= AFRICA.Grain1.terre$MAX_AGE
                    | 0 > AFRICA.Grain1.terre$MIN_AGE & 7.1 < AFRICA.Grain1.terre$MAX_AGE
                    | 0 < AFRICA.Grain1.terre$MIN_AGE & 7.1 > AFRICA.Grain1.terre$MAX_AGE)

Faune3.AFRICA <- AFRICA.Grain1.terre[wantFaune3,] 

## On retire un point en Australie
#wantFaune3 <- which(Faune3.AFRICA$LAT < -20)
#Faune3.AFRICA <- Faune3.AFRICA[-wantFaune3,]

#Matrice de P/A
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Species")
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Species", singletons = FALSE)
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Genus")
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Genus", singletons = FALSE)
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Genus", singletons = TRUE, min5 = TRUE)


#Modele 2 de selection, exclusif : on ne garde que les taxons disparaissant avant la borne sup et avant la borne inf
# Faune 3
Faune3B.AFRICA <- data.frame()

wantFaune3B <- which(0 <= AFRICA.Grain1.terre$MIN_AGE & 0 < AFRICA.Grain1.terre$MAX_AGE & 7.1 >= AFRICA.Grain1.terre$MAX_AGE)

Faune3B.AFRICA <- AFRICA.Grain1.terre[wantFaune3B,]

## On retire un point en Australie
wantFaune3B <- which(Faune3B.AFRICA$LAT < -20)
Faune3B.AFRICA <- Faune3B.AFRICA[-wantFaune3B,]

#Matrice de P/A
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Species")
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Species", singletons = FALSE)
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Species", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Species", singletons = TRUE, min5 = TRUE)

#Matrice de P/A GENUS
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Genus")
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Genus", singletons = FALSE)
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Genus", singletons = FALSE, min5 = TRUE)
MatP.A.Faune3B.AFR <- Presence_Absence_matrix(Faune3B.AFRICA, type = "Genus", singletons = TRUE, min5 = TRUE)


##################################################################################################################
######################################## LE NMDS VA ETRE UTILISE EN AMONT AFIN DE ################################
######################################## TRANSFORMER NOS DONNES QUALITATIVES (DISCRETES) #########################
######################################## EN DONNES CONTINUES, CES NOUVELLES DONNES ###############################
######################################## SERONT UTILISES POUR TROUVER LE BON NOMBRE DE ###########################
######################################## CLUSTERS : HCPC (HIERARCHICAL CLASSI ON PRINCIPAL COMPONANT) ############                                ##############################
##################################################################################################################

source("Presence_Absence_matrix.R")
library(cluster)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(vegan)
library(fossil)
source("IllustrationCluster.R")

## Contrairement à une PCA ou les valeurs initiales, quantitatives et continues sont utilisées pour réduire le nombre
## de variable, ici pour un MDS/PCoA il s'agit d'abord de transformer les données binaires en données de dissimilarités.

MatP.A.Faune3.AFR <- Presence_Absence_matrix(Faune3.AFRICA, type = "Species", singletons = FALSE, min5 = 2)

#Faune 3
Dist.Faune3.AFR <- vegdist(MatP.A.Faune3.AFR, method = "bray")
mds.Faune3.AFR <- cmdscale(Dist.Faune3.AFR, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3.AFR <- HCPC(as.data.frame(mds.Faune3.AFR$points))
hcpc.Faune3.AFR
x11()
IllustrationCluster(hcpc.Faune3.AFR)

#SIMPSON
MatP.A.Faune3.AFR_Inv <- t(MatP.A.Faune3.AFR)
Dist.Faune3.AFR <- ecol.dist(MatP.A.Faune3.AFR_Inv, method = simpson, type = "dis")
mds.Faune3.AFR <- cmdscale(Dist.Faune3.AFR, eig = TRUE, x.ret = TRUE)

hcpc.Faune3.AFR <- HCPC(as.data.frame(mds.Faune3.AFR$points))
hcpc.Faune3.AFR
x11()
IllustrationCluster(hcpc.Faune3.AFR, etik = TRUE)

#RAUP & CRICK
Dist.Faune3.AFRICA <- raupcrick(MatP.A.Faune3.AFR)
mds.Faune3.AFRICA.RC <- cmdscale(Dist.Faune3.AFRICA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune3.AFRICA.RC <- HCPC(as.data.frame(mds.Faune3.AFRICA.RC$point))
x11()
IllustrationCluster(hcpc.Faune3.AFRICA.RC, etik = TRUE, cexEtik = 0.1)

## VERSION B : EXCLUSIVE

#Faune 3B
Dist.Faune3B.AFR <- vegdist(MatP.A.Faune3B.AFR, method = "bray")
mds.Faune3B.AFR <- cmdscale(Dist.Faune3B.AFR, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3B.AFR <- HCPC(as.data.frame(mds.Faune3B.AFR$points)) #RESULTATS SIMILAIRES AVEC BDD INCLUSIFS
hcpc.Faune3B.AFR
x11()
IllustrationCluster(hcpc.Faune3B.AFR)

#SIMPSON
MatP.A.Faune3B.AFR_Inv <- t(MatP.A.Faune3B.AFR)
Dist.Faune3B.AFR <- ecol.dist(MatP.A.Faune3B.AFR_Inv, method = simpson, type = "dis")
mds.Faune3B.AFR <- cmdscale(Dist.Faune3B.AFR, eig = TRUE, x.ret = TRUE)

hcpc.Faune3B.AFR <- HCPC(as.data.frame(mds.Faune3B.AFR$points)) #RESULTATS SIMILAIRES AVEC BDD INCLUSIFS
hcpc.Faune3B.AFR
x11()
IllustrationCluster(hcpc.Faune3B.AFR)

#RAUP & CRICK
Dist.Faune3B.AFRICA <- raupcrick(MatP.A.Faune3B.AFRICA)
mds.Faune3B.AFRICA.RC <- cmdscale(Dist.Faune3B.AFRICA, eig = TRUE, x.ret= TRUE, k =3)

hcpc.Faune3B.AFRICA.RC <- HCPC(as.data.frame(mds.Faune3B.AFRICA.RC$point))
x11()
IllustrationCluster(hcpc.Faune3B.AFRICA.RC)

##################################################################################################################
#################################### VISUALISATION DES CLUSTERS DANS L'ESPACE ####################################
##################################################################################################################
library(RColorBrewer)

Plot.AFR <- data.frame()
LAT <- strsplit(dimnames(hcpc.Faune3B.AFR$data.clust)[[1]], "_")

Lat.AFR <- c()
Long.AFR <- c()
for(i in 1:length(LAT))
{
  Lat.AFR <- c(Lat.AFR, LAT[[i]][1])
  Long.AFR <- c(Long.AFR, LAT[[i]][2])
}
Lat.AFR <- as.numeric(Lat.AFR)
Long.AFR <- as.numeric(Long.AFR)
Color_C <- c()
for(i in 1:length(hcpc.Faune3B.AFR$data.clust$clust))
{
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 1){Color_C <- c(Color_C, "brown1")}
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 2){Color_C <- c(Color_C, "cyan")}
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 3){Color_C <- c(Color_C, "chartreuse3")}
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 4){Color_C <- c(Color_C, "black")}
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 5){Color_C <- c(Color_C, "darkorange")}
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 6){Color_C <- c(Color_C, "pink")}
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 7){Color_C <- c(Color_C, "darkorchid")}
  if(hcpc.Faune3B.AFR$data.clust$clust[i] == 8){Color_C <- c(Color_C, "yellow")}
  
}

PlotAFRIQUE <- data.frame(Lat.AFR, Long.AFR, Color_C)
PlotAFRIQUE$Color_C <- as.character(PlotAFRIQUE$Color_C)
plot(PlotAFRIQUE$Long.AFR, PlotAFRIQUE$Lat.AFR, col = PlotAFRIQUE$Color_C , pch = 16)

##### Extraires HCPC #####
hcpc.Faune3B.AFR$data.clust$clust
dimnames(hcpc.Faune3B.AFR$data.clust)[[1]]
Color_Clusters <- data.frame(hcpc.Faune3B.AFR$data.clust$clust, dimnames(hcpc.Faune3B.AFR$data.clust)[[1]])
unique(hcpc.Faune3B.AFR$data.clust$clust)

##############################################################################################################
############################################## ANCIEN MONDE ##################################################
##############################################################################################################
source("IllustrationCluster.R")

#Tester avec un NMDS (travail avec des rangs)
Dist.Faune1.FULLW <- vegdist(MatP.A.Faune1.FULLW, method = "bray")
mds.Faune1.FULLW <- cmdscale(Dist.Faune1.FULLW, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune1.FULLW <- HCPC(as.data.frame(mds.Faune1.FULLW$points)) # La
hcpc.Faune1.FULLW

IllustrationCluster(hcpc.Faune1.FULLW)

#SIMPSON
MatP.A.Faune1.FULLW_Inv <- t(MatP.A.Faune1.FULLW)
Dist.Faune1.FULLW <- ecol.dist(MatP.A.Faune1.FULLW_Inv, method = simpson, type = "dis")
mds.Faune1.FULLW <- cmdscale(Dist.Faune1.FULLW, eig = TRUE, x.ret = TRUE)

hcpc.Faune1.FULLW <- HCPC(as.data.frame(mds.Faune1.FULLW$points)) 
hcpc.Faune1.FULLW

IllustrationCluster(hcpc.Faune1.FULLW)

#Faune 2
Dist.Faune2.FULLW <- vegdist(MatP.A.Faune2.FULLW, method = "bray")
mds.Faune2.FULLW <- cmdscale(Dist.Faune2.FULLW, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune2.FULLW <- HCPC(as.data.frame(mds.Faune2.FULLW$points))
hcpc.Faune2.FULLW

IllustrationCluster(hcpc.Faune2.FULLW)

#SIMPSON
MatP.A.Faune2.FULLW_Inv <- t(MatP.A.Faune2.FULLW)
Dist.Faune2.FULLW <- ecol.dist(MatP.A.Faune2.FULLW_Inv, method = simpson, type = "dis")
mds.Faune2.FULLW <- cmdscale(Dist.Faune2.FULLW, eig = TRUE, x.ret = TRUE)

hcpc.Faune2.FULLW <- HCPC(as.data.frame(mds.Faune2.FULLW$points))
hcpc.Faune2.FULLW

IllustrationCluster(hcpc.Faune2.FULLW)

#Faune 3
Dist.Faune3.FULLW <- vegdist(MatP.A.Faune3.FULLW, method = "bray")
mds.Faune3.FULLW <- cmdscale(Dist.Faune3.FULLW, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3.FULLW <- HCPC(as.data.frame(mds.Faune3.FULLW$points))
hcpc.Faune3.FULLW

IllustrationCluster(hcpc.Faune3.FULLW)

#SIMPSON
MatP.A.Faune3.FULLW_Inv <- t(MatP.A.Faune3.FULLW)
Dist.Faune3.FULLW <- ecol.dist(MatP.A.Faune3.FULLW_Inv, method = simpson, type = "dis")
mds.Faune3.FULLW <- cmdscale(Dist.Faune3.FULLW, eig = TRUE, x.ret = TRUE)

hcpc.Faune3.FULLW <- HCPC(as.data.frame(mds.Faune3.FULLW$points))
hcpc.Faune3.FULLW

IllustrationCluster(hcpc.Faune3.FULLW)

## VERSION B : EXCLUSIVE
#Faune 1B
Dist.Faune1B.FULLW <- vegdist(MatP.A.Faune1B.FULLW, method = "bray")
mds.Faune1B.FULLW <- cmdscale(Dist.Faune1B.FULLW, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune1B.FULLW <- HCPC(as.data.frame(mds.Faune1B.FULLW$points)) 
hcpc.Faune1B.FULLW

IllustrationCluster(hcpc.Faune1B.FULLW)

#SIMPSON
MatP.A.Faune1B.FULLW_Inv <- t(MatP.A.Faune1B.FULLW)
Dist.Faune1B.FULLW <- ecol.dist(MatP.A.Faune1B.FULLW_Inv, method = simpson, type = "dis")
mds.Faune1B.FULLW <- cmdscale(Dist.Faune1B.FULLW, eig = TRUE, x.ret = TRUE)

hcpc.Faune1B.FULLW <- HCPC(as.data.frame(mds.Faune1B.FULLW$points)) 
hcpc.Faune1B.FULLW

IllustrationCluster(hcpc.Faune1B.FULLW)

#Faune 2B
Dist.Faune2B.FULLW <- vegdist(MatP.A.Faune2B.FULLW, method = "bray")
mds.Faune2B.FULLW <- cmdscale(Dist.Faune2B.FULLW, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune2B.FULLW <- HCPC(as.data.frame(mds.Faune2B.FULLW$points)) 
hcpc.Faune2B.FULLW

IllustrationCluster(hcpc.Faune2B.FULLW)

#SIMPSON
MatP.A.Faune2B.FULLW_Inv <- t(MatP.A.Faune2B.FULLW)
Dist.Faune2B.FULLW <- ecol.dist(MatP.A.Faune2B.FULLW_Inv, method = simpson, type = "dis")
mds.Faune2B.FULLW <- cmdscale(Dist.Faune2B.FULLW, eig = TRUE, x.ret = TRUE)

hcpc.Faune2B.FULLW <- HCPC(as.data.frame(mds.Faune2B.FULLW$points)) 
hcpc.Faune2B.FULLW

IllustrationCluster(hcpc.Faune2B.FULLW)

#Faune 3B
Dist.Faune3B.FULLW <- vegdist(MatP.A.Faune3B.FULLW, method = "bray")
mds.Faune3B.FULLW <- cmdscale(Dist.Faune3B.FULLW, eig = TRUE, x.ret= TRUE, k =2)

hcpc.Faune3B.FULLW <- HCPC(as.data.frame(mds.Faune3B.FULLW$points)) 
hcpc.Faune3B.FULLW

IllustrationCluster(hcpc.Faune3B.FULLW)

#SIMPSON
MatP.A.Faune3B.FULLW_Inv <- t(MatP.A.Faune3B.FULLW)
Dist.Faune3B.FULLW <- ecol.dist(MatP.A.Faune3B.FULLW_Inv, method = simpson, type = "dis")
mds.Faune3B.FULLW <- cmdscale(Dist.Faune3B.FULLW, eig = TRUE, x.ret = TRUE)

hcpc.Faune3B.FULLW <- HCPC(as.data.frame(mds.Faune3B.FULLW$points)) 
hcpc.Faune3B.FULLW

IllustrationCluster(hcpc.Faune3B.FULLW)

##################################################################################################################
#################################### VISUALISATION DES CLUSTERS DANS L'ESPACE ####################################
##################################################################################################################
library(RColorBrewer)

Plot.FULLW <- data.frame()
LAT <- strsplit(dimnames(hcpc.Faune3B.FULLW$data.clust)[[1]], "_")

Lat.FULLW <- c()
Long.FULLW <- c()
for(i in 1:length(LAT))
{
  Lat.FULLW <- c(Lat.FULLW, LAT[[i]][1])
  Long.FULLW <- c(Long.FULLW, LAT[[i]][2])
}
Lat.FULLW <- as.numeric(Lat.FULLW)
Long.FULLW <- as.numeric(Long.FULLW)
Color_C <- c()
for(i in 1:length(hcpc.Faune3B.FULLW$data.clust$clust))
{
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 1){Color_C <- c(Color_C, "brown2")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 2){Color_C <- c(Color_C, "cyan")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 3){Color_C <- c(Color_C, "chartreuse3")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 4){Color_C <- c(Color_C, "black")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 5){Color_C <- c(Color_C, "darkorange")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 6){Color_C <- c(Color_C, "pink")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 7){Color_C <- c(Color_C, "darkorchid")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 8){Color_C <- c(Color_C, "yellow")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 9){Color_C <- c(Color_C, "blanchedalmond")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 10){Color_C <- c(Color_C, "darkcyan")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 11){Color_C <- c(Color_C, "deeppink")}
  if(hcpc.Faune3B.FULLW$data.clust$clust[i] == 12){Color_C <- c(Color_C, "darkseagreen1")}
}

PlotAFRIQUE <- data.frame(Lat.FULLW, Long.FULLW, Color_C)
PlotAFRIQUE$Color_C <- as.character(PlotAFRIQUE$Color_C)
plot(PlotAFRIQUE$Long.FULLW, PlotAFRIQUE$Lat.FULLW, col = PlotAFRIQUE$Color_C , pch = 16, xlim = c(-20,150), ylim = c(-40,60))
