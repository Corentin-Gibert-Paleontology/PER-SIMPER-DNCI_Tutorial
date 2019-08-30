################################################################################################
################################################################################################
########### THIS ANALYSIS IS THE LAST PER-SIMPER ANALYSIS ######################################
###################### of OLD NOW and PBDB and Poitiers ########################################
################################### BDD ########################################################
################################################################################################
############ CLUSTERS ARE BUILD FROM UPGMA & PerSIMPER & PolyCohorte.R #########################
######################### PARAMETERS ARE : 0.1 grain (~11 km²) #################################
######################### Localities with only one species are removed #########################
######################### Singleton are excluded | Raup & Crick index ##########################
########################## Singleton are included inside PER-SIMPER   ##########################
################################################################################################


setwd("C:/Users/cgibert01/Desktop/Biblio post doc Poitiers/Data téléchargés")
source("IllustrationCluster.R")
source("PERSIMPER_multigroups.R")
source("Presence_Absence_matrix.R")
source("PERSIMPER.R")
source("FunctionPairsSimper.R")
source("Tri_SIMPER.R")
source("delta_ses_Boris.R")
source("BOOT_per.R")

library(cluster)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(vegan)
library(fossil)

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

#Pour un grain de 0.1 degres
Grain01.EU.LAT <- seq(from = Min_LAT.EU, to = Max_LAT.EU, 0.1)
Grain01.EU.LNG <- seq(from = Min_LNG.EU, to = Max_LNG.EU, 0.1)

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

for(i in 1:length(Liste.GRAIN01.EU))
{
  titre <- paste(round(Liste.GRAIN01.EU[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN01.EU[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN01.EU[[i]]))
  {
    Liste.GRAIN01.EU[[i]][j]$NAME <- titre
  }
}

FULLeuropeGRAIN01 <- data.frame()
for(i in 1:length(Liste.GRAIN01.EU))
{
  FULLeuropeGRAIN01 <- join(FULLeuropeGRAIN01, Liste.GRAIN01.EU[[i]], match="all", type="full")  
}

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


### On va utiliser non plus les longitudes et latitudes, mais les "nom" regroupés des localités.
LongLat <- strsplit(EU.Grain1.terre$NAME, "_")

Lat.EU <- c()
Long.EU <- c()
for(i in 1:length(LongLat))
{
  Lat.EU <- c(Lat.EU, LongLat[[i]][1])
  Long.EU <- c(Long.EU, LongLat[[i]][2])
}
Lat.EU <- as.numeric(Lat.EU)
Long.EU <- as.numeric(Long.EU)

EU.Grain1.terre <- cbind(EU.Grain1.terre, Lat.EU, Long.EU)

# Verification de la structure de la liste faunique

head(EU.Grain1.terre)

### PAR MN, AVEC WHICH : 
AGE_MN <- c(0, 1.95, 2.6, 3.4, 4.2, 5.3, 7.1, 8.2, 9, 9.5, 11.2, 12.5, 15.2, 17, 18, 20, 22.8, 23.8)
NOM_MN <- c("MN18","MN17","MN16","MN15","MN14","MN13","MN12","MN11","MN10","MN9","MN7_8","MN6","MN5","MN4","MN3","MN2","MN1")

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
    names.dataframe.eu <- paste(NOM_MN[i])
    N <- assign(names.dataframe.eu, EU.Grain1.terre[wantMN,])
    Liste.MN.EU[[length(Liste.MN.EU)+ 1]] <- N
  }
}

Faune1_MN <- list(MN1 = MN1, MN2 = MN2, MN3 = MN3, MN4 = MN4, MN5 = MN5)
SortiePer_SIMPER_EU1 <- list()

for(i in 1:length(Faune1_MN)){

## FAUNE 1 ##

EST <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.EU > 34 & Faune1_MN[[i]]$Lat.EU < 50 & Faune1_MN[[i]]$Long.EU > 22.5 & Faune1_MN[[i]]$Long.EU < 41)
NORD1 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.EU > 46 & Faune1_MN[[i]]$Lat.EU < 52 & Faune1_MN[[i]]$Long.EU > 5.5 & Faune1_MN[[i]]$Long.EU < 22.4)
NORD2 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.EU > 43 & Faune1_MN[[i]]$Lat.EU < 45 & Faune1_MN[[i]]$Long.EU > 20 & Faune1_MN[[i]]$Long.EU < 22)
NORD <- rbind(NORD1, NORD2)
NORD_OUEST <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.EU > 43 & Faune1_MN[[i]]$Lat.EU < 49 & Faune1_MN[[i]]$Long.EU > -1.5 & Faune1_MN[[i]]$Long.EU < 2.3)
SUD_OUEST1 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.EU > 39 & Faune1_MN[[i]]$Lat.EU < 42.8 & Faune1_MN[[i]]$Long.EU > -5 & Faune1_MN[[i]]$Long.EU < 9.5)
SUD_OUEST2 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.EU > 42.7 & Faune1_MN[[i]]$Lat.EU < 47 & Faune1_MN[[i]]$Long.EU > 2 & Faune1_MN[[i]]$Long.EU < 6.35)
SUD_OUEST <- rbind(SUD_OUEST1, SUD_OUEST2)

FullFauna <- list(EST = EST, NORD = NORD, NORD_OUEST = NORD_OUEST, SUD_OUEST = SUD_OUEST)

setwd("C:/Users/cgibert01/Desktop/PostDoc/Resultat Aout")
SortiePer_SIMPER_EU1[[i]] <- Tri_SIMPER(FullFauna, Info = paste("EU" , names(Faune1_MN[i])), LimSIMP = 4)
}

## FAUNE 2

Faune2_MN <- list(MN6 = MN6, MN7_8 = MN7_8, MN9 = MN9, MN10 = MN10, MN11 = MN11, MN12 = MN12, MN13 = MN13)
SortiePer_SIMPER_EU2 <- list()

for(i in 1:length(Faune2_MN)){
  
SUD_EST <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.EU > 36 & Faune2_MN[[i]]$Lat.EU < 43 & Faune2_MN[[i]]$Long.EU > 21 & Faune2_MN[[i]]$Long.EU < 38)  
want <- which(SUD_EST$NAME == "37.7_21.4" | SUD_EST$NAME == "40.2_22" | SUD_EST$NAME == "40.4_21.8" |
    SUD_EST$NAME == "40.5_21.8" | SUD_EST$NAME == "40.5_21.7" | SUD_EST$NAME == "40.5_21.3")
if(length(want) > 0)
{
SUD_EST <- SUD_EST[-want,]
}
NORD_EST <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.EU > 45 & Faune2_MN[[i]]$Lat.EU < 49.2 & Faune2_MN[[i]]$Long.EU > 26 & Faune2_MN[[i]]$Long.EU < 37)  
MED1 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.EU > 38 & Faune2_MN[[i]]$Lat.EU < 45.1 & Faune2_MN[[i]]$Long.EU > 7 & Faune2_MN[[i]]$Long.EU < 17)
MED2 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.EU > 36 & Faune2_MN[[i]]$Lat.EU < 42.5 & Faune2_MN[[i]]$Long.EU > -3.5 & Faune2_MN[[i]]$Long.EU < -0.35)
MED3 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$NAME == "40_3.8" | Faune2_MN[[i]]$NAME == "43.7_5.4" |
   Faune2_MN[[i]]$NAME == "43.6_5.3" | Faune2_MN[[i]]$NAME == "43.6_5.3" | Faune2_MN[[i]]$NAME == "45_6.2" |
   Faune2_MN[[i]]$NAME == "44.4_5" | Faune2_MN[[i]]$NAME == "44.3_5" | Faune2_MN[[i]]$NAME == "43.8_4.4" |
   Faune2_MN[[i]]$NAME == "43.6_3.8" | Faune2_MN[[i]]$NAME == "43.4_3.7" | Faune2_MN[[i]]$NAME == "42.7_2.9" |
   Faune2_MN[[i]]$NAME == "42.6_2.9" | Faune2_MN[[i]]$NAME == "42.2_2.7" | Faune2_MN[[i]]$NAME == "42_2.5" |
   Faune2_MN[[i]]$NAME == "37.7_21.4" | Faune2_MN[[i]]$NAME == "40.2_22" | Faune2_MN[[i]]$NAME == "40.4_21.8" |
   Faune2_MN[[i]]$NAME == "40.5_21.8" | Faune2_MN[[i]]$NAME == "40.5_21.7" | Faune2_MN[[i]]$NAME == "40.5_21.3")
MED <- rbind(MED1, MED2, MED3)
NORD <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.EU > 45.3 & Faune2_MN[[i]]$Lat.EU < 52 & Faune2_MN[[i]]$Long.EU > 4 & Faune2_MN[[i]]$Long.EU < 23)  
 
FullFauna <- list(SUD_EST = SUD_EST, NORD = NORD, NORD_EST = NORD_EST, MED = MED)

setwd("C:/Users/cgibert01/Desktop/PostDoc/Resultat Aout")
SortiePer_SIMPER_EU2[[i]] <- Tri_SIMPER(FullFauna, Info = paste("EU" , names(Faune2_MN[i])), LimSIMP = 4)
}

## FAUNE 3

Faune3_MN <- list(MN14 = MN14, MN15 = MN15, MN16 = MN16, MN17 = MN17, MN18 = MN18)
SortiePer_SIMPER_EU3 <- list()

for(i in 1:length(Faune3_MN)){
NORD_EST <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.EU > 47.8 & Faune3_MN[[i]]$Lat.EU < 55.5 & Faune3_MN[[i]]$Long.EU > 15.5 & Faune3_MN[[i]]$Long.EU < 36)
SUD1 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.EU > 34 & Faune3_MN[[i]]$Lat.EU < 47.75 & Faune3_MN[[i]]$Long.EU > 14.5 & Faune3_MN[[i]]$Long.EU < 37)
SUD2 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.EU > 36.2 & Faune3_MN[[i]]$Lat.EU < 40.41 & Faune3_MN[[i]]$Long.EU > -6.5 & Faune3_MN[[i]]$Long.EU < 4)
SUD3 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.EU > 34 & Faune3_MN[[i]]$Lat.EU < 45.1 & Faune3_MN[[i]]$Long.EU > 1.85 & Faune3_MN[[i]]$Long.EU < 19)
SUD4 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$NAME == "40.5_-1.1" | Faune3_MN[[i]]$NAME == "40.5_-1" | Faune3_MN[[i]]$NAME == "40.6_-1" |
  Faune3_MN[[i]]$NAME == "41_-2.2" | Faune3_MN[[i]]$NAME == "41.1_-2.3" | Faune3_MN[[i]]$NAME == "40.4_-2.6" | 
  Faune3_MN[[i]]$NAME == "40.5_-2.6"  | Faune3_MN[[i]]$NAME == "40.5_-2.7" | Faune3_MN[[i]]$NAME == "45.5_4.5" |
  Faune3_MN[[i]]$NAME == "45.4_4.8" | Faune3_MN[[i]]$NAME == "45.2_4.8" | Faune3_MN[[i]]$NAME == "45.9_4.7" | Faune3_MN[[i]]$NAME == "45.9_4.8")
SUD <- rbind(SUD1, SUD2, SUD3, SUD4)
NORD_OUEST1 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.EU > 47.8 & Faune3_MN[[i]]$Lat.EU < 57 & Faune3_MN[[i]]$Long.EU > -7 & Faune3_MN[[i]]$Long.EU < 15)
NORD_OUEST2 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.EU > 44.2 & Faune3_MN[[i]]$Lat.EU < 46.4 & Faune3_MN[[i]]$Long.EU > -7 & Faune3_MN[[i]]$Long.EU < 2.6)
NORD_OUEST3 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.EU > 40.8 & Faune3_MN[[i]]$Lat.EU < 43.3 & Faune3_MN[[i]]$Long.EU > -7 & Faune3_MN[[i]]$Long.EU < 0.7)
NORD_OUEST4 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$NAME == "40.8_-4.4" | Faune3_MN[[i]]$NAME == "40.5_-3.8" | Faune3_MN[[i]]$NAME == "41.2_-2.5")
NORD_OUEST <- rbind(NORD_OUEST1, NORD_OUEST2, NORD_OUEST3, NORD_OUEST4)
want <- which(SUD$NAME %in% NORD_OUEST$NAME)
if(length(want) > 0){
SUD <- SUD[-want,]
}

FullFauna <- list(NORD_EST = NORD_EST, SUD = SUD, NORD_OUEST = NORD_OUEST)

setwd("C:/Users/cgibert01/Desktop/PostDoc/Resultat Aout")
SortiePer_SIMPER_EU3[[i]] <- Tri_SIMPER(FullFauna, Info = paste("EU" , names(Faune3_MN[i])), LimSIMP = 4)
}

##################################################################################################################
################################################## ASIE ##########################################################
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

wantTime <- which((FULLasia$MAX_AGE - FULLasia$MIN_AGE) > 5)
FULLasia <- FULLasia[-wantTime,]
if(length(which((FULLasia$MAX_AGE - FULLasia$MIN_AGE) > 5)) == 0)
{
  print("OK")
}

#Maintenant il faut le faire par taille de grain.
Min_LAT.ASIA <- floor(min(FULLasia$LAT, na.rm = TRUE))
Max_LAT.ASIA <- ceiling(max(FULLasia$LAT, na.rm = TRUE))
Min_LNG.ASIA <- floor(min(FULLasia$LONG, na.rm = TRUE))
Max_LNG.ASIA <- ceiling(max(FULLasia$LONG, na.rm = TRUE))

#Pour un grain de 0.1 degres
Grain01.ASIA.LAT <- seq(from = Min_LAT.ASIA, to = Max_LAT.ASIA, 0.1)
Grain01.ASIA.LNG <- seq(from = Min_LNG.ASIA, to = Max_LNG.ASIA, 0.1)

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

for(i in 1:length(Liste.GRAIN01.ASIA))
{
  titre <- paste(round(Liste.GRAIN01.ASIA[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN01.ASIA[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN01.ASIA[[i]]))
  {
    Liste.GRAIN01.ASIA[[i]][j]$NAME <- titre
  }
}

FULLasiaGRAIN01 <- data.frame()
for(i in 1:length(Liste.GRAIN01.ASIA))
{
  FULLasiaGRAIN01 <- join(FULLasiaGRAIN01, Liste.GRAIN01.ASIA[[i]], match="all", type="full")  
}

ISNA <- is.na(FULLasiaGRAIN01)
summary(ISNA) #AUCUN NA

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

### On va utiliser non plus les longitudes et latitudes, mais les "nom" regroupés des localités.
LongLat <- strsplit(ASIA.Grain1.terre$NAME, "_")

Lat.ASI <- c()
Long.ASI <- c()
for(i in 1:length(LongLat))
{
  Lat.ASI <- c(Lat.ASI, LongLat[[i]][1])
  Long.ASI <- c(Long.ASI, LongLat[[i]][2])
}
Lat.ASI <- as.numeric(Lat.ASI)
Long.ASI <- as.numeric(Long.ASI)

ASIA.Grain1.terre <- cbind(ASIA.Grain1.terre, Lat.ASI, Long.ASI)

# Verification de la structure de la liste faunique

head(ASIA.Grain1.terre)

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
    names.dataframe.ASIA <- paste(NOM_MN[i])
    N <- assign(names.dataframe.ASIA, ASIA.Grain1.terre[wantMN,])
    Liste.MN.ASIA[[length(Liste.MN.ASIA)+ 1]] <- N
  }
}

## FAUNE 1 ##

### PAS DE MN1 ni MN2

Faune1_MN <- list(MN1 = MN1, MN2 = MN2, MN3 = MN3, MN4 = MN4, MN5 = MN5)
SortiePer_SIMPER_ASI1 <- list()

for(i in 3:length(Faune1_MN)){
EST_CHINE1 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.ASI > 31 & Faune1_MN[[i]]$Lat.ASI < 38 & Faune1_MN[[i]]$Long.ASI > 108 & Faune1_MN[[i]]$Long.ASI < 120)  
EST_CHINE2 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.ASI > 18 & Faune1_MN[[i]]$Lat.ASI < 24 & Faune1_MN[[i]]$Long.ASI > 99.2 & Faune1_MN[[i]]$Long.ASI < 105)  
EST_CHINE <- rbind(EST_CHINE1, EST_CHINE2)
TURQUIE <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.ASI > 36 & Faune1_MN[[i]]$Lat.ASI < 45 & Faune1_MN[[i]]$Long.ASI > 26 & Faune1_MN[[i]]$Long.ASI < 36)
NORD_CHINE1 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.ASI > 42.5 & Faune1_MN[[i]]$Lat.ASI < 47.5 & Faune1_MN[[i]]$Long.ASI > 84 & Faune1_MN[[i]]$Long.ASI < 115)
NORD_CHINE2 <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.ASI > 35 & Faune1_MN[[i]]$Lat.ASI < 39 & Faune1_MN[[i]]$Long.ASI > 101 & Faune1_MN[[i]]$Long.ASI < 106.2)
NORD_CHINE <- rbind(NORD_CHINE1, NORD_CHINE2)
INDE <- subset(Faune1_MN[[i]], Faune1_MN[[i]]$Lat.ASI > 23 & Faune1_MN[[i]]$Lat.ASI < 34 & Faune1_MN[[i]]$Long.ASI > 67 & Faune1_MN[[i]]$Long.ASI < 84)

FullFauna <- list(EST_CHINE = EST_CHINE, TURQUIE = TURQUIE, NORD_CHINE = NORD_CHINE, INDE = INDE)

setwd("C:/Users/cgibert01/Desktop/PostDoc/Resultat Aout")
SortiePer_SIMPER_ASI1[[i]] <- Tri_SIMPER(FullFauna, Info = paste("ASI", names(Faune1_MN[i])), LimSIMP = 4)
}

## FAUNE 2 ##

Faune2_MN <- list(MN6 = MN6, MN7_8 = MN7_8, MN9 = MN9, MN10 = MN10, MN11 = MN11, MN12 = MN12, MN13 = MN13)
SortiePer_SIMPER_ASI2 <- list()

for(i in 1:length(Faune2_MN)){
TURQUIE <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.ASI > 36 & Faune2_MN[[i]]$Lat.ASI < 45 & Faune2_MN[[i]]$Long.ASI > 25.5 & Faune2_MN[[i]]$Long.ASI < 45.1)
INDE1 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.ASI > 20 & Faune2_MN[[i]]$Lat.ASI < 38.4 & Faune2_MN[[i]]$Long.ASI > 52 & Faune2_MN[[i]]$Long.ASI < 94)
INDE2 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$NAME == "25.8_101.7" | Faune2_MN[[i]]$NAME == "25.7_101.9" | Faune2_MN[[i]]$NAME == "25.3_102.4" |
  Faune2_MN[[i]]$NAME == "25_102.1" | Faune2_MN[[i]]$NAME == "18.9_100.2" | Faune2_MN[[i]]$NAME == "23.6_103.2")
INDE <- rbind(INDE1, INDE2)
EST_CHINE1 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.ASI > 30 & Faune2_MN[[i]]$Lat.ASI < 40.05 & Faune2_MN[[i]]$Long.ASI > 101 & Faune2_MN[[i]]$Long.ASI < 120)
EST_CHINE2 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.ASI > 40.05 & Faune2_MN[[i]]$Lat.ASI < 53.5 & Faune2_MN[[i]]$Long.ASI > 111 & Faune2_MN[[i]]$Long.ASI < 120)
EST_CHINE <- rbind(EST_CHINE1, EST_CHINE2)
NORD1 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$Lat.ASI > 38.5 & Faune2_MN[[i]]$Lat.ASI < 52.1 & Faune2_MN[[i]]$Long.ASI > 45.1 & Faune2_MN[[i]]$Long.ASI < 97.3)
NORD2 <- subset(Faune2_MN[[i]], Faune2_MN[[i]]$NAME == "37.1_97.2" | Faune2_MN[[i]]$NAME == "37_97")
NORD <- rbind(NORD1, NORD2)
#want <- which(INDE$NAME %in% NORD$NAME)
#want

#NORD_OUEST <- NORD_OUEST[-want,]

FullFauna <- list(EST_CHINE = EST_CHINE, TURQUIE = TURQUIE, NORD = NORD, INDE = INDE)
  
setwd("C:/Users/cgibert01/Desktop/PostDoc/Resultat Aout")
SortiePer_SIMPER_ASI2[[i]] <- Tri_SIMPER(FullFauna, Info = paste("ASI",names(Faune2_MN[i])), LimSIMP = 4)
}

## FAUNE 3 ##

Faune3_MN <- list(MN14 = MN14, MN15 = MN15, MN16 = MN16, MN17 = MN17, MN18 = MN18)
SortiePer_SIMPER_ASI3 <- list()

for(i in 1:length(Faune3_MN)){
TURQUIE1 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > 31 & Faune3_MN[[i]]$Lat.ASI < 42 & Faune3_MN[[i]]$Long.ASI > 27 & Faune3_MN[[i]]$Long.ASI < 38.1)
TURQUIE2 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > 22 & Faune3_MN[[i]]$Lat.ASI < 29 & Faune3_MN[[i]]$Long.ASI > 38.5 & Faune3_MN[[i]]$Long.ASI < 54)
TURQUIE3 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$NAME == "37.1_42.2" | Faune3_MN[[i]]$NAME == "32.4_51.5")
TURQUIE <- rbind(TURQUIE1, TURQUIE2, TURQUIE3)
NORD <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > 37.1 & Faune3_MN[[i]]$Lat.ASI < 60 & Faune3_MN[[i]]$Long.ASI > 38.85 & Faune3_MN[[i]]$Long.ASI < 80.3)
INDE1 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > 6 & Faune3_MN[[i]]$Lat.ASI < 33.75 & Faune3_MN[[i]]$Long.ASI > 72 & Faune3_MN[[i]]$Long.ASI < 88.1)
INDE2 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > -9 & Faune3_MN[[i]]$Lat.ASI < 10 & Faune3_MN[[i]]$Long.ASI > 98 & Faune3_MN[[i]]$Long.ASI < 121)
INDE <- rbind(INDE1, INDE2)
SUD_EST1 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > 22.1 & Faune3_MN[[i]]$Lat.ASI < 30.85 & Faune3_MN[[i]]$Long.ASI > 98 & Faune3_MN[[i]]$Long.ASI < 121)
SUD_EST2 <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > 10.5 & Faune3_MN[[i]]$Lat.ASI < 22.01 & Faune3_MN[[i]]$Long.ASI > 98 & Faune3_MN[[i]]$Long.ASI < 115)
SUD_EST <- rbind(SUD_EST1, SUD_EST2)
NORD_EST <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.ASI > 31 & Faune3_MN[[i]]$Lat.ASI < 60 & Faune3_MN[[i]]$Long.ASI > 83 & Faune3_MN[[i]]$Long.ASI < 120)
#want <- which(SUD_EST$NAME %in% INDE$NAME)
#want
#NORD_OUEST <- NORD_OUEST[-want,]
  
FullFauna <- list(TURQUIE = TURQUIE, NORD = NORD, INDE = INDE, SUD_EST = SUD_EST, NORD_EST = NORD_EST)
  
  setwd("C:/Users/cgibert01/Desktop/PostDoc/Resultat Aout")
  SortiePer_SIMPER_ASI3[[i]] <- Tri_SIMPER(FullFauna, Info = paste("ASI",names(Faune3_MN[i])), LimSIMP = 4)
}

##############################################################################################################
############################################## AFRICA ########################################################
##############################################################################################################

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


wantTime <- which((FULLAFRICA$MAX_AGE - FULLAFRICA$MIN_AGE) > 5)
FULLAFRICA <- FULLAFRICA[-wantTime,]
if(length(which((FULLAFRICA$MAX_AGE - FULLAFRICA$MIN_AGE) > 5)) == 0)
{
  print("OK")
}

#Maintenant il faut le faire par taille de grain.
Min_LAT.AFRICA <- floor(min(FULLAFRICA$LAT, na.rm = TRUE))
Max_LAT.AFRICA <- ceiling(max(FULLAFRICA$LAT, na.rm = TRUE))
Min_LNG.AFRICA <- floor(min(FULLAFRICA$LONG, na.rm = TRUE))
Max_LNG.AFRICA <- ceiling(max(FULLAFRICA$LONG, na.rm = TRUE))

#Pour un grain de 0.1 degres
Grain01.AFRICA.LAT <- seq(from = Min_LAT.AFRICA, to = Max_LAT.AFRICA, 0.1)
Grain01.AFRICA.LNG <- seq(from = Min_LNG.AFRICA, to = Max_LNG.AFRICA, 0.1)

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


for(i in 1:length(Liste.GRAIN01.AFRICA))
{
  titre <- paste(round(Liste.GRAIN01.AFRICA[[i]][1,]$LAT, digits = 1), round(Liste.GRAIN01.AFRICA[[i]][1,]$LONG, digits = 1), sep = "_")
  for(j in 1:length(Liste.GRAIN01.AFRICA[[i]]))
  {
    Liste.GRAIN01.AFRICA[[i]][j]$NAME <- titre
  }
}

FULLAFRICAGRAIN01 <- data.frame()
for(i in 1:length(Liste.GRAIN01.AFRICA))
{
  FULLAFRICAGRAIN01 <- join(FULLAFRICAGRAIN01, Liste.GRAIN01.AFRICA[[i]], match="all", type="full")  
}

ISNA <- is.na(FULLAFRICAGRAIN01)
summary(ISNA) #AUCUN NA

#Suppression des Cetaces et des chauves pouris
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

### On va utiliser non plus les longitudes et latitudes, mais les "nom" regroupés des localités.
LongLat <- strsplit(AFRICA.Grain1.terre$NAME, "_")

Lat.AFR <- c()
Long.AFR <- c()
for(i in 1:length(LongLat))
{
  Lat.AFR <- c(Lat.AFR, LongLat[[i]][1])
  Long.AFR <- c(Long.AFR, LongLat[[i]][2])
}
Lat.AFR <- as.numeric(Lat.AFR)
Long.AFR <- as.numeric(Long.AFR)

AFRICA.Grain1.terre <- cbind(AFRICA.Grain1.terre, Lat.AFR, Long.AFR)

# Verification de la structure de la liste faunique

head(AFRICA.Grain1.terre)


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
    names.dataframe.eu <- paste(NOM_MN[i])
    N <- assign(names.dataframe.eu, AFRICA.Grain1.terre[wantMN,])
    Liste.MN.AFRICA[[length(Liste.MN.AFRICA)+ 1]] <- N
  }
}

summary(Liste.MN.AFRICA[[1]])
str(Liste.MN.AFRICA[[1]])

## FAUNE 3 ##

Faune3_MN <- list(MN14 = MN14, MN15 = MN15, MN16 = MN16, MN17 = MN17, MN18 = MN18)
SortiePer_SIMPER_AFR3 <- list()

for(i in 1:length(Faune3_MN)){
SUD <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.AFR > -35 & Faune3_MN[[i]]$Lat.AFR < -11 & Faune3_MN[[i]]$Long.AFR > 15 & Faune3_MN[[i]]$Long.AFR < 33)
EST <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.AFR > -10.7 & Faune3_MN[[i]]$Lat.AFR < 12 & Faune3_MN[[i]]$Long.AFR > -0.5 & Faune3_MN[[i]]$Long.AFR < 44)
NORD_EST <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.AFR > 22 & Faune3_MN[[i]]$Lat.AFR < 34 & Faune3_MN[[i]]$Long.AFR > 20 & Faune3_MN[[i]]$Long.AFR < 34)
NORD <- subset(Faune3_MN[[i]], Faune3_MN[[i]]$Lat.AFR > 30 & Faune3_MN[[i]]$Lat.AFR < 38 & Faune3_MN[[i]]$Long.AFR > -10 & Faune3_MN[[i]]$Long.AFR < 12)

#FullFauna <- list(SUD = SUD, NORD_EST = NORD_EST, EST = EST)

FullFauna <- list(SUD = SUD, NORD_EST = NORD_EST, EST = EST, NORD = NORD)
  
setwd("C:/Users/cgibert01/Desktop/PostDoc/Resultat Aout")
SortiePer_SIMPER_AFR3[[i]] <- Tri_SIMPER(FullFauna, Info = paste("AFR",names(Faune3_MN[i])), LimSIMP = 4)
}


SortieFINALE <- list(EU1 = SortiePer_SIMPER_EU1, EU2= SortiePer_SIMPER_EU2, EU3 = SortiePer_SIMPER_EU3, ASI1 = SortiePer_SIMPER_ASI1, 
                     ASI2 = SortiePer_SIMPER_ASI2, ASI3 = SortiePer_SIMPER_ASI3, AFR3 = SortiePer_SIMPER_AFR3)

