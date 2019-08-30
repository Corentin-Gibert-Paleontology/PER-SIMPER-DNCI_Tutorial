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
    names.dataframe.eu <- paste(NOM_MN[i])
    N <- assign(names.dataframe.eu, EU.Grain1.terre[wantMN,])
    Liste.MN.EU[[length(Liste.MN.EU)+ 1]] <- N
  }
}

##### SORTIE LONG-LAT DES LOCALITE #####
Localite_Faune1Europe <- c(unique(MN1$NAME), unique(MN2$NAME), unique(MN3$NAME), unique(MN4$NAME), unique(MN5$NAME))
Localite_Faune1Europe <- sort(unique(Localite_Faune1Europe))

Localite_Faune2Europe <- c(unique(MN6$NAME), unique(MN7_8$NAME), unique(MN9$NAME), unique(MN10$NAME), unique(MN11$NAME),
                         unique(MN12$NAME), unique(MN13$NAME))
Localite_Faune2Europe <- sort(unique(Localite_Faune2Europe))

Localite_Faune3Europe <- c(unique(MN14$NAME), unique(MN15$NAME), unique(MN16$NAME), unique(MN17$NAME), unique(MN18$NAME))
Localite_Faune3Europe <- sort(unique(Localite_Faune3Europe))

LocaliteEurope <- list(Localite_Faune1Europe, Localite_Faune2Europe, Localite_Faune3Europe)
capture.output(LocaliteEurope, file = "LocaliteEurope.txt")

## FAUNE 1 ##
ClEU_SE <- subset(MN1, MN1$LONG > 8.1 & MN1$LONG < 33.6 & MN1$LAT > 37.1 & MN1$LAT < 42)
ClEU_E <- subset(MN1, MN1$LONG > 24 & MN1$LONG < 33.5 & MN1$LAT > 46.5 & MN1$LAT < 48.6)
ClEU_EAST <- rbind(ClEU_SE, ClEU_E)
ClEU_Nord <- subset(MN1, MN1$LONG > 6.4 & MN1$LONG < 23.5 & MN1$LAT > 46.4 & MN1$LAT < 51.3)
want <- which(ClEU_Nord$NAME == "46.5_6.7" | ClEU_Nord$NAME == "46.8_6.5" | ClEU_Nord$NAME == "47_7.1")  
ClEU_OuestA <- subset(MN1, MN1$LONG < 6.7 & MN1$LAT > 37 & MN1$LAT < 48.2)
ClEU_Ouest <- rbind(ClEU_Nord[want, ], ClEU_OuestA)
ClEU_Nord <- ClEU_Nord[-want,]
wantNW <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 2.2 & ClEU_Ouest$LAT > 46.8 & ClEU_Ouest$LAT < 50)
wantSWfr <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 1.1 & ClEU_Ouest$LAT > 43.2 & ClEU_Ouest$LAT < 44.8)
wantESP <- which(ClEU_Ouest$LONG > -9.5 & ClEU_Ouest$LONG < -2.8 & ClEU_Ouest$LAT > 40.1 & ClEU_Ouest$LAT < 42) #celui là n'a pas bcp de logique
ClEU_W_Ouest <- rbind(ClEU_Ouest[wantNW, ], ClEU_Ouest[wantSWfr, ], ClEU_Ouest[wantESP,])
ToDelet <- c(wantNW, wantSWfr, wantESP)
ClEU_E_Ouest <- ClEU_Ouest[-ToDelet,]

ClEU_MN1 <- rbind(ClEU_Nord, ClEU_Ouest)
Group_EU_MN1 <- c(rep(1,7), rep(2,11))
ClEUROPE_MN1 <- Presence_Absence_matrix(ClEU_MN1, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1EuropeMN1_FALSE <- delta.ses_boris(ClEUROPE_MN1, Group_EU_MN1)
dev.print(device = pdf, file = "MN1_EU.pdf", width = 10)

ClEUSW1 <- subset(MN1, MN1$LONG < 10 & MN1$LAT < 42)
ClEUW1 <- subset(MN1, MN1$LAT >= 42 & MN1$LAT < 47 & MN1$LONG < 8)
ClEUNW1 <- subset(MN1, MN1$LAT >= 47 & MN1$LONG < 23)
ClEUCN1 <- subset(MN1, MN1$LAT >= 44 & MN1$LONG >= 23 & MN1$LONG < 43)
ClEUGK1 <- subset(MN1, MN1$LAT < 42 & MN1$LONG >= 10)
<
ClEU_MN1 <- rbind(ClEUW1, ClEUNW1)
Group_EU_MN1 <- c(rep(1,8), rep(2,7))
ClEUROPE_MN1 <- Presence_Absence_matrix(ClEU_MN1, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1EuropeMN1_FALSE <- delta.ses_boris(ClEUROPE_MN1, Group_EU_MN1)
dev.print(device = pdf, file = "OLD_MN1_EU.pdf", width = 10)

## MN2 ###
ClEU_SE <- subset(MN2, MN2$LONG > 20 & MN2$LONG < 40 & MN2$LAT > 30 & MN2$LAT < 42)
ClEU_E <- subset(MN2, MN2$LONG > 24 & MN2$LONG < 33.5 & MN2$LAT > 46.5 & MN2$LAT < 48.6)
ClEU_EAST <- rbind(ClEU_SE, ClEU_E)
ClEU_Nord <- subset(MN2, MN2$LONG > 6.4 & MN2$LONG < 23.5 & MN2$LAT > 46.4 & MN2$LAT < 51.3)
want <- which(ClEU_Nord$NAME == "46.5_6.7" | ClEU_Nord$NAME == "46.8_6.5" | ClEU_Nord$NAME == "47_7.1")  
ClEU_OuestA <- subset(MN2, MN2$LONG < 6.7 & MN2$LAT > 37 & MN2$LAT < 48.2)
ClEU_Ouest <- rbind(ClEU_Nord[want, ], ClEU_OuestA)
ClEU_Nord <- ClEU_Nord[-want,]
wantNW <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 2.2 & ClEU_Ouest$LAT > 46.8 & ClEU_Ouest$LAT < 50)
wantSWfr <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 1.1 & ClEU_Ouest$LAT > 43.2 & ClEU_Ouest$LAT < 50)
wantESP <- which(ClEU_Ouest$LONG > -9.5 & ClEU_Ouest$LONG < -2.8 & ClEU_Ouest$LAT > 40.1 & ClEU_Ouest$LAT < 42)
ClEU_W_Ouest <- rbind(ClEU_Ouest[wantNW, ], ClEU_Ouest[wantSWfr, ], ClEU_Ouest[wantESP,])
ToDelet <- c(wantNW, wantSWfr, wantESP)
ClEU_E_Ouest <- ClEU_Ouest[-ToDelet,]

ClEU_MN2 <- rbind(ClEU_Nord, ClEU_E_Ouest, ClEU_W_Ouest)
Group_EU_MN2 <- c(rep(1,16), rep(2,32), rep(3, 9))
ClEUROPE_MN2 <- Presence_Absence_matrix(ClEU_MN2, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1EuropeMN2_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN2, Group_EU_MN2, NS = TRUE, Info = "MN2_EU")

ClEU_MN2 <- rbind(ClEU_Nord, ClEU_Ouest)
Group_EU_MN2 <- c(rep(1,16), rep(2,41))
ClEUROPE_MN2 <- Presence_Absence_matrix(ClEU_MN2, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune1EuropeMN2_FALSE <- delta.ses_boris(ClEUROPE_MN2, Group_EU_MN2)
dev.print(device = pdf, file = "Ouest_MN2_EU.pdf", width = 10)

ClEUSW1 <- subset(MN2, MN2$LONG < 10 & MN2$LAT < 42)
ClEUW1 <- subset(MN2, MN2$LAT >= 42 & MN2$LAT < 47 & MN2$LONG < 8)
ClEUNW1 <- subset(MN2, MN2$LAT >= 47 & MN2$LONG < 23)
ClEUCN1 <- subset(MN2, MN2$LAT >= 44 & MN2$LONG >= 23 & MN2$LONG < 43)
ClEUGK1 <- subset(MN2, MN2$LAT < 42 & MN2$LONG >= 10)

ClEU_MN2 <- rbind(ClEUSW1, ClEUW1, ClEUNW1)
Group_EU_MN2 <- c(rep(1, 13), rep(2,27), rep(3, 18))
ClEUROPE_MN2 <- Presence_Absence_matrix(ClEU_MN2, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1EuropeMN2_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN2, Group_EU_MN2, NS = TRUE, Info = "OLD_MN2_EU")

### MN3 ###
ClEU_SE <- subset(MN3, MN3$LONG > 20 & MN3$LONG < 40 & MN3$LAT > 30 & MN3$LAT < 42)
ClEU_E <- subset(MN3, MN3$LONG > 24 & MN3$LONG < 33.5 & MN3$LAT > 46.5 & MN3$LAT < 48.6)
ClEU_EAST <- rbind(ClEU_SE, ClEU_E)
ClEU_Nord <- subset(MN3, MN3$LONG > 6.4 & MN3$LONG < 23.5 & MN3$LAT > 46.4 & MN3$LAT < 51.3)
want <- which(ClEU_Nord$NAME == "46.5_6.7" | ClEU_Nord$NAME == "46.8_6.5" | ClEU_Nord$NAME == "47_7.1")  
ClEU_OuestA <- subset(MN3, MN3$LONG < 6.7 & MN3$LAT > 37 & MN3$LAT < 48.2)
ClEU_Ouest <- rbind(ClEU_Nord[want, ], ClEU_OuestA)
ClEU_Nord <- ClEU_Nord[-want,]
wantNW <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 2.2 & ClEU_Ouest$LAT > 46.8 & ClEU_Ouest$LAT < 50)
wantSWfr <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 1.1 & ClEU_Ouest$LAT > 43.2 & ClEU_Ouest$LAT < 50)
wantESP <- which(ClEU_Ouest$LONG > -9.5 & ClEU_Ouest$LONG < -2.8 & ClEU_Ouest$LAT > 40.1 & ClEU_Ouest$LAT < 42)
ClEU_W_Ouest <- rbind(ClEU_Ouest[wantNW, ], ClEU_Ouest[wantSWfr, ], ClEU_Ouest[wantESP,])
ToDelet <- c(wantNW, wantSWfr, wantESP)
ClEU_E_Ouest <- ClEU_Ouest[-ToDelet,]

ClEU_MN3 <- rbind(ClEU_Nord, ClEU_E_Ouest, ClEU_W_Ouest, ClEU_EAST)
Group_EU_MN3 <- c(rep(1,18), rep(2,31), rep(3, 15), rep(4, 10))
ClEUROPE_MN3 <- Presence_Absence_matrix(ClEU_MN3, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1EuropeMN3_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN3, Group_EU_MN3, NS = TRUE, Info = "MN3_EU")

ClEU_MN3 <- rbind(ClEU_Nord, ClEU_Ouest, ClEU_EAST)
Group_EU_MN3 <- c(rep(1,18), rep(2,46), rep(3, 10))
ClEUROPE_MN3 <- Presence_Absence_matrix(ClEU_MN3, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune1EuropeMN3_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN3, Group_EU_MN3, NS = FALSE, Info = "Ouest_MN3_EU")

ClEUSW1 <- subset(MN3, MN3$LONG < 10 & MN3$LAT < 41.9)
ClEUW1 <- subset(MN3, MN3$LAT >= 42 & MN3$LAT < 47 & MN3$LONG < 8)
ClEUNW1 <- subset(MN3, MN3$LAT >= 47 & MN3$LONG < 23)
ClEUCN1 <- subset(MN3, MN3$LAT >= 44 & MN3$LONG >= 23 & MN3$LONG < 43)
ClEUGK1 <- subset(MN3, MN3$LAT < 42 & MN3$LONG >= 10)

ClEU_MN3 <- rbind(ClEUSW1, ClEUW1, ClEUNW1, ClEUGK1)
Group_EU_MN3 <- c(rep(1,17), rep(2,22), rep(3, 24), rep(4, 10))
ClEUROPE_MN3 <- Presence_Absence_matrix(ClEU_MN3, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1EuropeMN3_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN3, Group_EU_MN3, NS = TRUE, Info = "OLD_MN3_EU")


### MN4 ###
ClEU_SE <- subset(MN4, MN4$LONG > 20 & MN4$LONG < 40 & MN4$LAT > 30 & MN4$LAT < 42)
ClEU_E <- subset(MN4, MN4$LONG > 24 & MN4$LONG < 33.5 & MN4$LAT > 46.5 & MN4$LAT < 48.6)
ClEU_EAST <- rbind(ClEU_SE, ClEU_E)
ClEU_Nord <- subset(MN4, MN4$LONG > 6.4 & MN4$LONG < 23.5 & MN4$LAT > 46.4 & MN4$LAT < 51.3)
want <- which(ClEU_Nord$NAME == "46.5_6.7" | ClEU_Nord$NAME == "46.8_6.5" | ClEU_Nord$NAME == "47_7.1")  
ClEU_OuestA <- subset(MN4, MN4$LONG < 6.7 & MN4$LAT > 37 & MN4$LAT < 48.2)
ClEU_Ouest <- rbind(ClEU_Nord[want, ], ClEU_OuestA)
ClEU_Nord <- ClEU_Nord[-want,]
wantNW <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 2.2 & ClEU_Ouest$LAT > 46.8 & ClEU_Ouest$LAT < 50)
wantSWfr <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 1.1 & ClEU_Ouest$LAT > 43.2 & ClEU_Ouest$LAT < 50)
wantESP <- which(ClEU_Ouest$LONG > -9.5 & ClEU_Ouest$LONG < -2.8 & ClEU_Ouest$LAT > 40.1 & ClEU_Ouest$LAT < 42)
ClEU_W_Ouest <- rbind(ClEU_Ouest[wantNW, ], ClEU_Ouest[wantSWfr, ], ClEU_Ouest[wantESP,])
ToDelet <- c(wantNW, wantSWfr, wantESP)
ClEU_E_Ouest <- ClEU_Ouest[-ToDelet,]

ClEU_MN4 <- rbind(ClEU_Nord, ClEU_E_Ouest, ClEU_W_Ouest, ClEU_EAST)
Group_EU_MN4 <- c(rep(1,26), rep(2,32), rep(3, 12), rep(4,16))
ClEUROPE_MN4 <- Presence_Absence_matrix(ClEU_MN4, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1EuropeMN4_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN4, Group_EU_MN4, NS = TRUE, Info = "MN4_EU")

ClEU_MN4 <- rbind(ClEU_Nord, ClEU_Ouest, ClEU_EAST)
Group_EU_MN4 <- c(rep(1,26), rep(2,44), rep(3, 16))
ClEUROPE_MN4 <- Presence_Absence_matrix(ClEU_MN4, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune1EuropeMN4_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN4, Group_EU_MN4, NS = FALSE, Info = "Ouest_MN4_EU")

ClEUSW1 <- subset(MN4, MN4$LONG < 10 & MN4$LAT < 41.8)
ClEUW1 <- subset(MN4, MN4$LAT >= 42 & MN4$LAT < 47 & MN4$LONG < 8)
ClEUNW1 <- subset(MN4, MN4$LAT >= 47 & MN4$LONG < 23)
ClEUCN1 <- subset(MN4, MN4$LAT >= 44 & MN4$LONG >= 23 & MN4$LONG < 43)
ClEUGK1 <- subset(MN4, MN4$LAT < 42 & MN4$LONG >= 10)

ClEU_MN4 <- rbind(ClEUSW1, ClEUW1, ClEUNW1, ClEUGK1)
Group_EU_MN4 <- c(rep(1,18), rep(2,21), rep(3, 28), rep(4,16))
ClEUROPE_MN4 <- Presence_Absence_matrix(ClEU_MN4, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1EuropeMN4_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN4, Group_EU_MN4, NS = TRUE, Info = "OLD_MN4_EU")

### MN5 ###
ClEU_SE <- subset(MN5, MN5$LONG > 20 & MN5$LONG < 40 & MN5$LAT > 30 & MN5$LAT < 42)
ClEU_E <- subset(MN5, MN5$LONG > 24 & MN5$LONG < 33.5 & MN5$LAT > 46.5 & MN5$LAT < 48.6)
ClEU_EAST <- rbind(ClEU_SE, ClEU_E)
ClEU_Nord <- subset(MN5, MN5$LONG > 6.4 & MN5$LONG < 23.5 & MN5$LAT > 46.4 & MN5$LAT < 51.3)
want <- which(ClEU_Nord$NAME == "46.5_6.7" | ClEU_Nord$NAME == "46.8_6.5" | ClEU_Nord$NAME == "47_7.1")  
ClEU_OuestA <- subset(MN5, MN5$LONG < 6.7 & MN5$LAT > 37 & MN5$LAT < 48.2)
ClEU_Ouest <- rbind(ClEU_Nord[want, ], ClEU_OuestA)
ClEU_Nord <- ClEU_Nord[-want,]
wantNW <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 2.2 & ClEU_Ouest$LAT > 46.8 & ClEU_Ouest$LAT < 50)
wantSWfr <- which(ClEU_Ouest$LONG > -2 & ClEU_Ouest$LONG < 1.1 & ClEU_Ouest$LAT > 43.2 & ClEU_Ouest$LAT < 50)
wantESP <- which(ClEU_Ouest$LONG > -9.5 & ClEU_Ouest$LONG < -2.8 & ClEU_Ouest$LAT > 40 & ClEU_Ouest$LAT < 42)
ClEU_W_Ouest <- rbind(ClEU_Ouest[wantNW, ], ClEU_Ouest[wantSWfr, ], ClEU_Ouest[wantESP,])
ToDelet <- c(wantNW, wantSWfr, wantESP)
ClEU_E_Ouest <- ClEU_Ouest[-ToDelet,]

ClEU_MN5 <- rbind(ClEU_Nord, ClEU_E_Ouest, ClEU_W_Ouest, ClEU_EAST)
Group_EU_MN5 <- c(rep(1,45), rep(2,28), rep(3, 22), rep(4, 13))
ClEUROPE_MN5 <- Presence_Absence_matrix(ClEU_MN5, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1EuropeMN5_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN5, Group_EU_MN5, NS = TRUE, Info = "MN5_EU")

ClEU_MN5 <- rbind(ClEU_Nord, ClEU_Ouest, ClEU_EAST)
Group_EU_MN5 <- c(rep(1,45), rep(2,50), rep(3, 13))
ClEUROPE_MN5 <- Presence_Absence_matrix(ClEU_MN5, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune1EuropeMN5_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN5, Group_EU_MN5, NS = TRUE, Info = "Ouest_MN5_EU")

ClEUSW1 <- subset(MN5, MN5$LONG < 10 & MN5$LAT < 41.8)
ClEUW1 <- subset(MN5, MN5$LAT >= 42 & MN5$LAT < 47 & MN5$LONG < 8)
ClEUNW1 <- subset(MN5, MN5$LAT >= 47 & MN5$LONG < 23)
ClEUCN1 <- subset(MN5, MN5$LAT >= 44 & MN5$LONG >= 23 & MN5$LONG < 43)
ClEUGK1 <- subset(MN5, MN5$LAT < 42 & MN5$LONG >= 10)

ClEU_MN5 <- rbind(ClEUSW1, ClEUW1, ClEUNW1, ClEUGK1)
Group_EU_MN5 <- c(rep(1,22), rep(2,20), rep(3, 50), rep(4, 14))
ClEUROPE_MN5 <- Presence_Absence_matrix(ClEU_MN5, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1EuropeMN5_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN5, Group_EU_MN5, NS = TRUE, Info = "OLD_MN5_EU")

############################## FAUNE 2 #########################################
### MN6 ###
ClEU_SE <- subset(MN6, MN6$LONG > 20 & MN6$LONG < 40 & MN6$LAT > 33 & MN6$LAT < 42.8)
ClEU_E <- subset(MN6, MN6$LONG > 25.6 & MN6$LONG < 42 & MN6$LAT > 42.8 & MN6$LAT < 51)
ClEU_NordEst <- subset(MN6, MN6$LONG > 11.5 & MN6$LONG < 24 & MN6$LAT > 46 & MN6$LAT < 49)
ClEU_NordOuest <- subset(MN6, MN6$LONG < 11.3 & MN6$LAT > 45.7 & MN6$LAT < 51)
ClEU_Sud <- subset(MN6, MN6$LONG < 16 & MN6$LAT > 33 & MN6$LAT < 45.6)
ClEU_Nord <- rbind(ClEU_NordEst, ClEU_NordOuest)

ClEU_MN6 <- rbind(ClEU_SE, ClEU_E, ClEU_NordEst, ClEU_NordOuest, ClEU_Sud)
Group_EU_MN6 <- c(rep(1,18), rep(2,13), rep(3, 22), rep(4, 22), rep(5,31))
ClEUROPE_MN6 <- Presence_Absence_matrix(ClEU_MN6, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2EuropeMN6_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN6, Group_EU_MN6, NS = TRUE, Info = "MN6_EU")

ClEU_MN6 <- rbind(ClEU_SE, ClEU_E, ClEU_Nord, ClEU_Sud)
Group_EU_MN6 <- c(rep(1,18), rep(2,13), rep(3, 44), rep(5,31))
ClEUROPE_MN6 <- Presence_Absence_matrix(ClEU_MN6, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune2EuropeMN6_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN6, Group_EU_MN6, NS = TRUE, Info = "Nord_MN6_EU")

#Autre config
ClEU_SE <- subset(MN6, MN6$LONG > 20 & MN6$LONG < 40 & MN6$LAT > 33 & MN6$LAT < 43)
ClEU_Italie <- subset(MN6, MN6$LONG > 6 & MN6$LONG < 17 & MN6$LAT > 36 & MN6$LAT < 45.5)
ClEU_Spain <- subset(MN6, MN6$LONG > -8 & MN6$LONG < 12 & MN6$LAT > 36 & MN6$LAT < 41)
ClEU_South <- rbind(ClEU_Italie, ClEU_Spain)
ClEU_W <- subset(MN6,  MN6$LONG < 6 & MN6$LAT > 41 & MN6$LAT < 47)
ClEU_NW <- subset(MN6, MN6$LONG > 4 & MN6$LONG < 23 & MN6$LAT > 47)
#ClEU_N <- rbind(ClEU_W, ClEU_NW)
ClEU_CN <- subset(MN6,  MN6$LONG > 23.5 & MN6$LONG < 43  & MN6$LAT > 44)

ClEU_MN6 <- rbind(ClEU_SE, ClEU_South, ClEU_W, ClEU_NW, ClEU_CN)
Group_EU_MN6 <- c(rep(1,18), rep(2,8), rep(3, 19), rep(4, 44), rep(5,13))
ClEUROPE_MN6 <- Presence_Absence_matrix(ClEU_MN6, type = "Species", min5 = FALSE, singletons = TRUE)
Other2_Faune2EuropeMN6_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN6, Group_EU_MN6, NS = TRUE, Info = "Other_MN6_EU")

#Oldies
ClEUSW2 <- subset(MN6, MN6$LONG < 10 & MN6$LAT < 42)
ClEUW2 <- subset(MN6, MN6$LAT >= 42 & MN6$LAT < 47 & MN6$LONG < 8)
#ClEUW2 <- join(ClEUSW2, ClEUW2, match="all", type="full")
ClEUNW2 <- subset(MN6, MN6$LAT >= 47 & MN6$LONG < 23)
ClEUCN2 <- subset(MN6, MN6$LAT >= 43 & MN6$LONG >= 23 & MN6$LONG < 43)
ClEUGK2 <- subset(MN6, MN6$LAT < 42 & MN6$LONG >= 10)

ClEU_MN6 <- rbind(ClEUSW2, ClEUW2, ClEUNW2, ClEUCN2, ClEUGK2)
Group_EU_MN6 <- c(rep(1,17), rep(2,14), rep(3, 46), rep(4, 13), rep(5,19))
ClEUROPE_MN6 <- Presence_Absence_matrix(ClEU_MN6, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2EuropeMN6_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN6, Group_EU_MN6, NS = TRUE, Info = "OLD_MN6_EU")

### MN7 ###
ClEU_SE <- subset(MN7_8, MN7_8$LONG > 20 & MN7_8$LONG < 40 & MN7_8$LAT > 33 & MN7_8$LAT < 42.8)
ClEU_E <- subset(MN7_8, MN7_8$LONG > 25.6 & MN7_8$LONG < 42 & MN7_8$LAT > 42.8 & MN7_8$LAT < 51)
ClEU_NordEst <- subset(MN7_8, MN7_8$LONG > 11.5 & MN7_8$LONG < 24 & MN7_8$LAT > 46 & MN7_8$LAT < 49)
ClEU_NordOuest <- subset(MN7_8, MN7_8$LONG < 11.3 & MN7_8$LAT > 45.7 & MN7_8$LAT < 51)
ClEU_Sud <- subset(MN7_8, MN7_8$LONG < 16 & MN7_8$LAT > 33 & MN7_8$LAT < 45.6)
ClEU_Nord <- rbind(ClEU_NordEst, ClEU_NordOuest)

ClEU_MN7_8 <- rbind(ClEU_SE, ClEU_E, ClEU_NordEst, ClEU_NordOuest, ClEU_Sud)
Group_EU_MN7_8 <- c(rep(1,15), rep(2,20), rep(3, 33), rep(4, 15), rep(5,43))
ClEUROPE_MN7_8 <- Presence_Absence_matrix(ClEU_MN7_8, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2EuropeMN7_8_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN7_8, Group_EU_MN7_8, NS = TRUE, Info = "MN7_8_EU")

ClEU_MN7_8 <- rbind(ClEU_SE, ClEU_E, ClEU_Nord, ClEU_Sud)
Group_EU_MN7_8 <- c(rep(1,15), rep(2,20), rep(3, 48), rep(4,43))
ClEUROPE_MN7_8 <- Presence_Absence_matrix(ClEU_MN7_8, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune2EuropeMN7_8_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN7_8, Group_EU_MN7_8, NS = TRUE, Info = "Nord_MN7_8_EU")

#Autre config
ClEU_SE <- subset(MN7_8, MN7_8$LONG > 20 & MN7_8$LONG < 40 & MN7_8$LAT > 33 & MN7_8$LAT < 43)
ClEU_Italie <- subset(MN7_8, MN7_8$LONG > 6 & MN7_8$LONG < 17 & MN7_8$LAT > 36 & MN7_8$LAT < 45.5)
ClEU_Spain <- subset(MN7_8, MN7_8$LONG > -8 & MN7_8$LONG < 12 & MN7_8$LAT > 36 & MN7_8$LAT < 41)
ClEU_South <- rbind(ClEU_Italie, ClEU_Spain)
ClEU_W <- subset(MN7_8,  MN7_8$LONG < 6 & MN7_8$LAT > 41 & MN7_8$LAT < 47)
ClEU_NW <- subset(MN7_8, MN7_8$LONG > 4 & MN7_8$LONG < 23 & MN7_8$LAT > 47)
#ClEU_N <- rbind(ClEU_W, ClEU_NW)
ClEU_CN <- subset(MN7_8,  MN7_8$LONG > 23.5 & MN7_8$LONG < 43  & MN7_8$LAT > 44)

ClEU_MN7_8 <- rbind(ClEU_SE, ClEU_South, ClEU_W, ClEU_NW, ClEU_CN)
Group_EU_MN7_8 <- c(rep(1,15), rep(2,10), rep(3, 33), rep(4, 46), rep(5,20))
ClEUROPE_MN7_8 <- Presence_Absence_matrix(ClEU_MN7_8, type = "Species", min5 = FALSE, singletons = TRUE)
Other2_Faune2EuropeMN7_8_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN7_8, Group_EU_MN7_8, NS = TRUE, Info = "Other_MN7_8_EU")

#Oldies
ClEUSW2 <- subset(MN7_8, MN7_8$LONG < 10 & MN7_8$LAT < 42)
ClEUW2 <- subset(MN7_8, MN7_8$LAT >= 42 & MN7_8$LAT < 47 & MN7_8$LONG < 8)
#ClEUW2 <- join(ClEUSW2, ClEUW2, match="all", type="full")
ClEUNW2 <- subset(MN7_8, MN7_8$LAT >= 47 & MN7_8$LONG < 23)
ClEUCN2 <- subset(MN7_8, MN7_8$LAT >= 43 & MN7_8$LONG >= 23 & MN7_8$LONG < 43)
ClEUGK2 <- subset(MN7_8, MN7_8$LAT < 42 & MN7_8$LONG >= 10)

ClEU_MN7_8 <- rbind(ClEUSW2, ClEUW2, ClEUNW2, ClEUCN2, ClEUGK2)
Group_EU_MN7_8 <- c(rep(1,22), rep(2,20), rep(3, 46), rep(4, 20), rep(5,17))
ClEUROPE_MN7_8 <- Presence_Absence_matrix(ClEU_MN7_8, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2EuropeMN7_8_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN7_8, Group_EU_MN7_8, NS = TRUE, Info = "OLD_MN7_8_EU")

### MN9 ###
ClEU_SE <- subset(MN9, MN9$LONG > 20 & MN9$LONG < 40 & MN9$LAT > 33 & MN9$LAT < 42.8)
ClEU_E <- subset(MN9, MN9$LONG > 25.6 & MN9$LONG < 42 & MN9$LAT > 42.8 & MN9$LAT < 51)
ClEU_NordEst <- subset(MN9, MN9$LONG > 11.5 & MN9$LONG < 24 & MN9$LAT > 46 & MN9$LAT < 49)
ClEU_NordOuest <- subset(MN9, MN9$LONG < 11.3 & MN9$LAT > 45.7 & MN9$LAT < 51)
ClEU_Sud <- subset(MN9, MN9$LONG < 16 & MN9$LAT > 33 & MN9$LAT < 45.6)
ClEU_Nord <- rbind(ClEU_NordEst, ClEU_NordOuest)

ClEU_MN9 <- rbind(ClEU_SE, ClEU_E, ClEU_NordEst, ClEU_NordOuest, ClEU_Sud)
Group_EU_MN9 <- c(rep(1,23), rep(2,21), rep(3, 24), rep(4, 20), rep(5,41))
ClEUROPE_MN9 <- Presence_Absence_matrix(ClEU_MN9, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2EuropeMN9_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN9, Group_EU_MN9, NS = TRUE, Info = "MN9_EU")

ClEU_MN9 <- rbind(ClEU_SE, ClEU_E, ClEU_Nord, ClEU_Sud)
Group_EU_MN9 <- c(rep(1,23), rep(2,21), rep(3, 44), rep(4,41))
ClEUROPE_MN9 <- Presence_Absence_matrix(ClEU_MN9, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune2EuropeMN9_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN9, Group_EU_MN9, NS = TRUE, Info = "Nord_MN9_EU")

#Autre config
ClEU_SE <- subset(MN9, MN9$LONG > 20 & MN9$LONG < 40 & MN9$LAT > 33 & MN9$LAT < 43)
ClEU_Italie <- subset(MN9, MN9$LONG > 6 & MN9$LONG < 17 & MN9$LAT > 36 & MN9$LAT < 45.5)
ClEU_Spain <- subset(MN9, MN9$LONG > -8 & MN9$LONG < 12 & MN9$LAT > 36 & MN9$LAT < 41)
ClEU_South <- rbind(ClEU_Italie, ClEU_Spain)
ClEU_W <- subset(MN9,  MN9$LONG < 6 & MN9$LAT > 41 & MN9$LAT < 47)
ClEU_NW <- subset(MN9, MN9$LONG > 4 & MN9$LONG < 23 & MN9$LAT > 47)
#ClEU_N <- rbind(ClEU_W, ClEU_NW)
ClEU_CN <- subset(MN9,  MN9$LONG > 23.5 & MN9$LONG < 43  & MN9$LAT > 44)

ClEU_MN9 <- rbind(ClEU_SE, ClEU_South, ClEU_W, ClEU_NW, ClEU_CN)
Group_EU_MN9 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                  rep(2,length(Presence_Absence_matrix(ClEU_South, type = "Species", min5 = FALSE)[,1])), 
                  rep(3, length(Presence_Absence_matrix(ClEU_W, type = "Species", min5 = FALSE)[,1])), 
                  rep(4, length(Presence_Absence_matrix(ClEU_NW, type = "Species", min5 = FALSE)[,1])), 
                  rep(5,length(Presence_Absence_matrix(ClEU_CN, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN9 <- Presence_Absence_matrix(ClEU_MN9, type = "Species", min5 = FALSE, singletons = TRUE)
Other2_Faune2EuropeMN9_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN9, Group_EU_MN9, NS = TRUE, Info = "Other_MN9_EU")

#Oldies
ClEUSW2 <- subset(MN9, MN9$LONG < 10 & MN9$LAT < 42)
ClEUW2 <- subset(MN9, MN9$LAT >= 42 & MN9$LAT < 47 & MN9$LONG < 8)
#ClEUW2 <- join(ClEUSW2, ClEUW2, match="all", type="full")
ClEUNW2 <- subset(MN9, MN9$LAT >= 47 & MN9$LONG < 23)
ClEUCN2 <- subset(MN9, MN9$LAT >= 43 & MN9$LONG >= 23 & MN9$LONG < 43)
ClEUGK2 <- subset(MN9, MN9$LAT < 42 & MN9$LONG >= 10)

ClEU_MN9 <- rbind(ClEUSW2, ClEUW2, ClEUNW2, ClEUCN2, ClEUGK2)
Group_EU_MN9 <- c(rep(1,length(Presence_Absence_matrix(ClEUSW2, type = "Species", min5 = FALSE)[,1])), 
                  rep(2,length(Presence_Absence_matrix(ClEUW2, type = "Species", min5 = FALSE)[,1])), 
                  rep(3, length(Presence_Absence_matrix(ClEUNW2, type = "Species", min5 = FALSE)[,1])), 
                  rep(4, length(Presence_Absence_matrix(ClEUCN2, type = "Species", min5 = FALSE)[,1])), 
                  rep(5,length(Presence_Absence_matrix(ClEUGK2, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN9 <- Presence_Absence_matrix(ClEU_MN9, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2EuropeMN9_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN9, Group_EU_MN9, NS = TRUE, Info = "OLD_MN9_EU")

### MN10 ###
ClEU_SE <- subset(MN10, MN10$LONG > 20 & MN10$LONG < 40 & MN10$LAT > 33 & MN10$LAT < 42.8)
ClEU_E <- subset(MN10, MN10$LONG > 25.6 & MN10$LONG < 42 & MN10$LAT > 42.8 & MN10$LAT < 51)
ClEU_NordEst <- subset(MN10, MN10$LONG > 11.5 & MN10$LONG < 24 & MN10$LAT > 46 & MN10$LAT < 49)
ClEU_NordOuest <- subset(MN10, MN10$LONG < 11.3 & MN10$LAT > 45.7 & MN10$LAT < 51)
ClEU_Sud <- subset(MN10, MN10$LONG < 16 & MN10$LAT > 33 & MN10$LAT < 45.6)
ClEU_Nord <- rbind(ClEU_NordEst, ClEU_NordOuest)

ClEU_MN10 <- rbind(ClEU_SE, ClEU_E, ClEU_NordEst, ClEU_NordOuest, ClEU_Sud)
Group_EU_MN10 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN10 <- Presence_Absence_matrix(ClEU_MN10, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2EuropeMN10_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN10, Group_EU_MN10, NS = TRUE, Info = "MN10_EU")

ClEU_MN10 <- rbind(ClEU_SE, ClEU_E, ClEU_Nord, ClEU_Sud)
Group_EU_MN10 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_Nord, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN10 <- Presence_Absence_matrix(ClEU_MN10, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune2EuropeMN10_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN10, Group_EU_MN10, NS = TRUE, Info = "Nord_MN10_EU")

#Autre config
ClEU_SE <- subset(MN10, MN10$LONG > 20 & MN10$LONG < 40 & MN10$LAT > 33 & MN10$LAT < 43)
ClEU_Italie <- subset(MN10, MN10$LONG > 6 & MN10$LONG < 17 & MN10$LAT > 36 & MN10$LAT < 45.5)
ClEU_Spain <- subset(MN10, MN10$LONG > -8 & MN10$LONG < 12 & MN10$LAT > 36 & MN10$LAT < 41)
ClEU_South <- rbind(ClEU_Italie, ClEU_Spain)
ClEU_W <- subset(MN10,  MN10$LONG < 6 & MN10$LAT > 41 & MN10$LAT < 47)
ClEU_NW <- subset(MN10, MN10$LONG > 4 & MN10$LONG < 23 & MN10$LAT > 47)
#ClEU_N <- rbind(ClEU_W, ClEU_NW)
ClEU_CN <- subset(MN10,  MN10$LONG > 23.5 & MN10$LONG < 43  & MN10$LAT > 44)

ClEU_MN10 <- rbind(ClEU_SE, ClEU_South, ClEU_W, ClEU_NW, ClEU_CN)
Group_EU_MN10 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_South, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_W, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NW, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_CN, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN10 <- Presence_Absence_matrix(ClEU_MN10, type = "Species", min5 = FALSE, singletons = TRUE)
Other2_Faune2EuropeMN10_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN10, Group_EU_MN10, NS = TRUE, Info = "Other_MN10_EU")

#Oldies
ClEUSW2 <- subset(MN10, MN10$LONG < 10 & MN10$LAT < 42)
ClEUW2 <- subset(MN10, MN10$LAT >= 42 & MN10$LAT < 47 & MN10$LONG < 8)
#ClEUW2 <- join(ClEUSW2, ClEUW2, match="all", type="full")
ClEUNW2 <- subset(MN10, MN10$LAT >= 47 & MN10$LONG < 23)
ClEUCN2 <- subset(MN10, MN10$LAT >= 43 & MN10$LONG >= 23 & MN10$LONG < 43)
ClEUGK2 <- subset(MN10, MN10$LAT < 42 & MN10$LONG >= 10)

ClEU_MN10 <- rbind(ClEUSW2, ClEUW2, ClEUNW2, ClEUCN2, ClEUGK2)
Group_EU_MN10 <- c(rep(1,length(Presence_Absence_matrix(ClEUSW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEUW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN2, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEUGK2, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN10 <- Presence_Absence_matrix(ClEU_MN10, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2EuropeMN10_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN10, Group_EU_MN10, NS = TRUE, Info = "OLD_MN10_EU")

### MN11 ###
ClEU_SE <- subset(MN11, MN11$LONG > 20 & MN11$LONG < 40 & MN11$LAT > 33 & MN11$LAT < 42.8)
ClEU_E <- subset(MN11, MN11$LONG > 25.6 & MN11$LONG < 42 & MN11$LAT > 42.8 & MN11$LAT < 51)
ClEU_NordEst <- subset(MN11, MN11$LONG > 11.5 & MN11$LONG < 24 & MN11$LAT > 46 & MN11$LAT < 49)
ClEU_NordOuest <- subset(MN11, MN11$LONG < 11.3 & MN11$LAT > 45.55 & MN11$LAT < 51)
ClEU_Sud <- subset(MN11, MN11$LONG < 16 & MN11$LAT > 33 & MN11$LAT < 45.55)
ClEU_Nord <- rbind(ClEU_NordEst, ClEU_NordOuest)

ClEU_MN11 <- rbind(ClEU_SE, ClEU_E, ClEU_NordEst, ClEU_NordOuest, ClEU_Sud)
Group_EU_MN11 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN11 <- Presence_Absence_matrix(ClEU_MN11, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2EuropeMN11_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN11, Group_EU_MN11, NS = TRUE, Info = "MN11_EU")

ClEU_MN11 <- rbind(ClEU_SE, ClEU_E, ClEU_Nord, ClEU_Sud)
Group_EU_MN11 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_Nord, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN11 <- Presence_Absence_matrix(ClEU_MN11, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune2EuropeMN11_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN11, Group_EU_MN11, NS = TRUE, Info = "Nord_MN11_EU")

#Autre config
ClEU_SE <- subset(MN11, MN11$LONG > 20 & MN11$LONG < 40 & MN11$LAT > 33 & MN11$LAT < 43)
ClEU_Italie <- subset(MN11, MN11$LONG > 6 & MN11$LONG < 17 & MN11$LAT > 36 & MN11$LAT < 45.5)
ClEU_Spain <- subset(MN11, MN11$LONG > -8 & MN11$LONG < 12 & MN11$LAT > 36 & MN11$LAT < 41)
ClEU_South <- rbind(ClEU_Italie, ClEU_Spain)
ClEU_W <- subset(MN11,  MN11$LONG < 6 & MN11$LAT > 41 & MN11$LAT < 47)
ClEU_NW <- subset(MN11, MN11$LONG > 4 & MN11$LONG < 23 & MN11$LAT > 47)
#ClEU_N <- rbind(ClEU_W, ClEU_NW)
ClEU_CN <- subset(MN11,  MN11$LONG > 23.5 & MN11$LONG < 43  & MN11$LAT > 44)

ClEU_MN11 <- rbind(ClEU_SE, ClEU_South, ClEU_W, ClEU_NW, ClEU_CN)
Group_EU_MN11 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_South, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_W, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NW, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_CN, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN11 <- Presence_Absence_matrix(ClEU_MN11, type = "Species", min5 = FALSE, singletons = TRUE)
Other2_Faune2EuropeMN11_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN11, Group_EU_MN11, NS = TRUE, Info = "Other_MN11_EU")

#Oldies
ClEUSW2 <- subset(MN11, MN11$LONG < 10 & MN11$LAT < 42)
ClEUW2 <- subset(MN11, MN11$LAT >= 42 & MN11$LAT < 47 & MN11$LONG < 8)
#ClEUW2 <- join(ClEUSW2, ClEUW2, match="all", type="full")
ClEUNW2 <- subset(MN11, MN11$LAT >= 47 & MN11$LONG < 23)
ClEUCN2 <- subset(MN11, MN11$LAT >= 43 & MN11$LONG >= 23 & MN11$LONG < 43)
ClEUGK2 <- subset(MN11, MN11$LAT < 42 & MN11$LONG >= 10)

ClEU_MN11 <- rbind(ClEUSW2, ClEUW2, ClEUNW2, ClEUCN2, ClEUGK2)
Group_EU_MN11 <- c(rep(1,length(Presence_Absence_matrix(ClEUSW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEUW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN2, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEUGK2, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN11 <- Presence_Absence_matrix(ClEU_MN11, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2EuropeMN11_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN11, Group_EU_MN11, NS = TRUE, Info = "OLD_MN11_EU")

### MN12 ###
ClEU_SE <- subset(MN12, MN12$LONG > 20 & MN12$LONG < 40 & MN12$LAT > 33 & MN12$LAT < 42.8)
ClEU_E <- subset(MN12, MN12$LONG > 25.6 & MN12$LONG < 42 & MN12$LAT > 42.8 & MN12$LAT < 51)
ClEU_NordEst <- subset(MN12, MN12$LONG > 11.5 & MN12$LONG < 24 & MN12$LAT > 46 & MN12$LAT < 49)
ClEU_NordOuest <- subset(MN12, MN12$LONG < 11.3 & MN12$LAT > 45.55 & MN12$LAT < 51)
ClEU_Sud <- subset(MN12, MN12$LONG < 16 & MN12$LAT > 33 & MN12$LAT < 45.55)
ClEU_Nord <- rbind(ClEU_NordEst, ClEU_NordOuest)

ClEU_MN12 <- rbind(ClEU_SE, ClEU_E, ClEU_NordEst, ClEU_NordOuest, ClEU_Sud)
Group_EU_MN12 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN12 <- Presence_Absence_matrix(ClEU_MN12, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2EuropeMN12_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN12, Group_EU_MN12, NS = TRUE, Info = "MN12_EU")

ClEU_MN12 <- rbind(ClEU_SE, ClEU_E, ClEU_Nord, ClEU_Sud)
Group_EU_MN12 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_Nord, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN12 <- Presence_Absence_matrix(ClEU_MN12, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune2EuropeMN12_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN12, Group_EU_MN12, NS = TRUE, Info = "Nord_MN12_EU")

#Autre config
ClEU_SE <- subset(MN12, MN12$LONG > 20 & MN12$LONG < 40 & MN12$LAT > 33 & MN12$LAT < 43)
ClEU_Italie <- subset(MN12, MN12$LONG > 6 & MN12$LONG < 17 & MN12$LAT > 36 & MN12$LAT < 45.5)
ClEU_Spain <- subset(MN12, MN12$LONG > -8 & MN12$LONG < 12 & MN12$LAT > 36 & MN12$LAT < 41)
ClEU_South <- rbind(ClEU_Italie, ClEU_Spain)
ClEU_W <- subset(MN12,  MN12$LONG < 6 & MN12$LAT > 41 & MN12$LAT < 47)
ClEU_NW <- subset(MN12, MN12$LONG > 4 & MN12$LONG < 23 & MN12$LAT > 47)
#ClEU_N <- rbind(ClEU_W, ClEU_NW)
ClEU_CN <- subset(MN12,  MN12$LONG > 23.5 & MN12$LONG < 43  & MN12$LAT > 44)

ClEU_MN12 <- rbind(ClEU_SE, ClEU_South, ClEU_W, ClEU_NW, ClEU_CN)
Group_EU_MN12 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_South, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_W, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NW, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_CN, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN12 <- Presence_Absence_matrix(ClEU_MN12, type = "Species", min5 = FALSE, singletons = TRUE)
Other2_Faune2EuropeMN12_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN12, Group_EU_MN12, NS = TRUE, Info = "Other_MN12_EU")

#Oldies
ClEUSW2 <- subset(MN12, MN12$LONG < 10 & MN12$LAT < 42)
ClEUW2 <- subset(MN12, MN12$LAT >= 42 & MN12$LAT < 47 & MN12$LONG < 8)
#ClEUW2 <- join(ClEUSW2, ClEUW2, match="all", type="full")
ClEUNW2 <- subset(MN12, MN12$LAT >= 47 & MN12$LONG < 23)
ClEUCN2 <- subset(MN12, MN12$LAT >= 43 & MN12$LONG >= 23 & MN12$LONG < 43)
ClEUGK2 <- subset(MN12, MN12$LAT < 42 & MN12$LONG >= 10)

ClEU_MN12 <- rbind(ClEUSW2, ClEUW2, ClEUNW2, ClEUCN2, ClEUGK2)
Group_EU_MN12 <- c(rep(1,length(Presence_Absence_matrix(ClEUSW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEUW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN2, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEUGK2, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN12 <- Presence_Absence_matrix(ClEU_MN12, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2EuropeMN12_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN12, Group_EU_MN12, NS = TRUE, Info = "OLD_MN12_EU")

### MN13 ###
ClEU_SE <- subset(MN13, MN13$LONG > 20 & MN13$LONG < 40 & MN13$LAT > 33 & MN13$LAT < 42.8)
ClEU_E <- subset(MN13, MN13$LONG > 25.6 & MN13$LONG < 42 & MN13$LAT > 42.8 & MN13$LAT < 51)
ClEU_NordEst <- subset(MN13, MN13$LONG > 11.5 & MN13$LONG < 24 & MN13$LAT > 46 & MN13$LAT < 49)
ClEU_NordOuest <- subset(MN13, MN13$LONG < 11.3 & MN13$LAT > 45.55 & MN13$LAT < 51)
ClEU_Sud <- subset(MN13, MN13$LONG < 16 & MN13$LAT > 33 & MN13$LAT < 45.55)
ClEU_Nord <- rbind(ClEU_NordEst, ClEU_NordOuest)

ClEU_MN13 <- rbind(ClEU_SE, ClEU_E, ClEU_NordEst, ClEU_NordOuest, ClEU_Sud)
Group_EU_MN13 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN13 <- Presence_Absence_matrix(ClEU_MN13, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2EuropeMN13_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN13, Group_EU_MN13, NS = TRUE, Info = "MN13_EU")

ClEU_MN13 <- rbind(ClEU_SE, ClEU_E, ClEU_Nord, ClEU_Sud)
Group_EU_MN13 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_E, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_Nord, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN13 <- Presence_Absence_matrix(ClEU_MN13, type = "Species", min5 = FALSE, singletons = TRUE)
W_Faune2EuropeMN13_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN13, Group_EU_MN13, NS = TRUE, Info = "Nord_MN13_EU")

#Autre config
ClEU_SE <- subset(MN13, MN13$LONG > 20 & MN13$LONG < 40 & MN13$LAT > 33 & MN13$LAT < 43)
ClEU_Italie <- subset(MN13, MN13$LONG > 6 & MN13$LONG < 17 & MN13$LAT > 36 & MN13$LAT < 45.5)
ClEU_Spain <- subset(MN13, MN13$LONG > -8 & MN13$LONG < 12 & MN13$LAT > 36 & MN13$LAT < 41)
ClEU_South <- rbind(ClEU_Italie, ClEU_Spain)
ClEU_W <- subset(MN13,  MN13$LONG < 6 & MN13$LAT > 41 & MN13$LAT < 47)
ClEU_NW <- subset(MN13, MN13$LONG > 4 & MN13$LONG < 23 & MN13$LAT > 47)
#ClEU_N <- rbind(ClEU_W, ClEU_NW)
ClEU_CN <- subset(MN13,  MN13$LONG > 23.5 & MN13$LONG < 43  & MN13$LAT > 44)

ClEU_MN13 <- rbind(ClEU_SE, ClEU_South, ClEU_W, ClEU_NW, ClEU_CN)
Group_EU_MN13 <- c(rep(1,length(Presence_Absence_matrix(ClEU_SE, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEU_South, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_W, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NW, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEU_CN, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN13 <- Presence_Absence_matrix(ClEU_MN13, type = "Species", min5 = FALSE, singletons = TRUE)
Other2_Faune2EuropeMN13_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN13, Group_EU_MN13, NS = TRUE, Info = "Other_MN13_EU")

#Oldies
ClEUSW2 <- subset(MN13, MN13$LONG < 10 & MN13$LAT < 42)
ClEUW2 <- subset(MN13, MN13$LAT >= 42 & MN13$LAT < 47 & MN13$LONG < 8)
#ClEUW2 <- join(ClEUSW2, ClEUW2, match="all", type="full")
ClEUNW2 <- subset(MN13, MN13$LAT >= 47 & MN13$LONG < 23)
ClEUCN2 <- subset(MN13, MN13$LAT >= 43 & MN13$LONG >= 23 & MN13$LONG < 43)
ClEUGK2 <- subset(MN13, MN13$LAT < 42 & MN13$LONG >= 10)

ClEU_MN13 <- rbind(ClEUSW2, ClEUW2, ClEUNW2, ClEUCN2, ClEUGK2)
Group_EU_MN13 <- c(rep(1,length(Presence_Absence_matrix(ClEUSW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEUW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW2, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN2, type = "Species", min5 = FALSE)[,1])), 
                   rep(5,length(Presence_Absence_matrix(ClEUGK2, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN13 <- Presence_Absence_matrix(ClEU_MN13, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2EuropeMN13_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN13, Group_EU_MN13, NS = TRUE, Info = "OLD_MN13_EU")

###### FAUNE 3 ######################################################

## MN14
ClEU_Sud <- subset(MN14, MN14$LAT > 33 & MN14$LAT <= 45.1 & MN14$LONG > -10 & MN14$LONG < 50)
ClEU_NordOuest <- subset(MN14, MN14$LONG > -10 & MN14$LONG < 14.7 & MN14$LAT > 46 & MN14$LAT < 60)
ClEU_Centrale <- subset(MN14, MN14$LONG > 25 & MN14$LONG < 50 & MN14$LAT > 45.2 & MN14$LAT < 60)
ClEU_NordEst <- subset(MN14, MN14$LONG > 15.2 & MN14$LONG < 22.5 & MN14$LAT > 48.2 & MN14$LAT < 60)

ClEU_MN14 <- rbind(ClEU_Sud, ClEU_Centrale, ClEU_NordEst)
Group_EU_MN14 <- c(rep(1,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEU_Centrale, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN14 <- Presence_Absence_matrix(ClEU_MN14, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3EuropeMN14_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN14, Group_EU_MN14, NS = TRUE, Info = "MN14_EU")

#Oldies
ClEUSW3 <- subset(MN15, MN15$LONG < 10 & MN15$LAT < 45)
ClEUI3 <- subset(MN15, MN15$LAT < 45 & MN15$LONG >= 10 & MN15$LONG < 20)
ClEUNW3 <- subset(MN15, MN15$LAT >= 45 & MN15$LONG < 15)
ClEUCN3 <- subset(MN15, MN15$LAT >= 44 & MN15$LONG >= 15)
ClEUGK3 <- subset(MN15, MN15$LAT < 44 & MN15$LONG >= 20) 

ClEU_MN14 <- rbind(ClEUSW3, ClEUI3, ClEUNW3, ClEUCN3, ClEUGK3)
Group_EU_MN14 <- c(rep(1,length(Presence_Absence_matrix(ClEUSW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(2,length(Presence_Absence_matrix(ClEUI3, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN3, type = "Species", min5 = FALSE)[,1])),
                   rep(5,length(Presence_Absence_matrix(ClEUGK3, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN14 <- Presence_Absence_matrix(ClEU_MN14, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3EuropeMN14_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN14, Group_EU_MN14, NS = TRUE, Info = "OLD_MN14_EU")

## MN15
ClEU_Sud <- subset(MN15, MN15$LAT > 33 & MN15$LAT <= 45.1 & MN15$LONG > -10 & MN15$LONG < 50)
ClEU_NordOuest <- subset(MN15, MN15$LONG > -10 & MN15$LONG < 14.7 & MN15$LAT > 46 & MN15$LAT < 60)
ClEU_Centrale <- subset(MN15, MN15$LONG > 25 & MN15$LONG < 50 & MN15$LAT > 45.2 & MN15$LAT < 60)
ClEU_NordEst <- subset(MN15, MN15$LONG > 15.2 & MN15$LONG < 22.5 & MN15$LAT > 48.2 & MN15$LAT < 60)

ClEU_MN15 <- rbind(ClEU_Sud,  ClEU_NordOuest, ClEU_Centrale, ClEU_NordEst)
Group_EU_MN15 <- c(rep(1,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])),
                   rep(2, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])),
                   rep(3, length(Presence_Absence_matrix(ClEU_Centrale, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN15 <- Presence_Absence_matrix(ClEU_MN15, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3EuropeMN15_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN15, Group_EU_MN15, NS = TRUE, Info = "MN15_EU")

#Oldies
ClEUSW3 <- subset(MN15, MN15$LONG < 8 & MN15$LAT < 45)
ClEUI3 <- subset(MN15, MN15$LAT < 45 & MN15$LONG > 8 & MN15$LONG < 20)
ClEUNW3 <- subset(MN15, MN15$LAT >= 45 & MN15$LONG < 15)
ClEUCN3 <- subset(MN15, MN15$LAT >= 44 & MN15$LONG >= 15)
ClEUGK3 <- subset(MN15, MN15$LAT < 44 & MN15$LONG >= 20) 

ClEU_MN15 <- rbind(ClEUSW3, ClEUI3, ClEUNW3, ClEUCN3, ClEUGK3)
Group_EU_MN15 <- c(rep(1, length(Presence_Absence_matrix(ClEUSW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(2, length(Presence_Absence_matrix(ClEUI3, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN3, type = "Species", min5 = FALSE)[,1])),
                   rep(5, length(Presence_Absence_matrix(ClEUGK3, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN15 <- Presence_Absence_matrix(ClEU_MN15, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3EuropeMN15_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN15, Group_EU_MN15, NS = TRUE, Info = "OLD_MN15_EU")

## MN16
ClEU_Sud <- subset(MN16, MN16$LAT > 33 & MN16$LAT <= 45.1 & MN16$LONG > -10 & MN16$LONG < 50)
ClEU_NordOuest <- subset(MN16, MN16$LONG > -10 & MN16$LONG < 14.7 & MN16$LAT > 46 & MN16$LAT < 60)
ClEU_Centrale <- subset(MN16, MN16$LONG > 25 & MN16$LONG < 50 & MN16$LAT > 45.2 & MN16$LAT < 60)
ClEU_NordEst <- subset(MN16, MN16$LONG > 15.2 & MN16$LONG < 22.5 & MN16$LAT > 48.2 & MN16$LAT < 60)

ClEU_MN16 <- rbind(ClEU_Sud,  ClEU_NordOuest, ClEU_Centrale, ClEU_NordEst)
Group_EU_MN16 <- c(rep(1,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])),
                   rep(2, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])),
                   rep(3, length(Presence_Absence_matrix(ClEU_Centrale, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN16 <- Presence_Absence_matrix(ClEU_MN16, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3EuropeMN16_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN16, Group_EU_MN16, NS = TRUE, Info = "MN16_EU")

#Oldies
ClEUSW3 <- subset(MN16, MN16$LONG < 8 & MN16$LAT < 45)
ClEUI3 <- subset(MN16, MN16$LAT < 45 & MN16$LONG > 8 & MN16$LONG < 20)
ClEUNW3 <- subset(MN16, MN16$LAT >= 45 & MN16$LONG < 15)
ClEUCN3 <- subset(MN16, MN16$LAT >= 44 & MN16$LONG >= 15)
ClEUGK3 <- subset(MN16, MN16$LAT < 44 & MN16$LONG >= 20) 

ClEU_MN16 <- rbind(ClEUSW3, ClEUI3, ClEUNW3, ClEUCN3, ClEUGK3)
Group_EU_MN16 <- c(rep(1, length(Presence_Absence_matrix(ClEUSW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(2, length(Presence_Absence_matrix(ClEUI3, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN3, type = "Species", min5 = FALSE)[,1])),
                   rep(5, length(Presence_Absence_matrix(ClEUGK3, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN16 <- Presence_Absence_matrix(ClEU_MN16, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3EuropeMN16_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN16, Group_EU_MN16, NS = TRUE, Info = "OLD_MN16_EU")

## MN17
ClEU_Sud <- subset(MN17, MN17$LAT > 33 & MN17$LAT <= 45.1 & MN17$LONG > -10 & MN17$LONG < 50)
ClEU_NordOuest <- subset(MN17, MN17$LONG > -10 & MN17$LONG < 14.7 & MN17$LAT > 46 & MN17$LAT < 60)
ClEU_Centrale <- subset(MN17, MN17$LONG > 25 & MN17$LONG < 50 & MN17$LAT > 45.2 & MN17$LAT < 60)
ClEU_NordEst <- subset(MN17, MN17$LONG > 15.2 & MN17$LONG < 22.5 & MN17$LAT > 48.2 & MN17$LAT < 60)

ClEU_MN17 <- rbind(ClEU_Sud,  ClEU_NordOuest, ClEU_Centrale, ClEU_NordEst)
Group_EU_MN17 <- c(rep(1,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])),
                   rep(2, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])),
                   rep(3, length(Presence_Absence_matrix(ClEU_Centrale, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN17 <- Presence_Absence_matrix(ClEU_MN17, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3EuropeMN17_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN17, Group_EU_MN17, NS = TRUE, Info = "MN17_EU")

#Oldies
ClEUSW3 <- subset(MN17, MN17$LONG < 8 & MN17$LAT < 45)
ClEUI3 <- subset(MN17, MN17$LAT < 45 & MN17$LONG > 8 & MN17$LONG < 20)
ClEUNW3 <- subset(MN17, MN17$LAT > 45 & MN17$LONG < 15)
ClEUCN3 <- subset(MN17, MN17$LAT > 45 & MN17$LONG > 15)
ClEUGK3 <- subset(MN17, MN17$LAT < 44 & MN17$LONG > 20) 

ClEU_MN17 <- rbind(ClEUSW3, ClEUI3, ClEUNW3, ClEUCN3, ClEUGK3)
Group_EU_MN17 <- c(rep(1, length(Presence_Absence_matrix(ClEUSW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(2, length(Presence_Absence_matrix(ClEUI3, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN3, type = "Species", min5 = FALSE)[,1])),
                   rep(5, length(Presence_Absence_matrix(ClEUGK3, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN17 <- Presence_Absence_matrix(ClEU_MN17, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3EuropeMN17_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN17, Group_EU_MN17, NS = TRUE, Info = "OLD_MN17_EU")

## MN18
ClEU_Sud <- subset(MN18, MN18$LAT > 33 & MN18$LAT <= 45.1 & MN18$LONG > -10 & MN18$LONG < 50)
ClEU_NordOuest <- subset(MN18, MN18$LONG > -10 & MN18$LONG < 14.7 & MN18$LAT > 46 & MN18$LAT < 60)
ClEU_Centrale <- subset(MN18, MN18$LONG > 25 & MN18$LONG < 50 & MN18$LAT > 45.2 & MN18$LAT < 60)
ClEU_NordEst <- subset(MN18, MN18$LONG > 15.2 & MN18$LONG < 22.5 & MN18$LAT > 48.2 & MN18$LAT < 60)

ClEU_MN18 <- rbind(ClEU_Sud,  ClEU_NordOuest, ClEU_Centrale, ClEU_NordEst)
Group_EU_MN18 <- c(rep(1,length(Presence_Absence_matrix(ClEU_Sud, type = "Species", min5 = FALSE)[,1])),
                   rep(2, length(Presence_Absence_matrix(ClEU_NordOuest, type = "Species", min5 = FALSE)[,1])),
                   rep(3, length(Presence_Absence_matrix(ClEU_Centrale, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEU_NordEst, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN18 <- Presence_Absence_matrix(ClEU_MN18, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3EuropeMN18_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN18, Group_EU_MN18, NS = TRUE, Info = "MN18_EU")

#Oldies
ClEUSW3 <- subset(MN18, MN18$LONG < 8 & MN18$LAT < 45)
ClEUI3 <- subset(MN18, MN18$LAT < 45 & MN18$LONG > 8 & MN18$LONG < 20)
ClEUNW3 <- subset(MN18, MN18$LAT > 45 & MN18$LONG < 15)
ClEUCN3 <- subset(MN18, MN18$LAT > 45 & MN18$LONG > 15)
ClEUGK3 <- subset(MN18, MN18$LAT < 44 & MN18$LONG > 20) 

ClEU_MN18 <- rbind(ClEUSW3, ClEUI3, ClEUNW3, ClEUCN3, ClEUGK3)
Group_EU_MN18 <- c(rep(1, length(Presence_Absence_matrix(ClEUSW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(2, length(Presence_Absence_matrix(ClEUI3, type = "Species", min5 = FALSE)[,1])), 
                   rep(3, length(Presence_Absence_matrix(ClEUNW3, type = "Species", min5 = FALSE)[,1])), 
                   rep(4, length(Presence_Absence_matrix(ClEUCN3, type = "Species", min5 = FALSE)[,1])),
                   rep(5, length(Presence_Absence_matrix(ClEUGK3, type = "Species", min5 = FALSE)[,1])))
ClEUROPE_MN18 <- Presence_Absence_matrix(ClEU_MN18, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3EuropeMN18_FALSE <- PerSIMPER_onMatrix(ClEUROPE_MN18, Group_EU_MN18, NS = TRUE, Info = "OLD_MN18_EU")

#################################################################################
############### ASIE ############################################################
#################################################################################

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

summary(Liste.MN.ASIA[[1]])
str(Liste.MN.ASIA[[1]])

##### SORTIE LONG-LAT DES LOCALITE #####
Localite_Faune1Asie <- c(unique(MN1$NAME), unique(MN2$NAME), unique(MN3$NAME), unique(MN4$NAME), unique(MN5$NAME))
Localite_Faune1Asie <- sort(unique(Localite_Faune1Asie))

Localite_Faune2Asie <- c(unique(MN6$NAME), unique(MN7_8$NAME), unique(MN9$NAME), unique(MN10$NAME), unique(MN11$NAME),
                         unique(MN12$NAME), unique(MN13$NAME))
Localite_Faune2Asie <- sort(unique(Localite_Faune2Asie))

Localite_Faune3Asie <- c(unique(MN14$NAME), unique(MN15$NAME), unique(MN16$NAME), unique(MN17$NAME), unique(MN18$NAME))
Localite_Faune3Asie <- sort(unique(Localite_Faune3Asie))
LocaliteAsie <- list(Localite_Faune1Asie, Localite_Faune2Asie, Localite_Faune3Asie)
capture.output(LocaliteAsie, file = "LocaliteAsie.txt")

### MN2
ClASI_Chine <- subset(MN2, MN2$LONG > 80 & MN2$LONG < 115 & MN2$LAT > 35 & MN2$LAT < 48)
ClASI_SudEst <- subset(MN2, MN2$LONG > 90 & MN2$LONG < 120 & MN2$LAT > 0 & MN2$LAT < 35)
ClASI_Inde <- subset(MN2, MN2$LONG > 55 & MN2$LONG < 85 & MN2$LAT > 20 & MN2$LAT < 35)
ClASI_Centrale <- subset(MN2, MN2$LONG > 30 & MN2$LONG < 51 & MN2$LAT > 20 & MN2$LAT < 35)
ClASI_Turquie <- subset(MN2, MN2$LONG > 20 & MN2$LONG < 43 & MN2$LAT > 36 & MN2$LAT < 45)

ClASI_MN2 <- rbind(ClASI_Chine, ClASI_Inde)
Group_ASI_MN2 <- c(rep(1,10), rep(2,5))
ClASIE_MN2 <- Presence_Absence_matrix(ClASI_MN2, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1AsiaMN2_FALSE <- delta.ses_boris(ClASIE_MN2, Group_ASI_MN2)
dev.print(device = pdf, file = "MN2_ASI.pdf", width = 10)

## Oldies
ClEMED1 <- subset(MN2, MN2$LONG < 42 & MN2$LAT >= 35 & MN2$LAT < 42)
ClNord1 <- subset(MN2, MN2$LONG >= 58 & MN2$LAT >= 42)
ClWAS1 <- subset(MN2, MN2$LONG < 67 & MN2$LAT < 35)
ClInde1 <- subset(MN2, MN2$LONG >= 67 & MN2$LONG < 98 & MN2$LAT < 34)
ClSudChine1 <- subset(MN2, MN2$LONG >= 94 & MN2$LAT < 42 & MN2$LAT >= 32)
ClSEAS1 <- subset(MN2, MN2$LONG >= 98 & MN2$LAT < 32)

ClASI_MN2 <- rbind(ClNord1, ClSudChine1)
Group_ASI_MN2 <- c(rep(1,10), rep(2,18))
ClASIE_MN2 <- Presence_Absence_matrix(ClASI_MN2, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1AsiaMN2_FALSE <- delta.ses_boris(ClASIE_MN2, Group_ASI_MN2)
dev.print(device = pdf, file = "OLD_MN2_ASI.pdf", width = 10)

### MN3
ClASI_Chine <- subset(MN3, MN3$LONG > 80 & MN3$LONG < 115 & MN3$LAT > 35 & MN3$LAT < 48)
ClASI_Inde <- subset(MN3, MN3$LONG > 55 & MN3$LONG < 85 & MN3$LAT > 20 & MN3$LAT < 35)
ClASI_SudEst <- subset(MN3, MN3$LONG > 90 & MN3$LONG < 120 & MN3$LAT > 0 & MN3$LAT < 35)
ClASI_Turquie <- subset(MN3, MN3$LONG > 20 & MN3$LONG < 43 & MN3$LAT > 36 & MN3$LAT < 45)
ClASI_Centrale <- subset(MN3, MN3$LONG > 30 & MN3$LONG < 51 & MN3$LAT > 20 & MN3$LAT < 35)

ClASI_MN3 <- rbind(ClASI_Chine, ClASI_Inde, ClASI_SudEst, ClASI_Turquie)
Group_ASI_MN3 <- c(rep(1,6), rep(2,5), rep(3,4), rep(4,10))
ClASIE_MN3 <- Presence_Absence_matrix(ClASI_MN3, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1AsiaMN3_FALSE <- PerSIMPER_onMatrix(ClASIE_MN3, Group_ASI_MN3, NS = TRUE, Info = "MN3_ASI")

## Oldies
ClEMED1 <- subset(MN3, MN3$LONG < 42 & MN3$LAT >= 35 & MN3$LAT < 42)
ClNord1 <- subset(MN3, MN3$LONG >= 58 & MN3$LAT >= 42)
ClWAS1 <- subset(MN3, MN3$LONG < 67 & MN3$LAT < 35)
ClInde1 <- subset(MN3, MN3$LONG >= 67 & MN3$LONG < 98 & MN3$LAT < 34)
ClSudChine1 <- subset(MN3, MN3$LONG >= 94 & MN3$LAT < 42 & MN3$LAT >= 32)
ClSEAS1 <- subset(MN3, MN3$LONG >= 98 & MN3$LAT < 32)

ClASI_MN3 <- rbind(ClEMED1, ClNord1, ClInde1, ClSudChine1)
Group_ASI_MN3 <- c(rep(1,10), rep(2,11), rep(3,5), rep(4,18))
ClASIE_MN3 <- Presence_Absence_matrix(ClASI_MN3, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1AsiaMN3_FALSE <- PerSIMPER_onMatrix(ClASIE_MN3, Group_ASI_MN3, NS = TRUE, Info = "OLD_MN3_ASI")

### MN4
ClASI_Chine <- subset(MN4, MN4$LONG > 80 & MN4$LONG < 115 & MN4$LAT > 35 & MN4$LAT < 48)
ClASI_Inde <- subset(MN4, MN4$LONG > 55 & MN4$LONG < 85 & MN4$LAT > 20 & MN4$LAT < 35)
ClASI_SudEst <- subset(MN4, MN4$LONG > 90 & MN4$LONG < 120 & MN4$LAT > 0 & MN4$LAT < 35)
ClASI_Turquie <- subset(MN4, MN4$LONG > 20 & MN4$LONG < 43 & MN4$LAT > 36 & MN4$LAT < 45)
ClASI_Centrale <- subset(MN4, MN4$LONG > 30 & MN4$LONG < 51 & MN4$LAT > 20 & MN4$LAT < 35)

ClASI_MN4 <- rbind(ClASI_Chine, ClASI_SudEst, ClASI_Turquie)
Group_ASI_MN4 <- c(rep(1,6), rep(2,5), rep(3,15))
ClASIE_MN4 <- Presence_Absence_matrix(ClASI_MN4, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1AsiaMN4_FALSE <- PerSIMPER_onMatrix(ClASIE_MN4, Group_ASI_MN4, NS = TRUE, Info = "MN4_ASI")

ClEMED1 <- subset(MN4, MN4$LONG < 42 & MN4$LAT >= 35 & MN4$LAT < 42)
ClNord1 <- subset(MN4, MN4$LONG >= 58 & MN4$LAT >= 42)
ClWAS1 <- subset(MN4, MN4$LONG < 67 & MN4$LAT < 35)
ClInde1 <- subset(MN4, MN4$LONG >= 67 & MN4$LONG < 98 & MN4$LAT < 34)
ClSudChine1 <- subset(MN4, MN4$LONG >= 94 & MN4$LAT < 42 & MN4$LAT >= 32)
ClSEAS1 <- subset(MN4, MN4$LONG >= 98 & MN4$LAT < 32)

ClASI_MN4 <- rbind(ClEMED1, ClNord1, ClSudChine1)
Group_ASI_MN4 <- c(rep(1,15), rep(2,12), rep(3,18))
ClASIE_MN4 <- Presence_Absence_matrix(ClASI_MN4, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1AsiaMN4_FALSE <- PerSIMPER_onMatrix(ClASIE_MN4, Group_ASI_MN4, NS = TRUE, Info = "OLD_MN4_ASI")

### MN5
ClASI_Chine <- subset(MN5, MN5$LONG > 80 & MN5$LONG < 115 & MN5$LAT > 35 & MN5$LAT < 48)
ClASI_Inde <- subset(MN5, MN5$LONG > 55 & MN5$LONG < 85 & MN5$LAT > 20 & MN5$LAT < 35)
ClASI_SudEst <- subset(MN5, MN5$LONG > 90 & MN5$LONG < 120 & MN5$LAT > 0 & MN5$LAT < 35)
ClASI_Turquie <- subset(MN5, MN5$LONG > 20 & MN5$LONG < 43 & MN5$LAT > 36 & MN5$LAT < 45)
ClASI_Centrale <- subset(MN5, MN5$LONG > 30 & MN5$LONG < 51 & MN5$LAT > 20 & MN5$LAT < 35)

ClASI_MN5 <- rbind(ClASI_Chine, ClASI_Inde, ClASI_SudEst, ClASI_Turquie)
Group_ASI_MN5 <- c(rep(1,15), rep(2,10), rep(3,7), rep(4,14))
ClASIE_MN5 <- Presence_Absence_matrix(ClASI_MN5, type = "Species", min5 = FALSE, singletons = TRUE)
Faune1AsiaMN5_FALSE <- PerSIMPER_onMatrix(ClASIE_MN5, Group_ASI_MN5, NS = TRUE, Info = "MN5_ASI")

# Oldies
ClEMED1 <- subset(MN5, MN5$LONG < 42 & MN5$LAT >= 35 & MN5$LAT < 42)
ClNord1 <- subset(MN5, MN5$LONG >= 58 & MN5$LAT >= 42)
ClWAS1 <- subset(MN5, MN5$LONG < 67 & MN5$LAT < 35)
ClInde1 <- subset(MN5, MN5$LONG >= 67 & MN5$LONG < 98 & MN5$LAT < 34)
ClSudChine1 <- subset(MN5, MN5$LONG >= 94 & MN5$LAT < 42 & MN5$LAT >= 32)
ClSEAS1 <- subset(MN5, MN5$LONG >= 98 & MN5$LAT < 32)

ClASI_MN5 <- rbind(ClEMED1, ClNord1, ClInde1, ClSudChine1)
Group_ASI_MN5 <- c(rep(1,13), rep(2,20), rep(3,10), rep(4,29))
ClASIE_MN5 <- Presence_Absence_matrix(ClASI_MN5, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune1AsiaMN5_FALSE <- PerSIMPER_onMatrix(ClASIE_MN5, Group_ASI_MN5, NS = TRUE, Info = "OLD_MN5_ASI")

### FAUNE 2 #################################################

### MN6
ClASI_Chine <- subset(MN6, MN6$LONG > 94 & MN6$LONG < 120 & MN6$LAT > 33.45 & MN6$LAT < 42.2)
ClASI_ChineNord <- subset(MN6, MN6$LONG > 96 & MN6$LONG < 120 & MN6$LAT > 42.2 & MN6$LAT < 45)
ClASI_Nord <- subset(MN6, MN6$LONG > 50 & MN6$LONG < 93.4 & MN6$LAT > 38.5)
ClASI_Inde <- subset(MN6, MN6$LONG > 74.8 & MN6$LONG < 86 & MN6$LAT > 20 & MN6$LAT < 35)
ClASI_SudEst <- subset(MN6, MN6$LONG > 90 & MN6$LONG < 120 & MN6$LAT > 0 & MN6$LAT < 33.4)
ClASI_Centrale <- subset(MN6, MN6$LONG > 48 & MN6$LONG < 74 & MN6$LAT > 15 & MN6$LAT < 33)
ClASI_Turquie <- subset(MN6, MN6$LONG > 20 & MN6$LONG < 49 & MN6$LAT > 30 & MN6$LAT < 46)

ClASI_MN6 <- rbind(ClASI_Chine, ClASI_Nord, ClASI_Inde, ClASI_SudEst, ClASI_Centrale, ClASI_Turquie)
Group_ASI_MN6 <- c(rep(1,17), rep(3,13), rep(4,4), rep(5,5), rep(6,8), rep(7,17))
ClASIE_MN6 <- Presence_Absence_matrix(ClASI_MN6, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2AsiaMN6_FALSE <- PerSIMPER_onMatrix(ClASIE_MN6, Group_ASI_MN6, NS = TRUE, Info = "MN6_ASI")

## Oldies
ClEMED1 <- subset(MN6, MN6$LONG < 42 & MN6$LAT >= 35 & MN6$LAT < 42)
ClNord1 <- subset(MN6, MN6$LONG >= 58 & MN6$LAT >= 42)
ClWAS1 <- subset(MN6, MN6$LONG < 67 & MN6$LAT < 35)
ClInde1 <- subset(MN6, MN6$LONG >= 67 & MN6$LONG < 98 & MN6$LAT < 34)
ClSudChine1 <- subset(MN6, MN6$LONG >= 94 & MN6$LAT < 42 & MN6$LAT >= 32)
ClSEAS1 <- subset(MN6, MN6$LONG >= 98 & MN6$LAT < 32)

ClASI_MN6 <- rbind(ClEMED1, ClNord1, ClInde1, ClSudChine1)
Group_ASI_MN6 <- c(rep(1,16), rep(2,17), rep(3,10), rep(4,31))
ClASIE_MN6 <- Presence_Absence_matrix(ClASI_MN6, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2AsiaMN6_FALSE <- PerSIMPER_onMatrix(ClASIE_MN6, Group_ASI_MN6, NS = TRUE, Info = "OLD_MN6_ASI")

### MN7_8
ClASI_Chine <- subset(MN7_8, MN7_8$LONG > 94 & MN7_8$LONG < 120 & MN7_8$LAT > 33.45 & MN7_8$LAT < 42.2)
ClASI_ChineNord <- subset(MN7_8, MN7_8$LONG > 96 & MN7_8$LONG < 120 & MN7_8$LAT > 42.2 & MN7_8$LAT < 45)
ClASI_Nord <- subset(MN7_8, MN7_8$LONG > 50 & MN7_8$LONG < 93.4 & MN7_8$LAT > 38.5)
ClASI_Inde <- subset(MN7_8, MN7_8$LONG > 74.8 & MN7_8$LONG < 86 & MN7_8$LAT > 20 & MN7_8$LAT < 35)
ClASI_SudEst <- subset(MN7_8, MN7_8$LONG > 90 & MN7_8$LONG < 120 & MN7_8$LAT > 0 & MN7_8$LAT < 33.4)
ClASI_Centrale <- subset(MN7_8, MN7_8$LONG > 48 & MN7_8$LONG < 74 & MN7_8$LAT > 15 & MN7_8$LAT < 33)
ClASI_Turquie <- subset(MN7_8, MN7_8$LONG > 20 & MN7_8$LONG < 49 & MN7_8$LAT > 30 & MN7_8$LAT < 46)

ClASI_MN7_8 <- rbind(ClASI_Chine, ClASI_ChineNord, ClASI_Inde, ClASI_SudEst, ClASI_Centrale, ClASI_Turquie)
Group_ASI_MN7_8 <- c(rep(1,17), rep(2,8), rep(3,4), rep(4,7), rep(5,7), rep(6,19))
ClASIE_MN7_8 <- Presence_Absence_matrix(ClASI_MN7_8, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2AsiaMN7_8_FALSE <- PerSIMPER_onMatrix(ClASIE_MN7_8, Group_ASI_MN7_8, NS = TRUE, Info = "MN7_8_ASI")

ClEMED1 <- subset(MN7_8, MN7_8$LONG < 42 & MN7_8$LAT >= 35 & MN7_8$LAT < 42)
ClNord1 <- subset(MN7_8, MN7_8$LONG >= 58 & MN7_8$LAT >= 42)
ClWAS1 <- subset(MN7_8, MN7_8$LONG < 67 & MN7_8$LAT < 35)
ClInde1 <- subset(MN7_8, MN7_8$LONG >= 67 & MN7_8$LONG < 98 & MN7_8$LAT < 34)
ClSudChine1 <- subset(MN7_8, MN7_8$LONG >= 94 & MN7_8$LAT < 42 & MN7_8$LAT >= 32)
ClSEAS1 <- subset(MN7_8, MN7_8$LONG >= 98 & MN7_8$LAT < 32)

ClASI_MN7_8 <- rbind(ClEMED1, ClNord1, ClInde1, ClSudChine1)
Group_ASI_MN7_8 <- c(rep(1,15), rep(2,12), rep(3,12), rep(4,22))
ClASIE_MN7_8 <- Presence_Absence_matrix(ClASI_MN7_8, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2AsiaMN7_8_FALSE <- PerSIMPER_onMatrix(ClASIE_MN7_8, Group_ASI_MN7_8, NS = TRUE, Info = "OLD_MN7_8_ASI")

### MN9
ClASI_Chine <- subset(MN9, MN9$LONG > 94 & MN9$LONG < 120 & MN9$LAT > 33.45 & MN9$LAT < 42.2)
ClASI_ChineNord <- subset(MN9, MN9$LONG > 96 & MN9$LONG < 120 & MN9$LAT > 42.2 & MN9$LAT < 45)
ClASI_Nord <- subset(MN9, MN9$LONG > 50 & MN9$LONG < 93.4 & MN9$LAT > 38.5)
ClASI_Inde <- subset(MN9, MN9$LONG > 74.8 & MN9$LONG < 86 & MN9$LAT > 20 & MN9$LAT < 35)
ClASI_SudEst <- subset(MN9, MN9$LONG > 90 & MN9$LONG < 120 & MN9$LAT > 0 & MN9$LAT < 33.4)
ClASI_Centrale <- subset(MN9, MN9$LONG > 48 & MN9$LONG < 74 & MN9$LAT > 15 & MN9$LAT < 33)
ClASI_Turquie <- subset(MN9, MN9$LONG > 20 & MN9$LONG < 49 & MN9$LAT > 30 & MN9$LAT < 46)

ClASI_MN9 <- rbind(ClASI_Chine,  ClASI_Nord, ClASI_Turquie)
Group_ASI_MN9 <- c(rep(1,9), rep(2,6), rep(3,30))
ClASIE_MN9 <- Presence_Absence_matrix(ClASI_MN9, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2AsiaMN9_FALSE <- PerSIMPER_onMatrix(ClASIE_MN9, Group_ASI_MN9, NS = TRUE, Info = "MN9_ASI")

## Oldies
ClEMEDOuest2 <- subset(MN9, MN9$LONG < 29 & MN9$LAT >= 36 & MN9$LAT < 42)
ClEMEDEst2 <- subset(MN9, MN9$LONG >= 29 & MN9$LONG < 47 & MN9$LAT >= 33 & MN9$LAT < 42)
ClCentreAsie2 <- subset(MN9, MN9$LONG >= 47 & MN9$LAT < 44 & MN9$LAT >= 34 & MN9$LONG < 96)
ClNord2 <- subset(MN9, MN9$LAT >= 43)
ClNordChine2 <- subset(MN9, MN9$LONG >= 111 & MN9$LAT < 43 & MN9$LAT >= 40)
ClWAS2 <- subset(MN9, MN9$LONG < 67 & MN9$LAT < 33)
ClInde2 <- subset(MN9, MN9$LONG >= 67 & MN9$LONG < 93 & MN9$LAT < 34)
ClSudChine2 <- subset(MN9, MN9$LONG >= 93 & MN9$LAT < 40 & MN9$LAT >= 31)
ClSEAS2 <- subset(MN9, MN9$LONG >= 98 & MN9$LAT < 31)

ClASI_MN9 <- rbind(ClEMEDOuest2,  ClEMEDEst2, ClNord2, ClInde2, ClSudChine2)
Group_ASI_MN9 <- c(rep(1,9), rep(2,20), rep(3,7), rep(4,6), rep(5,9))
ClASIE_MN9 <- Presence_Absence_matrix(ClASI_MN9, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2AsiaMN9_FALSE <- PerSIMPER_onMatrix(ClASIE_MN9, Group_ASI_MN9, NS = TRUE, Info = "OLD_MN9_ASI")

### MN10
ClASI_Chine <- subset(MN10, MN10$LONG > 94 & MN10$LONG < 120 & MN10$LAT > 33.45 & MN10$LAT < 42.2)
ClASI_ChineNord <- subset(MN10, MN10$LONG > 96 & MN10$LONG < 120 & MN10$LAT > 42.2 & MN10$LAT < 45)
ClASI_Nord <- subset(MN10, MN10$LONG > 50 & MN10$LONG < 93.4 & MN10$LAT > 38.5)
ClASI_Inde <- subset(MN10, MN10$LONG > 74.8 & MN10$LONG < 89 & MN10$LAT > 20 & MN10$LAT < 35)
ClASI_SudEst <- subset(MN10, MN10$LONG > 90 & MN10$LONG < 120 & MN10$LAT > 0 & MN10$LAT < 33.4)
ClASI_Centrale <- subset(MN10, MN10$LONG > 48 & MN10$LONG < 74 & MN10$LAT > 15 & MN10$LAT < 33)
ClASI_Turquie <- subset(MN10, MN10$LONG > 20 & MN10$LONG < 49 & MN10$LAT > 30 & MN10$LAT < 46)

ClASI_MN10 <- rbind(ClASI_Chine,  ClASI_Nord, ClASI_Turquie)
Group_ASI_MN10 <- c(rep(1,9), rep(2,5), rep(3,30))
ClASIE_MN10 <- Presence_Absence_matrix(ClASI_MN10, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2AsiaMN10_FALSE <- PerSIMPER_onMatrix(ClASIE_MN10, Group_ASI_MN10, NS = TRUE, Info = "MN10_ASI")

## Oldies
ClEMEDOuest2 <- subset(MN10, MN10$LONG < 29 & MN10$LAT >= 36 & MN10$LAT < 42)
ClEMEDEst2 <- subset(MN10, MN10$LONG >= 29 & MN10$LONG < 47 & MN10$LAT >= 33 & MN10$LAT < 42)
ClCentreAsie2 <- subset(MN10, MN10$LONG >= 47 & MN10$LAT < 44 & MN10$LAT >= 34 & MN10$LONG < 96)
ClNord2 <- subset(MN10, MN10$LAT >= 43)
ClNordChine2 <- subset(MN10, MN10$LONG >= 111 & MN10$LAT < 43 & MN10$LAT >= 40)
ClWAS2 <- subset(MN10, MN10$LONG < 67 & MN10$LAT < 33)
ClInde2 <- subset(MN10, MN10$LONG >= 67 & MN10$LONG < 93 & MN10$LAT < 34)
ClSudChine2 <- subset(MN10, MN10$LONG >= 93 & MN10$LAT < 40 & MN10$LAT >= 31)
ClSEAS2 <- subset(MN10, MN10$LONG >= 98 & MN10$LAT < 32)

ClASI_MN10 <- rbind(ClEMEDOuest2,  ClEMEDEst2, ClNord2, ClInde2, ClSudChine2)
Group_ASI_MN10 <- c(rep(1,6), rep(2,23), rep(3,5), rep(4,5), rep(5,11))
ClASIE_MN10 <- Presence_Absence_matrix(ClASI_MN10, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2AsiaMN10_FALSE <- PerSIMPER_onMatrix(ClASIE_MN10, Group_ASI_MN10, NS = TRUE, Info = "OLD_MN10_ASI")

### MN11
ClASI_Chine <- subset(MN11, MN11$LONG > 94 & MN11$LONG < 120 & MN11$LAT > 33.45 & MN11$LAT < 42.2)
ClASI_ChineNord <- subset(MN11, MN11$LONG > 96 & MN11$LONG < 120 & MN11$LAT > 42.2 & MN11$LAT < 45)
ClASI_Nord <- subset(MN11, MN11$LONG > 50 & MN11$LONG < 93.4 & MN11$LAT > 38.5)
ClASI_Inde <- subset(MN11, MN11$LONG > 74.8 & MN11$LONG < 86 & MN11$LAT > 20 & MN11$LAT < 35)
ClASI_SudEst <- subset(MN11, MN11$LONG > 90 & MN11$LONG < 120 & MN11$LAT > 0 & MN11$LAT < 33.4)
ClASI_Centrale <- subset(MN11, MN11$LONG > 48 & MN11$LONG < 74 & MN11$LAT > 15 & MN11$LAT < 33)
ClASI_Turquie <- subset(MN11, MN11$LONG > 20 & MN11$LONG < 49 & MN11$LAT > 30 & MN11$LAT < 46)

ClASI_MN11 <- rbind(ClASI_Chine,  ClASI_Nord, ClASI_Turquie)
Group_ASI_MN11 <- c(rep(1,27), rep(2,9), rep(3,46))
ClASIE_MN11 <- Presence_Absence_matrix(ClASI_MN11, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2AsiaMN11_FALSE <- PerSIMPER_onMatrix(ClASIE_MN11, Group_ASI_MN11, NS = TRUE, Info = "MN11_ASI")

#Old way
ClEMEDOuest2 <- subset(MN11, MN11$LONG < 29 & MN11$LAT >= 36 & MN11$LAT < 42)
ClEMEDEst2 <- subset(MN11, MN11$LONG >= 29 & MN11$LONG < 47 & MN11$LAT >= 33 & MN11$LAT < 42)
ClCentreAsie2 <- subset(MN11, MN11$LONG >= 47 & MN11$LAT < 44 & MN11$LAT >= 34 & MN11$LONG < 96)
ClNord2 <- subset(MN11, MN11$LAT >= 44)
ClNordChine2 <- subset(MN11, MN11$LONG >= 111 & MN11$LAT < 43 & MN11$LAT >= 40)
ClWAS2 <- subset(MN11, MN11$LONG < 67 & MN11$LAT < 33)
ClInde2 <- subset(MN11, MN11$LONG >= 67 & MN11$LONG < 93 & MN11$LAT < 34)
ClSudChine2 <- subset(MN11, MN11$LONG >= 93 & MN11$LAT < 40 & MN11$LAT >= 31)
ClSEAS2 <- subset(MN11, MN11$LONG >= 98 & MN11$LAT < 32)

ClASI_MN11 <- rbind(ClEMEDOuest2,  ClEMEDEst2, ClCentreAsie2, ClNord2, ClNordChine2, ClInde2, ClSudChine2)
Group_ASI_MN11 <- c(rep(1,10), rep(2,34), rep(3,5), rep(4,10), rep(5,5), rep(6,5), rep(7, 27))
ClASIE_MN11 <- Presence_Absence_matrix(ClASI_MN11, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2AsiaMN11_FALSE <- PerSIMPER_onMatrix(ClASIE_MN11, Group_ASI_MN11, NS = TRUE, Info = "OLD_MN11_ASI")

### MN12
ClASI_Chine <- subset(MN12, MN12$LONG > 94 & MN12$LONG < 120 & MN12$LAT > 33.45 & MN12$LAT < 42.2)
ClASI_ChineNord <- subset(MN12, MN12$LONG > 96 & MN12$LONG < 120 & MN12$LAT > 42.2 & MN12$LAT < 45)
ClASI_Nord <- subset(MN12, MN12$LONG > 50 & MN12$LONG < 93.4 & MN12$LAT > 38.5)
ClASI_Inde <- subset(MN12, MN12$LONG > 74.8 & MN12$LONG < 86 & MN12$LAT > 20 & MN12$LAT < 35)
ClASI_SudEst <- subset(MN12, MN12$LONG > 90 & MN12$LONG < 120 & MN12$LAT > 0 & MN12$LAT < 33.4)
ClASI_Centrale <- subset(MN12, MN12$LONG > 48 & MN12$LONG < 74 & MN12$LAT > 15 & MN12$LAT < 33)
ClASI_Turquie <- subset(MN12, MN12$LONG > 20 & MN12$LONG < 49 & MN12$LAT > 30 & MN12$LAT < 46)

ClASI_MN12 <- rbind(ClASI_Chine,  ClASI_Nord, ClASI_Inde, ClASI_Centrale, ClASI_Turquie)
Group_ASI_MN12 <- c(rep(1,25), rep(2,8), rep(3,6), rep(4,7), rep(5,53))
ClASIE_MN12 <- Presence_Absence_matrix(ClASI_MN12, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2AsiaMN12_FALSE <- PerSIMPER_onMatrix(ClASIE_MN12, Group_ASI_MN12, NS = TRUE, Info = "MN12_ASI")

#Old way
ClEMEDOuest2 <- subset(MN12, MN12$LONG < 29 & MN12$LAT >= 36 & MN12$LAT < 42)
ClEMEDEst2 <- subset(MN12, MN12$LONG >= 29 & MN12$LONG < 47 & MN12$LAT >= 33 & MN12$LAT < 42)
ClCentreAsie2 <- subset(MN12, MN12$LONG >= 47 & MN12$LAT < 44 & MN12$LAT >= 34 & MN12$LONG < 96)
ClNord2 <- subset(MN12, MN12$LAT >= 44)
ClNordChine2 <- subset(MN12, MN12$LONG >= 111 & MN12$LAT < 43 & MN12$LAT >= 40)
ClWAS2 <- subset(MN12, MN12$LONG < 67 & MN12$LAT < 33)
ClInde2 <- subset(MN12, MN12$LONG >= 67 & MN12$LONG < 93 & MN12$LAT < 34)
ClSudChine2 <- subset(MN12, MN12$LONG >= 93 & MN12$LAT < 40 & MN12$LAT >= 31)
ClSEAS2 <- subset(MN12, MN12$LONG >= 98 & MN12$LAT < 32)

ClASI_MN12 <- rbind(ClEMEDOuest2,  ClEMEDEst2, ClCentreAsie2, ClNord2, ClInde2, ClSudChine2, ClWAS2)
Group_ASI_MN12 <- c(rep(1,10), rep(2,40), rep(3,6), rep(4,12), rep(5,9), rep(6,25), rep(7,5))
ClASIE_MN12 <- Presence_Absence_matrix(ClASI_MN12, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2AsiaMN12_FALSE <- PerSIMPER_onMatrix(ClASIE_MN12, Group_ASI_MN12, NS = TRUE, Info = "OLD_MN12_ASI")

### MN13
ClASI_Chine <- subset(MN13, MN13$LONG > 94 & MN13$LONG < 120 & MN13$LAT > 33.45 & MN13$LAT < 42.2)
ClASI_ChineNord <- subset(MN13, MN13$LONG > 96 & MN13$LONG < 120 & MN13$LAT > 42.2 & MN13$LAT < 45)
ClASI_Nord <- subset(MN13, MN13$LONG > 50 & MN13$LONG < 93.4 & MN13$LAT > 38.5)
ClASI_Inde <- subset(MN13, MN13$LONG > 74.8 & MN13$LONG < 86 & MN13$LAT > 20 & MN13$LAT < 35)
ClASI_SudEst <- subset(MN13, MN13$LONG > 90 & MN13$LONG < 120 & MN13$LAT > 0 & MN13$LAT < 33.4)
ClASI_Centrale <- subset(MN13, MN13$LONG > 48 & MN13$LONG < 74 & MN13$LAT > 15 & MN13$LAT < 33)
ClASI_Turquie <- subset(MN13, MN13$LONG > 20 & MN13$LONG < 49 & MN13$LAT > 30 & MN13$LAT < 46)

ClASI_MN13 <- rbind(ClASI_Chine,  ClASI_Nord, ClASI_Inde, ClASI_Centrale, ClASI_Turquie, ClASI_SudEst)
Group_ASI_MN13 <- c(rep(1,34), rep(2,12), rep(3,8), rep(4,7), rep(5,20), rep(6,5))
ClASIE_MN13 <- Presence_Absence_matrix(ClASI_MN13, type = "Species", min5 = FALSE, singletons = TRUE)
Faune2AsiaMN13_FALSE <- PerSIMPER_onMatrix(ClASIE_MN13, Group_ASI_MN13, NS = TRUE, Info = "MN13_ASI")

#Old way
ClEMEDOuest2 <- subset(MN13, MN13$LONG < 29 & MN13$LAT >= 36 & MN13$LAT < 42)
ClEMEDEst2 <- subset(MN13, MN13$LONG >= 29 & MN13$LONG < 47 & MN13$LAT >= 33 & MN13$LAT < 42)
ClCentreAsie2 <- subset(MN13, MN13$LONG >= 47 & MN13$LAT < 44 & MN13$LAT >= 34 & MN13$LONG < 96)
ClNord2 <- subset(MN13, MN13$LAT >= 44)
ClNordChine2 <- subset(MN13, MN13$LONG >= 111 & MN13$LAT < 43 & MN13$LAT >= 40)
ClWAS2 <- subset(MN13, MN13$LONG < 67 & MN13$LAT < 33)
ClInde2 <- subset(MN13, MN13$LONG >= 67 & MN13$LONG < 93 & MN13$LAT < 34)
ClSudChine2 <- subset(MN13, MN13$LONG >= 93 & MN13$LAT < 40 & MN13$LAT >= 31)
ClSEAS2 <- subset(MN13, MN13$LONG >= 98 & MN13$LAT < 32)

ClASI_MN13 <- rbind(ClEMEDEst2, ClCentreAsie2, ClNord2, ClNordChine2, ClWAS2, ClInde2, ClSudChine2)
Group_ASI_MN13 <- c(rep(1,16), rep(2,11), rep(3,13), rep(4,8), rep(5,5), rep(6,10), rep(7,32))
ClASIE_MN13 <- Presence_Absence_matrix(ClASI_MN13, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune2AsiaMN13_FALSE <- PerSIMPER_onMatrix(ClASIE_MN13, Group_ASI_MN13, NS = TRUE, Info = "OLD_MN13_ASI")

#################### FAUNE 3 #######################

### MN14
ClASI_ChineSud <- subset(MN14, MN14$LONG > 94 & MN14$LONG < 120 & MN14$LAT > 18 & MN14$LAT < 30.8)
ClASI_Nord <- subset(MN14, MN14$LONG > 20 & MN14$LONG < 130 & MN14$LAT > 30.8)
ClASI_Centrale <- subset(MN14, MN14$LONG > 48 & MN14$LONG < 60 & MN14$LAT > 15 & MN14$LAT < 30)
ClASI_Nord <- rbind(ClASI_Nord, ClASI_Centrale)
ClASI_Inde <- subset(MN14, MN14$LONG > 70 & MN14$LONG < 90 & MN14$LAT > 5 & MN14$LAT <= 30.8)
ClASI_SudEst <- subset(MN14, MN14$LONG > 91 & MN14$LONG < 130 & MN14$LAT > -10 & MN14$LAT < 17)

ClASI_MN14 <- rbind(ClASI_Nord, ClASI_Inde)
Group_ASI_MN14 <- c(rep(1,56), rep(2,5))
ClASIE_MN14 <- Presence_Absence_matrix(ClASI_MN14, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AsiaMN14_FALSE <- delta.ses_boris(ClASIE_MN14, Group_ASI_MN14)
dev.print(device = pdf, file = "MN14_ASI.pdf", width = 10)

#Oldies
ClEMED3 <- subset(MN14, MN14$LONG < 54 & MN14$LAT >= 35 & MN14$LAT < 48)
ClWAS3 <- subset(MN14, MN14$LONG < 67 & MN14$LAT < 35)
ClCentreAsie3 <- subset(MN14, MN14$LONG >= 67 & MN14$LAT < 35 & MN14$LAT >= 29 & MN14$LONG < 86)
ClNordEst3 <- subset(MN14, MN14$LAT >= 47 & MN14$LONG >= 103)
ClNord3 <- subset(MN14, MN14$LAT >= 35 & MN14$LONG >= 54 & MN14$LONG < 100)
ClNordChine3 <- subset(MN14, MN14$LONG >= 100 & MN14$LAT < 45 & MN14$LAT >= 31)
ClSEAS3 <- subset(MN14, MN14$LONG >= 94 & MN14$LAT < 31)
ClInde3 <- subset(MN14, MN14$LONG >= 67 & MN14$LONG < 93 & MN14$LAT < 29)

ClASI_MN14 <- rbind(ClEMED3, ClCentreAsie3, ClNord3, ClNordChine3)
Group_ASI_MN14 <- c(rep(1,18), rep(2,7), rep(3,11), rep(4,21))
ClASIE_MN14 <- Presence_Absence_matrix(ClASI_MN14, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AsiaMN14_FALSE <- PerSIMPER_onMatrix(ClASIE_MN14, Group_ASI_MN14, NS = TRUE, Info = "OLD_MN14_ASI")

### MN15
ClASI_ChineSud <- subset(MN15, MN15$LONG > 94 & MN15$LONG < 120 & MN15$LAT > 18 & MN15$LAT < 30.8)
ClASI_Nord <- subset(MN15, MN15$LONG > 20 & MN15$LONG < 130 & MN15$LAT > 30.8)
ClASI_Centrale <- subset(MN15, MN15$LONG > 48 & MN15$LONG < 60 & MN15$LAT > 15 & MN15$LAT < 30)
ClASI_Nord <- rbind(ClASI_Nord, ClASI_Centrale)
ClASI_Inde <- subset(MN15, MN15$LONG > 70 & MN15$LONG < 90 & MN15$LAT > 5 & MN15$LAT <= 30.8)
ClASI_SudEst <- subset(MN15, MN15$LONG > 91 & MN15$LONG < 130 & MN15$LAT > -10 & MN15$LAT < 17)

ClASI_MN15 <- rbind(ClASI_ChineSud, ClASI_Nord, ClASI_Inde)
Group_ASI_MN15 <- c(rep(1,5), rep(2,53), rep(3,7))
ClASIE_MN15 <- Presence_Absence_matrix(ClASI_MN15, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AsiaMN15_FALSE <- PerSIMPER_onMatrix(ClASIE_MN15, Group_ASI_MN15, NS = TRUE, Info = "MN15_ASI")

#Oldies
ClEMED3 <- subset(MN15, MN15$LONG < 54 & MN15$LAT >= 35 & MN15$LAT < 48)
ClWAS3 <- subset(MN15, MN15$LONG < 67 & MN15$LAT < 35)
ClCentreAsie3 <- subset(MN15, MN15$LONG >= 67 & MN15$LAT < 35 & MN15$LAT >= 29 & MN15$LONG < 86)
ClNordEst3 <- subset(MN15, MN15$LAT >= 47 & MN15$LONG >= 103)
ClNord3 <- subset(MN15, MN15$LAT >= 35 & MN15$LONG >= 54 & MN15$LONG < 100)
ClNordChine3 <- subset(MN15, MN15$LONG >= 100 & MN15$LAT < 45 & MN15$LAT >= 31)
ClSEAS3 <- subset(MN15, MN15$LONG >= 94 & MN15$LAT < 31)
ClInde3 <- subset(MN15, MN15$LONG >= 67 & MN15$LONG < 93 & MN15$LAT < 29)

ClASI_MN15 <- rbind(ClEMED3, ClCentreAsie3, ClNord3, ClNordChine3, ClSEAS3, ClInde3)
Group_ASI_MN15 <- c(rep(1,14), rep(2,10), rep(3,11), rep(4,22), rep(5,7), rep(6,5))
ClASIE_MN15 <- Presence_Absence_matrix(ClASI_MN15, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AsiaMN15_FALSE <- PerSIMPER_onMatrix(ClASIE_MN15, Group_ASI_MN15, NS = TRUE, Info = "OLD_MN15_ASI")

### MN16
ClASI_ChineSud <- subset(MN16, MN16$LONG > 94 & MN16$LONG < 120 & MN16$LAT > 18 & MN16$LAT < 30.8)
ClASI_Nord <- subset(MN16, MN16$LONG > 20 & MN16$LONG < 130 & MN16$LAT > 30.8)
ClASI_Centrale <- subset(MN16, MN16$LONG > 48 & MN16$LONG < 60 & MN16$LAT > 15 & MN16$LAT < 30)
ClASI_Nord <- rbind(ClASI_Nord, ClASI_Centrale)
ClASI_Inde <- subset(MN16, MN16$LONG > 70 & MN16$LONG < 90 & MN16$LAT > 5 & MN16$LAT <= 30.8)
ClASI_SudEst <- subset(MN16, MN16$LONG > 91 & MN16$LONG < 130 & MN16$LAT > -10 & MN16$LAT < 17)

ClASI_MN16 <- rbind(ClASI_Nord, ClASI_Inde)
Group_ASI_MN16 <- c(rep(1,70), rep(2,4))
ClASIE_MN16 <- Presence_Absence_matrix(ClASI_MN16, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AsiaMN16_FALSE <- delta.ses_boris(ClASIE_MN16, Group_ASI_MN16)
dev.print(device = pdf, file = "MN16_ASI.pdf", width = 10)


#Oldies
ClEMED3 <- subset(MN16, MN16$LONG < 54 & MN16$LAT >= 35 & MN16$LAT < 48)
ClWAS3 <- subset(MN16, MN16$LONG < 67 & MN16$LAT < 35)
ClCentreAsie3 <- subset(MN16, MN16$LONG >= 67 & MN16$LAT < 35 & MN16$LAT >= 29 & MN16$LONG < 86)
ClNordEst3 <- subset(MN16, MN16$LAT >= 47 & MN16$LONG >= 103)
ClNord3 <- subset(MN16, MN16$LAT >= 35 & MN16$LONG >= 54 & MN16$LONG < 100)
ClNordChine3 <- subset(MN16, MN16$LONG >= 100 & MN16$LAT < 45 & MN16$LAT >= 31)
ClSEAS3 <- subset(MN16, MN16$LONG >= 94 & MN16$LAT < 31)
ClInde3 <- subset(MN16, MN16$LONG >= 67 & MN16$LONG < 93 & MN16$LAT < 29)

ClASI_MN16 <- rbind(ClEMED3, ClCentreAsie3, ClNord3, ClNordEst3, ClNordChine3, ClSEAS3)
Group_ASI_MN16 <- c(rep(1,16), rep(2,9), rep(3,20), rep(4,6), rep(5,21), rep(6,5))
ClASIE_MN16 <- Presence_Absence_matrix(ClASI_MN16, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AsiaMN16_FALSE <- PerSIMPER_onMatrix(ClASIE_MN16, Group_ASI_MN16, NS = TRUE, Info = "OLD_MN16_ASI")

### MN17
ClASI_ChineSud <- subset(MN17, MN17$LONG > 94 & MN17$LONG < 120 & MN17$LAT > 18 & MN17$LAT < 30.8)
ClASI_Nord <- subset(MN17, MN17$LONG > 20 & MN17$LONG < 130 & MN17$LAT > 30.8)
ClASI_Centrale <- subset(MN17, MN17$LONG > 48 & MN17$LONG < 60 & MN17$LAT > 15 & MN17$LAT < 30)
ClASI_Nord <- rbind(ClASI_Nord, ClASI_Centrale)
ClASI_Inde <- subset(MN17, MN17$LONG > 70 & MN17$LONG < 90 & MN17$LAT > 5 & MN17$LAT <= 30.8)
ClASI_SudEst <- subset(MN17, MN17$LONG > 91 & MN17$LONG < 130 & MN17$LAT > -10 & MN17$LAT < 17)

ClASI_MN17 <- rbind(ClASI_ChineSud,  ClASI_Nord, ClASI_Inde, ClASI_SudEst)
Group_ASI_MN17 <- c(rep(1,9), rep(3,101), rep(4,5), rep(5,18))
ClASIE_MN17 <- Presence_Absence_matrix(ClASI_MN17, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AsiaMN17_FALSE <- PerSIMPER_onMatrix(ClASIE_MN17, Group_ASI_MN17, NS = TRUE, Info = "MN17_ASI")

ClASI_MN17 <- rbind(ClASI_Nord, ClASI_Inde)
Group_ASI_MN17 <- c(rep(1,101), rep(2,5))
ClASIE_MN17 <- Presence_Absence_matrix(ClASI_MN17, type = "Species", min5 = FALSE, singletons = TRUE)
MN17test <- delta.ses_boris(ClASIE_MN17, Group_ASI_MN17)

#Oldies
ClEMED3 <- subset(MN17, MN17$LONG < 54 & MN17$LAT >= 35 & MN17$LAT < 48)
ClWAS3 <- subset(MN17, MN17$LONG < 67 & MN17$LAT < 35)
ClCentreAsie3 <- subset(MN17, MN17$LONG >= 67 & MN17$LAT < 35 & MN17$LAT >= 29 & MN17$LONG < 86)
ClNordEst3 <- subset(MN17, MN17$LAT >= 47 & MN17$LONG >= 103)
ClNord3 <- subset(MN17, MN17$LAT >= 35 & MN17$LONG >= 54 & MN17$LONG < 100)
ClNordChine3 <- subset(MN17, MN17$LONG >= 100 & MN17$LAT < 45 & MN17$LAT >= 31)
ClSEAS3 <- subset(MN17, MN17$LONG >= 94 & MN17$LAT < 31)
ClInde3 <- subset(MN17, MN17$LONG >= 67 & MN17$LONG < 93 & MN17$LAT < 29)

ClASI_MN17 <- rbind(ClEMED3, ClWAS3, ClCentreAsie3, ClNordEst3, ClNord3, ClNordChine3, ClSEAS3)
Group_ASI_MN17 <- c(rep(1,33), rep(2,5), rep(3,10), rep(4,12), rep(5,32), rep(6,16), rep(7,28))
ClASIE_MN17 <- Presence_Absence_matrix(ClASI_MN17, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AsiaMN17_FALSE <- PerSIMPER_onMatrix(ClASIE_MN17, Group_ASI_MN17, NS = TRUE, Info = "OLD_MN17_ASI")

### MN18
ClASI_ChineSud <- subset(MN18, MN18$LONG > 94 & MN18$LONG < 120 & MN18$LAT > 18 & MN18$LAT < 30.8)
ClASI_Nord <- subset(MN18, MN18$LONG > 20 & MN18$LONG < 130 & MN18$LAT > 30.8)
ClASI_Centrale <- subset(MN18, MN18$LONG > 48 & MN18$LONG < 60 & MN18$LAT > 15 & MN18$LAT < 30)
ClASI_Nord <- rbind(ClASI_Nord, ClASI_Centrale)
ClASI_Inde <- subset(MN18, MN18$LONG > 70 & MN18$LONG < 90 & MN18$LAT > 5 & MN18$LAT <= 30.8)
ClASI_SudEst <- subset(MN18, MN18$LONG > 91 & MN18$LONG < 130 & MN18$LAT > -10 & MN18$LAT < 17)

ClASI_MN18 <- rbind(ClASI_ChineSud,  ClASI_Nord, ClASI_Inde, ClASI_SudEst)
Group_ASI_MN18 <- c(rep(1,44), rep(2,114), rep(3,25), rep(4,51))
ClASIE_MN18 <- Presence_Absence_matrix(ClASI_MN18, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AsiaMN18_FALSE <- PerSIMPER_onMatrix(ClASIE_MN18, Group_ASI_MN18, NS = TRUE, Info = "MN18_ASI")

#Oldies
ClEMED3 <- subset(MN18, MN18$LONG < 54 & MN18$LAT >= 35 & MN18$LAT < 48)
ClWAS3 <- subset(MN18, MN18$LONG < 67 & MN18$LAT < 35)
ClCentreAsie3 <- subset(MN18, MN18$LONG >= 67 & MN18$LAT < 35 & MN18$LAT >= 29 & MN18$LONG < 86)
ClNordEst3 <- subset(MN18, MN18$LAT >= 47 & MN18$LONG >= 103)
ClNord3 <- subset(MN18, MN18$LAT >= 35 & MN18$LONG >= 54 & MN18$LONG < 100)
ClNordChine3 <- subset(MN18, MN18$LONG >= 100 & MN18$LAT < 45 & MN18$LAT >= 31)
ClSEAS3 <- subset(MN18, MN18$LONG >= 94 & MN18$LAT < 31)
ClInde3 <- subset(MN18, MN18$LONG >= 67 & MN18$LONG < 93 & MN18$LAT < 29)

ClASI_MN18 <- rbind(ClEMED3, ClWAS3, ClCentreAsie3, ClNordEst3, ClNord3, ClNordChine3, ClSEAS3, ClInde3)
Group_ASI_MN18 <- c(rep(1,41), rep(2,10), rep(3,5), rep(4,13), rep(5,15), rep(6,38), rep(7,100), rep(8,24))
ClASIE_MN18 <- Presence_Absence_matrix(ClASI_MN18, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AsiaMN18_FALSE <- PerSIMPER_onMatrix(ClASIE_MN18, Group_ASI_MN18, NS = TRUE, Info = "OLD_MN18_ASI")

#################################################################################
############### AFRIQUE #########################################################
#################################################################################

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
    names.dataframe.AFRICA <- paste(NOM_MN[i])
    N <- assign(names.dataframe.AFRICA, AFRICA.Grain1.terre[wantMN,])
    Liste.MN.AFRICA[[length(Liste.MN.AFRICA)+ 1]] <- N
  }
}

##### SORTIE LONG-LAT DES LOCALITE #####

Localite_Faune3Afrique <- c(unique(MN14$NAME), unique(MN15$NAME), unique(MN16$NAME), unique(MN17$NAME), unique(MN18$NAME))
Localite_Faune3Afrique <- sort(unique(Localite_Faune3Afrique))
capture.output(Localite_Faune3Afrique, file = "LocaliteAfrique.txt")


##### FAUNE 3 ##############

## MN14
ClAFR_Sud <- subset(MN14, MN14$LONG > 15 & MN14$LONG < 27.6 & MN14$LAT > -40 & MN14$LAT < -29.1)
ClAFR_SudEst <- subset(MN14, MN14$LONG > 20 & MN14$LONG < 35 & MN14$LAT > -29.1 & MN14$LAT < -13)
ClAFR_Est <- subset(MN14, MN14$LONG > 27 & MN14$LONG < 45 & MN14$LAT > -12 & MN14$LAT < 14.7)
ClAFR_NordSA <- subset(MN14, MN14$LONG > -10 & MN14$LONG < 36.2 & MN14$LAT > 14.7)

ClAFR_MN14 <- rbind(ClAFR_Est, ClAFR_NordSA)
Group_AFR_MN14 <- c(rep(1,28), rep(2,7))
ClAFR_MN14 <- Presence_Absence_matrix(ClAFR_MN14, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AFRMN14_FALSE <- delta.ses_boris(ClAFR_MN14, Group_AFR_MN14)
dev.print(device = pdf, file = "MN14_AFR.pdf", width = 10)

## MN15
ClAFR_Sud <- subset(MN15, MN15$LONG > 15 & MN15$LONG < 27.6 & MN15$LAT > -40 & MN15$LAT < -29.1)
ClAFR_SudEst <- subset(MN15, MN15$LONG > 20 & MN15$LONG < 35 & MN15$LAT > -29.1 & MN15$LAT < -13)
ClAFR_Est <- subset(MN15, MN15$LONG > 27 & MN15$LONG < 45 & MN15$LAT > -12 & MN15$LAT < 14.7)
ClAFR_NordSA <- subset(MN15, MN15$LONG > -10 & MN15$LONG < 36.2 & MN15$LAT > 14.7)

ClAFR_MN15 <- rbind(ClAFR_SudEst, ClAFR_Est, ClAFR_NordSA)
Group_AFR_MN15 <- c(rep(1,5), rep(2,39), rep(3,10))
ClAFR_MN15 <- Presence_Absence_matrix(ClAFR_MN15, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AFRMN15_FALSE <- PerSIMPER_onMatrix(ClAFR_MN15, Group_AFR_MN15, NS = TRUE, Info = "MN15_AFR")

#Oldies
ClEst3 <- subset(MN15, MN15$LONG >= 29 & MN15$LAT >= -13 & MN15$LAT < 15)
ClEstNord3 <- subset(MN15, MN15$LONG >= 23 & MN15$LAT >= 15)
ClEstNord32 <- subset(MN15, MN15$LONG >= 20 & MN15$LONG < 23 & MN15$LAT >= 30 & MN15$LAT < 33)
ClCentre3 <- subset(MN15, MN15$LONG >= 9 & MN15$LONG < 20 & MN15$LAT >= 15 & MN15$LAT < 17)
ClNordOuest3 <- subset(MN15, MN15$LONG > -10 & MN15$LAT >= 31 & MN15$LONG < 11)
ClSud3 <- subset(MN15, MN15$LAT < -13)
ClEstNord3 <- join(ClEstNord3, ClEstNord32, match="all", type="full")
ClNord3 <- join(ClEstNord3, ClNordOuest3, match="all", type="full")
ClEst3 <- join(ClEst3, ClCentre3, match="all", type="full")

ClAFR_MN15 <- rbind(ClSud3, ClEst3, ClNord3)
Group_AFR_MN15 <- c(rep(1,6), rep(2,41), rep(3,8))
ClAFR_MN15 <- Presence_Absence_matrix(ClAFR_MN15, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AFRMN15_FALSE <- PerSIMPER_onMatrix(ClAFR_MN15, Group_AFR_MN15, NS = TRUE, Info = "OLD_MN15_AFR")

## MN16
ClAFR_Sud <- subset(MN16, MN16$LONG > 15 & MN16$LONG < 27.6 & MN16$LAT > -40 & MN16$LAT < -29.1)
ClAFR_SudEst <- subset(MN16, MN16$LONG > 20 & MN16$LONG < 35 & MN16$LAT > -29.1 & MN16$LAT < -13)
ClAFR_Est <- subset(MN16, MN16$LONG > 27 & MN16$LONG < 45 & MN16$LAT > -12 & MN16$LAT < 14.7)
ClAFR_NordSA <- subset(MN16, MN16$LONG > -10 & MN16$LONG < 36.2 & MN16$LAT > 14.7)

ClAFR_MN16 <- rbind(ClAFR_SudEst, ClAFR_Est, ClAFR_NordSA)
Group_AFR_MN16 <- c(rep(1,5), rep(2,35), rep(3,10))
ClAFR_MN16 <- Presence_Absence_matrix(ClAFR_MN16, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AFRMN16_FALSE <- PerSIMPER_onMatrix(ClAFR_MN16, Group_AFR_MN16, NS = TRUE, Info = "MN16_AFR")

#Oldies
ClEst3 <- subset(MN16, MN16$LONG >= 29 & MN16$LAT >= -13 & MN16$LAT < 15)
ClEstNord3 <- subset(MN16, MN16$LONG >= 23 & MN16$LAT >= 15)
ClEstNord32 <- subset(MN16, MN16$LONG >= 20 & MN16$LONG < 23 & MN16$LAT >= 30 & MN16$LAT < 33)
ClCentre3 <- subset(MN16, MN16$LONG >= 9 & MN16$LONG < 20 & MN16$LAT >= 15 & MN16$LAT < 17)
ClNordOuest3 <- subset(MN16, MN16$LONG > -10 & MN16$LAT >= 31 & MN16$LONG < 11)
ClSud3 <- subset(MN16, MN16$LAT < -13)
ClEstNord3 <- join(ClEstNord3, ClEstNord32, match="all", type="full")
ClNord3 <- join(ClEstNord3, ClNordOuest3, match="all", type="full")
ClEst3 <- join(ClEst3, ClCentre3, match="all", type="full")

ClAFR_MN16 <- rbind(ClSud3, ClEst3, ClNord3)
Group_AFR_MN16 <- c(rep(1,6), rep(2,36), rep(3,9))
ClAFR_MN16 <- Presence_Absence_matrix(ClAFR_MN16, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AFRMN16_FALSE <- PerSIMPER_onMatrix(ClAFR_MN16, Group_AFR_MN16, NS = TRUE, Info = "OLD_MN16_AFR")

## MN17
ClAFR_Sud <- subset(MN17, MN17$LONG > 15 & MN17$LONG < 27.6 & MN17$LAT > -40 & MN17$LAT < -29.1)
ClAFR_SudEst <- subset(MN17, MN17$LONG > 20 & MN17$LONG < 35 & MN17$LAT > -29.1 & MN17$LAT < -13)
ClAFR_Est <- subset(MN17, MN17$LONG > 27 & MN17$LONG < 45 & MN17$LAT > -12 & MN17$LAT < 14.7)
ClAFR_NordSA <- subset(MN17, MN17$LONG > -10 & MN17$LONG < 36.2 & MN17$LAT > 14.7)

ClAFR_MN17 <- rbind(ClAFR_SudEst, ClAFR_Est, ClAFR_NordSA, ClAFR_Sud)
Group_AFR_MN17 <- c(rep(1,15), rep(2,46), rep(3,24), rep(4,12))
ClAFR_MN17 <- Presence_Absence_matrix(ClAFR_MN17, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AFRMN17_FALSE <- PerSIMPER_onMatrix(ClAFR_MN17, Group_AFR_MN17, NS = TRUE, Info = "MN17_AFR")

#Oldies
ClEst3 <- subset(MN17, MN17$LONG >= 29 & MN17$LAT >= -13 & MN17$LAT < 15)
ClEstNord3 <- subset(MN17, MN17$LONG >= 23 & MN17$LAT >= 15)
ClEstNord32 <- subset(MN17, MN17$LONG >= 20 & MN17$LONG < 23 & MN17$LAT >= 30 & MN17$LAT < 33)
ClCentre3 <- subset(MN17, MN17$LONG >= 9 & MN17$LONG < 20 & MN17$LAT >= 15 & MN17$LAT < 17)
ClNordOuest3 <- subset(MN17, MN17$LONG > -10 & MN17$LAT >= 31 & MN17$LONG < 11)
ClSud3 <- subset(MN17, MN17$LAT < -13)
ClEstNord3 <- join(ClEstNord3, ClEstNord32, match="all", type="full")
ClNord3 <- join(ClEstNord3, ClNordOuest3, match="all", type="full")
ClEst3 <- join(ClEst3, ClCentre3, match="all", type="full")

ClAFR_MN17 <- rbind(ClSud3, ClEst3, ClEstNord3, ClNordOuest3)
Group_AFR_MN17 <- c(rep(1,27), rep(2,47), rep(3,13), rep(4,10))
ClAFR_MN17 <- Presence_Absence_matrix(ClAFR_MN17, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AFRMN17_FALSE <- PerSIMPER_onMatrix(ClAFR_MN17, Group_AFR_MN17, NS = TRUE, Info = "OLD_MN17_AFR")

## MN18
ClAFR_Sud <- subset(MN18, MN18$LONG > 15 & MN18$LONG < 27.6 & MN18$LAT > -40 & MN18$LAT < -29.1)
ClAFR_SudEst <- subset(MN18, MN18$LONG > 20 & MN18$LONG < 35 & MN18$LAT > -29.1 & MN18$LAT < -13)
ClAFR_Est <- subset(MN18, MN18$LONG > 27 & MN18$LONG < 45 & MN18$LAT > -12 & MN18$LAT < 14.7)
ClAFR_NordSA <- subset(MN18, MN18$LONG > -10 & MN18$LONG < 36.2 & MN18$LAT > 14.7)

ClAFR_MN18 <- rbind(ClAFR_SudEst, ClAFR_Est, ClAFR_NordSA, ClAFR_Sud)
Group_AFR_MN18 <- c(rep(1,30), rep(2,54), rep(3,38), rep(4,25))
ClAFR_MN18 <- Presence_Absence_matrix(ClAFR_MN18, type = "Species", min5 = FALSE, singletons = TRUE)
Faune3AFRMN18_FALSE <- PerSIMPER_onMatrix(ClAFR_MN18, Group_AFR_MN18, NS = TRUE, Info = "MN18_AFR")

#Oldies
ClEst3 <- subset(MN18, MN18$LONG >= 29 & MN18$LAT >= -13 & MN18$LAT < 15)
ClEstNord3 <- subset(MN18, MN18$LONG >= 23 & MN18$LAT >= 15)
ClEstNord32 <- subset(MN18, MN18$LONG >= 20 & MN18$LONG < 23 & MN18$LAT >= 30 & MN18$LAT < 33)
ClCentre3 <- subset(MN18, MN18$LONG >= 9 & MN18$LONG < 20 & MN18$LAT >= 15 & MN18$LAT < 17)
ClNordOuest3 <- subset(MN18, MN18$LONG > -10 & MN18$LAT >= 31 & MN18$LONG < 11)
ClSud3 <- subset(MN18, MN18$LAT < -13)
ClEstNord3 <- join(ClEstNord3, ClEstNord32, match="all", type="full")
ClNord3 <- join(ClEstNord3, ClNordOuest3, match="all", type="full")
ClEst3 <- join(ClEst3, ClCentre3, match="all", type="full")

ClAFR_MN18 <- rbind(ClSud3, ClEst3, ClNordOuest3, ClEstNord3)
Group_AFR_MN18 <- c(rep(1,64), rep(2,56), rep(3,19), rep(4,17))
ClAFR_MN18 <- Presence_Absence_matrix(ClAFR_MN18, type = "Species", min5 = FALSE, singletons = TRUE)
OLD_Faune3AFRMN18_FALSE <- PerSIMPER_onMatrix(ClAFR_MN18, Group_AFR_MN18, NS = TRUE, Info = "OLD_MN18_AFR")


## Liste a sortir pour ANOSIM

ResultatsEuropeNEW <- list(Faune1EuropeMN1_FALSE = Faune1EuropeMN1_FALSE, Faune1EuropeMN2_FALSE = Faune1EuropeMN2_FALSE, 
                        Faune1EuropeMN3_FALSE = Faune1EuropeMN3_FALSE, Faune1EuropeMN4_FALSE = Faune1EuropeMN4_FALSE,
                        Faune1EuropeMN4_FALSE = Faune1EuropeMN4_FALSE, Faune1EuropeMN5_FALSE = Faune1EuropeMN5_FALSE,
                        Faune2EuropeMN6_FALSE = Faune2EuropeMN6_FALSE, Faune2EuropeMN7_8_FALSE = Faune2EuropeMN7_8_FALSE,
                        Faune2EuropeMN9_FALSE = Faune2EuropeMN9_FALSE, Faune2EuropeMN10_FALSE = Faune2EuropeMN10_FALSE, 
                        Faune2EuropeMN11_FALSE = Faune2EuropeMN11_FALSE, Faune2EuropeMN12_FALSE = Faune2EuropeMN12_FALSE, 
                        Faune2EuropeMN13_FALSE = Faune2EuropeMN13_FALSE, Faune3EuropeMN14_FALSE = Faune3EuropeMN14_FALSE,
                        Faune3EuropeMN15_FALSE = Faune3EuropeMN15_FALSE, Faune3EuropeMN16_FALSE = Faune3EuropeMN16_FALSE, 
                        Faune3EuropeMN17_FALSE = Faune3EuropeMN17_FALSE, Faune3EuropeMN18_FALSE = Faune3EuropeMN18_FALSE)

capture.output(ResultatsEuropeNEW, file = "ResultatsEuropeNEW.txt")

ResultatsEuropeW <- list(W_Faune1EuropeMN2_FALSE = W_Faune1EuropeMN2_FALSE, W_Faune1EuropeMN3_FALSE = W_Faune1EuropeMN3_FALSE,
                         W_Faune1EuropeMN4_FALSE = W_Faune1EuropeMN4_FALSE, W_Faune1EuropeMN5_FALSE = W_Faune1EuropeMN5_FALSE,
                         W_Faune2EuropeMN6_FALSE = W_Faune2EuropeMN6_FALSE, W_Faune2EuropeMN7_8_FALSE = W_Faune2EuropeMN7_8_FALSE, 
                         W_Faune2EuropeMN9_FALSE = W_Faune2EuropeMN9_FALSE, W_Faune2EuropeMN10_FALSE = W_Faune2EuropeMN10_FALSE, 
                         W_Faune2EuropeMN11_FALSE = W_Faune2EuropeMN11_FALSE, W_Faune2EuropeMN12_FALSE = W_Faune2EuropeMN12_FALSE, 
                         W_Faune2EuropeMN13_FALSE = W_Faune2EuropeMN13_FALSE)

capture.output(ResultatsEuropeW, file = "ResultatsEurope_W.txt")

ResultatsEuropeOLD <- list(OLD_Faune1EuropeMN1_FALSE = OLD_Faune1EuropeMN1_FALSE, OLD_Faune1EuropeMN2_FALSE = OLD_Faune1EuropeMN2_FALSE,
                           OLD_Faune1EuropeMN3_FALSE = OLD_Faune1EuropeMN3_FALSE, OLD_Faune1EuropeMN4_FALSE = OLD_Faune1EuropeMN4_FALSE,
                           OLD_Faune1EuropeMN5_FALSE = OLD_Faune1EuropeMN5_FALSE, OLD_Faune2EuropeMN6_FALSE = OLD_Faune2EuropeMN6_FALSE,
                           OLD_Faune2EuropeMN7_8_FALSE = OLD_Faune2EuropeMN7_8_FALSE, OLD_Faune2EuropeMN9_FALSE = OLD_Faune2EuropeMN9_FALSE,
                           OLD_Faune2EuropeMN10_FALSE = OLD_Faune2EuropeMN10_FALSE, OLD_Faune2EuropeMN11_FALSE = OLD_Faune2EuropeMN11_FALSE,
                           OLD_Faune2EuropeMN12_FALSE = OLD_Faune2EuropeMN12_FALSE, OLD_Faune2EuropeMN13_FALSE = OLD_Faune2EuropeMN13_FALSE,
                           OLD_Faune3EuropeMN14_FALSE = OLD_Faune3EuropeMN14_FALSE, OLD_Faune3EuropeMN15_FALSE = OLD_Faune3EuropeMN15_FALSE,
                           OLD_Faune3EuropeMN16_FALSE = OLD_Faune3EuropeMN16_FALSE, OLD_Faune3EuropeMN17_FALSE = OLD_Faune3EuropeMN17_FALSE,
                           OLD_Faune3EuropeMN18_FALSE = OLD_Faune3EuropeMN18_FALSE)

capture.output(ResultatsEuropeOLD, file = "ResultatsEuropeOLD.txt")

ResultatAsieNEW <- list(Faune1AsiaMN2_FALSE = Faune1AsiaMN2_FALSE, Faune1AsiaMN3_FALSE = Faune1AsiaMN3_FALSE, 
                        Faune1AsiaMN4_FALSE = Faune1AsiaMN4_FALSE, Faune1AsiaMN5_FALSE = Faune1AsiaMN5_FALSE,
                        Faune2AsiaMN6_FALSE = Faune2AsiaMN6_FALSE, Faune2AsiaMN7_8_FALSE = Faune2AsiaMN7_8_FALSE,
                        Faune2AsiaMN9_FALSE = Faune2AsiaMN9_FALSE, Faune2AsiaMN10_FALSE = Faune2AsiaMN10_FALSE,
                        Faune2AsiaMN11_FALSE = Faune2AsiaMN11_FALSE, Faune2AsiaMN12_FALSE = Faune2AsiaMN12_FALSE,
                        Faune2AsiaMN13_FALSE = Faune2AsiaMN13_FALSE, Faune3AsiaMN14_FALSE = Faune3AsiaMN14_FALSE,
                        Faune3AsiaMN15_FALSE = Faune3AsiaMN15_FALSE, Faune3AsiaMN16_FALSE = Faune3AsiaMN16_FALSE, 
                        Faune3AsiaMN17_FALSE = Faune3AsiaMN17_FALSE, Faune3AsiaMN18_FALSE = Faune3AsiaMN18_FALSE)

capture.output(ResultatAsieNEW, file = "ResultatAsieNEW.txt")

ResultatAsieOLD <- list(OLD_Faune1AsiaMN2_FALSE = OLD_Faune1AsiaMN2_FALSE, OLD_Faune1AsiaMN3_FALSE = OLD_Faune1AsiaMN3_FALSE, 
                        OLD_Faune1AsiaMN4_FALSE = OLD_Faune1AsiaMN4_FALSE, OLD_Faune1AsiaMN5_FALSE = OLD_Faune1AsiaMN5_FALSE,
                        OLD_Faune2AsiaMN6_FALSE = OLD_Faune2AsiaMN6_FALSE, OLD_Faune2AsiaMN7_8_FALSE = OLD_Faune2AsiaMN7_8_FALSE,
                        OLD_Faune2AsiaMN9_FALSE = OLD_Faune2AsiaMN9_FALSE, OLD_Faune2AsiaMN10_FALSE = OLD_Faune2AsiaMN10_FALSE,
                        OLD_Faune2AsiaMN11_FALSE = OLD_Faune2AsiaMN11_FALSE, OLD_Faune2AsiaMN12_FALSE = OLD_Faune2AsiaMN12_FALSE,
                        OLD_Faune2AsiaMN13_FALSE = OLD_Faune2AsiaMN13_FALSE, OLD_Faune3AsiaMN14_FALSE = OLD_Faune3AsiaMN14_FALSE,
                        OLD_Faune3AsiaMN15_FALSE = OLD_Faune3AsiaMN15_FALSE, OLD_Faune3AsiaMN16_FALSE = OLD_Faune3AsiaMN16_FALSE, 
                        OLD_Faune3AsiaMN17_FALSE = OLD_Faune3AsiaMN17_FALSE, OLD_Faune3AsiaMN18_FALSE = OLD_Faune3AsiaMN18_FALSE)

capture.output(ResultatAsieOLD, file = "ResultatAsieOLD.txt")



ResultatsAfriqueNEW <- list(Faune3AFRMN14_FALSE = Faune3AFRMN14_FALSE, Faune3AFRMN15_FALSE = Faune3AFRMN15_FALSE, 
                            Faune3AFRMN16_FALSE = Faune3AFRMN16_FALSE, Faune3AFRMN17_FALSE = Faune3AFRMN17_FALSE, 
                            Faune3AFRMN18_FALSE = Faune3AFRMN18_FALSE)

capture.output(ResultatsAfriqueNEW, file = "ResultatsAfriqueNEW.txt")

ResultatsAfriqueOLD <- list(OLD_Faune3AFRMN15_FALSE = OLD_Faune3AFRMN15_FALSE, 
                            OLD_Faune3AFRMN16_FALSE = OLD_Faune3AFRMN16_FALSE, OLD_Faune3AFRMN17_FALSE = OLD_Faune3AFRMN17_FALSE, 
                            OLD_Faune3AFRMN18_FALSE = OLD_Faune3AFRMN18_FALSE)

capture.output(ResultatsAfriqueOLD, file = "ResultatsAfriqueOLD.txt")









