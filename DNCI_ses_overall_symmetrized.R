
################ UNDER DEVELOPMENT ######################
## To contact me : corentingibert@gmail.com (feel free)
################ EN DEVELOPPEMENT #######################
# This function calls/needs DNCI_ses_overall function ###
# This function calls/needs PerSIMPER_Overall function ##

######################### CAUTION #######################
#### This function is a wraper for DNCI_ses_overall to ##
### compute robust overall DNCI values by making all ####
### groups even. ########################################
##### By making all groups even, this function can ######
##### change massively the original DNCI_overall ########
###### IF (!) one group is way smaller than the others ##
############## Consequently you should (A) run a lot ####
########### of iterations or (B) remove the smallest ####
######## group ##########################################
#########################################################

### Arguments are similar to DNCI_ses_overall.R 
##### BUT (!) you can choose the number of iteration to
######## compute mean robust overall DNCI value : NbrRerun

DNCI.ses_overall_symmetrized <- function(Mat, Group, NbrReRun = 100, count = FALSE) 
{
  Number_ofpairs <- combn(1:length(unique(Group)), 2)
  Unik_group <- unique(Group)
  
  List_result <- c()
  
  for(x in 1:NbrReRun)
  {
    SampleGroup <- c()
    for(y in 1:length(Unik_group)) #CHANGER PAR LA
    {
      SampleGroup <- c(SampleGroup, sample(which(Group == y), min(table(Group))))
    }
    Mat_Sampled <- Mat[SampleGroup,]
    Group_Sampled <- Group[SampleGroup]
    if(length(which(apply(Mat_Sampled, 2, sum) == 0)) != 0)
    {
      Mat_Sampled <- Mat_Sampled[,-which(apply(Mat_Sampled, 2, sum) == 0)]
    }
    
    if(length(Number_ofpairs[1,]) > 2){
      #Analyse_Pairs <- PerSIMPER_onMatrix(Mat_Sampled, Group_Sampled, NomCluster = LETTERS[Unik_group], NS = FALSE, 
      #                                    overall = FALSE)
      Analyse_Overall <- DNCI.ses_overall(Mat_Sampled, Group_Sampled, count = count)

      if(x == 1)
      {
        List_result <- Analyse_Overall
      }
      if(x != 1)
      {
        List_result <- rbind(List_result, Analyse_Overall)
      }
    }
  }
  return(List_result)
}
