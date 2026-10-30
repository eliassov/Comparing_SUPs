#run GAUSS with synthetic microdata (created in microdata.R in this project)
##Load packages
library(tidyverse)
library(SSBtools)
library(GaussSuppression)
library(Matrix)
library(ellipsis)
library(Rglpk)
library(rtauargus)
library(purrr)
###
#functions
prepare_hrc_for_GAUSS <- function(path){
  #Read Hierarchy from SAS/TauArgus Process
  hier <- readLines(basename(path))
  #Change format slightly to fit input for GAUSS (Is there an easier way?)
  hier <- paste0("@@", hier)
  hier <- c("@T",hier)
  #make data.frame for calling GAUSS with "level" and "code"
  df <- data.frame(raw = hier, stringsAsFactors = FALSE)
  # level = leading @ exactly as in textfile
  df$level <- str_extract(df$raw, "^@+")
  df$level[is.na(df$level)] <- ""   # no @ → empty string
  # code = rest after leading @
  df$code <- str_replace(df$raw, "^@+", "")
  hier <- data.frame(df$level,df$code)
  colnames(hier) <- c("level","code")
  #return
  return(hier)
}
###specify parameters for SDC
pvalue <- 5
###
##create hrc's for GAUSS
#apply to all hierarchies the prepare hrc function
hrc_GAUSS <- lapply(hierarchies,prepare_hrc_for_GAUSS)
#give the according names to all elements of the list so it can be input to GAUSS
names(hrc_GAUSS) <- paste0("var", seq_along(hrc_GAUSS))
#run GAUSS Suppression with correct parameters for fair comparison
#######Call GAUSS
tab_gauss <- SuppressDominantCells(data=df_microdata,
                                   hierarchies = hrc_GAUSS,
                                   numVar = "response", 
                                   contributorVar = "id",
                                   pPercent = pvalue,
                                   allDominance = TRUE,
                                   singletonMethod = "numttTtT",
                                   lpPackage ="Rglpk",
                                   protectionIntervals = TRUE, 
                                   intervalSuppression = FALSE)

tab_gauss <- add_mean_n_at(tab_gauss, hrc_GAUSS)
######Just GAUSS Suppression for measuring actual time needed
start_time_GAUSS <- Sys.time()
tab_gauss_2 <- SuppressDominantCells(data=df_microdata,
                                     hierarchies = hrc_GAUSS,
                                     numVar = "response", 
                                     contributorVar = "id",
                                     pPercent = pvalue,
                                     singletonMethod = "numttTtT")
end_time_GAUSS <- Sys.time()
running_time_GAUSS_2 <- end_time_GAUSS - start_time_GAUSS

