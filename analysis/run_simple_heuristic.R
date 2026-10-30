##Load packages
library(dplyr)
library(sdcTable)
library(GaussSuppression)
#functions
### Special functions are defined
# Using hidden limits interval calculation to the primary suppressed cells
primary_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 9
}
hidden_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 12
}
forced_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  !(taudata$Status[ma] %in% c(9, 12))
}


# renaming allows the hierarchies to be used directly in GaussSuppression
names(hrc_GAUSS) <-  names(df_microdata)[1:2] 

# Maybe handle  "TOTAL" -> "T" already in the hierarchies 
hrc_GAUSS <- lapply(hrc_GAUSS, function(x) {
  x[x == "TOTAL"] <- "T"
  x
})

hier_names <- names(hrc_GAUSS)

#create sdcProblem object
prob.microDat <- makeProblem(
  data = df_microdata,
  dimList = hrc_GAUSS,
  dimVarInd = 1:(length(hier_names)),
  freqVarInd = NULL,
  numVarInd = "response",
  weightInd = NULL,
  sampWeightInd = NULL)


#primary suppressions
prob.microDat <- primarySuppression(prob.microDat,type = "p", p=5, numVarName="response")

sdcTable_method <- "SIMPLEHEURISTIC"  
# sdcTable_method <-  "SIMPLEHEURISTIC_OLD"
# sdcTable_method <- "GAUSS" # also possible ...  

#secondary suppressions with sdcTable_method
resSIMPLE <- protectTable(prob.microDat, method = sdcTable_method)
#output data.frame
result_simpleheuristic <- getInfo(resSIMPLE, type = "finalData")

result_simpleheuristic <- as.data.frame(result_simpleheuristic )

#Run GAUSS to calculate "up" and "lo" as given by the simple_heuristic SUP
#Recode result_simpleheuristic to mimic output of Tauargus to use as input
#to GAUSS with special functions
out_simple <- result_simpleheuristic %>% 
  mutate(Status = recode(sdcStatus,
                         "s" = 2,
                         "x" = 12,
                         "u" = 9)) %>% 
  select(-sdcStatus)


out_simple <- as.data.frame(out_simple)

#Run GAUSS to calculate "up" and "lo" as given by the simple_heuristic SUP
out_simple_2 <- SuppressDominantCells(data=df_microdata,
                                      numVar = "response",
                                      hierarchies = hrc_GAUSS,
                                      contributorVar = "id",
                                      primary = primary_tau,
                                      hidden = hidden_tau, 
                                      forced = forced_tau,
                                      taudata = out_simple,
                                      singletonMethod = "none",
                                      lpPackage = "highs", 
                                      action_unused_dots = "none", # for GaussSuppression ver. 1.2.0
                                      forcedInOutput = FALSE)

out_simple_2 <-  out_simple_2 %>% 
  mutate(suppressed = ifelse(is.na(suppressed),T,suppressed))

#Sum of exact disclosures 
sum(out_simple_2[out_simple_2$primary, "up"] - out_simple_2[out_simple_2$primary, "lo"] == 0, na.rm = TRUE)
