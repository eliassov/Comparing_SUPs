###packages
library(sdcTable)
library(stringr)
library(tidyverse)
###functions
write_argus_hrc <- function(df){
  write.table(
    df,
    file = paste0(deparse(substitute(df)), ".hrc"),
    sep = "",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  
  lines <- readLines(paste0(deparse(substitute(df)), ".hrc"))
  lines <- sub("@@", "", lines)
  writeLines(lines[2:length(lines)], paste0(deparse(substitute(df)), ".hrc"))
}
find_prime_positions <- function(hrc_path){
  ###Find all the "deepest" positions in the hierarchy aka "prime positions"
  lines <- readLines(hrc_path)
  count_levels <- str_count(str_extract(lines, "^@+"), "@")
  count_levels <- ifelse(is.na(count_levels), 0, count_levels)
  prime <- NULL
  #deepest position <==> following position has less or equal number of @s
  for(i in 1:(length(count_levels)-1)){
    if(count_levels[i+1]<=count_levels[i]){
      prime <- c(prime,lines[i])
    }else{}
  }
  #last position is always a prime position (if not it wouldn't be last entry in hrc)
  #therefore always add it to prime
  prime <- c(prime, lines[length(lines)])
  #delete the @s, since they are no longer needed
  prime <-str_remove(prime,"^@+") 
  return(prime)
}
# -----------------------------
# create codes (01, 0101, 010501, ...)
# -----------------------------
make_codes <- function(parent, n) {
  base <- sprintf("%02d", seq_len(n))
  if (parent == "TOTAL") {
    base
  } else {
    paste0(parent, base)
  }
}
# -----------------------------
# Add a level to a given hierarchy
# -----------------------------
add_level <- function(hier, parent, n) {
  
  children <- make_codes(parent, n)
  
  hier <- hier_add(
    hier,                 # <-- positionsbasiert!
    root  = parent,
    nodes = children
  )
  
  hier
}
#Builds a hierarchy from a structured list "spec"
build_hierarchy <- function(spec, root = "TOTAL") {
  hier <- hier_create(root)
  
  for (parent in names(spec)) {
    hier <- add_level(hier, parent, spec[[parent]])
  }
  
  hier
}
#clean working directory from previous run
clean_up <- function(x){
  hierarchies <- list.files(
    pattern = "^hier[0-9]+_df\\.hrc$",
    full.names = TRUE
  )
  if (length(hierarchies) > 0) {
    file.remove(hierarchies)
  }
  
  hierarchies_df <- list.files(
    pattern = "^hier[0-9]+_df",
    full.names = TRUE
  )
  if (length(hierarchies_df) > 0) {
    file.remove(hierarchies_df)
  }
}
###
clean_up()
# -----------------------------
# Build hierarchy 1
# -----------------------------
spec1 <- list(
  TOTAL = 7,
  "01"  = 12,
  "0101" =2
  # "02"  = 5,
  # "03"  = 7,
  # "0301" = 2,
  # "030101" =3
)
hier1 <- build_hierarchy(spec1)
#save hierarchy as data.frame
hier1_df <- hier_export(hier1, as = "df", path = "hier1_df")
#write hierarchies in Argus readable format
write_argus_hrc(hier1_df)
# -----------------------------
# Build hierarchy 2
# -----------------------------
# hier2_df <- region
# write_argus_hrc(hier2_df)
spec2 <- list(
  TOTAL = 7,
  "01"  = 4,
  "0101"  = 2
  # "02"  = 3,
  # "0201" = 3
)
hier2 <- build_hierarchy(spec2)
# #save hierarchy as data.frame
hier2_df <- hier_export(hier2, as="df", path = "hier2_df")
#write hierarchies in Argus readable format
write_argus_hrc(hier2_df)
# -----------------------------
# Build hierarchy 3
# -----------------------------
# spec3 <- list(
#   TOTAL = 3,
#   "01"  = 3,
#   "0101"  = 2,
#   "02"  = 3,
#   "0201" = 3
# )
# hier3 <- build_hierarchy(spec3)
# #save hierarchy as data.frame
# hier3_df <- hier_export(hier3, as="df", path = "hier3_df")
# #write hierarchies in Argus readable format
# write_argus_hrc(hier3_df)
# # -----------------------------
# # Build hierarchy 4
# # -----------------------------
# spec4 <- list(
#   TOTAL = 3,
#   "01"  = 3,
#   "0101"  = 2,
#   "02"  = 3,
#   "0201" = 3
# )
# hier4 <- build_hierarchy(spec4)
# #save hierarchy as data.frame
# hier4_df <- hier_export(hier4, as="df", path = "hier4_df")
# #write hierarchies in Argus readable format
# write_argus_hrc(hier4_df)


#Read all hierarchy files
hierarchies <- list.files(
  pattern = "^hier[0-9]+_df\\.hrc$",
  full.names = TRUE
)
hierarchies <- sub("^\\./", "", hierarchies)
###Find all the "deepest" positions in the hierarchy ==> "prime positions"
#apply to all hierarchies the find_prime_positions function
prime_positions <- lapply(hierarchies,find_prime_positions)
#give the according names to all elements of the list
names(prime_positions) <- paste0("var", seq_along(prime_positions))
#Create all combinations of prime positions to create micro data
df_combination <- expand_grid(!!!prime_positions)
####Build the microdata from the "building blocks" of the prime positions


#create microdata as a random sample of combinations
#number of rows
n_ids <- 1000

df_microdata <- df_combination %>%
  slice_sample(n = n_ids, replace = TRUE) %>% #draw random combination for each
  mutate(id = seq_len(n_ids)) #each combination gets a unique id (no holdings)

#use add_response to create values
df_microdata <- add_response(df_microdata)

#delete all empty responses, convert id to chr and round response variable
df_microdata <- df_microdata %>% 
    mutate(id = as.character(id)) %>% 
    filter(response>0) %>% 
    mutate(response = round(response))



