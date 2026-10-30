#packages
library(tidyverse)
library(sdcTable)
#functions
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
#
#create hrc`s from file and modify
#large regional hrc
region <- readLines("data-raw/region_large.hrc")
region_large <- SSBtools::Hrc2DimList(region)
#medium size regional hrc (less entries but deeper)
region_mid <- readLines("data-raw/region.hrc")
region_mid <- SSBtools::Hrc2DimList(region_mid)

#nace hrc
nace_5 <- readLines("data-raw/nace_5.hrc")
nace_5 <- SSBtools::Hrc2DimList(nace_5)
#
nace <- readLines("data-raw/nace.hrc")
nace_3 <- SSBtools::Hrc2DimList(nace)
#nace medium hrc (delete all the deepest levels)
nace_2 <- nace_3 %>% 
          filter(!grepl("@@@@",levels))

#now create some arbitrary dimensions:
#"large" is another large hrc like region or nace
#for the case that we want to build a large 4 dimensional table
#(i.e. region x nace x "large" x "header dimension")

# -----------------------------
# Build hierarchy 
# -----------------------------
spec1 <- list(
  TOTAL = 7,
  "01"  = 12,
  "0101" =2,
  "02"  = 5,
  "0201"  = 5,
  "0202"  = 2,
  "0203"  = 12,
  "020302"  = 5,
  "020303"  = 2,
  "020304"  = 3,
  "020308"  = 7,
  "020312"  = 5,
  "0204"  = 3,
  "0205"  = 8,
  "03"  = 7,
  "0301" = 2,
  "030101" =3,
  "04"=13,
  "0401"=3,
  "0402"=7,
  "0405"=9,
  "040501"=3,
  "040502"=9,
  "040503"=5,
  "040507"=3,
  "040509"=2,
  "0405"=9,
  "0406"=4,
  "0407"=3,
  "0408"=4,
  "0409"=7,
  "0412"=8,
  "0413"=2,
  "05"=13,
  "0501"=3,
  "0502"=7,
  "0505"=9,
  "050501"=3,
  "050502"=9,
  "050503"=5,
  "050507"=3,
  "050509"=2,
  "0505"=9,
  "0506"=4,
  "0507"=3,
  "0508"=4,
  "0509"=7,
  "0512"=8,
  "0513"=2
)
dim1 <- build_hierarchy(spec1)
#save hierarchy as data.frame
large <- hier_export(dim1, as = "df", path = "dim1_df")

#Create arbitrary dimensions of certain size, that could 
#be a typical table header dimension:
spec2 <- list(
  TOTAL = 7,
  "01"  = 2,
  "03"  = 3,
  "07"  = 2
)
dim2 <- build_hierarchy(spec2)
#save hierarchy as data.frame
table_header <- hier_export(dim2, as = "df", path = "dim2_df")

spec3 <- list(
  TOTAL = 2,
  "01"  = 1,
  "02"  = 1
)
dim3 <- build_hierarchy(spec3)
#save hierarchy as data.frame
minimal <- hier_export(dim3, as = "df", path = "dim3_df")