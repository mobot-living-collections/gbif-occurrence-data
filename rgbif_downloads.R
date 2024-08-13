# https://docs.ropensci.org/rgbif/articles/downloading_a_long_species_list.html
 
# rgbif for downloading occurrence data from GBIF via R
# 07/31/2024
install.packages("usethis")
library(usethis)

install.packages("rgbif")
library(rgbif)

usethis::edit_r_environ()
library(dplyr)
library(readr)

taxa_list <- read.csv(".csv")
#gbif_taxon_keys <- taxa_list %>%

#taxon_key <- name_backbone_checklist(taxa_list, strict = FALSE, verbose = TRUE)

###rgibf credentials
# fill in your gbif.org credentials 
user <- "" # your name
pwd <- "" # your gbif.org password
email <- "" # your email 


 #####################################################

####find Taxon Usage Key  
GBIFTaxonomyList<-name_backbone_checklist(taxa_list)

##combine with sp so you can see what was searched for
GBIFTaxonomyList<-cbind(taxa_list, GBIFTaxonomyList)
colnames(GBIFTaxonomyList)[1]<-"SearchedForTaxon"
colnames(GBIFTaxonomyList)

#Explore GBIF Taxonomy List
colnames((GBIFTaxonomyList))
table(GBIFTaxonomyList$matchType)
# EXACT      FUZZY HIGHERRANK       NONE 
# 4106         28         66          1

table(GBIFTaxonomyList$status)
# ACCEPTED DOUBTFUL  SYNONYM 
# 3590        4      606

table(GBIFTaxonomyList$rank)
# FAMILY       FORM      GENUS     PHYLUM    SPECIES SUBSPECIES    VARIETY 
# 15         18         24          1       3576        151        415

#remove taxa that have rank higher than species
GBIFTaxonomyList<-GBIFTaxonomyList[GBIFTaxonomyList$rank=="SPECIES",]
table(GBIFTaxonomyList$rank)


##Find if any taxon yielded no match
sum(is.na(GBIFTaxonomyList$usageKey))
# [1] 1
GBIFTaxonomyList<-data.frame(GBIFTaxonomyList)

GBIFTaxonomyList[is.na(GBIFTaxonomyList$usageKey),]
#  SearchedForTaxon usageKey acceptedUsageKey scientificName canonicalName rank status confidence matchType kingdom phylum order family genus species
#  NA             <NA>       NA               NA           <NA>          <NA> <NA>   <NA>         NA      <NA>    <NA>   <NA>  <NA>   <NA>  <NA>    <NA>
#  kingdomKey phylumKey classKey orderKey familyKey genusKey speciesKey synonym class verbatim_name verbatim_index
#  NA         NA        NA       NA       NA        NA       NA         NA      NA  <NA>          <NA>             NA

sum(is.na(GBIFTaxonomyList$acceptedUsageKey))
# [1] 3225

sum(!is.na(GBIFTaxonomyList$acceptedUsageKey))
# [1] 352


#Create table for use in Find species occurrence
##replace usage key with acceptedUsageKey

###Create variable which list usage keys for finding occurrences
TaxonKeyMyList<-GBIFTaxonomyList$usageKey
length(TaxonKeyMyList)
###replace synonymy usage keys with accepted usage keys
TaxonKeyMyList[!is.na(GBIFTaxonomyList$acceptedUsageKey)]<-GBIFTaxonomyList$acceptedUsageKey[!is.na(GBIFTaxonomyList$acceptedUsageKey)]

#RemoveNA
TaxonKeyMyList[is.na(TaxonKeyMyList)]
TaxonKeyMyList<-TaxonKeyMyList[!is.na(TaxonKeyMyList)]
length(TaxonKeyMyList)

##only pull ones where taxon key matches
length(unique(GBIFTaxonomyList$speciesKey))
# [1] 3470 

#############################################################################################
# 4. Find Species Occurrences.
#############################################################################################


#A taxon key from the GBIF backbone. All included and synonym taxa are included in the search, so a search for Aves with taxonKey=212 (i.e. /occurrence/search?taxonKey=212) will match all birds, no matter which species.Parameter may be repeated.

# need to make this download specific to the area, after 1965
# this is also the function where you can specify polygon coordinates to limit the search
# this download saves in the GBIF website
occ_download(
  pred_in("speciesKey", TaxonKeyMyList),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN', 'HUMAN_OBSERVATION')),
  #pred_in("stateProvince", c('TN', 'NC', 'AL', 'GA', 'SC', 'KY')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("country", "US"),
  pred_gte("year", 1965),
  #pred_gte("month", 10),
  format = "DWCA",
  user=user,pwd=pwd,email=email
)
#<<gbif download>>
#Your download is being processed by GBIF:
#  https://www.gbif.org/occurrence/download/0045900-240626123714530
#Most downloads finish within 15 min.
#Check status with
#occ_download_wait('0045900-240626123714530')
#After it finishes, use
#d <- occ_download_get('0045900-240626123714530') %>%
#  occ_download_import()
#to retrieve your download.
#Download Info:
#  Username: taubuchon
#E-mail: taubuchon@mobot.org
#Format: DWCA
#Download key: 0045900-240626123714530
#Created: 2024-08-02T19:48:08.371+00:00
#Citation Info:  
#  Please always cite the download DOI when using this data.
#https://www.gbif.org/citation-guidelines
#DOI: 10.15468/dl.6jnj4q
#Citation:
 # GBIF Occurrence Download https://doi.org/10.15468/dl.6jnj4q Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2024-08-02


####### occ_download_get grabs the download from gbif 
####### occ_download_get retrieves download, _import imports the download from local file system
gbifDownload <- occ_download_get("", overwrite = TRUE) %>%
  occ_download_import()







  
  






