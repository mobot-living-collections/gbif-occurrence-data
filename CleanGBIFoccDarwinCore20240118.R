#packages you might need to install or require
#install.packages('data.table')
#install.packages("coordinatecleaner")
require(data.table)
#install.packages('bit64')
require(bit64)
#install.packages("finch")
require(finch)

#choose the path where your file exists
setwd("//mbggis10/gisdata/GlobalWork_expeditions/OccurrenceData/OccurrenceDB_TemporaryDownloads/GBIF/GBIF20240119_RotaPoly")



###choose file to read in
##GBIF data has all types of obscure character types and quotes in text fields this more soffisticated table readder will help you catch them
#gbifData<-data.table::fread("occurrence.txt")
#gbifData<-data.table::fread("occurrence.txt", quote="")


#another way to read in file is with finch
#this package will read in the zip file
#takes slightly longer
##made specifically for darwin core.  on small amount of testing performed better than fread
out <- dwca_read("0065722-231120084113126.zip", read = TRUE)
dim(out$data$occurrence.txt)
gbifData<-out$data$occurrence.txt



###check to make sure the deminsions of the file you read in are correct
##number of occurences included in your download by 259 fileds
dim(gbifData)


####now we want to choose the fields (rows) we are interested in and build a dataframe for occurences database
colnames(gbifData)

unique(gbifData$taxonRank)

###remove everything with only higher taxonomic rank, not usefull for our puposes
gbifData<-gbifData[gbifData$taxonRank!="GENUS",]
gbifData<-gbifData[gbifData$taxonRank!="FAMILY",]
gbifData<-gbifData[gbifData$taxonRank!="CLASS",]
gbifData<-gbifData[gbifData$taxonRank!="KINGDOM",]
gbifData<-gbifData[gbifData$taxonRank!="PHYLUM",]
gbifData<-gbifData[gbifData$taxonRank!="ORDER",]
gbifData<-gbifData[gbifData$taxonRank!="UNRANKED",]

###has GeosaptialIssues?
unique(gbifData$hasGeospatialIssues)


###build a clean scientific name with no authors.
#snameNoAuthors<-paste(gbifData$genus, gbifData$species, sep=" ")
unique(gbifData$taxonRank)
snameNoAuthors<-gbifData$species
gbifData<-data.frame(gbifData, snameNoAuthors)
gbifData$snameNoAuthors<-as.character(gbifData$snameNoAuthors)


#building taxon
var<-rep("var.", length(gbifData[gbifData$taxonRank=="VARIETY",]$snameNoAuthors))
gbifData[gbifData$taxonRank=="VARIETY",]$snameNoAuthors<-paste(gbifData[gbifData$taxonRank=="VARIETY",]$species, 
                                                               var, gbifData[gbifData$taxonRank=="VARIETY",]$infraspecificEpithet, sep= " ")


var<-rep("subsp.", length(gbifData[gbifData$taxonRank=="SUBSPECIES",]$snameNoAuthors))
gbifData[gbifData$taxonRank=="SUBSPECIES",]$snameNoAuthors<-paste(gbifData[gbifData$taxonRank=="SUBSPECIES",]$species, 
                                                                  var, gbifData[gbifData$taxonRank=="SUBSPECIES",]$infraspecificEpithet, sep= " ")

var<-rep("f.", length(gbifData[gbifData$taxonRank=="FORM",]$snameNoAuthors))
gbifData[gbifData$taxonRank=="FORM",]$snameNoAuthors<-paste(gbifData[gbifData$taxonRank=="FORM",]$species, 
                                                            var, gbifData[gbifData$taxonRank=="FORM",]$infraspecificEpithet, sep= " ")

colnames(gbifData)


##might should do this using instituiton code.


###remove tropicos data--we will get these from tropicos including those with hidden lat long
##what is the tropicos institution code?  MO? 

###look at all of the unique institutions
unique(gbifData$institutionCode)

table(gbifData$institutionCode)

#check number of Tropicos spec
dim(gbifData[gbifData$institutionCode=="MO",])

###remove tropicos specemins
gbifData<-gbifData[gbifData$institutionCode!="MO",]

##old code for pulling collumn names
#colnames(gbifData)[c(1, 60, 64, 65, 71, 73, 77, 78, 90, 92, 103, 107,
#                    111, 116, 125, 126, 129, 130, 137, 138, 139, 140, 151, 
#                    152, 227, 228, 232, 242, 260)]



##choose your collumns
#gbifDataExport<-gbifData[c(1, 60, 64, 65, 71, 73, 77, 78, 90, 92, 103, 107,
#                           111, 116, 125, 126, 129, 130, 137, 138, 139, 140, 151, 
#                           152, 227, 228, 232, 242, 260)] 



##choose your collumns ##currently works but the code not commented out is better
#gbifDataExport<-gbifData[c(1, 14, 18, 19, 25, 27, 31, 32, 50, 52, 57, 61,
#                           65, 70, 79, 80, 81, 83, 84, 91, 92, 93, 94, 105, 106, 
#                           178, 179, 184, 194, 213)]


gbifDataExport<-gbifData[,c("gbifID", "institutionCode", "basisOfRecord", "informationWithheld", "recordedBy", "individualCount", "lifeStage", "reproductiveCondition", 
                            "associatedTaxa", "occurrenceRemarks", "eventDate", "year", "habitat", "fieldNotes", "countryCode",                  
                            "stateProvince", "locality", "verbatimLocality", "locationRemarks", "decimalLatitude",              
                            "decimalLongitude", "coordinateUncertaintyInMeters", "georeferenceSources", "georeferenceRemarks", "issue",                         
                            "mediaType", "acceptedTaxonKey", "acceptedScientificName", "snameNoAuthors")] 
    

##examine col names
colnames(gbifDataExport)




###seperate the date from the time
##what type of data is eventdate
###change to a date for Access
class(gbifDataExport$eventDate)

gbifDataExport$eventDate<-as.Date(gbifDataExport$eventDate)
head(gbifDataExport$eventDate)



##rename collumns
colnames(gbifDataExport)<-c("OccurrenceID", "institutionCode", "basisOfRecord", "informationWithheld", "Collector", "individualCount", "lifeStage",                     
                            "PhenologyNotes", "associatedTaxa", "occurrenceRemarks",            
                            "OccurrenceDate", "Collectionyear", "Habitat", "fieldNotes", "Country", "StateProvince", "LocationName",                        
                            "LocationDetails", "Notes", "DecimalLatitude",              
                            "DecimalLongitude", "CoordinateUncertaintyDistance", "georeferenceSources",          
                            "GeoreferenceRemarks", "DataIssue", "mediaType",                    
                            "acceptedTaxonKey", "acceptedScientificName", "Taxon" )   


##add collumns needed for merge 
CoordinateUncertaintyUnit<-rep("m", length(gbifDataExport$Taxon))
EORank<-rep(NA, length(gbifDataExport$Taxon))

gbifDataExport<-data.frame(gbifDataExport, CoordinateUncertaintyUnit, EORank)


##########################################
#Part 2 of Data Cleaning, remvoing bad coordinates
#################################################################################################
###round to only include data with 3 decimal places
#https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
#load("gbifDataExport_2020-08-11.RData")

##create funciton
decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

#test function
decimalplaces(23.43234525)
decimalplaces(334.3410000000000000)
decimalplaces(2.000)



##check that data is present
gbifDataExport[is.na(gbifDataExport$DecimalLatitude),]
#remove na data
gbifDataExport<-gbifDataExport[!is.na(gbifDataExport$DecimalLatitude),]

#apply function to data
NumDecLat<-sapply(gbifDataExport$DecimalLatitude, FUN=decimalplaces)
NumDecLon<-sapply(gbifDataExport$DecimalLongitude, FUN=decimalplaces)


gbifDataExport<-data.frame(gbifDataExport, NumDecLat, NumDecLon)


###only keep data with 3 or greater decimal precision in both lat and lon
gbifDataExport<-gbifDataExport[gbifDataExport$NumDecLat>2,]
gbifDataExport<-gbifDataExport[gbifDataExport$NumDecLon>2,]


#####Coordinate Cleanner
require(CoordinateCleaner)


###removed if 1 km from geographic centriod
gbifDataExport<-cc_cen(gbifDataExport, lon = "DecimalLongitude", lat = "DecimalLatitude",
                       species = "Taxon", buffer = 1000, geod = TRUE, test = "both",
                       ref = NULL, verify = FALSE, value = "clean", verbose = FALSE)

##not neccessary as country code function is not working
####removed if country code does not match
##for some reason getting an error while using countryCorde column 
#going to try and convert it to what i belive is proper iso3
#USA<-rep("USA", length(gbifDataExport$Taxon))
#USA2<-"USA"
#gbifDataExport$countryCode<-USA
#gbifDataExport<-cc_coun(gbifDataExport, lon="DecimalLongitude", lat="DecimalLatitude",
#                            iso3="countryCode", value = "clean", ref = NULL, ref_col="iso_a3",
#                            verbose = FALSE)

####remove dupplicate records
#may not want to remove duplicate records...if a site is revisited more than one time could be useful to know.  
#gbifDataExport<-cc_dupl(gbifDataExport, lon = "DecimalLongitude", lat = "DecimalLatitude",
#                              species = "Taxon", additions = NULL, value = "clean",
#                              verbose = FALSE)




####removes records within 0.5 degrre radions of GBIF headquarters
gbifDataExport<-cc_gbif(gbifDataExport, lon = "DecimalLongitude", lat = "DecimalLatitude",
                        species = "Taxon", buffer = 1000, geod = TRUE, verify = FALSE,
                        value = "clean", verbose = FALSE)


####removes records in the Vicinity of Biodiversity Institutions
gbifDataExport<-cc_inst(gbifDataExport, lon = "DecimalLongitude", lat = "DecimalLatitude",
                        species = "Taxon", buffer = 100, geod = TRUE, ref = NULL,
                        verify = FALSE, verify_mltpl = 10, value = "clean",
                        verbose = FALSE)
### Identify Non-terrestrial Coordinates
gbifDataExport<-cc_sea(gbifDataExport, lon = "DecimalLongitude", lat = "DecimalLatitude",
                       ref = NULL, scale = 110, value = "clean", speedup = TRUE,
                       verbose = FALSE)
##### Identify Invalid lat/lon Coordinates
gbifDataExport<-cc_val(gbifDataExport, lon = "DecimalLongitude", lat = "DecimalLatitude",
                       value = "clean", verbose = FALSE)
##### Identify Zero Coordinates
gbifDataExport<-cc_zero(gbifDataExport, lon = "DecimalLongitude", lat = "DecimalLatitude",
                        buffer = 0.5, value = "clean", verbose = FALSE)
##write file
#setwd("//mbggis10/gisdata/GlobalWork_expeditions/OccurrenceData/OccurrenceDB_TemporaryDownloads/GBIF")
write.table(gbifDataExport, file ="GBIF20240119_RotaPoly.txt", row.names = FALSE, fileEncoding = "UTF-16LE", sep = "\t", quote =F)


