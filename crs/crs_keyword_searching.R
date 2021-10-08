#Description: Keyword searching for CRS
#Author: Dan W
#Creation: 2019
#Last revision: NA
#Notes:

required.packages <- c("data.table")
lapply(required.packages, require, character.only = T)

keep <- c(
  "CRSid"
  ,
  "ProjectNumber"
  ,
  "Year"
  ,
  "Aid_t"
  ,
  "FlowName"
  ,
  "DonorName"
  ,
  "RecipientName"
  ,
  "USD_Commitment_Defl"
  ,
  "USD_Disbursement_Defl"
  ,
  "PurposeName"
  ,
  "ProjectTitle"
  ,
  "ShortDescription"
  ,
  "LongDescription"
  ,
  "Gender"
  ,
  "ChannelReportedName"
)

crs <- crs[, ..keep]
crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  | 
    FlowName == "Private Development Finance"
  ]

crs <- crs[as.character(Year) >= 2014]

major.keywords <- c(
  #"keyword1"
  #,
  #"keyword2"
)

minor.keywords <- c(
  #"keyword3"
  #,
  #"keyword4"
)

disqualifying.keywords <- c(
  #"keyword5"
  #,
  #"keyword6"
  )

disqualifying.sectors <- c(
  #"sector1"
  #,
  #"sector2"
)

crs$relevance <- "None"
if(length(minor.keywords) > 0){
  crs[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(crs$ProjectTitle, crs$ShortDescription, crs$LongDescription)))]$relevance <- "Minor"
}
if(length(major.keywords) > 0){
  crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$LongDescription))]$relevance <- "Minor"
  crs[grepl(paste(major.keywords, collapse = "|"), tolower(paste(crs$ShortDescription, crs$ProjectTitle)))]$relevance <- "Major"
}

crs$check <- "No"
crs[relevance == "Minor"]$check <- "potential false positive"

if(length(disqualifying.sectors) > 0){
  crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$check <- "potential false negative"
  crs[relevance != "None"][PurposeName %in% disqualifying.sectors]$relevance <- "None"
}

if(length(disqualifying.keywords) > 0){
  crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$check <- "potential false negative"
  crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$ProjectTitle, crs[relevance != "None"]$ShortDescription, crs[relevance != "None"]$LongDescription)))]$relevance <- "None"
}


crs_output <- crs
rm(crs)

crs.years <- dcast.data.table(crs_output, Year ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.donors <- dcast.data.table(crs_output, Year + DonorName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.recipients <- dcast.data.table(crs_output, Year + RecipientName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.sectors <- dcast.data.table(crs_output, Year + PurposeName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))
crs.flows <- dcast.data.table(crs_output, Year + FlowName ~ relevance, value.var = "USD_Disbursement_Defl", fun.aggregate = function (x) sum(x, na.rm=T))