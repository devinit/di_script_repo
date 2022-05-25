#Description:  Retrieve aggregate totals from UN OCHA's FTS API
#Author: Dan W
#Creation date: Oct 2021
#Last revision:
#Notes: 

fts_get_appeals <- function(years=NULL){
  
  required.packages <- c("data.table","jsonlite")
  lapply(required.packages, require, character.only=T)
  
  base.url <- "https://api.hpc.tools/v1/public/plan/year/"
  
  plans.list <- list()
  if(length(years) >= 1){
    for(i in 1:length(years)){
      url <- paste0(base.url, years[i])
      plans <- fromJSON(url, flatten = T)$data
      plans.list[[i]] <- plans
    }
    
    plans.list <- rbindlist(plans.list, fill = T)
  } else {
    plans.list <- fromJSON(base.url)$data
  }
  return(plans.list)
}
