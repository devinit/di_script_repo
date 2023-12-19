#Description: Access FTS API to get flows data. 
#Author: Dan W
#Creation date: Feb 2021
#Last revision:
#Notes: Specifying at least a year is recommended. Option to unnest output (recommended) is included. This process will take time with large downloads.

fts_get_flows <- function(year = NULL, planid = NULL, emergencyid = NULL, globalclusterid = NULL, destinationlocationid = NULL, unnest = T){
  lapply(c("data.table", "jsonlite", "httr"), require, character.only=T)
  if(!is.null(year)){
    year <- paste0("year=", paste0(year, collapse=","))
  }
  if(!is.null(planid)){
    planid <- paste0("planid=", paste0(planid, collapse=","))
  }
  if(!is.null(emergencyid)){
    emergencyid <- paste0("emergencyid=", paste0(emergencyid, collapse=","))
  }
  if(!is.null(globalclusterid)){
    globalclusterid <- paste0("globalclusterid=", paste0(globalclusterid, collapse=","))
  }
  if(!is.null(destinationlocationid)){
    destinationlocationid <- paste0("destinationlocationid:", paste0(destinationlocationid, collapse=","))
  }
  
  call.filter <- NULL
  if(!is.null(destinationlocationid)){
    call.filter <- paste0("&filterby=", destinationlocationid)
  }
  
  hpc <- "https://api.hpc.tools/v1/public/fts/flow?"
  call.param <- paste(year, planid, emergencyid, globalclusterid, call.filter, "format=json&limit=1000", sep="&")
  call <- paste0(hpc, call.param)
  fts <- fromJSON(content(GET(call), type = "text", encoding = "UTF-8"), flatten = T)
  
  flowslist <- list()
  flowslist[[1]] <- (fts$data$flows)
  i <- 2
  while (!is.null(fts$meta$nextLink)){
    nextLink <- fts$meta$nextLink
    fts <- fromJSON(content(GET(nextLink), type = "text", encoding = "UTF-8"), flatten = T)
    flowslist[[i]] <- (fts$data$flows)
    i <- i + 1
  }
  
  flows <- rbindlist(flowslist, fill=T, use.names = T)
  
  if(unnest){
    message("Un-nesting output. This may take some time.")
    fts_unnest_flows <- function(fts, cols = c("sourceObjects", "destinationObjects"), splits = "type", remove.nested = T, group.same = T){
      require(data.table)
      if(length(cols) != length(splits) & length(splits) != 1) stop("There must be one split for each nested col, or a single common split for all nested cols.", call.=F)
      fts <- as.data.table(fts)
      expand.splits <- data.table(cols = cols, splits = splits)
      for(i in 1:nrow(expand.splits)){
        col <- expand.splits[i]$cols
        split <- expand.splits[i]$splits
        if(group.same){
          expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(t(unlist(split(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")), as.data.table(aggregate(x, by = as.data.table(x)[, ..split], FUN = function(y) paste(y, collapse = "; ")))[, ..split]))))), fill=T)
        } else {
          expanded <- rbindlist(lapply(as.list(fts[, ..col])[[1]], function(x) if(nrow(x) == 0) as.data.table(x)[, (split) := NA] else data.table(unlist(split(x, as.data.table(x)[, ..split])))), fill=T)
        }
        names(expanded) <- paste(col, names(expanded), sep="_")
        split.cols <- unique(names(expanded)[grepl(paste0("[.]", split, "\\d*$"), names(expanded))])
        expanded[, (split.cols) := NULL]
        expanded[, (split.cols) := NULL]
        expanded <- expanded[,which(unlist(lapply(expanded, function(x)!(all(is.na(x))|all(is.null(x)))))),with=F]
        fts <- cbind(fts, expanded)
        if(remove.nested) fts[, (col) := NULL][]
      }
      return(fts)
    }
    
    flows <- fts_unnest_flows(flows)
    
  }
  
  return(flows)
}
