#Description: Un-nest flows data from the API 
#Author: Dan W
#Creation date: Feb 2021
#Last revision:
#Notes: For large downloads will take some time.

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