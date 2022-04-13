tabulate_dac_api <- function(table = NULL, parameters = list(c(NULL), c(NULL)), start_year = 2000, end_year = format(Sys.Date(), "%Y")){
  
  required.packages <- c("data.table","jsonlite", "rstudioapi")
  invisible(suppressPackageStartupMessages(lapply(required.packages, require, character.only=T)))
  
  if(is.null(table)|any(sapply(parameters, is.null))|is.null(start_year)|is.null(end_year)){
    
    stop("Please specify a valid table, parameters, start, and end year.")
  }
  
  pre <- "https://stats.oecd.org/SDMX-JSON/data/"
  param_f <- paste0("/", paste0(lapply(parameters, function(x) paste0(x, collapse = "+")), collapse = "."))
  suff <- paste0("/all?startTime=", start_year, "&endTime=", end_year)
  
  api <- paste0(pre, table, param_f, suff)
  
  api_out <- read_json(api, simplifyVector = T)
  
  series_names <- api_out$structure$dimensions$series$name
  series_con <- api_out$structure$dimensions$series$values
  series_con <- lapply(series_con, function(x) data.table(cbind(id = as.numeric(rownames(x))-1, name = x$name)))
  
  obs_names <- api_out$structure$dimensions$observation$values[[1]]$name
  
  row_ids <- strsplit(names(api_out$dataSets$series), ":")
  row_names <- rbindlist(lapply(row_ids, function(x) sapply(1:length(x), function(i) series_con[[i]][id == x[[i]]][,2])))
  names(row_names) <- series_names
  
  dac_tab <- rbindlist(lapply(api_out$dataSets$series, function(x) x$observations), fill = T)
  dac_tab[dac_tab == "NULL"] <- 0
  dac_tab <- sapply(dac_tab, function(x) sapply(x, function(y) as.numeric(y[[1]])))
  if(is.null(dim(dac_tab)[1]))
    dac_tab <- t(dac_tab)
  dac_tab <- data.table(dac_tab)
  names(dac_tab) <- obs_names
  
  dac_tab <- cbind(row_names, dac_tab)
  
  return(dac_tab)
}
