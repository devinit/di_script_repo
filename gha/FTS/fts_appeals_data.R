#Description: Several functions which retrieve info and aggregate totals from UN OCHA's FTS webpages
#Author: Dan W
#Creation date: Feb 2021
#Last revision:
#Notes: fts_get_appeal_urls() will retrieve a list of HRPs for each year, including each id; fts_get_appeal_totals() will get headline totals of funding and requirements for each HRP, and separate out COVID plans in 2020; fts_get_appeal_clusters() will get cluster funding and requirements for each HRP

#List of appeals
fts_get_appeal_urls <- function(years){
  
  required.packages <- c("data.table","jsonlite","httr","XML")
  lapply(required.packages, require, character.only=T)
  
  plans.list <- list()
  
  for(i in 1:length(years)){
    
    base.url <- "https://fts.unocha.org/appeals/overview/"
    
    url <- paste0(base.url, years[i])
    
    data <- htmlParse(GET(url))
    
    plans <- data.table(year = years[i], plan_name = xpathSApply(data, "//td/a", xmlValue), id = xpathSApply(data, "//td/a", xmlAttrs))
    plans <- plans[grepl("appeals", id)]
    plans[, id := gsub("appeals|summary|[/]", "", id)]
    
    plans.list[[i]] <- plans
  }
  
  plans.list <- rbindlist(plans.list)
  return(plans.list)
}

#Overall appeal requirements and contributions
fts_get_appeal_totals <- function(appeal_id, year){
  
  required.packages <- c("data.table","jsonlite","httr","XML")
  lapply(required.packages, require, character.only=T)
  
  planlink <- paste0('https://fts.unocha.org/appeals/', appeal_id, "/clusters")
  
  data <- htmlParse(GET(planlink))
  
  plan_name = xpathSApply(data, "//h1[@class='cd-page-title']", xmlValue)
  plan_name <- gsub("\\n", "", plan_name)
  
  total_table <- xpathSApply(data, "//table[@class='header-totals']")
  
  if(length(total_table) != 0){
    table <- data.table(readHTMLTable(total_table[[1]]))
    table <- setnames(data.table(tail(t(table), -1)), gsub("Filtered outgoing", "Outgoing", gsub("Filtered incoming", "Total incoming", table$V1)))
    table <- table[, lapply(.SD, function(x) as.numeric(gsub("US|[$]|,|%", "", x)))]
    
    out <- cbind(appeal_id = appeal_id, plan_name = plan_name, year = year, table)
  } else {
    out <- cbind(appeal_id = appeal_id, plan_name = plan_name, year = year)
  }
  return(out)
}

#Overall appeal requirements and contributions split by COVID
fts_get_appeal_covid <- function(appeal_id, year){
    
    required.packages <- c("data.table","jsonlite","httr","XML")
    lapply(required.packages, require, character.only=T)
    
    planlink <- paste0('https://fts.unocha.org/appeals/', appeal_id, "/summary")
    
    data <- htmlParse(GET(planlink))
    
    plan_name = xpathSApply(data, "//h1[@class='cd-page-title']", xmlValue)
    plan_name <- gsub("\\n", "", plan_name)
    
    tables <- xpathSApply(data, "//div[@class='funding-progress-bar']", xmlGetAttr, "data-bs-content")
    
    if(length(tables) != 0){
      tables <- readHTMLTable(tables)
      names.tables <- xpathSApply(data, "//div[@class='funding-info']", xmlValue)
      
      if(any(grepl("COVID-19", names.tables))){
        covid <- data.table(transpose(tables[grepl(" COVID-19",names.tables)][[1]]))
        non.covid <- data.table(transpose(tables[grepl("-COVID-19",names.tables)][[1]]))
        names(covid) <- paste0("COVID.",unlist(covid[1]))
        names(non.covid) <- unlist(non.covid[1])
      } else {
        if(grepl("COVID", plan_name)){
          covid <- data.table(transpose(tables[[1]]))
          names(covid) <- paste0("COVID.",unlist(covid[1]))
          non.covid <- covid
          non.covid[non.covid != 0] <- 0
          names(non.covid) <- gsub("COVID.", "",names(covid))
        } else {
          non.covid <- data.table(transpose(tables[[1]]))
          names(non.covid) <- unlist(non.covid[1])
          covid <- non.covid
          covid[covid != 0] <- 0
          names(covid) <- paste0("COVID.",names(non.covid))
        }
      }
      
      covid <- covid[-1]
      non.covid <- non.covid[-1]
      
      out <- cbind(appeal_id = appeal_id, plan_name = plan_name, year = year, covid, non.covid)
    } else {
      out <- cbind(appeal_id = appeal_id, plan_name = plan_name, year = year)
    }
    return(out)
}

#Cluster funding and requirements
fts_get_appeal_clusters <- function(appeal_id, year){
  
  required.packages <- c("data.table","jsonlite","httr","XML")
  lapply(required.packages, require, character.only=T)
  
  planlink <- paste0('https://fts.unocha.org/appeals/', appeal_id, "/global-clusters")
  
  data <- htmlParse(GET(planlink))
  
  plan_name = xpathSApply(data, "//h1[@class='cd-page-title']", xmlValue)
  plan_name <- gsub("\\n", "", plan_name)
  
  tables <- readHTMLTable(xpathSApply(data, "//div[@class='view-content row']")[[1]])
  names(tables) <- tables[1,]
  names(tables)[grepl("cluster|sector", names(tables), ignore.case = T)] <- "Cluster"
  tables <- data.table(tables[-1,])
  
  out <- cbind(appeal_id = appeal_id, plan_name = plan_name, year = year, tables)
  return(out)
}
