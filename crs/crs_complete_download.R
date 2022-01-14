#Description: Downloads complete CRS based on start and end year parameters. 
#Author: Dan W
#Creation: Oct 2021
#Last revision: NA
#Notes: These are large downloads: they will take some time. When choosing a start or end year which falls in the middle of a multi-year file provided by OECD, the whole file is downloaded first.

crs_get <- function(start_year = NULL, end_year = NULL){
  lapply(c("data.table", "rvest"), require, character.only = T)
  
  base.url <- "https://stats.oecd.org/DownloadFiles.aspx?DatasetCode=CRS1"
  downloads <- html_attr(html_nodes(read_html(base.url), "a"), "onclick")
  downloads <- rev(paste0("http://stats.oecd.org/FileView2.aspx?IDFile=", gsub("_", "-", gsub("return OpenFile|[(][)];", "", downloads))))
  download_names <- rev(html_text(html_nodes(read_html(base.url), "a")))
  download_years <- gsub("[/].*|\\D", "", download_names)
  download_years_s <- as.numeric(substr(download_years, 1,4))
  download_years_e <- as.numeric(paste0(substr(download_years, 1,2), substr(download_years,nchar(download_years)-1,nchar(download_years))))
  
  if(!is.null(start_year)){
    downloads <- downloads[download_years_e >= start_year]
    download_years_s <- download_years_s[download_years_e >= start_year]
    download_names <- download_names[download_years_e >= start_year]
  }
  if(!is.null(end_year)){
    downloads <- downloads[download_years_s <= end_year]
    download_names <- download_names[download_years_s <= end_year]
  }
  
  crs <- list()
  for(i in 1:length(downloads)){
    message(paste0("Downloading "), download_names[i])
    download <- downloads[i]
    temp <- tempfile()
    download.file(download, temp, mode="wb", quiet=T)
    filename <- unzip(temp, list=T)$Name
    unztemp <- unzip(temp, filename)
    crs[[i]] <- fread(unztemp, fill=T)
    unlink(temp)
    file.remove(unztemp)
  }
  
  crs <- rbindlist(crs, fill = T)
  crs <- crs[Year >= start_year & Year <= end_year]
  return(crs)
}
