#Description: Allows single-line requiring and installing of packages
#Author: Dan W
#Creation: Oct 2021
#Last revision: NA
#Notes: 

requirex <- function(required_packages = c(), ...){
  install.packages(required_packages[!(required_packages %in% installed.packages())])
  lapply(required_packages, require, character.only=T, ...)
}

