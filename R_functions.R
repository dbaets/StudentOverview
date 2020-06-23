library(tidyverse)
library(readxl)

# loading .xlsx files
f_load_xlsx <- function(path, sheet){
  t_input <- read_xlsx(path = path,sheet = sheet)
  return(t_input)
}


f_selectdf <- function(vakcijfer){
  t_input <- NA
  if (vakcijfer == 1){
    t_input <- t_fys
  } else if(vakcijfer == 2){
    t_input <- t_che
  } else if(vakcijfer == 3){
    t_input <- t_bio
  } else if(vakcijfer == 4){
    t_input <- t_nwe
  } else{
    print("Vak selectie is onjuist. Dit vak is niet geladen.")
  }
}