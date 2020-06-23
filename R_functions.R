library(tidyverse)
library(readxl)

# loading .xlsx files
f_load_xlsx <- function(path, sheet){
  t_input <- read_xlsx(path = path,sheet = sheet)
  return(t_input)
}
