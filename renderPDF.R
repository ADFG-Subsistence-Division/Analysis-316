library(tinytex)
library(tidyverse)

rmd_files <- list.files(paste0(getwd(),"/R-Code/"), 
                         pattern = "\\.Rmd?$", 
                         recursive = TRUE, 
                         full.names = TRUE) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "Step 3|Step 4|Step 5|Step 6")) %>% 
  pull(value)
  
all_pdfs <- sapply(rmd_files, rmarkdown::render, output_format = "pdf_document")
tinytex::install_tinytex()
