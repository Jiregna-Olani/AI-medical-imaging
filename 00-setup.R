install.packages("pak")

pak::pkg_install(c(

  # requirements
  c("gt", "ggrepel", "here", "janitor", "psych", "readxl", "RColorBrewer" , "scales", "stringr", "tidyr", "tidyverse")

), dependencies = TRUE)

renv::snapshot()
