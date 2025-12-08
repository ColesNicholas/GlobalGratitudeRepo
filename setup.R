## --------------------------------------------------------------------------------------
# setup.R
## --------------------------------------------------------------------------------------

# install + load
pkg_install_load <- function(pkgs) {
  to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

# download packages
core_pkgs <- c(
  # tidyverse + data management
  "tidyverse", "stringr", "synthpop", 
  "here", "openxlsx", "readr",
  
  # country/global
  "countrycode", "rnaturalearth", "rnaturalearthdata", 
  "ggspatial",
  
  # visualization
  "effsize", "metafor", "scales", 
  "ggtext", "cowplot", "ggExtra", 
  "ggrepel", "gridExtra", "styler",
  
  # analysis
  "psych", "lavaan", "semTools",
  "BayesFactor", "lme4", "lmerTest",
  "emmeans", "gtools", "pbkrtest",
  "overlapping"
)

pkg_install_load(core_pkgs)

# global options
options(
  scipen = 999,
  digits = 3,
  dplyr.summarise.inform = FALSE,
  repos = c(CRAN = "https://cloud.r-project.org"),
  stringsAsFactors = FALSE
)

# Optional: style code
style_project <- function() {
  styler::style_dir(".", filetype = c("R", "Rmd"))
}
