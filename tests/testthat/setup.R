library(testthat)
library(sf)
library(terra)
library(dplyr)
library(tibble)

# Determine project root safely
cur_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (basename(cur_dir) == "testthat") {
  proj_root <- dirname(dirname(cur_dir))
} else if (basename(cur_dir) == "tests") {
  proj_root <- dirname(cur_dir)
} else {
  proj_root <- cur_dir
}

# Temporarily switch to project root to allow relative sources inside helpers.R
old_wd <- getwd()
setwd(proj_root)

if (!exists("ensure_geometry_name", mode = "function")) {
  source(file.path(proj_root, "R/helpers.R"), local = FALSE)
}
if (!exists("identify_overlaps", mode = "function")) {
  source(file.path(proj_root, "R/functions.R"), local = FALSE)
}

setwd(old_wd)
