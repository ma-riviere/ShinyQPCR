#########################
# Managing dependencies #
#########################

print("[INFO] Setting up globals")

project_packages <- c()

"%ni%" <- Negate("%in%")

# Sys.setenv(MAKEFLAGS = "-j4")

options(install.packages.check.source = "no")

get_pkg_name <- function(pkg) {
  pkg_name <- pkg
  if (grepl("/", pkg, fixed = TRUE)) {
    pkg_path <- stringr::str_split(pkg, "/")[[1]]
    pkg_name <- pkg_path[length(pkg_path)]
  }
  return(pkg_name)
}

activate_packages <- function() {
  for (pkg in project_packages) {
    activate_package(pkg)
  }
}

activate_package <- function(pkg) {
  pkg_name <- get_pkg_name(pkg)
  if (pkg_name %in% installed.packages()) {
    library(pkg_name, character.only = TRUE, quiet=TRUE)
  }
}

update_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if(pkg %ni% project_packages) {
      project_packages <<- c(project_packages, pkg)
    }
  }
  
  
  for (pkg in project_packages) {
    
    pkg_name <- get_pkg_name(pkg)
    
    if(!(pkg_name %in% installed.packages())) {
      if(grepl("/", pkg, fixed=TRUE)) {
        remotes::install_github(pkg, upgrade = "never", quiet = TRUE, auth_token = Sys.getenv("GITHUB_PAT_R_INSTALL")) # repos = ""
      } else {
        install.packages(pkg, character.only = TRUE, type = "binary", quiet = TRUE, verbose = FALSE)
      }
      
    }
    activate_package(pkg)
  }
  renv::snapshot(type="all", prompt=F)
  #knitr::write_bib(c(.packages(), project_packages), here::here("res/bib", "packages.bib"))
}

# update_packages(c("renv", "here", "glue", "styler", "remotes"))

# -------------------------------------------------------------

pkg_list <- c(
  "shiny",
  "shinyjs",
  "tidyverse",
  "broom",
  # "lmerTest",
  # "broom.mixed",
  # "emmeans",
  # "optimx",
  # "pbkrtest",
  "performance",
  "insight",
  "effectsize",
  "ggnewscale",
  'gtools',
  "rlist",
  "promises",
  "future",
  "future.callr",
  "ipc",
  "magrittr",
  "conflicted",
  "fitdistrplus",
  "goftest",
  "nortest",
  "janitor",
  "stringr",
  "shinydashboard",
  "dashboardthemes",
  # "shinydashboardPlus",
  "shinycssloaders",
  "DT",
  "plotly",
  "ggpubr",
  "moments",
  "effsize",
  "pwr",
  "rentrez",
  "knitr",
  "kableExtra",
  "XML",
  "writexl",
  "rsconnect"
)

# update_packages(pkg_list)

# TODO: automatically write those library() in an external R script and source it from the app.R
## TODO: detect the platform: only source that file if shinyapps 

print("[INFO] Loading libraries")

library(here)
library(glue)
library(shiny)
library(shinyjs)
library(tidyverse)
library(broom)
library(performance)
library(insight)
library(effectsize)
library(gtools)
library(ggnewscale)
library(rlist)
library(promises)
library(future)
library(future.callr)
library(ipc)
library(magrittr)
library(conflicted)
library(fitdistrplus)
library(goftest)
library(nortest)
library(janitor)
library(stringr)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)
library(DT)
library(plotly)
library(ggpubr)
library(moments)
library(effsize)
library(pwr)
library(rentrez)
library(writexl)
library(XML)

# -------------------------------------------------------------

print("[INFO] Loading options")

conflicted::conflict_prefer("filter", "dplyr", quiet = T)
conflicted::conflict_prefer("group_by", "dplyr", quiet = T)
conflicted::conflict_prefer("select", "dplyr", quiet = T)
conflicted::conflict_prefer("box", "shinydashboard", quiet = T)
conflicted::conflict_prefer("isolate", "shiny", quiet = T)
conflicted::conflict_prefer("layout", "plotly", quiet = T)
conflicted::conflict_prefer("extract", "tidyr", quiet = T)

options(install.packages.check.source = "no")
options(dplyr.print_min = 6, dplyr.print_max = 6, scipen = 999, digits = 5)
options(spinner.color = "#0dc5c1")
options(shiny.maxRequestSize = 30 * 1024^2)

alpha <- 0.05
trend <- 0.1
threshold.reg <- 1

set.seed(42)

colors <- c("#00AFBB", "#FF7373")
layer.order <- c("EGL", "EGLo", "EGLi", "ML", "MLPC", "PC", "IGL", "WM")

# -------------------------------------------------------------

print("[INFO] Loading entrez")

ekey <- Sys.getenv("ENTREZ_KEY")
if(!is.null(ekey) & ekey != "") {
  print(glue("[INFO] Entrez API Key detected and loaded: {ekey}"))
  rentrez::set_entrez_key(ekey)
} else {
  print("[INFO] No Entrez API Key detected. Please specify one as the ENTREZ_KEY environment variable.")
}

# -------------------------------------------------------------

# print("[INFO] Loading parallel options")

# plan(callr)
