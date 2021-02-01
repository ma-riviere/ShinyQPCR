#########################
# Managing dependencies #
#########################

print("[INFO] Setting up globals")

set.github_pat <- function(env.var) {
  github.pat <- Sys.getenv(env.var)
  if (github.pat != "") {
    print(paste("[INFO] Found GITHUB Access Token: ", github.pat))
    GITHUB_PAT <- github.pat
  }
}

set.install_type <- function() {
  sys.name <- Sys.info()[["sysname"]]
  if (sys.name == "Windows") {
    print("[INFO] Windows detected, installing packages as 'binary'")
    options(install.packages.check.source = "no")
    return("binary")
  } else if (sys.name == "Linux") {
    print("[INFO] Linux detected, installing packages as 'source'")
    options(install.packages.check.source = "yes")
    return("source")
  } else {
    print("[INFO] No install type setup for your system, using 'source' as default")
    options(install.packages.check.source = "yes")
    return("source")
  }
}

set.github_pat("GITHUB_PAT_R_INSTALL")
pkg.install.type <- set.install_type()

"%ni%" <- Negate("%in%")

Sys.setenv(MAKEFLAGS = "-j4")

# -------------------------------------------

suppressPackageStartupMessages(TRUE)

project_packages <- c()

get_pkg_name <- function(pkg) {
  pkg_name <- pkg
  if (grepl("/", pkg, fixed = TRUE)) {
    pkg_path <- stringr::str_split(pkg, "/")[[1]]
    pkg_name <- pkg_path[length(pkg_path)]
  }
  if (grepl("@", pkg_name, fixed = TRUE)) {
    pkg_path <- stringr::str_split(pkg, "@")[[1]]
    pkg_name <- pkg_path[1]
  }
  # print(pkg_name)
  return(pkg_name)
}

get_pkg_version <- function(pkg) {
  pkg_version <- NA_character_
  
  if (grepl("@", pkg, fixed = TRUE)) {
    pkg_path <- stringr::str_split(pkg, "@")[[1]]
    pkg_version <- pkg_path[length(pkg_path)]
  }
  # print(pkg_version)
  return(pkg_version)
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

pkg_is_installed <- function(pkg) {
  
  is_installed <- FALSE
  pkg_name <- get_pkg_name(pkg)
  
  if(pkg_name %in% installed.packages()) {
    pkg_version <- get_pkg_version(pkg)
    if(!is.na(pkg_version)) {
      if(pkg_version == packageVersion(pkg_name)) {
        # Packaged is installed and version required matches the installed one
        # print(glue("Package {pkg_name} is already installed"))
        is_installed <- TRUE
      }
    } else {
      # Package is installed and no specific version was asked for
      # print(glue("Package {pkg_name} is already installed"))
      is_installed <- TRUE
    }
  }
  return(is_installed)
}

update_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if(pkg %ni% project_packages) {
      project_packages <<- c(project_packages, pkg)
    }
  }
  
  for (pkg in project_packages) {
    
    if(!pkg_is_installed(pkg)) {
      # remotes::install_github(pkg, upgrade = "never", quiet = TRUE)
      renv::install(pkg, type = pkg.install.type, quiet = TRUE, verbose = FALSE) #character.only = TRUE,  repos = "http://cran.r-project.org"
    }
    activate_package(pkg)
  }
  renv::snapshot(type="all", prompt=F)
}

# update_packages(c("renv", "here", "glue", "styler", "remotes", "magrittr", "miniUI", "tools"))


remove_dependencies <- function(pkg, recursive = FALSE) {
  d <- package_dependencies(, installed.packages(), recursive = recursive)
  depends <- if (!is.null(d[[pkg]])) d[[pkg]] else character()
  needed <- unique(unlist(d[!names(d) %in% c(pkg, depends)]))
  toRemove <- depends[!depends %in% needed]
  if (length(toRemove)) {
    toRemove <- select.list(c(pkg, sort(toRemove)), multiple = TRUE, title = "Select packages to remove")
    remove.packages(toRemove)
    return(toRemove)
  } else {
    invisible(character())
  }
}

# ----------------------------------------------------------------------------

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
