print("[INFO] Setting up globals")

project_packages <- c("renv", "here", "glue", "styler", "remotes")

"%ni%" <- Negate("%in%")

update_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (pkg %ni% project_packages) {
      project_packages <<- c(project_packages, pkg)
    }
  }
  #print(project_packages)

  for (pkg in project_packages) {
    if (!(pkg %in% installed.packages())) {
      if (grepl("/", pkg, fixed = TRUE)) {
        remotes::install_github(pkg)
      } else {
        install.packages(pkg, character.only = TRUE) #, type = "binary"
      }
    }

    if (pkg %in% installed.packages()) {
      library(pkg, character.only = TRUE)
    }
  }
  if (!("renv" %in% installed.packages())) renv::snapshot(type = "all", prompt = F)
}

# -------------------------------------------------------------

options(install.packages.check.source = "no")
options(dplyr.print_min = 6, dplyr.print_max = 6, scipen = 999, digits = 5)
options(spinner.color = "#0dc5c1")
options(shiny.maxRequestSize = 30 * 1024^2)

# -------------------------------------------------------------

pkg_list <- c(
  "shiny",
  "shinyjs",
  "tidyverse",
  "rlist",
  "promises",
  "future",
  "future.callr",
  "ipc",
  "magrittr",
  "conflicted",
  "janitor",
  "stringr",
  "shinydashboard",
  "dashboardthemes",
  "shinydashboardPlus",
  "shinycssloaders",
  "DT",
  "plotly",
  "ggpubr",
  "moments",
  "effsize",
  "pwr",
  "rentrez",
  "XML",
  "rsconnect"
)

update_packages(pkg_list)

#TODO: detect the platform: if shinyapps ...
#TODO: automatically write those library()

library(here)
library(glue)
library(shiny)
library(shinyjs)
library(tidyverse)
library(rlist)
library(promises)
library(future)
library(future.callr)
library(ipc)
library(magrittr)
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
library(XML)

# -------------------------------------------------------------

conflicted::conflict_prefer("filter", "dplyr", quiet = T)
conflicted::conflict_prefer("group_by", "dplyr", quiet = T)
conflicted::conflict_prefer("select", "dplyr", quiet = T)
conflicted::conflict_prefer("box", "shinydashboard", quiet = T)
conflicted::conflict_prefer("isolate", "shiny", quiet = T)
conflicted::conflict_prefer("layout", "plotly", quiet = T)

alpha <- 0.05
set.seed(42)

colors <- c("#00AFBB", "#FF7373")

# -------------------------------------------------------------

ekey <- Sys.getenv("ENTREZ_KEY")
if(!is.null(ekey) & ekey != "") {
  print(glue("[INFO] Entrez API Key detected and loaded: {ekey}"))
  set_entrez_key(ekey)
} else {
  print("[INFO] No Entrez API Key detected. Please specify one as the ENTREZ_KEY environment variable.")
}

plan(callr)
