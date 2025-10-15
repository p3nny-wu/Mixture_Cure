# 01_requirements.R — packages & session info
needed <- c(
  "survival","flexsurv","flexsurvcure",
  "ggplot2","tidyr","dplyr",
  "ggpubr","ggbreak","ggsurvfit"
)

install_if_missing <- function(pkgs) {
  to_install <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(to_install)) {
    install.packages(to_install, repos = "https://cloud.r-project.org")
  }
}
install_if_missing(needed)

# Load packages
suppressPackageStartupMessages({
  library(survival); library(flexsurv); library(flexsurvcure);
  library(ggplot2); library(tidyr); library(dplyr)
  library(ggpubr); library(ggbreak); library(ggsurvfit)
})

# Create output folders if they don't exist
dirs <- c("data","figs","session-info")
for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# Record session info at the end of a full run:
# writeLines(capture.output(sessionInfo()), "session-info/sessionInfo.txt")
