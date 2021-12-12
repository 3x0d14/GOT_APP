my_packages = c("shiny",
                "dplyr",
                "sf",
                "ggplot2",
                "leaflet",
                "plotly",
                "shinyjs",
                "shinyBS",
                "padr",
                "png")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))