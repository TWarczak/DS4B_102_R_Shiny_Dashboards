# DS4B 102-R: SHINY APPS - LEVEL 1 ----
# R PACKAGES

r_pkgs <- c(
    # Core
    "tidyverse",
    "tidyquant",

    # Database
    "odbc",
    "RSQLite",

    # Visualization
    "plotly",

    # Shiny-verse
    "flexdashboard",
    "shiny",
    "shinyWidgets",
    "shinyjs",

    # Modeling & Machine Learning
    "parsnip",
    "rsample",
    "xgboost"
    )

install.packages(r_pkgs)


# Protip: If you are installing this packages and if you want to know what packages are installed so you don´t install them again you can use

install.packages(r_pkgs[!(r_pkgs %in% installed.packages())])
