# Biodiversity DashBoard

The Biodiversity Dashboard is a Shiny app built to display data on endangered and threatened species across different continents, regions, and countries. The data used in this project is derived from the World Wildlife Fund and the International Union for Conservation of Nature (IUCN).

#### Installation

1. To run the app, you need to have R installed on your computer.
2. You can download or clone the project from GitHub. Open R and install the required packages.

```R 
install.packages(c("shiny", "shinythemes", "RColorBrewer", "tm", "DT", "memoise", "rworldmap", "leaflet"))
```

#### Usage

To run the app, open the app.R file in RStudio and click on the "Run App" button. Alternatively, you can run the following command in R.

```R
shiny::runApp("path/to/app.R")
```

Once the app is running, you can select the desired tab to view the data. The first tab displays data on the number of critically endangered, endangered, and vulnerable species across different taxonomic groups. The second tab displays data on the number of threatened species across different continents, regions, and countries.

#### Data

The data used in this project is derived from the International Union for Conservation of Nature (IUCN). The data is stored in two CSV files, File_2.csv and File_6.csv, which are loaded into the app using the read.csv() function.

