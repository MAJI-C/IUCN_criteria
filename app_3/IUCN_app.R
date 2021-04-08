library("shiny")
library("shinythemes")
library("RColorBrewer")
library("tm")
library("DT")
library("memoise")
library('rworldmap')
library('leaflet')



### Table 
Tab_1 <- read.csv("File_2.csv", header=TRUE)
Tab_2 <- read.csv("File_6.csv", header=TRUE)
sketch_1 = htmltools::withTags(table(class = 'display',
                                     thead(
                                       tr(
                                         th(rowspan=2, 'Year'),
                                         th(colspan = 11, 'Critically Endangered'),
                                         th(colspan = 11, 'Endangered'),
                                         th(colspan=11, 'Vulnerable')),
                                       tr(
                                         lapply(rep(c('Mammals', ' Birds', ' Reptiles', 'Amphibians', 
                                                      'Fishes', 'Insects', ' Molluscs', 'Other Inverts', 'Plants', 'Fungi & Protists', 'Total'), 3), th)))))

sketch_2 = htmltools::withTags(table(class = 'display',
                                     thead(
                                       tr(
                                         th(rowspan=2, 'Country'),
                                         th(rowspan=2, 'Continent'),
                                         th(rowspan=2, 'Region'),
                                         th(colspan = 10, ' Threatened species'),
                                         th(colspan = 10, 'Animals'),
                                         th(colspan = 10, 'Plants')
                                       ),
                                       tr(
                                         lapply(c('Mammals', ' Birds', ' Reptiles', 'Amphibians', 
                                                  'Fishes', ' Molluscs', 'Other Inverts', 'Plants', 'Fungi & Protists', 'Total'),  th),
                                         lapply(rep(c(' Extinct', 'Extinct in the Wild', ' Critically Endangered', ' Endangered',
                                                      ' Vulnerable', 'Near Threatened', 'Lower Risk/conservation dependent', 
                                                      ' Data Deficient', ' Least Concern', 'Total'), 2), th)))))

### 
vars<-c("Threatened Mammals" = "Threatened_Mammals",
        "Threatened Birds" = "Threatened_Birds",	
        "Threatened Reptiles" = "Threatened_Reptiles",
        "Threatened Amphibians" = "Threatened_Amphibians",
        "Threatened Fishes"="Threatened_Fishes",	
        "Threatened Molluscs"="Threatened_Molluscs",	
        "Threatened Other Inverts"="Threatened_Other_Inverts",	
        "Threatened Plants"="Threatened_Plants",	
        "Threatened Fungi"="Threatened_Fungi",
        "Threatened Total"= "Threatened_Total",
        "Extinct Animals"="Animals_EX",	
        'Extinct in the Wild Animals'="Animals_EW",	
        'Critically Endangered Animals'="Animals_CR",	
        'Endangered Animals'="Animals_EN",
        'Vulnerable Animals'="Animals_VU",
        'Near Threatened Animals'="Animals_NT",
        'Lower Risk/conservation dependent Animals'="Animals_LR.cd",
        'Data Deficient Animals'=" Animals_DD",
        'Least Concern Animals'="Animals_LC",	
        "Animals Total"	="Animals_Total",
        "Extinct Plants"="Plants_EX",
        'Extinct in the Wild Plants'="Plants_EW",
        'Critically Endangered Plants'="Plants_CR",	
        'Endangered Plants'="Plants_EN",
        'Vulnerable Plants'="Plants_VU",
        'Near Threatened Plants'="Plants_NT",	
        'Lower Risk/conservation dependent Plants'="Plants_LR",	
        'Data Deficient Plants'="Plants_DD",
        'Least Concern Plants'="Plants_LC",	
        "Plants Total"="Plants_Total")


dF<-data.frame(Country=Tab_2$Country, Threatened_Mammals=Tab_2$Threatened_Mammals, Threatened_Birds=Tab_2$Threatened_Birds, 
               Threatened_Reptiles=Tab_2$Threatened_Reptiles, Threatened_Amphibians=Tab_2$Threatened_Amphibians,
               Threatened_Fishes=Tab_2$Threatened_Fishes, Threatened_Molluscs=Tab_2$Threatened_Molluscs,
               Threatened_Other_Inverts =Tab_2$Threatened_Other.Inverts, Threatened_Plants=Tab_2$Threatened_Plants,
               Threatened_Fungi=Tab_2$Threatened_Fungi, Threatened_Total=Tab_2$Threatened_Total, Animals_EX	= Tab_2$Animals_EX,
               Animals_EW	= Tab_2$Animals_EW, Animals_CR = Tab_2$Animals_CR, Animals_EN = Tab_2$Animals_EN, 
               Animals_VU	= Tab_2$Animals_VU, Animals_NT = Tab_2$Animals_NT, 
               Animals_DD = Tab_2$Animals_DD, Animals_LC=Tab_2$Animals_LC, Animals_Total=Tab_2$Animals_Total,
               Plants_EX=Tab_2$Plants_EX, Plants_EW	= Tab_2$Plants_EW, Plants_CR=Tab_2$Plants_CR, 
               Plants_EN=Tab_2$Plants_EN, Plants_VU	= Tab_2$Plants_VU, Plants_NT = Tab_2$Plants_NT, 
               Plants_LR= Tab_2$Plants_LR, Plants_DD=Tab_2$Plants_DD, Plants_LC	= Tab_2$Plants_LC, Plants_Total=Tab_2$Plants_Total,
               Animals_LR.cd = Tab_2$Animals_LR.cd)

sPDF<-joinCountryData2Map( dF, joinCode = "NAME", nameJoinColumn = "Country")
dF <- sPDF@data

ui<-shinyUI(
  navbarPage("IUCN Red List Categories", id="nav", theme = shinytheme("yeti"),
                   tags$head(tags$style(
                     HTML('#controls {background-color: rgba(0,0,0,0); border: 0; box-shadow: none;}
                          #category {background-color: rgba(0,0,0,0.5); text-align: center;;}
                          #myMap {background-color: white;}
                          #myPlot {background-color: #CD0000 ;}'))),
                   ### Intro
                   tabPanel("About", 
                            fluidRow ( titlePanel( title=h1('IUCN Red List Categories', align='center')), 
                                       br(),
                                       column(6, 
                                              p('Extinct (EX) – No known individuals remaining.'),
                                              br(),
                                              p('Extinct in the wild (EW) – Known only to survive in captivity, or as a naturalized population outside its historic range.'),
                                              br(),
                                              p('Critically endangered (CR) – Extremely high risk of extinction in the wild.'),
                                              br(),
                                              p('Endangered (EN) – High risk of extinction in the wild.'),
                                              br(),
                                              p('Vulnerable (VU) – High risk of endangerment in the wild.'),
                                              br(),
                                              p('Near threatened (NT) – Likely to become endangered in the near future.'),
                                              br(),
                                              p('Least concern (LC) – Lowest risk. Does not qualify for a more at-risk category. Widespread and abundant taxa are included in this category.'),
                                              br(),
                                              p('Data deficient (DD) – Not enough data to make an assessment of its risk of extinction.'),
                                              br(),
                                              p('Not evaluated (NE) – Has not yet been evaluated against the criteria.')),
                                       column(6, tags$img(src='https://upload.wikimedia.org/wikipedia/en/e/ec/IUCN_Red_List.svg', height=550, weight=550)))),
                   
                   ### Map
                   tabPanel("Map", 
                            leafletOutput ("myMap", "200%", 800),
                            absolutePanel(id='controls', 
                                          class='panel panel-default', fixed=TRUE, draggable = FALSE, top=70,
                                          left='auto', right=0, bottom='auto', width=500,
                                          height='auto', h4(" Red List Category summary country totals "),
                                          selectInput('category', "Category", vars),
                                          conditionalPanel("input.category == 'Extinct Animals'"), 
                                          plotOutput  ('myBar', height=250, width=300),
                                          br(),
                                          plotOutput('myBox', height=350, width=450))),
                   
                   
                   tabPanel( "Plot", titlePanel(title= h1(" Changes in numbers of species in the threatened categories IUCN\n from 1996 to 2016 ")),
                             fluidRow(id="myPlot",
                                      column(4, plotOutput("myPlot_1")),
                                      column(4, plotOutput("myPlot_2")),
                                      column(4, plotOutput("myPlot_3"))),
                             br(),               
                             fluidRow(id="myInput",
                                      column(4, selectInput("variable", "Group:",
                                                            c("Mammals" = "CR_Mammals", 
                                                              "Birds" = "CR_Birds", 
                                                              "Reptilies"="CR_Reptiles",
                                                              "Amphibians"="CR_Amphibians",
                                                              "Fishes" = "CR_Fishes",
                                                              "Insects"="CR_Insects",
                                                              "Molluscs"="CR_Molluscs",
                                                              "Other Invertebrates"="CR_Other_Int",
                                                              "Plants"="CR_Plants"))),
                                      column(4, selectInput("variable_1", "Group:",
                                                            c("Mammals" = "EN_Mammals", 
                                                              "Birds" = "EN_Birds", 
                                                              "Reptilies"="EN_Reptiles",
                                                              "Amphibians"="EN_Amphibians",
                                                              "Fishes" = "EN_Fishes",
                                                              "Insects"="EN_Insects",
                                                              "Molluscs"="EN_Molluscs",
                                                              "Other Invertebrates"="EN_Other_Int",
                                                              "Plants"="EN_Plants"))),
                                      column(4, selectInput("variable_2", "Group:",
                                                            c("Mammals" = "VU_Mammals", 
                                                              "Birds" = "VU_Birds", 
                                                              "Reptilies"="VU_Reptiles",
                                                              "Amphibians"="VU_Amphibians",
                                                              "Fishes" = "VU_Fishes",
                                                              "Insects"="VU_Insects",
                                                              "Molluscs"="VU_Molluscs",
                                                              "Other Invertebrates"="VU_Other_Int",
                                                              "Plants"="VU_Plants"))))), 
                   #### Tables  
                   tabPanel("Table 1", DT :: dataTableOutput("Table_1")),
                   
                   tabPanel("Table 2", DT :: dataTableOutput("Table_2"))))


server<-function(input, output, session) {
  
  ### Tabeli 
  output$Table_1 <- DT :: renderDataTable({
    DT :: datatable(Tab_1, options = list(paging = FALSE), class = 'cell-border strip', container = sketch_1, rownames = FALSE)})
  
  output$Table_2 <- DT :: renderDataTable({
    DT:: datatable(Tab_2, options = list(pageLength = 7), class = 'cell-border strip', container = sketch_2, rownames = FALSE)})
  
  ### trend    
  output$myPlot_1 <- renderPlot({
    formulaText <- reactive({
      paste(input$variable, "~ Year ")})
    output$caption <- renderText({
      formulaText()})
    plot(as.formula(formulaText()), 
         data = Tab_1, type="l", col="white", yaxt='n', ylab='',frame.plot=F, lwd=3, col.lab='white')
    axis(1,  las=1, col.axis="white")
    axis(2,  las=1, col.axis="white")
    title(main='CR', col.main='white')}, bg="transparent")
  
  output$myPlot_2 <- renderPlot({
    formulaText <- reactive({
      paste(input$variable_1, "~ Year ")})
    output$caption <- renderText({
      formulaText()})
    plot(as.formula(formulaText()), 
         data = Tab_1, type="l", col="white",  yaxt='n', ylab='', frame.plot=F, lwd=3, col.lab='white')
    axis(1,  las=1, col.axis="white")
    axis(2,  las=1, col.axis="white")
    title(main='EN', col.main='white')}, bg="transparent")
  
  
  output$myPlot_3 <- renderPlot({
    formulaText <- reactive({
      paste(input$variable_2, "~ Year ")})
    output$caption <- renderText({
      formulaText()})
    plot(as.formula(formulaText()), 
         data = Tab_1, type='l', col = 'white',  yaxt='n', ylab='', frame.plot=F, lwd=3, col.lab='white')
    axis(1,  las=1, col.axis="white")
    axis(2,  las=1, col.axis="white")
    title(main='VU', col.main='white', main.font=3)}, bg="transparent")
  
  
  ### Map 
  output$myMap  <- renderLeaflet ({ 
    leaflet() %>%
      addTiles() %>%
      fitBounds(0, 0, 11, 11) %>%
      setView(320, 0, zoom = 2.2 )})
  
  
  observe ({
    colorBy<-input$category
    colorData<-sPDF[[colorBy]]
    pal<-colorNumeric('YlOrRd', colorData)
    
    labels <- sprintf("<strong>%s</strong><br/>%g <sup></sup ", dF$Country, colorData) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy('myMap', data=sPDF) %>%
      clearShapes() %>%
      addPolygons(fillColor = pal(colorData), 
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      addLegend("bottomleft", pal = pal, values =  colorData,  opacity = 1, title = colorBy, layerId="colorLegend")})
  
  output$myBar<-renderPlot({
    par(mar=c(3,12,0,0))
    data1<-dF[order(dF[,input$category], decreasing = TRUE),]
    barplot(sort(dF[,input$category], decreasing = TRUE)[1:20], names.arg = data1$Country[1:20],
            cex.names=0.8, axis.lty = 90, horiz=TRUE, col='red2',  border = NA, las=2)}, bg="transparent") 
  
  output$myBox<-renderPlot({
    par(mar=c(10,5,0,0)) 
    boxplot(split(dF[,input$category], dF$REGION ), col='red2', las=2, frame.plot=F)}, bg='transparent')
}
shinyApp(ui=ui, server=server)
