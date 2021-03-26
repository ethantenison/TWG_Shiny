# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
library(shiny)
library(visNetwork)
library(shinyWidgets)
library(readr)
library(DT)
library(dplyr)
library(rmarkdown)
library(igraph)
library(sna)
library(qgraph)
library(ggplot2)
library(xtable)
library(stats)
library(htmlwidgets)
library(htmltools)
library(rpivotTable)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(mice)
library(VIM)
library(psych)
library(semPlot)
library(lavaan)
library(kableExtra)
library(tidyverse)
library(htmlTable)
library(htmlwidgets)
library(ggmap)
library(DiagrammeR)
library(DiagrammeRsvg)
library(motifr)


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #

 g1 <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1.rds")
 g1_agriculture <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_agriculture.rds")
 g1_environment <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_environment.rds")
 g1_flooding <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_flooding.rds")
 g1_groundwater <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_groundwater.rds")
 g1_innovation <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_innovation.rds")
 g1_municipal <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_municipal.rds")
 g1_oilandgas <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_oilandgas.rds")
 g1_rural <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_rural.rds")

data <- list("g1" = g1,
             "g1_agriculture" = g1_agriculture,
             "g1_environment" = g1_environment,
             "g1_flooding" = g1_flooding,
             "g1_groundwater" = g1_groundwater,
             "g1_innovation" = g1_innovation,
             "g1_municipal" = g1_municipal,
             "g1_oilandgas" = g1_oilandgas,
             "g1_rural" = g1_rural)

g1 <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g12.rds")
g1_agriculture <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_agriculture2.rds")
g1_environment <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_environment2.rds")
g1_flooding <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_flooding2.rds")
g1_groundwater <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_groundwater2.rds")
g1_innovation <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_innovation2.rds")
g1_municipal <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_municipal2.rds")
g1_oilandgas <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_oilandgas2.rds")
g1_rural <- readRDS("C:/Users/tenis/Desktop/Data_Projects/TWG_Shiny/Texas_Water_Governance/data/g1_rural2.rds")

data2 <- list("g1" = g1,
             "g1_agriculture" = g1_agriculture,
             "g1_environment" = g1_environment,
             "g1_flooding" = g1_flooding,
             "g1_groundwater" = g1_groundwater,
             "g1_innovation" = g1_innovation,
             "g1_municipal" = g1_municipal,
             "g1_oilandgas" = g1_oilandgas,
             "g1_rural" = g1_rural)

combined_data <- list("Edge Focused" = data,
                      "Edges and Nodes" = data2)


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #

ui <- fluidPage(

    # Application title
    titlePanel("Texas Water Governance Network"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            selectInput("focus", "Network Focus",
                        c("Edges and Nodes",
                        "Edge Focused")),
            
            selectInput("sectors", "Sector",
                        c("All Sectors" = "g1",
                          "Agriculture" = "g1_agriculture",
                          "Groundwater" = "g1_environment",
                          "Oil and Gas" = "g1_oilandgas",
                          "Rural Utilities" = "g1_rural",
                          "Municipal" = "g1_municipal",
                          "Environment" = "g1_environment",
                          "Flooding" = "g1_flooding",
                          "Innovation" = "g1_innovation")),
            
            sliderInput("edge_width", "Edge Width",
                        min = 0, max = 10,
                        value = 2),
            
            sliderInput("node_size", "Node Size",
                        min = 0, max = 10,
                        value = 2)
            
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", visNetworkOutput("twg_network",height = "800px")),
                    tabPanel("Table", DT::dataTableOutput("table"))
            
        )
        )
    )
)

# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# --------Dashboard Server------- #
# ------------------------------- #
# ------------------------------- #

server <- function(input, output) {
    


    output$twg_network <- renderVisNetwork({
        
        
        ledges <-
            data.frame(
                label = c(
                    "Agriculture",
                    "Environment",
                    "Flooding",
                    "Groundwater",
                    "Oil and Gas",
                    "Rural Utilities",
                    "Innovation",
                    "Municipal"
                ),
                arrows = c("to"),
                color = c(
                    "#7be141",
                    "#00b294",
                    "#97c2fc",
                    "#ad85e4",
                    "#fb7e81",
                    "#ffa807",
                    "#ec008c",
                    "#eb7df4"
                )
            )
        
        lnodes <-
            data.frame(
                label = c(
                    "Government",
                    "NGO",
                    "Private",
                    "Public",
                    "Utilities",
                    "Physical Feature",
                    "Academic",
                    "Local",
                    "Regional",
                    "Statewide"
                ),
                color.background = c(
                    "#fb7e81",
                    "#ad85e4",
                    "#7be141",
                    "#eb7df4",
                    "#ffa807",
                    "#f6eb14",
                    "#97c2fc",
                    "white",
                    "white",
                    "white"
                ),
                color.border = c(
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black",
                    "black"
                ),
                shape = c(
                    "box",
                    "box",
                    "box",
                    "box",
                    "box",
                    "box",
                    "box",
                    "dot",
                    "triangle",
                    "square"
                )
            )
        
        
        
        
        gvis <- toVisNetworkData(combined_data[[input$focus]][[input$sectors]])
        gvis$nodes$size = gvis$nodes$size + input$node_size * 2

        
        visNetwork(
            sort(gvis$nodes),
            gvis$edges,
            #main = "Policy and Governance Across Texas Water Sectors",
            width = "100%",
            height = "850px"
        ) %>%
            visEdges(
                smooth = T,
                arrows = list(to = list(
                    enabled = TRUE, scaleFactor = .5
                )),
                color = list(highlight = "black"),
                width = input$edge_width
            ) %>% #https://datastorm-open.github.io/visNetwork/edges.html
            visNodes(color = list(
                background = "white",
                border = "black",
                highlight = list(background = "#A9A9A9", border = "black"),
                hover = list(background = "#A9A9A9", border = "black")
            )) %>%
            visPhysics(stabilization = FALSE)  %>%
            visIgraphLayout(smooth = FALSE,
                            physics = FALSE,
                            layout = "layout_with_fr",
                            randomSeed = 27) %>%
            visInteraction(navigationButtons = TRUE) %>%
            visOptions(
                selectedBy = list(variable = c("type"), multiple = TRUE),
                highlightNearest = list(enabled = T, hover = T),
                nodesIdSelection = TRUE
            ) %>%
            visLegend(
                position = "left",
                addNodes = lnodes,
                addEdges = ledges,
                useGroups = FALSE,
                ncol = 1,
                width = 0.2,
                zoom = FALSE
            ) %>% addFontAwesome()
    })
    
    output$table <- renderDataTable({
        
        gvis <- toVisNetworkData(combined_data[[input$focus]][[input$sectors]])
        nodelist <- gvis$nodes
        nodelist$size <- as.integer(nodelist$size)
        rownames(nodelist)<-NULL
        nodelist <- nodelist %>% select('id', 'level', 'type', 'size') %>% rename("Node" = "id", "Juristication" = "level",
                                                                                  "Oragnizatino Type" = "type", "Number of Connections" = "size")
        
        datatable(nodelist, options = list(pageLength = 20))
    })
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- # 
shinyApp(ui = ui, server = server)
