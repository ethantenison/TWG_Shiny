# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# -----------Libraries----------- #
# ------------------------------- #
# ------------------------------- #
library(visNetwork)
library(shinyWidgets)
library(readr)
library(DT)
library(igraph)
library(sna)
library(qgraph)
library(ggplot2)
library(htmlwidgets)
library(htmltools)
library(mice)
library(VIM)
library(lavaan)
library(kableExtra)
library(tidyverse)
library(htmlTable)
library(htmlwidgets)
library(ggmap)
library(rsconnect)


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ------- Reference Data -------- #
# ------------------------------- #
# ------------------------------- #

cords <- read_csv("./data/node_coords.csv")
cords <- select(cords, id, x, y)
row.names(cords) <- cords[['id']]

g1 <- readRDS("./data/g1.rds")
g1_agriculture <- readRDS("./data/g1_agriculture.rds")
g1_environment <- readRDS("./data/g1_environment.rds")
g1_flooding <- readRDS("./data/g1_flooding.rds")
g1_groundwater <- readRDS("./data/g1_groundwater.rds")
g1_innovation <- readRDS("./data/g1_innovation.rds")
g1_municipal <- readRDS("./data/g1_municipal.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas.rds")
g1_rural <- readRDS("./data/g1_rural.rds")

data <- list(
    "g1" = g1,
    "g1_agriculture" = g1_agriculture,
    "g1_environment" = g1_environment,
    "g1_flooding" = g1_flooding,
    "g1_groundwater" = g1_groundwater,
    "g1_innovation" = g1_innovation,
    "g1_municipal" = g1_municipal,
    "g1_oilandgas" = g1_oilandgas,
    "g1_rural" = g1_rural
)



g1 <- readRDS("./data/g12.rds")
g1_agriculture <- readRDS("./data/g1_agriculture2.rds")
g1_environment <- readRDS("./data/g1_environment2.rds")
g1_flooding <- readRDS("./data/g1_flooding2.rds")
g1_groundwater <- readRDS("./data/g1_groundwater2.rds")
g1_innovation <- readRDS("./data/g1_innovation2.rds")
g1_municipal <- readRDS("./data/g1_municipal2.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas2.rds")
g1_rural <- readRDS("./data/g1_rural2.rds")

data2 <- list(
    "g1" = g1,
    "g1_agriculture" = g1_agriculture,
    "g1_environment" = g1_environment,
    "g1_flooding" = g1_flooding,
    "g1_groundwater" = g1_groundwater,
    "g1_innovation" = g1_innovation,
    "g1_municipal" = g1_municipal,
    "g1_oilandgas" = g1_oilandgas,
    "g1_rural" = g1_rural
)



g1 <- readRDS("./data/g13.rds")
g1_agriculture <- readRDS("./data/g1_agriculture3.rds")
g1_environment <- readRDS("./data/g1_environment23.rds")
g1_flooding <- readRDS("./data/g1_flooding3.rds")
g1_groundwater <- readRDS("./data/g1_groundwater3.rds")
g1_innovation <- readRDS("./data/g1_innovation3.rds")
g1_municipal <- readRDS("./data/g1_municipal3.rds")
g1_oilandgas <- readRDS("./data/g1_oilandgas3.rds")
g1_rural <- readRDS("./data/g1_rural3.rds")

data3 <- list(
    "g1" = g1,
    "g1_agriculture" = g1_agriculture,
    "g1_environment" = g1_environment,
    "g1_flooding" = g1_flooding,
    "g1_groundwater" = g1_groundwater,
    "g1_innovation" = g1_innovation,
    "g1_municipal" = g1_municipal,
    "g1_oilandgas" = g1_oilandgas,
    "g1_rural" = g1_rural
)



combined_data <- list(
    "Edge Focused" = data,
    "Edges and Nodes" = data2,
    "Node Focused" = data3
)



titles <- list(
    "g1" = "All Sectors" ,
    "g1_agriculture" = "Agriculture",
    "g1_environment" = "Groundwater",
    "g1_oilandgas" = "Oil and Gas",
    "g1_rural" = "Rural Utilities",
    "g1_municipal" = "Municipal",
    "g1_environment" = "Environment",
    "g1_flooding" = "Flooding",
    "g1_innovation" = "Innovation"
)


# ------------------------------- #
# ------------------------------- #
# ------------SECTION:----------- #
# ----------Dashboard UI--------- #
# ------------------------------- #
# ------------------------------- #

ui <- fluidPage(

    # Application title
    titlePanel(
        h1("Texas Water Governance Network", align = "center")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            selectInput("focus", "Network Focus",
                        c("Edges and Nodes",
                        "Edge Focused",
                        "Node Focused")),
            
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
                        value = 2),
            
           imageOutput("legend"),
            
            
            
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

server <- function(input, output, session) {
    output$legend <- renderImage({
        if (input$focus == "Edges and Nodes" & input$sectors == "g1") {
            filename <-
                normalizePath(file.path('./www/images/full_network_sector_legend.png'))
            
            width  <- session$clientData$output_legend_width
            height <- session$clientData$output_legend_height
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        
        else if (input$focus == "Edges and Nodes" &
                 input$sectors != "g1") {
            filename <-
                normalizePath(file.path(
                    './www/images/full_network_sectorattributes_legend.png'
                ))
            
            width  <- session$clientData$output_legend_width
            height <- session$clientData$output_legend_height
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Edge Focused" &
                 input$sectors == "g1") {
            filename <-
                normalizePath(file.path('./www/images/edge_network_sector_legend.png'))
            
            width  <- session$clientData$output_legend_width
            height <- session$clientData$output_legend_height
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Edge Focused" &
                 input$sectors != "g1") {
            filename <-
                normalizePath(file.path(
                    './www/images/edge_network_sectorattributes_legend.png'
                ))
            
            width  <- session$clientData$output_legend_width
            height <- session$clientData$output_legend_height
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        else if (input$focus == "Node Focused") {
            filename <-
                normalizePath(file.path('./www/images/node_network_legend.png'))
            
            width  <- session$clientData$output_legend_width
            height <- session$clientData$output_legend_height
            
            list(
                src = filename,
                alt = paste("test"),
                width = width,
                height = height
            )
        }
        
    }, deleteFile = FALSE)
    
    output$twg_network <- renderVisNetwork({
        gvis <-
            toVisNetworkData(combined_data[[input$focus]][[input$sectors]])
        
        nodes <- sort(gvis$nodes)
        nodes$size <- nodes$size + input$node_size * 2
        
        edges <- gvis$edges
        edges$label <- edges$type
        
        network <- visNetwork(
            nodes,
            edges,
            main = titles[[input$sectors]],
            width = "100%",
            height = "850px"
        ) %>%
            visEdges(
                arrows = list(to = list(
                    enabled = TRUE, scaleFactor = .5
                )),
                color = list(highlight = "black"),
                width = input$edge_width,
                label = TRUE
            ) %>% #https://datastorm-open.github.io/visNetwork/edges.html
            visNodes(color = list(
                background = "white",
                border = "black",
                highlight = list(background = "#A9A9A9", border = "black"),
                hover = list(background = "#A9A9A9", border = "black")
            )) %>%
            visIgraphLayout(
                smooth = list(enabled = T, type = 'dynamic'),
                physics = FALSE,
                layout = "layout_with_fr",
                randomSeed = 27
            ) %>%
            visInteraction(navigationButtons = TRUE) %>%
            visOptions(
                selectedBy = list(variable = c("type"), multiple = TRUE),
                highlightNearest = list(enabled = T, hover = T),
                nodesIdSelection = TRUE
            ) %>%
            addFontAwesome()
        
        if (input$focus == "Edges and Nodes" &
            input$sectors == "g1" |
            input$focus == "Edge Focused" &
            input$sectors == "g1" |
            input$focus == "Node Focused" & input$sectors == "g1") {
            network$x$nodes <-
                network$x$nodes %>% left_join(cords, by = 'id') %>% select(-c('x.x', 'y.x')) %>% rename(x = x.y, y =
                                                                                                            y.y)
            network
        }
        else {
            network
        }
        
    })
    
    output$table <- renderDataTable({
        gvis <-
            toVisNetworkData(combined_data[[input$focus]][[input$sectors]])
        nodelist <- gvis$nodes
        nodelist$size <- as.integer(nodelist$size)
        rownames(nodelist) <- NULL
        nodelist <-
            nodelist %>% dplyr::select('id', 'level', 'type', 'size') %>% dplyr::rename(
                "Node" = "id",
                "Juristication" = "level",
                "Oragnizatino Type" = "type",
                "Number of Connections" = "size"
            )
        
        datatable(nodelist, options = list(pageLength = 20))
    })
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- # 
shinyApp(ui = ui, server = server)
