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
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            visNetworkOutput("twg_network",height = "800px")
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
        
        gvis <- toVisNetworkData(combined_data[[input$focus]][[input$sectors]])
        nodelist <- gvis$nodes
        
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
                color = list(highlight = "black")
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
             addFontAwesome()# %>%
            # visLegend(
            #     position = "left",
            #     addNodes = lnodes,
            #     addEdges = ledges,
            #     useGroups = FALSE,
            #     stepY = 100
             #) 
    })
}

# ------------------------------- #
# ------------------------------- #
# -----Run the application------- #
# ------------------------------- #
# ------------------------------- # 
shinyApp(ui = ui, server = server)
