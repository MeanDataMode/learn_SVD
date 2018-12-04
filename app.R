library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(readr)
library(RColorBrewer)


# Load data
load("data/data.Rda")
cust_names <- row.names(data)
SVD <- svd(x = data)
data <- data.frame(data)


# Define UI
ui <- fluidPage(theme = shinytheme("slate"),
                fluidRow(
                    column(12,
                           titlePanel("Learning Singular Value Decomposition for Dimensionality Reduction"))),
                    fluidRow(
                        column(12,
                               h4("by: Anthony 'Tony' Layton"),
                               fluidRow(
                                   column(4,
                                          # Select Customer to plot
                                          selectizeInput(inputId = "Customer", 
                                                         label = h2("Customer Name"), 
                                                         choices = cust_names, 
                                                         selected = "Erin", 
                                                         multiple = TRUE),
                                          # Select Number of Recommendations to display.
                                          sliderInput(inputId = "nRecommend",
                                                      label = h3("Number of Recommendations:"),
                                                      min = 1,
                                                      max = 8,
                                                      value = 8)),
                                    column(8,
                                           h3("Recommended Drinks"),
                                           tableOutput('table'))),
                                fluidRow(
                                    column(12, 
                                       sliderInput(inputId = "components",
                                                   label = h3("Number of Components:"),
                                                   min = 2,
                                                   max = 8,
                                                   value = 3))))),
                    fluidRow(
                        mainPanel(
                            fluidRow(
                                column(width = 3,
                                       h4("Model Variance:")),
                                column(width = 6,
                                       h4(textOutput("variance")))),
                            fluidRow(
                                column(width = 12,
                                       h2("Original Matrix"),
                                       tableOutput('original_data'))),
                            fluidRow(
                                column(width = 12,
                                       plotOutput('original_heat'))),
                            fluidRow(
                                column(width = 12, 
                                       h2("Singular Value Decomposition"))),
                            fluidRow(
                                column(width = 6,
                                       tableOutput("SVD_d")),
                                column(width = 6, 
                                       tableOutput("SVD_v"))),
                            fluidRow(
                                column(width = 6, 
                                       tableOutput("SVD_u"))),
                            fluidRow(
                                column(width = 6, 
                                       plotOutput("SVD_dp")),
                                column(width = 6, 
                                       plotOutput("SVD_vp"))),
                            fluidRow(
                                column(width = 6, 
                                       plotOutput("SVD_up"))),
                            fluidRow(
                                column(width = 12, 
                                       h2("Reconstructed Matrix with 'n' Variables"))),
                            fluidRow(
                                column(width = 12, 
                                       tableOutput('recon_table'))),
                            fluidRow(
                                column(width = 12, 
                                       plotOutput("recon_plot"))),
                            fluidRow(
                                column(width = 12, 
                                       h2("Reconstructed (-) Original = Recommended"))),
                            fluidRow(
                                column(width = 12, 
                                       tableOutput("recommend_t"))),
                            fluidRow(
                                column(width = 12, 
                                       plotOutput("recommend_heat")))
                            )
                        )
                )
                                    

# Define server function
server <- function(input, output) {
    # Compute Components
    comp <- reactive({
        input$components
    })
    
    # Compute Variance
    output$variance <- reactive({format(sum(prop.table(SVD$d^2)[1:comp()]), digits = 2)})
    
    # SVD$d
    output$SVD_d <- renderTable({diag(SVD$d[1:comp()])})
    output$SVD_dp <- renderPlot({
        heatmap(diag(SVD$d[1:comp()]), Rowv=NA, Colv=NA, col = brewer.pal(5,"Blues"), revC = T, scale= "none", margins=c(2,2))})
    
    # SVD$u
    output$SVD_u <- renderTable({SVD$u[,1:comp()]})        
    output$SVD_up <- renderPlot({
        heatmap(SVD$u[,1:comp()], Rowv=NA, Colv=NA, col = brewer.pal(5,"Blues"), revC = T, scale= "none", margins=c(2,2))})
    
    # SVD$v
    output$SVD_v <- renderTable({t(SVD$v[,1:comp()])})
    output$SVD_vp <- renderPlot({
        heatmap(t(SVD$v[,1:comp()]), Rowv=NA, Colv=NA, col = brewer.pal(5,"Blues"), revC = T, scale= "none", margins=c(2,2))
    })
    
    # Reconstruct
    recon_matrix <- reactive({(SVD$u[,1:comp()] %*% diag(SVD$d[1:comp()]) %*% t(SVD$v[,1:comp()]))})
    output$recon_table <- renderTable({recon_matrix()}, include.rownames=TRUE)
    output$recon_plot <- renderPlot({
        heatmap(recon_matrix(), Rowv=NA, Colv=NA, col = brewer.pal(5,"Blues"), revC = T, scale= "none", margins=c(2,2))
    })
    
    # Recommend
    recommend <- reactive({
        recon_matrix() - data
        })
    
    output$recommend_t <- renderTable({
        recommend()
        }, include.rownames=TRUE)
    output$recommend_heat <- renderPlot({
        heatmap(data.matrix(recommend()), Rowv=NA, Colv=NA, col = brewer.pal(5,"Blues"), scale="none", revC = T, margins=c(10,10))
    })
    
    
    
    
    
    
    # Recommendation Engine User Interface
    select_drink <- reactive({
            format(sort(recommend()[input$Customer,], decreasing = T), digits = 2)
    })
    
    long_select_customer <- reactive({
        select_drink() %>%
            gather("Drink", "Index")
    })
    
    n_long_select_customer <- reactive({
        long_select_customer()[1:(input$nRecommend), ]
    })
    
    output$table <- renderTable({
        n_long_select_customer()
    })
    
    output$original_data <- renderTable({
        data
    },
    include.rownames=TRUE
    )
    
    output$original_heat <- renderPlot({
        heatmap(data.matrix(data), Rowv=NA, Colv=NA, col = brewer.pal(5,"Blues"), revC = T, scale= "none", margins=c(10,10))
    })    
}

# Create Shiny object
shinyApp(ui = ui, server = server)