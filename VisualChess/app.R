## Required libraries
library(shiny)
library(ggplot2)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Random Visual Chess "),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
        
        helpText("This app is inspired by the work of Antonieta Sosa entitled 'Ajedrez Visual (Visual Chess)'.",
                  "The idea is to randomly assign colors to the cells based on a binomial distribution. When a cell is selected to change the color, the color is randomly selected from a predefined color palette (every color has the same probability for being chosen).",
                   "Users can play with the board size and the probability to assign colors.",
                   "Enjoy it!"),
        hr(),
        
        ## Color probability slidebar
        sliderInput("prob_color",
                    "Probability of color within the cell:",
                    min = 0,
                    max = 1,
                    value = 0.22),
        
        ## Number of squares slide bar
        sliderInput("num_squares",
                    "Size of the chess board:",
                    min = 3,
                    max = 100,
                    value = 15)
      ),
      
      # Show the Visual Chess (Heatmap)
      mainPanel(
         plotOutput("VisualChessPlot")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$VisualChessPlot <- renderPlot({
     
     ############################
     ## Generate color palette ##
     ############################
     create.palette <- ggthemes::few_pal(palette = "Dark")
     color.palette <- c(create.palette(8), "#f7f7f7", "#1a1a1a")
     color.palette.names <- c(paste("C", 1:(length(color.palette)-1), sep = "_"), "BG")
     names(color.palette) <- color.palette.names
     
     ############################
     ## Initialize chess board ##
     ############################
     square.data <- data.frame(Cell = rep(0, input$num_squares**2),
                               X = 1:input$num_squares,
                               Y = rep(1:input$num_squares, each = input$num_squares),
                               Color_class = "BG")
     
     ##################################
     ## Random assignation of colors ##
     ##################################
     random.colors <- 
       sapply(square.data$Cell, function(cc){
         
         ## Binomial test
         ## The probability of observe a suqare of another color is 0.2 (as in the original painting)
         color.prob <- rbinom(n = 1, size = 1, prob = input$prob_color)
         
         ## If this is positive, then randomly assign a color 
         if(color.prob){
           
           ## Choose a color for the square
           color.index <- round(runif(n = 1, min = 1, max = length(color.palette.names)-1))
           color.class <- names(color.palette)[color.index]
           color.hexa <- color.palette[color.index]
           
           color.class
           
           ## Otherwise, keep the background color  
         } else {
           
           "BG"
         }
       })
     
     
     ##################
     ## Draw heatmap ##
     ##################
     square.data$Color_class <- random.colors
     
     ggplot(data = square.data, aes(x = X, y = Y, fill = Color_class)) +
       geom_tile(colour = "white", show.legend = FALSE, width=0.95, height=0.95) + 
       coord_equal() +
       scale_fill_manual(values = color.palette, breaks = names(color.palette)) +
       theme_void()
     
     # renderText({
     #   paste("The current time is", Sys.time())
     # })
   }, height = 1500, width = 1500)
}

# Run the application 
shinyApp(ui = ui, server = server)

