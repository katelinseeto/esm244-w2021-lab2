# Attach packages
library(shiny)
library(tidyverse)
library(palmerpenguins)

# Create the user interface
ui <- fluidPage(
    titlePanel("I am adding a TITLE!"),
    sidebarLayout(
        sidebarPanel("put my widgets here!",
                     radioButtons(inputId = "penguin_species", #how widget is stored
                                  label = "Choose penguin species:", #how is labeled
                                  choices = c("Adelie", "Cool Chinstrap" = "Chinstrap",                                               "Gentoo"))
                     ),
        mainPanel("Here's my graph!",
                  plotOutput(outputId = "penguin_plot"))
    )
)

# Create the server function
server <- function(input, output)  {

    penguin_select <- reactive({
        penguins %>%
            filter(species == input$penguin_species)
    })

    output$penguin_plot <- renderPlot({
        ggplot(data = penguin_select(), aes(x = flipper_length_mm, y = body_mass_g))+
            geom_point()
    })

}

# Combine into an app
shinyApp(ui= ui, server = server)

