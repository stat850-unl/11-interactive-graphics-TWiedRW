#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tidyverse)

# Data --------------------------------------------------------------------


# Get the Data

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
cocktails$alcoholic = str_to_sentence(cocktails$alcoholic)
cocktails$category = str_to_sentence(cocktails$category)
cocktails$ingredient = str_to_sentence(cocktails$ingredient)

cocktails = cocktails %>% 
    group_by(drink) %>% 
    mutate(ingredient_list = reduce(ingredient, paste),
           ingredient = trimws(ingredient)) %>% 
    select(drink, alcoholic, category, glass, ingredient, measure, ingredient_list)


# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cocktail Recipe Ideas"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        # Side panel
        sidebarPanel(
            h2('Types of Drinks'),
            selectInput('cat',
                        'Category',
                        choices = c('-- All --', sort(unique((cocktails$category))))),
            radioButtons('class', 
                         'Alcohol',
                         choices = c(sort(unique(str_to_sentence(cocktails$alcoholic))))),
            h2('Ingredient Selection'),
            h5('Note: will return all drinks with at least one matching ingredient.'),
            
            checkboxGroupInput('ingred',
                          'Ingredients',
                          choices = sort(unique(str_to_sentence(cocktails$ingredient))))
        ),

        # Main panel
        mainPanel(
            plotOutput('com_plot'),
            tableOutput('data_tab')
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    ingred_list = reactive({
        cocktails %>%
            filter(
                if (input$cat == '-- All --'){
                    TRUE
                }
                else {
                    category %in% input$cat
                    },
                alcoholic %in% c(input$class)
                )
    })
    
    #input
    observeEvent(ingred_list(), {
        choices_ing = as.character(sort(unique(ingred_list()$ingredient)))

        updateCheckboxGroupInput(
            inputId = 'ingred',
            choices = choices_ing
        )
    })
    
    complexity_plot = reactive({
        ingred_list() %>% 
            group_by(drink) %>% 
            summarize(Count = n()) %>% 
            ggplot(mapping = aes(x = Count)) +
            geom_density() +
            labs(title = 'Complexity of Drinks',
                 x = 'Number of ingredients',
                 y = 'Density') +
            scale_x_continuous(breaks = 0:30) +
            theme_bw()
    })
    
    
        drink_rec = reactive({
            
            ingred_list() %>% 
                filter(
                    if (length(input$ingred) == 0){
                        TRUE
                    } else {
                        (sapply(tolower(input$ingred), grepl, tolower(ingredient_list)) %>%
                             as.numeric() %>%
                             sum()) > 0
                    }
                ) %>% 
                arrange(drink, ingredient) %>% 
                select(-ingredient_list, -alcoholic) 
            
                # The following would create a row for each drink with corresponding 
                # ingredients. For some reason, it causes errors...
            
                # tidyr::pivot_wider(id_cols = c(drink, alcoholic, category, glass),
                #                    names_from = ingredient,
                #                    values_from = measure,
                #                    values_fill = NA)

                
    })

    output$com_plot = renderPlot(complexity_plot())
    output$data_tab = renderTable(drink_rec())
    
}

# Run the application 
shinyApp(ui = ui, server = server)