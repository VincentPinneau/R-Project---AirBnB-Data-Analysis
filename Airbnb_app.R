library(shiny)
library(ggplot2)
library(dplyr)

# Load the data
load("C:/Users/vince/OneDrive/Documents/DSTI/Big Data Processing with R/Project/AirBnB.Rdata")

# Define UI for first Shiny app
ui_app1 <- fluidPage(
  # Application title
  titlePanel("Relationship between prices and apartment features"),
  
  # Sidebar with the possibility to select the size measurment, the amenities
  # and the pricing of the apartments.
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "size",
                  label = "Apartment size measurement",
                  choices = c("accommodates","bathrooms","bedrooms","beds"),
                  selected = "accomodates"),
      selectInput(inputId = "amenity",
                  label = "Amenity",
                  choices = c("TV","Internet","Kitchen","Smoking","Heating","Air_Conditioning","Washer","Dryer"),
                  selected = "TV"),
      selectInput(inputId = "price",
                  label = "Price",
                  choices = c("price","weekly_price","monthly_price"),
                  selected = "price")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output:----
      plotOutput("Plotapp1")
    )
  )
)

# Define server logic for first Shiny app
server_app1 <- function(input, output) {

  # Select only the apartments
  L2 = filter(L, property_type == 'Apartment')
  
  # Turn the prices into numeric types.
  L2$price <- as.numeric(gsub("\\$", "", L2$price))
  L2$weekly_price <- as.numeric(gsub("\\$", "", L2$weekly_price))
  L2$monthly_price <- as.numeric(gsub("\\$", "", L2$monthly_price))
  
  # Create dummy variables for different amenities.
  
  TV <- grepl("TV",L2$amenities)
  L2 <- cbind(L2, TV)
  L2$TV[L2$TV == TRUE] <- 'TV'
  L2$TV[L2$TV == FALSE] <- 'No TV'
  
  Internet <- grepl("Internet",L2$amenities)
  L2 <- cbind(L2, Internet)
  L2$Internet[L2$Internet == TRUE] <- 'Internet'
  L2$Internet[L2$Internet == FALSE] <- 'No Internet'
  
  Kitchen <- grepl("Kitchen",L2$amenities)
  L2 <- cbind(L2, Kitchen)
  L2$Kitchen[L2$Kitchen == TRUE] <- 'Kitchen'
  L2$Kitchen[L2$Kitchen == FALSE] <- 'No Kitchen'
  
  Heating <- grepl("Heating",L2$amenities)
  L2 <- cbind(L2, Heating)
  L2$Heating[L2$Heating == TRUE] <- 'Heating'
  L2$Heating[L2$Heating == FALSE] <- 'No heating'
  
  Smoking <- grepl("Smoking Allowed",L2$amenities)
  L2 <- cbind(L2, Smoking)
  L2$Smoking[L2$Smoking == TRUE] <- 'Smoking allowed'
  L2$Smoking[L2$Smoking == FALSE] <- 'No smoking'
  
  Washer <- grepl("Washer",L2$amenities)
  L2 <- cbind(L2, Washer)
  L2$Washer[L2$Washer == TRUE] <- 'Washer'
  L2$Washer[L2$Washer == FALSE] <- 'No washer'
  
  Dryer <- grepl("Dryer",L2$amenities)
  L2 <- cbind(L2, Dryer)
  L2$Dryer[L2$Dryer == TRUE] <- 'Dryer'
  L2$Dryer[L2$Dryer == FALSE] <- 'No dryer'
  
  Air_Conditioning <- grepl("Air Conditioning",L2$amenities)
  L2 <- cbind(L2, Air_Conditioning)
  L2$Air_Conditioning[L2$Air_Conditioning == TRUE] <- 'Air conditioning'
  L2$Air_Conditioning[L2$Air_Conditioning == FALSE] <- 'No air conditioning'
  
  output$Plotapp1 <- renderPlot({
    
    # Display the plot depending on input parameters
    price_col <- sym(input$price)
    size_col <- sym(input$size)
    amenity_col <- sym(input$amenity)
    
    grouped.L2 = group_by(L2, !!size_col, room_type, !!amenity_col)
    out = summarise(grouped.L2,
                    price_mean = mean(!!price_col,na.rm=TRUE))
    ggplot(data = out) +
      theme_light() +
      geom_point(aes(x = !!size_col, y = price_mean)) +
      geom_smooth(aes(x = !!size_col, y = price_mean)) +
      facet_grid(paste(input$amenity, '~ room_type'))
  })
}

# Define UI for second Shiny app
ui_app2 <- fluidPage(
  # App title ----
  titlePanel("Number of apartments per owner"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:----
      plotOutput(outputId = "Plotapp2")
      
    ), 
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select the number of bins ----
      numericInput(inputId = "bins",
                   label = "Number of bins:",
                   value = 155, min = 1, max = 155, step = 1),
      # Input: Select the length of the x-axis ----
      numericInput(inputId = "xlim",
                   label = "Length of the x_axis:",
                   value = 155, min = 1, max = 155, step = 1),
      # Input: Select the length of the y-axis ----
      numericInput(inputId = "ylim",
                   label = "Length of the y_axis:",
                   value = 41000, min = 1, max = 41000, step = 1),
      # Input: Select the color of the histogram ----
      selectInput(inputId = "col.hist",
                  label = "Color of the histogram",
                  choices = c("grey","black","lavender","lightblue","pink"),
                  selected = "grey"),
    )
  )
)

# Define server logic for second Shiny app
server_app2 <- function(input, output) {
  
  #Data manipulation
  ##Creating a variable that counts the number of apartments per owner.
  
  L3 <- filter(L, property_type == 'Apartment')
  
  counts <- L3 %>%
    # counts the number of apartments for each host
    group_by(host_id) %>%
    summarise(apartments_count = n())
  
  L3 <- merge(L3, counts, by = "host_id", all.x = TRUE)
  
  output$Plotapp2 <- renderPlot({
    ggplot(data = L3) +
      theme_light() +
      geom_histogram(aes(x = apartments_count), bins = input$bins, fill = input$col.hist) +
      labs(y = "owners_count") +
      xlim(0, input$xlim) +
      ylim(0, input$ylim)
  })
  
}

# Define UI for first Shiny app
ui_app3 <- fluidPage(
  # Application title
  titlePanel("Renting price per city quarter"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "neighbourhood",
                  label = "Neighbourhood variable",
                  choices = c("neighbourhood_cleansed","neighbourhood"),
                  selected = "neighbourhood_cleansed"),
      numericInput(inputId = "min_neighbourhood_average_price",
                   label = "Minimum neighbourhood average price:",
                   value = 1, min = 1, max = 1000, step = 1),
      numericInput(inputId = "max_neighbourhood_average_price",
                   label = "Maximum neighbourhood average price:",
                   value = 500, min = 1, max = 1000, step = 1),
      numericInput(inputId = "min_neighbourhood_Cl_average_price",
                   label = "Minimum neighbourhood_cleansed average price:",
                   value = 1, min = 1, max = 1000, step = 1),
      numericInput(inputId = "max_neighbourhood_Cl_average_price",
                   label = "Maximum neighbourhood_cleansed average price:",
                   value = 500, min = 1, max = 1000, step = 1),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output:----
      plotOutput("Plotapp3")
    )
  )
)

# Define server logic for first Shiny app
server_app3 <- function(input, output) {

  # Create a new sub-dataset
  L4 <- L
  
  # Turn the prices into numeric types.
  L4$price <- as.numeric(gsub("\\$", "", L4$price))
  
  # We create new variables that give the average price of each quarter.
  N_price <- L4 %>%
    group_by(neighbourhood) %>%
    summarise(Neighbourhood_average_price = mean(price, na.rm = TRUE))
  
  N_price
  
  NC_price <- L4 %>%
    group_by(neighbourhood_cleansed) %>%
    summarise(Neighbourhood_Cl_average_price = mean(price, na.rm = TRUE))
  
  NC_price
  
  # And we then merge these variables in our new dataset.
  L4 <- merge(L4, N_price,  by = "neighbourhood", all.x = TRUE)
  L4 <- merge(L4, NC_price,  by = "neighbourhood_cleansed", all.x = TRUE)
  
  output$Plotapp3 <- renderPlot({
    # Display the plot depending on input parameters
    if (input$neighbourhood == "neighbourhood"){
      L41 <- filter(L4, Neighbourhood_average_price < input$max_neighbourhood_average_price, Neighbourhood_average_price > input$min_neighbourhood_average_price)
      grouped.L4 = group_by(L41,neighbourhood)
      ggplot(data = grouped.L4) +
        theme_light() +
        geom_boxplot(aes(x = neighbourhood,y = price))
    } else {
      L41 <- filter(L4, Neighbourhood_Cl_average_price < input$max_neighbourhood_Cl_average_price, Neighbourhood_Cl_average_price > input$min_neighbourhood_Cl_average_price)
      grouped.L4 = group_by(L41,neighbourhood_cleansed)
      ggplot(data = grouped.L4) +
        theme_light() +
        geom_boxplot(aes(x = neighbourhood_cleansed,y = price))
    }
  })
}

# Define UI for first Shiny app
ui_app4 <- fluidPage(
  # Application title
  titlePanel("Visit frequency of the different quarters according to time"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "neighbourhood_choice",
                  label = "Neighbourhood variable",
                  choices = c("neighbourhood_cleansed","neighbourhood"),
                  selected = "neighbourhood_cleansed"),
      selectizeInput(inputId = "neighbourhood1", label = "Neighbourhood choices",
                     choices = unique(L$neighbourhood),
                     options = list(maxItems = 4)),
      selectizeInput(inputId = "neighbourhood_Cl", label = "Neighbourhood_cleansed choices",
                     choices = unique(L$neighbourhood_cleansed),
                     options = list(maxItems = 4)),
      dateInput(inputId = "min_date_filter", label = "Minimum Date Filter", 
                value = "2009-01-01", format = "yyyy-mm-dd"),
      dateInput(inputId = "max_date_filter", label = "Maximum Date Filter", 
                value = "2024-01-01", format = "yyyy-mm-dd")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output:----
      plotOutput("Plot1App4"),
      plotOutput("Plot2App4")
    )
  )
)

# Define server logic for first Shiny app
server_app4 <- function(input, output) {

  # Create a new sub-dataset by joining the L and R tables.
  R2 <- R %>%
    rename("id" = "listing_id")
  
  L5 <- left_join(L, R2, by = "id")
  
  # We change the type of dates from factor to date.
  L5$date <- as.Date(L5$date)
  
  output$Plot1App4 <- renderPlot({
    if (input$neighbourhood_choice == "neighbourhood") {
      L51 <- filter(L5, neighbourhood %in% input$neighbourhood1, date > input$min_date_filter, date < input$max_date_filter)
      ggplot(L51, aes(x = date, color = neighbourhood)) +
        geom_density() +
        labs(title = "Density Plot of quarter visits according to time")
    } else {
      L51 <- filter(L5, neighbourhood_cleansed %in% input$neighbourhood_Cl, date > input$min_date_filter, date < input$max_date_filter)
      ggplot(L51, aes(x = date, color = neighbourhood_cleansed)) +
        geom_density() +
        labs(title = "Density Plot of quarter visits according to time")
    }
  })
  
  output$Plot2App4 <- renderPlot({
    if (input$neighbourhood_choice == "neighbourhood") {
      L51 <- filter(L5, neighbourhood %in% input$neighbourhood1, date > input$min_date_filter, date < input$max_date_filter)
      ggplot(L51, aes(x = date, color = neighbourhood)) +
        geom_freqpoly() +
        labs(title = "Visit frequency of the different quarters according to time")
    } else {
      L51 <- filter(L5, neighbourhood_cleansed %in% input$neighbourhood_Cl, date > input$min_date_filter, date < input$max_date_filter)
      ggplot(L51, aes(x = date, color = neighbourhood_cleansed)) +
        geom_freqpoly() +
        labs(title = "Visit frequency of the different quarters according to time")
    }
  })
}

# Define UI for the combined Shiny app
ui <- navbarPage(
  title = "Combined Shiny App",
  tabPanel("App 1", ui_app1),
  tabPanel("App 2", ui_app2),
  tabPanel("App 3", ui_app3),
  tabPanel("App 4", ui_app4)
)

# Define server logic for the combined Shiny app
server <- function(input, output, session) {
  # No need to use callModule, define server logic directly
  server_app1(input, output)
  server_app2(input, output)
  server_app3(input, output)
  server_app4(input, output)
}

# Run the application 
shinyApp(ui = ui, server = server)