### Reference
# Formula
# Body mass index, kg/m2 = weight, kg / (height, m)2
# Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2

library(shiny)
library(readxl)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hematological Drug Assistant"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput("weight",
                    "Weight(KG)",
                    "50"),
          textInput("height",
                    "Height(CM)",
                    "170"),
          selectInput("regimens",
                      "Regimens Selected",
                      selected = "R-CHOP",
                      choices = c("R-CHOP", "regimen2"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
          tabPanel("Recommendation", 
                   tableOutput("table1"),
                  DT::DTOutput("table2")),
           tabPanel("Reference",
                    tableOutput("table3")),
          tabPanel("About me",
                   textOutput("about"),
                   plotOutput("plt"),)
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bmi <- reactive({
    w <- as.numeric(input$weight)
    h <- as.numeric(input$height)
    w /((h / 100)^2)
  })
  
  bsa <- reactive({
    # Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2
    w <- as.numeric(input$weight)
    h <- as.numeric(input$height)
    (h * w / 3600)^0.5
  })
  
  regimen <- reactive({
    read_excel(paste0(getwd(), "/data/", input$regimens, ".xlsx"),
               col_types = c("text", "text", "numeric", 
                              "numeric", "numeric", "text"))
  })
  
  calculate_regimen <- reactive({
    df2 <- as.data.frame(regimen())
    bsa <- as.numeric(bsa())
    for (i in 1:nrow(df2)) {
      if (df2[i,2] == "monoclone"){
        print("no more operation")
      }else if (df2[i,3] == 0) {
        df2[i,4] = df2[i,4] * bsa
        df2[i,5] = df2[i,5] * bsa
      } else {
        df2[i,3] = df2[i,3] * bsa
      }
    }
    df <- df2
  })
  
  output$table1 <- renderTable({
    data.frame(Weight = input$weight,
               Height = input$height,
               BMI = bmi(),
               BSA = bsa())
  })
  
  output$table2 <- DT::renderDT({
    as.data.frame(calculate_regimen())
  })
  
  output$table3 <- renderTable({
    regimen()
  })
  
  output$about <- renderText({
   
    print(paste("This application is under developed by fallingstar, The calculation of Body Mass
          Index(BMI) and Body Surface Area(BSA) is refered to the Mosteller formula, in which 
          m2 = [ Height, cm x Weight, kg  / 3600 ]1/2. In addition, the refered drug does and regimens are taken according to 
          血液科临床处方手册"))
  }
  )
  
  output$plt <- renderPlot({
    img <- jpeg::readJPEG(paste0(getwd(),"/image/logo.jpg"))
    plot(as.raster(img))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
