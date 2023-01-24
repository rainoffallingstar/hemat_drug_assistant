### Reference
# Formula
# Body mass index, kg/m2 = weight, kg / (height, m)2
# Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2

library(shiny)
library(shinyWidgets)
library(readxl)
library(fs)
library(dplyr)
library(stringr)

# Define UI for application that draws a histogram
disease_list <- list()
disease_names<- list.dirs(paste0(getwd(), "/data/"),
                                 full.names = FALSE, recursive = FALSE)

for (i in 1:length(disease_names)){
  filelist <- list.files(paste0(getwd(), "/data/",disease_names[i],"/"), 
                                  pattern = ".xlsx", recursive = FALSE)
  for (a in 1:length(filelist) ){
    filelist[a] <- substring(filelist[a],1,nchar(filelist[a])-5)
  }
  disease_list[[i]] <- filelist
}
names(disease_list) <-disease_names

# define  functions to process the strings in the excel files
# make all drug names display in the same way
update_druglist_title <- function(x,y) {
  raw_data <- x
  for (i in 1:nrow(raw_data)){
    raw_data[i,y] <- raw_data[i,y] %>% 
      str_to_lower() %>% 
      str_to_sentence()
  }
  return(raw_data)
}
database <- read_excel("data/standerd_drug_names.xlsx") %>% 
  update_druglist_title(1) %>% 
  update_druglist_title(2)
                          
  
get_druglist_title <- function(x,y,z) {
  raw_data <- x
  for (i in 1:nrow(raw_data)){
    if (str_detect(raw_data[i,y],"[\\p{Han}]")){
      id <- grep(raw_data[i,y],database$药名)
    }else{
      pro <- raw_data[i,y] %>% 
        str_to_lower() %>% 
        str_to_sentence()
      id <- grep(pro,database$Drugs)
    }
    
    if (is.na(id[1])){
      raw_data[i,y] <- raw_data[i,y]
    }else {
      raw_data[i,y] <- database[id[1],z] 
    }
    
  }
  return(raw_data)
}



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
          pickerInput("regimens",
                      "Regimens Selected",
                      selected = "R-CHOP",
                      choices = disease_list
          ),
          prettyRadioButtons(
            inputId = "gender",
            label = "Choose Gender:", 
            choices = c("Common", "Female", "Male"),
            inline = TRUE, 
            status = "danger",
            fill = TRUE
          ),
          switchInput(
            inputId = "Id078",
            onLabel = "En",
            offLabel = "中"
          )
          
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
          tabPanel("Recommendation", 
                   tableOutput("table1"),
                   tableOutput("table2"),
                   textOutput("warn")),
          tabPanel("SideEffects",
                    tableOutput("table3")),
          tabPanel("About me",
                   textOutput("about"),
                   textOutput("formula"),
                   textOutput("references"),
                   plotOutput("plt")
                  )
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
    if (input$gender == "Common"){
      (h * w / 3600)^0.5
    } else if (input$gender == "Female"){
    #0.00586×身高（cm）+0.0126×体重（kg）-0.0461
      0.00586 * h + 0.0126 * w - 0.0461
    } else{
    #0.00607×身高（cm）+0.0127×体重（kg）- 0.0698
      0.00607 * h + 0.0127 * w - 0.0698
    }
  })
  mod <-reactive({
    if (input$Id078 == FALSE){
      mod = 1
    }else{
      mod = 2
    }
  })
  regimen <- reactive({
    n=1
    drug_path = paste0(getwd(), "/data/",disease_names[n],"/",input$regimens, ".xlsx")
    while (file.exists(drug_path) == FALSE && n <= length(disease_names)-1) {
      n = n+1
      drug_path = paste0(getwd(), "/data/",disease_names[n],"/",input$regimens, ".xlsx")
    }
    
    read_excel(drug_path,
               col_types = c("text", "text", "numeric", 
                              "numeric", "numeric", "numeric", "text")) %>% 
      get_druglist_title(1,mod()) 
   
  })
  
  calculate_regimen <- reactive({
    df2 <- as.data.frame(regimen())
    bsa <- as.numeric(bsa())
    for (i in 1:nrow(df2)) {
      if (df2[i,2] == "monoclone"){
        df2[i,4] = df2[i,4]+ df2[i,3]
      }else if (df2[i,3] != 0 & df2[i,2] == "weight"){
        df2[i,4] = df2[i,3] * as.numeric(input$weight)
      }else if (df2[i,3] == 0 & df2[i,2] != "monoclone" & df2[i,2] != "weight") {
        df2[i,5] = df2[i,5] * bsa
        df2[i,6] = df2[i,6] * bsa
      }else if (df2[i,3] == 0 & df2[i,2] == "weight"){
        df2[i,5] = df2[i,5] * as.numeric(input$weight)
        df2[i,6] = df2[i,6] * as.numeric(input$weight)
      } else {
        df2[i,4] = df2[i,3] * bsa
      }
    }
    df <- df2 
  })
  
  side_effect_table <- reactive({
    regimen <- regimen()
    drug_list <- regimen[["药名"]]
    if (mod() == 1){
      side_effect_database <- database %>% 
        update_druglist_title(mod()) %>% 
        filter( 药名 %in% drug_list) %>% 
        select(药名,"副作用/SideEffect","可能的解救措施/Measures") %>% 
        distinct()
      colnames(side_effect_database) <- c("药名","副作用","可能的解救措施")
    }else{
      side_effect_database <- database %>% 
        update_druglist_title(mod()) %>% 
        filter( Drugs %in% drug_list) %>% 
        select(Drugs,"副作用/SideEffect","可能的解救措施/Measures") %>% 
        distinct()
      colnames(side_effect_database) <- c("Drugs","Side Effects","Measures")
    }
    
    side_effect <-  side_effect_database 
  })
  
  output$table1 <- renderTable({
    if (mod() == 1){
      data.frame(体重 = input$weight,
                 身高 = input$height,
                 身体质量指数 = bmi(),
                 人体表面面积 = bsa())
    } else{
      data.frame(Weight = input$weight,
                 Height = input$height,
                 BodyMassIndex = bmi(),
                 BodySurfaceArea = bsa())
    }
    
  })
  
  output$table2 <- renderTable({
    if (mod() == 1){
    as.data.frame(calculate_regimen()) %>% 
      select(-"类型")
    }else{
      df <- as.data.frame(calculate_regimen()) %>% 
        select(-"类型")
      colnames(df) <- c("Drugs","Guideline Dose(mg/m2)","Calculation","Min Dose",
                        "Max Dose","Unit")
      df <- df
    }
  })
  output$warn <- renderText({
    print("*Warning:This application is under active development for hematological professionals,please check the regimens carefully before making it a part of routine clinical operations and desicion-making process.")
  })
  
  output$table3 <- renderTable({
    side_effect_table()
  })
  
  output$about <- renderText({
   
    print(paste("This application is under developed by Yanhua Zheng,Dr.Qinchuan Yu, Xin Ding and Prof.Xiaoxue Wang from the department of hematology,CMU1H, and Dr.Linfeng He from Institute for Empirical Social Science Research,XJTU. "))
  }
  )
  
  output$formula <- renderText({
    print("The calculation of Body Mass
          Index(BMI) is based on the famous formula BMI = weight,kg/(height,cm/100)^2, and Body Surface Area(BSA) is generally refered to the Mosteller formula, in which 
          m2 = [ Height, cm x Weight, kg  / 3600 ]^1/2. However, we also provide some alternations for the situation of different genders refered to the Songshan Zhao's formula, in which 
          the BSA for Female = 0.00586× height,cm + 0.0126× weight,kg -0.0461 and BSA for male = 0.00607× height,cm +0.0127 × weight,kg -0.0698.")
  })
  
  output$references <- renderText({
    print(" In addition, the citated drug does and regimens are mainly taken from an chinese regimens handbook for clinical hematology，Uptodate，USMLE Step 1&2 and BRS Pharmacology.")
  })
  
  output$plt <- renderPlot({
    img <- jpeg::readJPEG(paste0(getwd(),"/image/logo.jpg"))
    plot(as.raster(img))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
