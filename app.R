### Reference
# Formula
# Body mass index, kg/m2 = weight, kg / (height, m)2
# Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2

library(shiny)
library(shinyWidgets)
library(shinythemes)
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

# Define a function to switch all text on the interface between Chinese and English
switch_language <- function(lang) {
  if (lang == "Chinese") {
    updateTextInput(session = getDefaultReactiveDomain(), "weight", "体重(KG)", "50")
    updateTextInput(session = getDefaultReactiveDomain(), "height", "身高(CM)", "170")
    updatePickerInput(session = getDefaultReactiveDomain(), "regimens", "选择方案", selected = "R-CHOP", choices = disease_list)
    updatePrettyRadioButtons(session = getDefaultReactiveDomain(), "gender", "选择性别:", choices = c("通用", "女性", "男性"))
    updateSwitchInput(session = getDefaultReactiveDomain(), "Id078", onLabel = "En", offLabel = "中")
  } else {
    updateTextInput(session = getDefaultReactiveDomain(), "weight", "Weight(KG)", "50")
    updateTextInput(session = getDefaultReactiveDomain(), "height", "Height(CM)", "170")
    updatePickerInput(session = getDefaultReactiveDomain(), "regimens", "Regimens Selected", selected = "R-CHOP", choices = disease_list)
    updatePrettyRadioButtons(session = getDefaultReactiveDomain(), "gender", "Choose Gender:", choices = c("Common", "Female", "Male"))
    updateSwitchInput(session = getDefaultReactiveDomain(), "Id078", onLabel = "En", offLabel = "中")
  }
}

switch_fun <- function(x,y,z){
  if (x == TRUE){
    result = y
  }else {
    result = z
  }
  return(result)
}


ui <- fluidPage(theme = shinytheme("journal"),

    # Application title
    titlePanel(title = uiOutput("titlepan")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput("weight",
                    "体重(KG)",
                    "50"),
          textInput("height",
                    "身高(CM)",
                    "170"),
          pickerInput("regimens",
                      "选择方案",
                      selected = "R-CHOP",
                      choices = disease_list
          ),
          prettyRadioButtons(
            inputId = "gender",
            label = "选择性别:", 
            choices = c("通用", "女性", "男性"),
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
          tabPanel(title = uiOutput("Recommendation"), 
                   tableOutput("table1"),
                   tableOutput("table2"),
                   textOutput("warn")),
          tabPanel(title = uiOutput("SideEffects"),
                    tableOutput("table3")),
          tabPanel(title = uiOutput("Aboutme"),
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
  
  # Switch language when button is clicked
  observeEvent(input$Id078, {
    if (input$Id078 == TRUE) {
      switch_language("English")
    } else {
      switch_language("Chinese")
    }
  })
  output$titlepan = renderText({
    switch_fun(input$Id078, "Hematological Drug Assistant","血液科用药助手") 
  })
  output$Recommendation = renderText({
    switch_fun(input$Id078, "Recommendation","方案推荐") 
  })
  output$SideEffects = renderText({
    switch_fun(input$Id078, "SideEffects", "药物副作用") 
  })
  output$Aboutme = renderText({
    switch_fun(input$Id078, "About me","关于我") 
  })
  
  
  bmi <- reactive({
    w <- as.numeric(input$weight)
    h <- as.numeric(input$height)
    w /((h / 100)^2)
  })
  
  bsa <- reactive({
    # Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2
    w <- as.numeric(input$weight)
    h <- as.numeric(input$height)
    if (input$gender == "Common" | input$gender == "通用" ){
      (h * w / 3600)^0.5
    } else if (input$gender == "Female" | input$gender == "女性" ){
    #0.00586×身高（cm）+0.0126×体重（kg）-0.0461
      0.00586 * h + 0.0126 * w - 0.0461
    } else{
    #0.00607×身高（cm）+0.0127×体重（kg）- 0.0698
      0.00607 * h + 0.0127 * w - 0.0698
    }
  })
  mod <-reactive({
    switch_fun(input$Id078,2,1)
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
    if (mod() == 1){
      print("*警告：该应用程序正在面向血液学专业人员积极开发中，请在将其纳入日常临床操作和决策过程之前仔细检查方案内容。")
    } else {
      print("*Warning:This application is under active development for hematological professionals,please check the regimens carefully before making it a part of routine clinical operations and desicion-making process.")
    }
  })
  
  output$table3 <- renderTable({
    side_effect_table()
  })
  
  output$about <- renderText({
    if (mod() == 1){
      print(paste("本应用程序由中国医科大学附属第一医院血液科的Yanhua Zheng,Dr.Qinchuan Yu, Xin Ding & Prof.Xiaoxue Wang以及西安交通大学社会科学研究所的Dr.Linfeng开发。"))
    } else {
      print(paste("This application is under developed by Yanhua Zheng,Dr.Qinchuan Yu, Xin Ding and Prof.Xiaoxue Wang from the department of hematology,CMU1H, and Dr.Linfeng He from Institute for Empirical Social Science Research,XJTU. "))
    }
  })
  
  output$formula <- renderText({
    if (mod() == 1){
      print("身体质量指数（BMI）的计算基于著名的公式BMI = 体重，kg /（身高，cm / 100）^ 2，而身体表面积（BSA）通常是指Mosteller公式，其中m2 = [身高，cm x 体重，kg / 3600] ^ 1/2。但是，我们还提供了一些针对不同性别情况的替代方案，参考了Songshan Zhao的公式，其中女性的BSA = 0.00586×身高，cm + 0.0126×体重，kg-0.0461，男性的BSA = 0.00607×身高，cm +0.0127×体重，kg-0.0698。")
    } else {
      print("The calculation of Body Mass Index(BMI) is based on the famous formula BMI = weight,kg/(height,cm/100)^2, and Body Surface Area(BSA) is generally refered to the Mosteller formula, in which m2 = [ Height, cm x Weight, kg  / 3600 ]^1/2. However, we also provide some alternations for the situation of different genders refered to the Songshan Zhao's formula, in which the BSA for Female = 0.00586× height,cm + 0.0126× weight,kg -0.0461 and BSA for male = 0.00607× height,cm +0.0127 × weight,kg -0.0698.")
    }
  })
  
  output$references <- renderText({
    if (mod() == 1){
      print("此外，引用的药物剂量和方案主要来自于中国临床血液学方案手册，Uptodate，USMLE Step 1＆2和BRS药理学。")
    } else {
      print(" In addition, the citated drug does and regimens are mainly taken from an chinese regimens handbook for clinical hematology，Uptodate，USMLE Step 1&2 and BRS Pharmacology.")
    }
  })


  
  output$plt <- renderPlot({
    img <- jpeg::readJPEG(paste0(getwd(),"/image/logo.jpg"))
    plot(as.raster(img))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
