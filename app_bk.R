### Reference
# Formula
# Body mass index, kg/m2 = weight, kg / (height, m)2
# Body surface area (the Mosteller formula), m2 = [ Height, cm x Weight, kg  / 3600 ]1/2
#install.packages("bslib")
pak::pak("rainoffallingstar/mellon")
library(shiny)
library(shinyWidgets)
library(readxl)
library(fs)
library(dplyr)
library(tidyr)
library(stringr)
library(bslib)
library(thematic)
library(mellon)

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
agvhd <- read_excel("data/agvhd.xlsx")
cart <- read_excel("data/cart.xls")


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

gender_db <- data.frame(Chinese = c("通用", "女性", "男性"),
                        English = c("Common", "Female", "Male"))

card_names <- c("基本信息","治疗方案")

# Define a function to switch all text on the interface between Chinese and English
switch_language <- function(lang,input_scr,input_age,input_weight,input_height,input_regimens,input_genders) {
  
  if (lang == "Chinese") {
    card_names <- c("基本信息","治疗方案")
    updateTextInput(session = getDefaultReactiveDomain(), "scr",
                    "肌酐", input_scr)
    updateTextInput(session = getDefaultReactiveDomain(), "aged",
                    "年龄", input_age)
    updateTextInput(session = getDefaultReactiveDomain(), "weight", "体重(KG)", input_weight)
    updateTextInput(session = getDefaultReactiveDomain(), "height", "身高(CM)", input_height)
    updatePickerInput(session = getDefaultReactiveDomain(), "regimens", "选择方案", selected = input_regimens, choices = disease_list)
    updatePrettyRadioButtons(session = getDefaultReactiveDomain(), "gender", "选择性别:", 
                             selected = input_genders,
                             choiceNames = gender_db$Chinese, choiceValues = gender_db$Chinese)
    updateSwitchInput(session = getDefaultReactiveDomain(), "Id078", onLabel = "En", offLabel = "中")
  } else {
    card_names <- c("基本信息_en","治疗方案_en")
    updateTextInput(session = getDefaultReactiveDomain(), "scr",
                    "Serum creatinine(umol/L)", input_scr)
    updateTextInput(session = getDefaultReactiveDomain(), "aged",
                    "Age",input_age)
    updateTextInput(session = getDefaultReactiveDomain(), "weight", "Weight(KG)", input_weight)
    updateTextInput(session = getDefaultReactiveDomain(), "height", "Height(CM)", input_height)
    updatePickerInput(session = getDefaultReactiveDomain(), "regimens", "Regimens Selected", selected = input_regimens, choices = disease_list)
    updatePrettyRadioButtons(session = getDefaultReactiveDomain(), "gender", "Choose Gender:",
                             selected = input_genders,
                             choiceNames = gender_db$English, choiceValues = gender_db$Chinese)
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


ui <- page_fluid(
  
  theme = bslib::bs_theme(bootswatch = "journal"),
                # Application title
                titlePanel(title = uiOutput("titlepan")),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    tags$head(includeHTML(("baidu-analytics.html"))),
                    dropdown(
                      
                      tags$h3(uiOutput("advance")),
                      
                      textInput("aged",
                                "年龄"),
                      textInput("scr",
                                "肌酐"),
                      
                      style = "material-circle", icon = icon("sliders"),
                      status = "danger", width = "300px",
                      animate = animateOptions(
                        enter = animations$fading_entrances$fadeInLeftBig,
                        exit = animations$fading_exits$fadeOutRightBig
                      )
                    ),
                    br(),
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
                      selected = "通用",
                      inline = TRUE, 
                      status = "danger",
                      fill = TRUE,
                      choiceNames = gender_db$Chinese,
                      choiceValues = gender_db$Chinese
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
                               card(
                                 full_screen = TRUE,
                                 card_header(card_names[1]),
                                 card_body(tableOutput("table1"))
                               ),
                               card(
                                 full_screen = TRUE,
                                 card_header(card_names[2]),
                                 card_body(tableOutput("table2")
                                           ),
                                 card_footer(textOutput("warn")))
                               ),
                      tabPanel(title = uiOutput("SideEffects"),
                               card(
                                 full_screen = TRUE,
                                 card_header("药物副作用"),
                                 card_body(tableOutput("table3"))
                               )),
                      tabPanel(title = uiOutput("Aboutme"),
                               card(
                                 full_screen = TRUE,
                                 card_header("关于作者"),
                                 card_body(textOutput("about"),
                                           textOutput("formula"),
                                           textOutput("references"),
                                           plotOutput("plt"))
                               )
                               
                      ),
                      tabPanel(title = uiOutput("Survey"),
                               card(
                                 full_screen = TRUE,
                                 card_header("aGVHD Survey"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "skin",
                                             label = "皮肤：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = agvhd$skin,
                                             choiceValues = agvhd$grade
                                           ),
                                           br(),
                                           prettyRadioButtons(
                                             inputId = "liver",
                                             label = "肝脏：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = agvhd$liver,
                                             choiceValues = agvhd$grade
                                           ),
                                           br(),
                                           prettyRadioButtons(
                                             inputId = "gastric",
                                             label = "胃肠道：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = agvhd$gastric,
                                             choiceValues = agvhd$grade
                                           )
                                           ),
                                 card_footer(
                                   actionButton("submit", "Submit"),
                                   textOutput("grade"))
                               )
                      ),
                      tabPanel(title = uiOutput("Survey2"),
                               card(
                                 full_screen = TRUE,
                                 card_header("IPI Lyphoma Survey"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "age",
                                             label = "年龄：", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("<= 60y","> 60y"),
                                             choiceValues = c(0,1)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "ECOG",
                                             label = "ECOG评分：", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("活动能力完全正常","自由活动&一般轻度的体力劳动",
                                                             "自由走动&不能从事任何劳动","轮椅或者卧床为主","卧床不起&生活不能自理","死亡"),
                                             choiceValues = c(0,1,2,3,4,5)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "ann",
                                             label = "临床分期：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("I","II","III","IV"),
                                             choiceValues = c(1,2,3,4)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "out_of_node",
                                             label = "结外器官受侵数目:", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("<= 1","> 1"),
                                             choiceValues = c(0,1)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "ldh",
                                             label = "LDH:", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("正常","异常"),
                                             choiceValues = c(0,1)
                                           )
                                 ),
                                 card_footer(
                                   actionButton("submit2", "Submit"),
                                   textOutput("grade2"))
                               )
                      ),
                      tabPanel(title = uiOutput("Survey3"),
                               card(
                                 full_screen = TRUE,
                                 card_header("CAR-T ICANS Survey"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "attention",
                                             label = "定向力（正确数）：年，月，城市，医院", 
                                             selected = "4",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("0","1","2","3","4"),
                                             choiceValues = c(0,1,2,3,4)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "named",
                                             label = "命名能力（命名数）：说出三件物品的名称（如笔、钟表、按钮）", 
                                             selected = "3",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("0","1","2","3"),
                                             choiceValues = c(0,1,2,3)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "listen",
                                             label = "听从指挥能力：听从简单指令", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("能","否"),
                                             choiceValues = c(1,0)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "write",
                                             label = "书写能力:写出一句完整的话", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("能","否"),
                                             choiceValues = c(1,0)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "count",
                                             label = "计算能力:简单计算或数数", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("能","否"),
                                             choiceValues = c(1,0)
                                           )
                                 ),
                                 card_footer(
                                   actionButton("submit3", "Submit"),
                                   tableOutput("grade3"))
                               )
                      ),
                      tabPanel(title = uiOutput("Survey4"),
                               card(
                                 full_screen = TRUE,
                                 card_header("Myeloma Survey"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "HGB",
                                             label = "血红蛋白:", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(">100 g/L","85-100 g/L","<85 g/L"),
                                             choiceValues = c(1,2,3)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "Serum_Ca",
                                             label = "血清钙:", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("≤2.65 mmol/L（11.5 mg/dl)",">2.65 mmol/L（11.5 mg/dl)"),
                                             choiceValues = c(1,2)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "bone_image",
                                             label = "骨骼检查：", 
                                             selected = "3",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("骨骼结构正常或孤立性骨浆细胞瘤","骨骼检查中溶骨病变大于 3 处","其他"),
                                             choiceValues = c(1,2,3)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "M_protein",
                                             label = "血清或尿骨髓瘤蛋白：", 
                                             selected = "3",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("1）IgG<50 g/L 2）IgA<30 g/L 3）本周蛋白<4 g/24h","1）IgG>70 g/L 2）IgA>50 g/L 3）本周蛋白>12 g/24h","以上均不符合"),
                                             choiceValues = c(1,2,3)
                                           ),
                                           
                                           prettyRadioButtons(
                                             inputId = "serum_jg",
                                             label = "血清肌酐水平：", 
                                             selected = "A",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" <177 μmol/L（2.0 mg/dl）","≥177 μmol/L（2.0 mg/dl）"),
                                             choiceValues = c("A","B")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "B2_MG",
                                             label = "β2 微球蛋白：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" <3.5 mg/L ","[3.5-5.5) mg/L","≥5.5 mg/L"),
                                             choiceValues = c("1","2","3")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "W_MG",
                                             label = "白蛋白：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" ≥35 g/L"," < 35 g/L"),
                                             choiceValues = c("1","2")
                                           )
                                 ),
                                 card_footer(
                                   textOutput("grade4"))
                               )
                      ),
                      tabPanel(title = uiOutput("Survey5"),
                               card(
                                 full_screen = TRUE,
                                 card_header("PHSC counts-before"),
                                 card_body(br(),
                                           textInput("PB_WBC",
                                                     "外周血白细胞(10^9/L)",
                                                     "10"),
                                           textInput("PB_CD34",
                                                     "外周血CD34%(%)",
                                                     "5")
                                 ),
                                 card_footer(
                                   actionButton("submit_PHSC", "Submit"),
                                   textOutput("PHSC_before"))
                               ),
                               
                               card(
                                 full_screen = TRUE,
                                 card_header("PHSC counts-after"),
                                 card_body(br(),
                                           textInput("collection_WBC",
                                                     "采集物白细胞(10^9/L)",
                                                     "10"),
                                           textInput("collection_CD34",
                                                     "采集物CD34%(%)",
                                                     "5"),
                                           textInput("collection_volumn",
                                                     "采集物体积(ml)",
                                                     "200")
                                 ),
                                 card_footer(
                                   actionButton("submit_collection", "Submit"),
                                   textOutput("PHSC_after"))
                               )
                      ),
                      tabPanel(title = uiOutput("Surveymore"),
                               # 4Ts
                               card(
                                 full_screen = TRUE,
                                 card_header("HIT 4Ts"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "PLT_change",
                                             label = "血小板减少：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" 下降<30%(绝对值下降<10×10^9/L) ",
                                                             "下降30-50%(绝对值下降10-19×10^9/L)",
                                                             "下降>50%(绝对值下降≥20×10^9/L)"),
                                             choiceValues = c("0","1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "PLT_time",
                                             label = "血小板减少时间(Timing)：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("使用肝素≤1天,但既往无肝素接触史 ",
                                                             "使用肝素>10天或时间不能确定,或≤1天(过去30-100天内曾经使用肝素)",
                                                             "使用肝素5-10天或≤1天(过去30天内曾经使用肝素)"),
                                             choiceValues = c("0","1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "PLT_agg",
                                             label = "血栓形成(Thrombosis)：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" 无 ",
                                                             "血栓再发或加重,非坏死性皮肤损伤、可疑血栓",
                                                             "新发血栓、皮肤坏疽、静注后急性全身反应"),
                                             choiceValues = c("0","1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "PLT_reason",
                                             label = "其他致血小板减少原因：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("证据明确",
                                                             "疑诊",
                                                             "无证据"),
                                             choiceValues = c("0","1","2")
                                           )
                                 ),
                                 card_footer(
                                   actionButton("submit_HIT", "Submit"),
                                   textOutput("HIT_score"))
                               ),
                               # DIC诊断积分
                               
                               card(
                                 full_screen = TRUE,
                                 card_header("DIC诊断积分"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "PLT",
                                             label = "PLT(*10^9/L)：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" >100 ","<100","<50"),
                                             choiceValues = c("0","1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "FDPs",
                                             label = "FDP :", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" 无增加（<5mg/L） ","中度增加（5-<9mg/L）","显著增加（>=9mg/L）"),
                                             choiceValues = c("0","1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "PT",
                                             label = "PT延长(s):", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" <3） ","3-6",">6"),
                                             choiceValues = c("0","1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "FBG",
                                             label = "FBG纤维蛋白(g/L)：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" >1 ","<1"),
                                             choiceValues = c("0","1")
                                           )
                                 ),
                                 card_footer(
                                   actionButton("submit_DIC", "Submit"),
                                   textOutput("DIC_score"))
                               ),
                               
                               # 化疗后骨髓抑制分度
                               card(
                                 full_screen = TRUE,
                                 card_header("化疗致血小板减少分度"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "PLT_total",
                                             label = "PLT(*10^9/L)：", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(" [75-100) ","[50-75)",
                                                             "[25-50)","< 25"),
                                             choiceValues = c("1","2","3","4")
                                           )
                                 ),
                                 card_footer(
                                   textOutput("CIT_score"))
                               ),
                               card(
                                 full_screen = TRUE,
                                 card_header("糖皮质激素剂量换算"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "tpz_name",
                                             label = "药物名称：", 
                                             selected = "地塞米松",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("可的松","氢化可的松","强的松","强的松龙",
                                                             "甲强龙","曲安西龙","倍他米松",
                                                             "地塞米松",
                                                             "氯地米松" ),
                                             choiceValues = c("可的松","氢化可的松","强的松","强的松龙",
                                                              "甲强龙","曲安西龙","倍他米松",
                                                              "地塞米松",
                                                              "氯地米松" )
                                           ),
                                           textInput("tpz_dose",
                                                     "剂量（mg）：",
                                                     "2")
                                           
                                 ),
                                 card_footer(
                                   tableOutput("tpz_table"))
                               ),
                               card(
                                 full_screen = TRUE,
                                 card_header("骨髓增生异常综合征M.D.安德森预后评分系统(MDAPSS)"),
                                 card_body(br(),
                                           prettyRadioButtons(
                                             inputId = "ECOG_mds",
                                             label = "1.体能状态(ECOG)≥2分", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("是","否"),
                                             choiceValues = c("2","0" )
                                           ),
                                           prettyRadioButtons(
                                             inputId = "age_mds",
                                             label = "2.年龄(岁)", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("60~64","≥65","<60"),
                                             choiceValues = c("1","2","0" )
                                           ),
                                           prettyRadioButtons(
                                             inputId = "blast_mds",
                                             label = "3.骨髓原始细胞比例(%)", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("5~10","11~29"),
                                             choiceValues = c("1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "plt_mds",
                                             label = "4.血小板(x109/L)", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("50~199","30~49","<30"),
                                             choiceValues = c("1","2","3")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "hb_mds",
                                             label = "5.血红蛋白(g/dL)", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("< 12 g/L","≥ 12 g/L"),
                                             choiceValues = c("2","0")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "wbc_mds",
                                             label = "6.白细胞(x109/L)", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c(">20","≤20"),
                                             choiceValues = c("2","0")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "kay_mds",
                                             label = "7.染色体核型(7号染色体异常或>2个异常)", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("是","否"),
                                             choiceValues = c("3","0" )
                                           ),
                                           prettyRadioButtons(
                                             inputId = "transfunsion_mds",
                                             label = "8.既往输血", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("是","否"),
                                             choiceValues = c("1","0" )
                                           )
                                           
                                 ),
                                 card_footer(
                                   tableOutput("mdapss_table"))
                               ),
                               
                               #EBMT risk score
                               card(
                                 full_screen = TRUE,
                                 card_header("the EBMT risk score"),
                                 card_body(br(),
                                           
                                           prettyRadioButtons(
                                             inputId = "age_EBMT",
                                             label = "Age of the patient, years", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("<20","20-40",">40"),
                                             choiceValues = c("0","1","2" )
                                           ),
                                           prettyRadioButtons(
                                             inputId = "stage_EBMT",
                                             label = "Disease stage", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("Early/aplastic anemia ","Intermediate","Late "),
                                             choiceValues = c("0","1","2")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "time_EBMT",
                                             label = "Time interval from diagnosis to transplant, months ", 
                                             selected = "1",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("<12 or transplanted in first CR ",">12"),
                                             choiceValues = c("0","1")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "donor_EBMT",
                                             label = "Donor type ", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("HLA-identical sibling donor ","Unrelated donor, other "),
                                             choiceValues = c("0","1")
                                           ),
                                           prettyRadioButtons(
                                             inputId = "sex_EBMT",
                                             label = "Donor recipient sex combination c", 
                                             selected = "0",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("All other ","Female donor, male recipient "),
                                             choiceValues = c("0","1")
                                           )
                                           
                                 ),
                                 card_footer(
                                   textOutput("EBMT_table"),
                                   plotOutput("EBMT_plt"))
                               ),
                               card(
                                 full_screen = TRUE,
                                 card_header("血清钙离子矫正（通用剂量单位）"),
                                 card_body(br(),
                                           textInput("Serum_Calcium",
                                                     "Serum Calcium(mg/dL)",
                                                     "10"),
                                           prettyRadioButtons(
                                             inputId = "normal_albumin",
                                             label = "Normal albumin(g/dL)", 
                                             selected = "4",
                                             inline = TRUE, 
                                             status = "danger",
                                             fill = TRUE,
                                             choiceNames = c("4g/dL ","4.4g/dL "),
                                             choiceValues = c("4","4.4")
                                           ),
                                           textInput("Patient_albumin",
                                                     "Patient albumin (g/dL)",
                                                     "3.9")
                                 ),
                                 card_footer(
                                   textOutput("adjust_calcium"))
                               )
                               
                      )
                    )
                  )
                )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()
  
  # Switch language when button is clicked
  observeEvent(input$Id078, {
    if (input$Id078 == TRUE) {
      switch_language("English",input$scr,input$aged,input$weight,input$height,input$regimens, input$gender)
    } else {
      switch_language("Chinese",input$scr,input$aged,input$weight,input$height,input$regimens,input$gender)
    }
  })
  output$titlepan = renderText({
    switch_fun(input$Id078, "Hematological Drug Assistant","血液科用药助手") 
  })
  output$advance = renderText({
    switch_fun(input$Id078, "Advanced Settings","更多") 
  })
  "Advanced Settings"
  output$Recommendation = renderText({
    withProgress(message = 'loading ...',
                 detail = 'This may take a while...', value = 1, {
    switch_fun(input$Id078, "Recommendation","方案推荐") 
                 })
  })
  output$SideEffects = renderText({
    switch_fun(input$Id078, "SideEffects", "药物副作用") 
  })
  output$Aboutme = renderText({
    switch_fun(input$Id078, "About me","关于我") 
  })
  output$Survey = renderText({
    switch_fun(input$Id078, "aGVHD","aGVHD") 
  })
  output$Survey2 = renderText({
    switch_fun(input$Id078, "IPI","IPI") 
  })
  output$Survey3 = renderText({
    switch_fun(input$Id078, "CAR-T","CAR-T") 
  })
  output$Survey4 = renderText({
    switch_fun(input$Id078, "MM","MM") 
  })
  
  output$Survey5 = renderText({
    switch_fun(input$Id078, "PHSC-Count","外周采干计数") 
  })
  output$Surveymore = renderText({
    switch_fun(input$Id078, "More","更多") 
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
  
  ccr <- reactive({
    if (is.null(input$age)| is.null(input$scr)){
      ccr <- NULL
    } else if (input$gender == "Common" | input$gender == "通用"){
      ccr <- (140- as.numeric(input$aged)) * as.numeric(input$weight)/ 0.818 / as.numeric(input$scr)
    } else if (input$gender == "Female" | input$gender == "女性"){
      ccr <- 0.85 * 1.23 * (140- as.numeric(input$aged)) * as.numeric(input$weight)/ as.numeric(input$scr)
    }else {
      ccr <- 1.23 * (140- as.numeric(input$aged)) * as.numeric(input$weight)/ as.numeric(input$scr)
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
                 人体表面面积 = bsa(),
                 肌酐清除率 = ccr()) 
    } else{
      data.frame(Weight = input$weight,
                 Height = input$height,
                 BodyMassIndex = bmi(),
                 BodySurfaceArea = bsa(),
                 EstimatedCCr = ccr()) 
    }
    
  })
  
  output$table2 <- renderTable({
    withProgress(message = 'Calculating ...',
                 detail = 'This may take a while...', value = 1, {
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
    withProgress(message = 'Reading informations ...',
                 detail = 'This may take a while...', value = 1, {
    if (mod() == 1){
      print(paste("本应用程序由来自GmadeStudio HBSig和CMU1H的Yanhua Zheng,Dr.Qinchuan Yu, Xin Ding & Prof.Xiaoxue Wang以及西安交通大学社会科学研究所的Dr.Linfeng开发。"))
    } else {
      print(paste("This application is under developed by Yanhua Zheng,Dr.Qinchuan Yu, Xin Ding and Prof.Xiaoxue Wang from GmadeStudio HBSig and the department of hematology,CMU1H, and Dr.Linfeng He from Institute for Empirical Social Science Research,XJTU. "))
    }
                 })
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
      print("此外，引用的药物剂量和方案主要来自于中国临床血液学方案手册，Uptodate，USMLE Step 1＆2和BRS药理学。如果你在论文写作中使用了本软件，请引用我们Yanhua Zheng, Qinchuan, Y., Xin, D., Linfeng, H., & Xiaoxue, W. (2024). rainoffallingstar/hemat_drug_assistant: release (v0.02). Zenodo. https://doi.org/10.5281/zenodo.10951894")
    } else {
      print(" In addition, the citated drug does and regimens are mainly taken from an chinese regimens handbook for clinical hematology，Uptodate，USMLE Step 1&2 and BRS Pharmacology. If you use our aplication in writing papers,please cite us as the following: Yanhua Zheng, Qinchuan, Y., Xin, D., Linfeng, H., & Xiaoxue, W. (2024). rainoffallingstar/hemat_drug_assistant: release (v0.02). Zenodo. https://doi.org/10.5281/zenodo.10951894")
    }
  })
  
  
  
  output$plt <- renderPlot({
    img <- jpeg::readJPEG(paste0(getwd(),"/image/logo.jpg"))
    plot(as.raster(img))
  })
  
  
  grades <- eventReactive(input$submit, {
    skin <- as.numeric(input$skin)
    liver <- as.numeric(input$liver)
    gastric <- as.numeric(input$gastric)
    if (skin + liver + gastric == 0){
      grades = 0
    }else if (skin + liver + gastric <= 2 &  liver == 0 & gastric == 0){
      grades = 1
    }else if (skin == 4 | liver == 4 ){
      grades = 4
    } else if (liver == 2 | liver == 3 | gastric == 2 | gastric == 3 | gastric == 4){
      grades = 3
    }else {
      grades = 2
    }
    grades
    
  })
  
  output$grade <- renderText({
    
    print(paste("GVHD总分级：",grades()))
  })
  
  IPI_grades <- eventReactive(input$submit2, {
    age <- as.numeric(input$age)
    out_of_node <- as.numeric(input$out_of_node)
    if (as.numeric(input$ECOG)>= 2){
      p_ECGO <- 1
    }else {
      p_ECGO <- 0
    }
    if (as.numeric(input$ann)>2){
      p_ann <- 1
    }else {
      p_ann <- 0
    }
    IPI_grades <- age + out_of_node + p_ECGO + p_ann + as.numeric(input$ldh)
  })
  
  output$grade2 <- renderText({
    
    if (IPI_grades() < 2){
      print(paste("IPI评分：",IPI_grades(),"分，为低危"))
    }else if (IPI_grades() == 2){
      print(paste("IPI评分：",IPI_grades(),"分，为中低危"))
    }else if (IPI_grades() == 3){
      print(paste("IPI评分：",IPI_grades(),"分，为中高危"))
    }else {
      print(paste("IPI评分：",IPI_grades(),"分，为高危"))
    }
    
  })
  
  Ican_grades <- eventReactive(input$submit3, {
    
    Ican_grades <- as.numeric(input$count) + as.numeric(input$write) + as.numeric(input$listen) + as.numeric(input$attention) + as.numeric(input$named)
  })
  
  output$grade3 <- renderTable({
    
    if (Ican_grades() >= 7){
      cart[1,]
    }else if (Ican_grades() >= 3){
      cart[2,]
    }else if (Ican_grades() >= 1){
      cart[3,]
    }else {
      cart[4,]
    }
    
  })
  
  tr <- function(x){
    as.numeric(x)
  }
  
  ds <- reactive({
    DS <- c("DS I期","DS II期","DS III期")
    if (tr(input$HGB) == 1 & tr(input$Serum_Ca) == 1 & tr(input$bone_image) == 1 & tr(input$M_protein) == 1){
      ds <- DS[1]
    } else if (tr(input$HGB) == 3 | tr(input$Serum_Ca) == 2 | tr(input$bone_image) == 2 | tr(input$M_protein) == 2) {
      ds <- DS[3]
    }else {
      ds <- DS[2]
    }
  })
  
  iss <- reactive({
    ISS <- c("ISS I期","ISS II期","ISS III期")
    if (tr(input$B2_MG) == 1 & tr(input$W_MG) == 1){
      iss <- ISS[1]
    } else if (tr(input$B2_MG) == 3 ){
      iss <- ISS[3]
    }else {
      iss <- ISS[2]
    }
  })
  output$grade4 <- renderText({
   
    print(paste("该病人是：",ds(),paste0(input$serum_jg,"亚型"),iss()))
  })
  
  phsc_count_before <- eventReactive(input$submit_PHSC, {
    PB_WBC <- as.numeric(input$PB_WBC)
    PB_CD34 <- as.numeric(input$PB_CD34)
    phsc_count_before <- 10 *  PB_WBC * PB_CD34
    return(phsc_count_before)
  })
  
  output$PHSC_before <- renderText({
    print(paste("计算公式为：10*PB_WBC(10^9)*PB_CD34(%),采集前外周血干细胞计数为：",phsc_count_before()))
  })
  
  phsc_count_after <- eventReactive(input$submit_collection, {
    collection_WBC <- as.numeric(input$collection_WBC)
    collection_CD34 <- as.numeric(input$collection_CD34)
    collection_volumn <- as.numeric(input$collection_volumn)
    phsc_count_after <- collection_WBC / 10 * collection_CD34 * collection_volumn / as.numeric(input$weight)
    return(phsc_count_after)
  })
  
  output$PHSC_after <- renderText({
    print(paste("计算公式为：collection_WBC(10^9)/10*collection_CD34(%)*collection_volumn(ml)/weight(kg),采集物干细胞计数为：",phsc_count_after(),"(10^6/kg)"))
  })
  
  cal_DICscore <- eventReactive(input$submit_DIC, {
    score <- as.numeric(input$PLT) + 
      as.numeric(input$FDPs) + 
      as.numeric(input$PT) +
      as.numeric(input$FBG)
    return(score)
  })
  
  output$DIC_score <- renderText({
    if (cal_DICscore() >= 5){
      print(paste("积分>=5分，符合典型DIC"))
    } else {
      print(paste("积分<5分，提示非典型DIC"))
    }
    
  })
  
  cal_HITscore <- eventReactive(input$submit_HIT, {
    score <- as.numeric(input$PLT_change) + 
      as.numeric(input$PLT_time) + 
      as.numeric(input$PLT_agg) +
      as.numeric(input$PLT_reason)
    return(score)
  })
  
  output$HIT_score <- renderText({
    if (cal_HITscore() <= 3){
      print(paste("0-3分,轻度怀疑:诊断HIT概率很低"))
    } else if (cal_HITscore() >= 6) {
      print(paste("6-8分,高度怀疑:抗体阴性者诊断HIT概率为16%,抗体阳性者诊断HIT概率为98%。"))
    } else {
      print(paste("4-5分,中度怀疑:抗体阴性者诊断HIT概率为0.6%,抗体阳性者诊断HIT概率为58.2%"))
    }
    
  })
  output$CIT_score <- renderText({
    paste("CIT grade is",input$PLT_total)
  })
  output$tpz_table <- renderTable({
    
    tpz_refertable <- data.frame(
      tpznames = c("可的松","氢化可的松","强的松","强的松龙",
                   "甲强龙","曲安西龙","倍他米松",
                   "地塞米松",
                   "氯地米松" ),
      basedose = c(25,20,5,5,4,4,0.8,0.75,0.5)
    ) 
    rownames(tpz_refertable) <- tpz_refertable$tpznames
    input_dose_times <- as.numeric(input$tpz_dose) / tpz_refertable[input$tpz_name,][["basedose"]]
    tpz_refertable %>% 
      dplyr::mutate(dose = basedose * input_dose_times) %>% 
      dplyr::select(-basedose,-tpznames) %>%  
      t() %>% 
      as.data.frame() %>% 
      dplyr::mutate(单位 = "mg")
    
  })
  output$mdapss_table <- renderTable({
    
    mdapss_refertable <- data.frame(
      积分 = c("0-4","5-6","7-8",">=9"),
      分组 = c("低危","中危1","中危2","高危"),
      中位生存月 = c(54,25,14,6),
      三年生存率 = c(63,34,16,4),
      index = c(4,6,8,9)
    ) 
    index_sum <- c(input$transfunsion_mds,input$kay_mds,input$wbc_mds,
                   input$hb_mds,input$plt_mds,input$blast_mds,
                   input$age_mds,input$ECOG_mds) %>% 
      as.numeric() %>% sum()
    index_sum_fix <- index_sum
    if (index_sum >= 9){
      index_sum_fix = 9
    }
    mdapss_refertable <- mdapss_refertable %>% 
      dplyr::filter(index >= index_sum_fix) %>% 
      dplyr::arrange(index) %>% 
      dplyr::mutate(score = index_sum) %>% 
      dplyr::select(-index)
    
    mdapss_refertable[1,]
    
  })
  
  output$EBMT_table <- renderText({
    
    index_sum <- c(input$age_EBMT,input$stage_EBMT,input$time_EBMT,
                   input$donor_EBMT,input$sex_EBMT) %>% 
      as.numeric() %>% sum()
    paste0( "the EBMT risk score is",index_sum)
  })
  
  output$EBMT_plt <- renderPlot({
    img <- jpeg::readJPEG(paste0(getwd(),"/image/EBMT.jpg"))
    plot(as.raster(img))
  })
  
  output$adjust_calcium <- renderText({
    if (as.numeric(input$Patient_albumin) > as.numeric(input$normal_albumin)){
      paste0("the serum albumin should be lower than normal albumin")
    } else {
      adjust_cal <- as.numeric(input$Serum_Calcium) + 
        0.8 * (as.numeric(input$normal_albumin) - as.numeric(input$Patient_albumin))
      paste0("the adjust serum albumin is"," ",adjust_cal,"mg/dL")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
