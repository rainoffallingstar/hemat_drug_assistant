library(readxl)
library(fs)
library(dplyr)
library(xlsx)

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
db <- data.frame()

usual_error <- read_excel("data/usual_error.xlsx")
names_normalized <- function(x,y){
  df <- x
  for (c in 1:nrow(df)){
    correct_str <- usual_error %>% filter(zh %in% df[c,y])
    df[c,y] <- correct_str[1,2]
      
    }
  }

for (i in 1:length(disease_names)) {
  for (a in 1:length(disease_list[[i]])) {
    filedir <- paste0(getwd(),"/data/",names(disease_list)[i],
                      "/",disease_list[[i]][a], ".xlsx")
    dbs <- read_excel(filedir) %>% 
      names_normalized(1)
    write.xlsx(dbs, paste0(getwd(),"/newdata/",names(disease_list)[i],
                           "/",disease_list[[i]][a], ".xlsx"))
  }
  
}
write.csv(db,"side.csv")
