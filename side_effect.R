library(readxl)
library(fs)
library(dplyr)

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

for (i in 1:length(disease_names)) {
  for (a in 1:length(disease_list[[i]])) {
    dbs <- read_excel(paste0(getwd(),"/data/",names(disease_list)[i],
                             "/",disease_list[[i]][a], ".xlsx")) 
    db <- rbind(db,dbs[,1])
  }
  
}
write.csv(db,"side.csv")
