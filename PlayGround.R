# source('functions.R')
# libraries()
# # wd <- getwd()
# path <- "C:/Users/temp_atadde/Desktop/Data Dump/Central_Valley_SD_223-Grade_9.xls"
# sheets <- excel_sheets(path)
# foreach(i =1:length(sheets), .combine = rbind, .packages =c('readxl')) %dopar%{
#   read_excel(path, sheet = sheets[i])
# } -> table; table <- data.table(table); rm(sheets)


# asciiPath <- "//Dc1fs/dc1ehd/share/Score Reports/Florida/Paper Reports/2016-17/Fall 2016/ASCII Layout/Florida ASCII Fall 2016_07152016.xls"
# ascii <- getASCII(asciiPath)
# state <- "FL"
# directory <- "//Dc1fs/dc1ehd/share/Score Reports/Florida/Paper Reports/2016-17/Fall 2016/QC Samples/Finaldata/Fall EOC/Round 01/Spreadsheets/family"
# # directory <- "C:/Users/temp_atadde/Desktop/UI - Copy/family"
# master <- makeMaster(wd, directory, state)
# rm(wd); rm(directory); rm(state)


# foreach(k = 1:length(path)) %do%{
#   if(file_ext(path[k]) %in% "csv"| file_ext(path[k]) %in% "txt"){
#     table <- fread(path[k],strip.white = T,na.strings = c("","NA"))
#   }else{
#     sheets <- excel_sheets(path[k])
#     foreach(i =1:length(sheets), .combine = rbind, .packages =c('readxl')) %dopar%{
#       read_excel(path[k], sheet = sheets[i])
#     } -> table; table <- data.table(table); rm(sheets)
#   }
# 
#   if(is.null(table) | !is.data.frame(table)) return(list("case" = 1))
#   if(names(table)[1] %in% "V1") table <- data.table(table %>% select(c(2:dim(table)[2])))
#   return(list("data" = table ,"case" = 2))
# 
# } -> dataList ; rm(table);rm(k)
# 
# tableList <- list()
# foreach(k = 1:length(dataList), .combine = c) %do%{
#   if(dataList[[k]]$case == 1) return(TRUE)
#   if(dataList[[k]]$case == 2){ 
#     tableList[[k]] <- dataList[[k]]$data
#     return(FALSE)
#   }
# } -> check; rm(k)
# 
# if(all(check %in% FALSE)) table <- rbindlist(tableList, use.names = T) ; rm(check); rm(dataList); rm(tableList) 

# filepath <- "H:/share/Score Reports/West Virginia/Paper Reports/Spring 2016/SDF/WV Spring 2016 - Summative SDF.xlsx"
# rio::convert(filepath,"C:/Users/temp_atadde/Desktop/WV Spring 2016 - Summative SDF.txt")


# filepath <- "//Dc1fs/dc1ehd/share/Score Reports/Ohio-OST (OCBA)/2015-2016/Paper Reporting/Summer 2016/SDF/G3ELA_ProductionExaminee_20160906145119_OST_Summer.txt"
# table <- fread(filepath)
# filepath <- "../../WV Spring 2016 - Summative SDF.csv"
# table <- getFile(filepath)
# key <- table$keyOptions
# table <- table$data
# names(table)[which(names(table) %in% key)] <- "KEY"
# setkey(table,KEY)
# whichFilterCols1 <- c(names(table)[23], names(table)[17])
# foreach(i=1:length(whichFilterCols1),.combine = c) %do%{
#   get <- which(names(table) %in% whichFilterCols1[i])
#   get <- sort(str_trim(as.character(unique(table[[get]]))))
#   get <- paste0(i,"::", get[1:length(get)])
#   return(get)
# } -> Options; rm(i); rm(get)
# whichFilterVals1 <- Options[c(3,49)]
# 
# filteredTable <- FilterBy(table, whichFilterCols1, whichFilterVals1)
