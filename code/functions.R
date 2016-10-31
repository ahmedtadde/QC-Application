libraries <- function(){
  if(!require("pacman")) install.packages("pacman",dependencies = T)
  library(pacman)
  p_load(tools)
  p_load(bit64)
  p_load(feather)
  p_load(stringi)
  p_load(stringr)
  p_load(tidyr)
  p_load(plyr)
  p_load(dplyr)
  p_load(data.table)
  p_load(DT)
  p_load(shiny)
  p_load(shinythemes)
  p_load(foreign)
  p_load(readxl)
  p_load(foreach)
  p_load(snow)
  p_load(doSNOW)
  registerDoSNOW(makeCluster(detectCores()))
  p_load(doParallel)
  registerDoParallel(makeCluster(detectCores()))
  
}

`%notin%` <-  Negate(`%in%`)

toCharacter<- function(data){
  
  library(stringi)
  library(data.table)
  
  data <- data.table(data)
  colNames <- names(data)
  
  foreach(i = 1:dim(data)[2], .packages = c("data.table","stringi"), .combine = cbind) %dopar%{
    gsub("(^|[^0-9])0+", "\\1", tolower(stri_trim(as.character(data[[i]]))))
  } -> data; data <- data.table(data)
  
  setnames(data, names(data), colNames)
  return(data)
}

ReportTypeInteger <- function(x){
  library(stringi)
  if(!is.na(stri_split(as.character(x), fixed = "\\.")[[1]][1])){
    return(as.integer(stri_trim(stri_split(as.character(x),fixed = "\\.")[[1]][1])))
  }
}

ReportTypeDescription <- function(x,data){
  library(dplyr)
  library(data.table)
  library(stringi)
  
  df <- data %>% filter(data[[1]] %in% x)
  return(stri_trim(df$Description))
}



getDATA <- function(filename){
  library(data.table)
  library(readxl)
  library(tools)
  library(parallel)
  
  `%notin%` <-  Negate(`%in%`)
  
  if(file_ext(filename) %notin% "xls" | file_ext(filename) %notin% "xlsx"| file_ext(filename) %notin% "xlsm"){
    DT <- fread(filename,
                strip.white = T, 
                stringsAsFactors = F,
                na.strings = c("","NA")
    )
    
    DT <- data.table(DT[,lapply(.SD,as.character)])
    
    
    return(DT)
  }else{
    sheets <- excel_sheets(filename)
    foreach(i =1:length(sheets), .combine = rbind, .packages = c("readxl")) %dopar%{
      read_excel(filename, sheet = sheets[i])
    } -> DT;
    return(data.table(DT))
  }
}


getASCII <- function(path){
  
  
  file.rename(path$datapath,paste0(path$datapath, ".xls"))
  table <- data.table(toCharacter(read_excel(paste0(path$datapath, ".xls"), sheet = 1)))
  
  
  if(all(c("Record Type","Variable Name",
           "Description","Minimum value",
           "Maximum value","Nominal value definitions") %in% names(table)
  )){
    
    ColNames <- unique(names(table))
    table <- data.table(data.frame(table)[,!duplicated(names(table), fromLast = T)])
    setnames(table, names(table), ColNames)
    
    
    
    table <- table %>% select(
      which(names(table) %in% c("Record Type",
                                "Variable Name",
                                "Description",
                                "Minimum value",
                                "Maximum value",
                                "Nominal value definitions"
      )
      )
    )
    
    
    table$`Record Type` <- unlist(parLapply(makeCluster(detectCores()), table$`Record Type`, ReportTypeInteger))
    table <- data.table(table); setkeyv(table, "Record Type")
    
    # dt <- data.table(read_excel(path, sheet = 2))
    dt <- data.table(toCharacter(read_excel(paste0(path$datapath, ".xls"), sheet = 2)))
    setnames(dt, names(dt)[1], "Record Type")
    dt <- dt %>% select(which(names(dt) %in% c("Record Type","Description"))) %>% filter(!is.na(Description))
    dt$`Record Type` <- unlist(parLapply(makeCluster(detectCores()), dt$`Record Type`, ReportTypeInteger))
    dt <- data.table(dt); setkeyv(dt, "Record Type")
    
    table$`Record Type` <- unlist(parLapply(makeCluster(detectCores()), table$`Record Type`, ReportTypeDescription, dt))
    rm(dt)
    return(table)
  }else{
    
    return(data.table("STATUS" = "Empty", "MESSAGE" = "Selected File is NOT an ASCII Layout File"))
    
  }
}



getCUTPOINTS <- function(path){
  library(dplyr)
  library(data.table)
  library(stringi)
  library(foreach)
  library(parallel)
  
  
  table <- fread(path,strip.white = T,na.strings = c(""," ","NA",0), stringsAsFactors = F)
  if(names(table)[1] %in% "[") table[[1]] <- NULL
  if(names(table)[length(names(table))] %in% "]") table[[length(names(table))]] <- NULL
  
  foreach(i =1:dim(table)[2],.combine = c)%do%{
    if(all(table[[i]] %in% "[") == T) return(i)
    if(all(table[[i]] %in% "]") == T) return(i)
  } -> removeCols
  
  if(length(removeCols)!=0){
    table <- data.table(select(table, -c(removeCols)))
  }
  
  
  if(dim(table)[2]==1){
    newColNames <- stri_split(names(table),fixed = "/")[[1]]
    if(length(newColNames) !=0){
      foreach(i = 1: length(newColNames)) %do%{
        newColNames[i] <- gsub("\\]","",newColNames[i])
        newColNames[i] <- gsub("VAR","",newColNames[i])
        newColNames[i] <- stri_trim(newColNames[i])
      }
    }
    
    newColNames <- newColNames[-c(which(newColNames %in% ""))]
    
    table <- as.data.frame(table)
    foreach(i =1:dim(table)[1], .combine = rbind) %do%{
      return(stri_split(table[i,],fixed = "\t")[[1]])
    } -> table; table <- data.table(table)
    
    foreach(i=1:dim(table)[2], .combine = c)%do%{
      count <- which(grepl("\\]$", table[[i]]))
      if(length(count) == length(table[[i]])){
        table[[i]] <- gsub("\\]","",table[[i]])
        return(i)
      }
    } -> upTo
    
    if(length(upTo) ==1){
      table <- data.table(select(table, c(1:upTo)))
    }
    
    setnames(table, names(table), newColNames[1:upTo])
    setnames(table, names(table)[1], "Subject")
    
    table$Subject <- gsub("\\[","",table$Subject)
    table$Subject <- gsub("\\]","",table$Subject)
    table$Subject <- gsub("\\(","",table$Subject)
    table$Subject <- gsub("\\)","",table$Subject)
    table$Subject <- stri_trim(as.character(table$Subject))
    
    
  }else{
    
    foreach(i = 1: length(names(table))) %do%{
      names(table)[i] <- gsub("\\]","",names(table)[i])
      names(table)[i] <- gsub("\\[","",names(table)[i])
      names(table)[i] <- gsub("/VAR","",names(table)[i], fixed = T)
    }
    
    setnames(table, names(table)[1], "Subject")
    table$Subject <- gsub("\\[","",table$Subject)
    table$Subject <- gsub("\\]","",table$Subject)
    table$Subject <- gsub("\\(","",table$Subject)
    table$Subject <- gsub("\\)","",table$Subject)
    table$Subject <- stri_trim(as.character(table$Subject))
    
  }
  
  table <- data.table(table); setkey(table, Subject)
  return(table)
}








getCompareFiles <- function(path){
  
  library('readxl')
  
  foreach(k = 1:length(path)) %do%{
    if(file_ext(path[k]) %in% "csv"| file_ext(path[k]) %in% "txt"){
      table <- fread(path[k],strip.white = T,na.strings = c("","NA"))
    }else{
      sheets <- excel_sheets(path[k])
      foreach(i =1:length(sheets), .combine = rbind, .packages =c('readxl')) %dopar%{
        read_excel(path[k], sheet = sheets[i])
      } -> table; table <- data.table(table); rm(sheets)
    }
    
    if(is.null(table) | !is.data.frame(table)) return(list("case" = 1))
    if(names(table)[1] %in% "V1") table <- data.table(table %>% select(c(2:dim(table)[2])))
    return(list("data" = table ,"case" = 2))
    
  } -> dataList ; rm(table);rm(k)
  
  tableList <- list()
  foreach(k = 1:length(dataList), .combine = c) %do%{
    if(dataList[[k]]$case == 1) return(TRUE)
    if(dataList[[k]]$case == 2){
      tableList[[k]] <- dataList[[k]]$data
      return(FALSE)
    }
  } -> check; rm(k)
  
  if(all(check %in% F) %notin% T) return(list("data" = NULL ,"keyOptions" = NULL, "case" = 1))
  
  table <- rbindlist(tableList, use.names = T) ; rm(check); rm(dataList); rm(tableList)
  
  possibleKeys <-  names(table)[which(grepl("ssid|sasid|uin|student id|studentid|myid", names(table), ignore.case = T))]
  return(list("data" = table,"keyOptions" = possibleKeys, "case" = 2))
}





FilterBy <- function(table, columns, values){
  
  data <- table
  foreach(i = 1:length(values), .combine = c, .packages= c('stringi')) %dopar%{
    stri_split(values[i], fixed ="::")[[1]][1]
  } -> filterCols 
  
  foreach(i= 1:length(filterCols)) %do%{
    foreach(j=1:length(values), .combine = c, .packages = c("stringi","dplyr")) %dopar%{
      if(setequal(filterCols[i], stri_split(values[j], fixed ="::")[[1]][1])){
        return(stri_split(values[j], fixed ="::")[[1]][2])
      }
    }-> Vals
    
    return(Vals)
  }-> filterVals
  
  
  foreach(i =1:length(filterCols)) %do%{
    filterCol <- which(stri_trim(names(data)) %in% c(stri_trim(columns[as.integer(filterCols[i])])))
    data <- data.table(data %>% filter(tolower(stri_trim(as.character(data[[filterCol]]))) %in% tolower(stri_trim(as.character(filterVals[[i]])))))
    setkey(data, KEY)
    
  }
  return(data)
}


compareFiles <- function(df1,cols1,df2,cols2){
  
  x <- stri_trim(cols1)
  y <- stri_trim(cols2)
  names(df1) <- stri_trim(names(df1))
  names(df2) <- stri_trim(names(df2))
  
  
  
  if("All" %in% x){
    x <- names(df1)[which(names(df1) != "KEY")]
  }
  
  if("All" %in% y){
    y <- names(df2)[which(names(df2) != "KEY")]
  }
  
  if(length(x) == length(y)){
    
    if(length(x) > 1){
      foreach(i = 1:length(x), .combine = cbind) %do%{
        data.table(select(df1, which(names(df1) %in% x[i])))
      } -> table; table <- data.table(table)
    }else{
      table <- data.table(select(df1, which(names(df1) %in% x)))
    }
    
    if(is.data.table(table) == T & all(dim(table) !=  0) == T){
      dataColnames1 <- c("KEY",names(table))
      setnames(table, names(table), paste0("V",1:length(x)))
      data1 <- data.table("KEY" = df1$KEY,table)
      data1$KEY <- gsub("(^|[^0-9])0+", "\\1", tolower(stri_trim(as.character(data1$KEY))))
      setkey(data1, KEY)
    }else{
      return(list("case"= 999))
    }
    
    if(length(y) > 1){
      foreach(i = 1:length(y), .combine = cbind) %do%{
        data.table(select(df2, which(names(df2) %in% y[i])))
      } -> table; table <- data.table(table)
    }else{
      table <- data.table(select(df2, which(names(df2) %in% y)))
    }
    
    if(is.data.table(table) == T & all(dim(table) !=  0) == T){
      dataColnames2 <- c("KEY",names(table))
      setnames(table, names(table), paste0("V",1:length(y)))
      data2 <- data.table("KEY" = df2$KEY,table)
      data2$KEY <- gsub("(^|[^0-9])0+", "\\1", tolower(stri_trim(as.character(data2$KEY))))
      setkey(data2, KEY)
    }else{
      return(list("case"= 999))
    }
    
    
    
    matchdataColnames <- paste0(dataColnames1,"/==/", dataColnames2)
    mismatchdataColnames <- paste0(dataColnames1,"||", dataColnames2)
    
    commonKeyValues <- intersect(data1$KEY, data2$KEY)
    
    if(length(commonKeyValues) == 0){
      return(list("case" = 0))
    }else{
      
      In1Not2  <- data.table(data1 %>% filter(KEY %in% setdiff(data1$KEY, commonKeyValues)))
      setkey(In1Not2 ,KEY)
      In2Not1  <- data.table(data2 %>% filter(KEY %in% setdiff(data2$KEY, commonKeyValues)))
      setkey(In2Not1 ,KEY)
      
      comparisonData1 <- data.table(data1 %>% filter(KEY %in% commonKeyValues))
      setkey(comparisonData1,KEY)
      comparisonData2 <- data.table(data2 %>% filter(KEY %in% commonKeyValues))
      setkey(comparisonData2,KEY)
    }
    
    comparisonData1 <- toCharacter(comparisonData1); setkey(comparisonData1,KEY)
    comparisonData2 <- toCharacter(comparisonData2); setkey(comparisonData1,KEY)
    perfect <- data.table(intersect(comparisonData1,comparisonData2))
    setkey(perfect, KEY)
    rm(data1); rm(data2)
    
    if(setequal(dim(perfect)[1],min(dim(comparisonData1)[1], dim(comparisonData2)[1])) %in% TRUE){
      
      setnames(In1Not2, names(In1Not2), dataColnames1)
      setnames(In2Not1, names(In2Not1), dataColnames2)
      setnames(perfect, names(perfect), matchdataColnames)
      
      return(list("Match" = perfect, "IN-1-NOT-2" = In1Not2 , "IN-2-NOT-1" = In2Not1 ,"case" = 1))
    }else{
      
      if(dim(perfect)[1] == 0){
        mismatch1to2 <- data.table(comparisonData1); setkey(mismatch1to2,KEY)
      }else{
        x <- comparisonData1$KEY[which(comparisonData1$KEY %notin% perfect$KEY)]
        mismatch1to2 <- data.table(comparisonData1 %>% filter(KEY %in% x)); setkey(mismatch1to2,KEY)
      }
      rm(comparisonData1)
      
      
      keys <- unique(mismatch1to2$KEY)
      foreach(i= 1:length(keys)) %do%{
        
        table <- data.table(mismatch1to2 %>% filter(KEY %in% keys[i]))
        
        if(dim(table)[1] > 1){
          colNames <- names(table)
          foreach(k=1:length(colNames), .packages = 'base', .combine = cbind) %dopar%{
            if(length(unique(table[[k]]))==1){
              return(unique(table[[k]]))
            }else{
              return(paste0(unique(as.character(table[[k]])),collapse = "_"))
            }
          } -> data; data <- data.table(data); setnames(data, names(data), c(colNames))
          
          return(data)
        }else{
          return(table)
        }
        
      }-> mismatch1to2
      
      mismatch1to2 <- rbindlist(mismatch1to2,use.names = T)
      setkey(mismatch1to2,KEY)
      
      if(dim(perfect)[1] == 0){
        mismatch2to1 <- data.table(comparisonData2); setkey(mismatch2to1,KEY)
      }else{
        x <- comparisonData2$KEY[which(comparisonData2$KEY %notin% perfect$KEY)]
        mismatch2to1 <- data.table(comparisonData2 %>% filter(KEY %in% x)); setkey(mismatch2to1,KEY)
      }
      rm(comparisonData2)
      
      keys <- unique(mismatch2to1$KEY)
      foreach(i= 1:length(keys)) %do%{
        
        table <- data.table(mismatch2to1 %>% filter(KEY %in% keys[i]))
        
        if(dim(table)[1] > 1){
          colNames <- names(table)
          foreach(k=1:length(colNames), .packages = 'base', .combine = cbind) %dopar%{
            if(length(unique(table[[k]]))==1){
              return(unique(table[[k]]))
            }else{
              return(paste0(unique(as.character(table[[k]]))),collapse = "_")
            }
          } -> data; data <- data.table(data); setnames(data, names(data), c(colNames))
          
          return(data)
        }else{
          return(table)
        }
        
      }-> mismatch2to1
      
      mismatch2to1 <- rbindlist(mismatch2to1,use.names = T)
      setkey(mismatch2to1,KEY)
      
      
      
      if(dim(mismatch2to1)[1] == dim(mismatch1to2)[1] &
         dim(mismatch2to1)[2] == dim(mismatch1to2)[2] &
         setequal(names(mismatch2to1), names(mismatch1to2)) %in% TRUE &
         setequal(mismatch2to1$KEY, mismatch1to2$KEY) %in% TRUE){
        
        foreach(i =1:length(names(mismatch1to2)), .combine = cbind) %dopar%{
          paste(mismatch1to2[[i]], mismatch2to1[[i]], sep = "||")
        } -> difference
        difference <- data.table(difference)
        setnames(difference, names(difference), mismatchdataColnames)
        setnames(In1Not2, names(In1Not2), dataColnames1)
        setnames(In2Not1, names(In2Not1), dataColnames2)
        setnames(perfect, names(perfect), matchdataColnames)
        
        return(list("Match" = perfect, "Mismatch" = difference, "IN-1-NOT-2" = In1Not2,"IN-2-NOT-1" = In2Not1, "case" = 2))
        
      }else{
        return(list("case" = 3))
      }
      
    }
  }else{
    return(list("case" = 4))
  }
}












akeMaster <- function(wd, directory, state){
  
  # wd = working directory
  # directory = path to spreasheet directory
  # state = which state
  
  setwd(directory)
  selection <-  list.files(pattern = "*.csv") # list of all spreadsheets in directory
  masterFiles <- which(grepl("master", selection, ignore.case = T)) # find names contain word Master
  NotMasterFiles <- which(grepl("^family|^label|^district|^csr", selection, ignore.case = T))
  if(length(NotMasterFiles) != 0){
    masterFiles <- masterFiles[-c(NotMasterFiles)]
  }; rm(NotMasterFiles)
  
  #IF there are master files
  if(length(masterFiles) != 0){
    selection <- selection[-c(masterFiles)] # remove master files from list
  }; rm(masterFiles)
  
  #read-in data from each spreadsheet and combine results into a list object
  DataList <- parLapply(makeCluster(detectCores()),selection,getDATA)
  spreadsheetCount <- length(DataList)
  
  #extract the variables names from each data
  colNames <- foreach(i=1:length(DataList)) %dopar%{return(names(DataList[[i]]))}
  
  #TWO CASES : (1) when all spreadsheets contain exactly the same variable number and names;
  #            (2) when (1) does not apply
  
  
  
  if(setequal(Reduce(intersect, colNames), unique(unlist(colNames)))){
    # CASE(1)
    
    # ROW BIND all the spreadsheets' data into one master/super data
    DT <- rbindlist(DataList); rm(DataList); rm(selection)
    
    # detect all ID variables
    ids <- names(DT)[which(grepl("id", as.character(names(DT)), ignore.case = T))]
    
    # detect all variables related to District Identification
    districtCols <-  names(DT)[which(grepl("dis|dcr", names(DT), ignore.case = T))]
    
    if(length(ids) !=0 & length(districtCols) !=0){
      
      # detect THE District ID variable specifically ( if applicable)
      if(length(intersect(ids, districtCols)) != 0){
        
        # get the district id variable and convert to INTEGER TYPE (useful for sorting master table later)
        districtCols <- intersect(ids, districtCols)
        foreach(k=1:length(districtCols)) %do%{
          DT[[which(names(DT) %in% districtCols[k])]] <- as.character(DT[[which(names(DT) %in% districtCols[k])]])
          toInteger <- as.integer(DT[[which(names(DT) %in% districtCols[k])]])
          
          if(all(is.na(toInteger)) %in% FALSE){
            DT[[which(names(DT) %in% districtCols[k])]] <- toInteger
          }
        }
      }
    }
    
    
    # detect all variables related to School Identification
    schoolCols <-  names(DT)[which(grepl("sch|bcr", names(DT), ignore.case = T))]
    
    if(length(ids) !=0 & length(schoolCols) !=0){
      # detect THE School ID variable specifically ( if applicable)
      if(length(intersect(ids, schoolCols)) != 0){
        
        # get the district id variable and convert to INTEGER TYPE (useful for sorting master table later)
        schoolCols <- intersect(ids, schoolCols)
        foreach(k=1:length(schoolCols)) %do%{
          DT[[which(names(DT) %in% schoolCols[k])]] <- as.character(DT[[which(names(DT) %in% schoolCols[k])]])
          toInteger <- as.integer(DT[[which(names(DT) %in% schoolCols[k])]])
          
          if(all(is.na(toInteger)) %in% FALSE){
            DT[[which(names(DT) %in% schoolCols[k])]] <- toInteger
          }
        }
      }
    }
    
    
    # detect all variables related to Grade Identification
    gradeCols <- names(DT)[which(grepl("gr|sgr", names(DT), ignore.case = T))]
    
    if(length(gradeCols) !=0){
      foreach(k=1:length(gradeCols)) %do%{
        DT[[which(names(DT) %in% gradeCols[k])]] <- as.integer(DT[[which(names(DT) %in% gradeCols[k])]])
      }
    }
    
    # subjectCols <- names(DT)[which(grepl("subj|test", as.character(names(DT)), ignore.case = T))]
    
    if(length(districtCols) != 0 & length(schoolCols) !=0 & length(gradeCols) != 0){
      setkeyv(DT,c(districtCols,schoolCols,gradeCols))
    }else{
      setkey(DT)
    }
    
    rm(list = c("ids","districtCols","schoolCols","gradeCols"))
    
    write.csv(DT, paste0(directory,"/1MASTER_",state,".csv"), row.names = F)
    # data.table::fwrite(DT, paste0(directory,"/1MASTER_",state,".csv"))
    setwd(wd)
    return(spreadsheetCount)
  }else{   
    
    # CASE(2)
    
    # Make variables sets
    foreach(j =1:length(colNames), .combine = c) %dopar%{
      return(paste0(colNames[[j]], collapse = "|"))
    }-> colNamesByGroup; colNamesByGroup <- unique(colNamesByGroup)
    
    #identify datsets related to each sets of variables 
    foreach(j =1:length(colNamesByGroup)) %do%{
      
      foreach(k =1:length(colNames), .combine = c, .packages = c('stringi','dplyr')) %dopar%{
        if(setequal(colNames[[k]], stri_split(colNamesByGroup[[j]], fixed = "|")[[1]])) return(k)
      }-> idx
      
      return(idx)
      
    }-> DT.byGroup
    
    # combine spreadsheet data based on membership to a variables set
    foreach(j=1:length(DT.byGroup)) %do%{
      idx <- DT.byGroup[[j]]
      dataGroupList <- list()
      foreach(k =1:length(idx)) %do%{
        dataGroupList[[k]] <- DataList[[idx[k]]]
      };rm(idx)
      
      return(rbindlist(dataGroupList, use.names = T))
    }-> DT.byGroup
    
    
    
    
    foreach(j=1:length(DT.byGroup)) %do%{
      
      DT <- DT.byGroup[[j]]
      # detect all ID variables
      ids <- names(DT)[which(grepl("id", as.character(names(DT)), ignore.case = T))]
      
      # detect all variables related to District Identification
      districtCols <-  names(DT)[which(grepl("dis|dcr", names(DT), ignore.case = T))]
      
      if(length(ids) !=0 & length(districtCols) !=0){
        
        # detect THE District ID variable specifically ( if applicable)
        if(length(intersect(ids, districtCols)) != 0){
          
          # get the district id variable and convert to INTEGER TYPE (useful for sorting master table later)
          districtCols <- intersect(ids, districtCols)
          foreach(k=1:length(districtCols)) %do%{
            DT[[which(names(DT) %in% districtCols[k])]] <- as.character(DT[[which(names(DT) %in% districtCols[k])]])
            toInteger <- as.integer(DT[[which(names(DT) %in% districtCols[k])]])
            
            if(all(is.na(toInteger)) %in% FALSE){
              DT[[which(names(DT) %in% districtCols[k])]] <- toInteger
            }
          }
        }
      }
      
      
      # detect all variables related to School Identification
      schoolCols <-  names(DT)[which(grepl("sch|bcr", names(DT), ignore.case = T))]
      
      if(length(ids) !=0 & length(schoolCols) !=0){
        # detect THE School ID variable specifically ( if applicable)
        if(length(intersect(ids, schoolCols)) != 0){
          
          # get the district id variable and convert to INTEGER TYPE (useful for sorting master table later)
          schoolCols <- intersect(ids, schoolCols)
          foreach(k=1:length(schoolCols)) %do%{
            DT[[which(names(DT) %in% schoolCols[k])]] <- as.character(DT[[which(names(DT) %in% schoolCols[k])]])
            toInteger <- as.integer(DT[[which(names(DT) %in% schoolCols[k])]])
            
            if(all(is.na(toInteger)) %in% FALSE){
              DT[[which(names(DT) %in% schoolCols[k])]] <- toInteger
            }
          }
        }
      }
      
      
      # detect all variables related to Grade Identification
      gradeCols <- names(DT)[which(grepl("gr|sgr", names(DT), ignore.case = T))]
      
      if(length(gradeCols) !=0){
        foreach(k=1:length(gradeCols)) %do%{
          DT[[which(names(DT) %in% gradeCols[k])]] <- as.integer(DT[[which(names(DT) %in% gradeCols[k])]])
        }
      }
      
      # subjectCols <- names(DT)[which(grepl("subj|test", as.character(names(DT)), ignore.case = T))]
      
      if(length(districtCols) != 0 & length(schoolCols) !=0 & length(gradeCols) != 0){
        setkeyv(DT,c(districtCols,schoolCols,gradeCols))
      }else{
        setkey(DT)
      }
      
      rm(list = c("ids","districtCols","schoolCols","gradeCols"))
      
      write.csv(DT, paste0(directory,"/",j,"MASTER_",state,".csv"),row.names = F)
      
      # data.table::fwrite(DT, paste0(directory,"/",j,"MASTER_",state,".csv"))
      
    }
    
    setwd(wd)
    return(spreadsheetCount)
  }
}