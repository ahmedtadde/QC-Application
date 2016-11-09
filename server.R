source('functions.R')
libraries()
WD <- getwd()
shinyServer(function(input, output, session) {
  
  ##=============================================================================
  ##
  ##                                  TAB 1 (ASCII)
  ##
  ##=============================================================================
  
  PATH <- reactive({
    
    if (is.null(input$ascii)){
      return(NULL)
    }else{
      return(input$ascii)
    }
  })
  
  ASCII <- reactive({
    if (is.null(PATH())){
      return(NULL)
    }else{
      return(getASCII(PATH()))
    }
  })
  
  output$ascii.status <- renderUI({
    
    if(is.null(ASCII())){
      tags$b(tags$em(""))
    }else{
      if(names(ASCII())[1] %in% "STATUS"){
        tags$b(tags$em("Selected File is NOT an ASCII Layout File"))
      }else{
        tags$b(tags$em("ASCII FILE LOADED"))
      }
      
    }
    
  })
  
  
  output$record.type <- renderUI({
    if(is.null(ASCII())){
      return(NULL)
    }else if(names(ASCII())[1] %in% "STATUS"){
      return(NULL)
    }else{
      selectizeInput("recordtype",
                     "Record Type",
                     selected = "Select",
                     choices = c("Select", ASCII()$`Record Type`),
                     multiple = F
      )
      
    }
  })
  
  output$ascii.table <- DT::renderDataTable({
    
    if(is.null(ASCII())){
      return(NULL)
    }else if(is.null(PATH())){
      return(NULL)
    }else if(is.null(input$recordtype)){
      return(NULL)
    }else if(input$recordtype %in% "Select"){
      return(NULL)
    }else if(names(ASCII())[1] %in% "STATUS"){
      return(NULL)
    }else{
      
      table <- ASCII() %>% 
        filter(`Record Type` %in% input$recordtype )%>% 
        filter(`Variable Name` != "rectype") %>%
        select(c(2:6))
      
      table$`Minimum value` <- as.integer(table$`Minimum value`)
      table$`Maximum value` <- as.integer(table$`Maximum value`)
      
      datatable(table,rownames = F)
    }
  })
  
  
  ##=============================================================================
  ##
  ##                                  TAB 2 (DATA QC)
  ##
  ##=============================================================================
  
  directory <- reactive({
    if(is.null(input$path)) return(NULL)
    if("" %in% input$path) return(NULL)
    if(length(input$path) == 0)return(NULL)
    setwd(input$path)
    files <- c(list.files(pattern = "*.txt"), list.files(pattern = "*.csv"))
    setwd(WD)
    return(files)
  })
  
  output$fileSelection <- renderUI({
    if(is.null(directory)) return(NULL)
    selectizeInput("FileSelection",
                   "Select File(s)",
                   choices = c("All",sort(directory())),
                   multiple = T,
                   selected = "",
                   width = '80%'
                   
    )
  })
  
  
  FileSelectionGO <- eventReactive(input$fileSelectionGO,{
    if(is.null(input$fileSelectionGO)) return(NULL)
    if(input$fileSelectionGO <= 0) return(NULL)
    if(is.null(input$FileSelection)) return(NULL)
    if("" %in% input$FileSelection) return(NULL)
    
    
    setwd(input$path)
    
    
    
    if(length(input$FileSelection) ==1){
      
      if("All" %in% input$FileSelection){
        selection <-  c(list.files(pattern = "*.txt"), list.files(pattern = "*.csv"))
        masterFiles <- which(grepl("master", selection, ignore.case = T))
        if(length(masterFiles)!=0){
          selection <- selection[-c(which(grepl("master", selection, ignore.case = T)))]
        }; rm(masterFiles)
        
        DataList <- parLapply(makeCluster(detectCores()),selection, getDATA)
        check <- foreach(i=1:length(DataList),.combine = c) %dopar%{
          dim(DataList[[i]])[2]
        }
        
        if(length(unique(check)) != 1){
          rm(check); rm(DataList); rm(selection)
          return(NULL)
        }else{
          DT <- rbindlist(DataList); rm(DataList); rm(check); rm(selection)
        }
        
      }else{
        DT <- getDATA(input$FileSelection)
      }
      
      
    }else{
      
      if("All" %in% input$FileSelection){
        selection <-  c(list.files(pattern = "*.txt"), list.files(pattern = "*.csv"))
        masterFiles <- which(grepl("master", selection, ignore.case = T))
        if(length(masterFiles)!=0){
          selection <- selection[-c(which(grepl("master", selection, ignore.case = T)))]
        }; rm(masterFiles)
        DataList <- parLapply(makeCluster(detectCores()),selection,getDATA)
        check <- foreach(i=1:length(DataList),
                         .combine = c,
                         .packages = c('base','parallel','doParallel',"snow","doSNOW","data.table")) %dopar%{
                           dim(DataList[[i]])[2]
                         }
        
        if(length(unique(check)) != 1){
          rm(check); rm(DataList); rm(selection)
          return(NULL)
        }else{
          DT <- rbindlist(DataList); rm(DataList); rm(check); rm(selection)
        }
        
      }else{
        
        DataList <- parLapply(makeCluster(detectCores()),input$FileSelection,getDATA)
        check <- foreach(i=1:length(DataList),
                         .combine = c,
                         .packages = c('base','parallel','doParallel',"snow","doSNOW","data.table")) %dopar%{
                           dim(DataList[[i]])[2]
                         }
        
        if(length(unique(check)) != 1){
          rm(check); rm(DataList)
          return(NULL)
        }else{
          DT <- rbindlist(DataList); rm(DataList); rm(check)
        }
      }
    }
    
    setwd(WD)
    return(data.table(DT))
    
  })
  
  dataset <- reactive({
    if(is.null(FileSelectionGO())) return(NULL)
    return(data.table(FileSelectionGO()))
  })
  
  
  output$recordtype.QC <- renderUI({
    if(is.null(PATH())){
      return(NULL)
    }else if(is.null(ASCII())){
      return(NULL)
    }else if(names(ASCII())[1] %in% "STATUS"){
      return(NULL)
    }else{
      selectizeInput("recordtype.qc",
                     "Select Record Type",
                     selected = "",
                     choices = c("", ASCII()$`Record Type`),
                     multiple = F
      )
    }
  })
  
  output$Apply.ASCII <- renderUI({
    
    if(is.null(PATH())){
      return(NULL)
    }else if(is.null(ASCII())){
      return(NULL)
    }else if(names(ASCII())[1] %in% "STATUS"){
      return(NULL)
    }else if(is.null(input$recordtype.qc)){
      return(NULL)
    }else if("" %in% input$recordtype.qc){
      return(NULL)
    }else{
      
      radioButtons("Apply.Ascii", 
                   label = "Apply ASCII Specs",
                   choices = c("No","Yes"),
                   selected = "No",
                   inline = T
      )
    }
    
    
  })
  
  ApplyASCII <- reactive({
    
    if(is.null(input$Apply.Ascii)) return(NULL)
    if(input$Apply.Ascii %in% "No") return(NULL)
    
    table <- ASCII() %>% 
      filter(`Record Type` %in% input$recordtype.qc )%>% 
      filter(`Variable Name` != "rectype") %>%
      select(c(2:6))
    
    
    
    all.variables <- table$`Variable Name`
    all.descriptions <- table$`Description`
    table$`Minimum value` <- as.integer(table$`Minimum value`)
    table$`Maximum value` <- as.integer(table$`Maximum value`)
    
    searchTerms <- c("lev|lvl|pass|gender|ucrxgen|grade|flag|condition|_complete|procomp|status|subject|test|title_iii|proficiency")
    Rows <- which(grepl(searchTerms, table$`Variable Name`, ignore.case = T))
    
    if(length(Rows) != 0){
      
      table <- table[Rows,] %>% select(which(names(table) %in% c("Variable Name","Description","Nominal value definitions")))
      table <- data.table(table)
      setnames(table, names(table), c("variable","description","values"))
      table <- data.table(table %>% filter(!is.na(values)))
      
      if(dim(table)[1] !=0){
        
        Values <- table$values
        foreach(i =1: length(Values), .packages = c('stringi','tidyr','foreach'), .combine = rbind) %dopar%{
          
          
          
          result <- gsub(";",",", Values[i], ignore.case = T)
          result <- gsub("<", "", result, ignore.case = T)
          result <- gsub(">", "", result, ignore.case = T)
          result <- gsub("\\.", ",", result, ignore.case = T)
          result <- gsub("blank","", result, ignore.case = T)
          result <- gsub(",$", "", result, ignore.case = T)
          result <- gsub("^,", "", result, ignore.case = T)
          
          result <- stri_trim(result)
          
          result <- unlist(stri_split(result, fixed = ","))
          dt <- data.frame("code" = result)
          dt <- separate(dt, code, c("code","description"), sep ="=")
          
          foreach(j = 1:length(dt$code)) %do%{
            if(unlist(stri_split(dt$code[j], fixed =" "))[1] %in% "Note"){
              dt$code[j] <- unlist(stri_split(dt$code[j], fixed = " "))[3]
            }
          };rm(j)
          
          dt$variable <- table$variable[i]
          
          return(dt)
        } -> dt  ; dt <- data.table("variable" =dt$variable, "value" = dt$code, "meaning" = dt$description)
        
        
        GET <- list("all.variables" = all.variables,
                    "all.descriptions" = all.descriptions,
                    "map" = dt, "case" = 1)
        
      }else{
        
        GET <- list("all.variables" = all.variables,
                    "all.descriptions" = all.descriptions,
                    "case"= 2)
        
      }
      
      
    }else{
      GET <- list("all.variables" = all.variables,
                  "all.descriptions" = all.descriptions,
                  "case"= 2)
    }
    
    
    
    
    
  })
  
  
  
  ApplyAscii <- reactive({
    
    if(is.null(ApplyASCII())) return(NULL)
    
    return(ApplyASCII())
    
  }) 
  
  
  
  output$apply.ascii.message <- renderTable({
    
    if(is.null(dataset())) return(NULL)
    if(is.null(ApplyAscii())) return(NULL)
    
    if(all(tolower(stri_trim(names(dataset()))) %in% tolower(stri_trim(ApplyAscii()$all.variables))) != T){
      
      tableMismatches <- tolower(stri_trim(names(dataset()))) %in% tolower(stri_trim(ApplyAscii()$all.variables))
      tableMismatches <- names(dataset())[which(tableMismatches %in% FALSE)]
      
      asciiMismatches <- tolower(stri_trim(ApplyAscii()$all.variables)) %in% tolower(stri_trim(names(dataset())))
      asciiMismatches <- ApplyAscii()$all.variables[which(asciiMismatches %in% FALSE)]
      
      table <- data.table("Number of Variables" = c(length(ApplyAscii()$all.variables), length(names(dataset()))),
                          "Missing Variable(s)" = c(paste0(tableMismatches,collapse = "|"), paste0(asciiMismatches,collapse = "|")))
      
      row.names(table) <- c("ASCII","SPREADSHEET")
      
      return(table)
    }else{
      
      return(NULL)
      
    }
  })
  
  
  PathtoCutpoints <- eventReactive(input$cutpointTables,{
    
    if(input$cutpointTables <= 0) return(NULL)
    filepaths <- choose.files(default = "../..",
                              caption = "Select tables",
                              multi = T
    )
    if(length(filepaths) == 0) return(NULL)
    
    return(filepaths)
  }) 
  
  CutpointFiles <- reactive({
    
    if(is.null(PathtoCutpoints())) return(NULL)
    filepaths <- PathtoCutpoints()
    foreach(i=1:length(filepaths))%dopar%{
      source('functions.R')
      return(getCUTPOINTS(filepaths[i]))
    } -> tables
    
    foreach(i=1:length(tables),.combine = c ) %dopar%{
      if(is.null(tables[[i]])) return(T)
      if(!is.data.table(tables[[i]])) return(T)
      return(FALSE)
    } -> checkTables
    
    if(sum(checkTables) != 0 ) return(NULL)
    
    filenames <- foreach(i=1:length(filepaths), .combine = c) %do%{ return(stri_split(basename(filepaths[i]), fixed = ".")[[1]][1])} 
    names(tables) <- filenames; rm(filenames); rm(filepaths)
    
    return(tables)
    
  })
  
  numberOfTables <- reactive({
    if(is.null(CutpointFiles())) return(NULL)
    if(length(CutpointFiles())== 0)  return(NULL)
    return(length(CutpointFiles()))
  })
  
  
  output$displayCutpointTables <- renderUI({
    if(is.null(numberOfTables())) return(NULL)
    
    if(numberOfTables() == 1){
      tabsetPanel(type = "tabs", 
                  tabPanel(names(CutpointFiles())[1], DT::dataTableOutput("ViewTable1"))
      )
      
    }else if(numberOfTables() ==2){
      tabsetPanel(type = "tabs", 
                  tabPanel(names(CutpointFiles())[1], DT::dataTableOutput("ViewTable1")),
                  tabPanel(names(CutpointFiles())[2], DT::dataTableOutput("ViewTable2"))
      )
    }else if(numberOfTables() ==3){
      tabsetPanel(type = "tabs", 
                  tabPanel(names(CutpointFiles())[1], DT::dataTableOutput("ViewTable1")),
                  tabPanel(names(CutpointFiles())[2], DT::dataTableOutput("ViewTable2")),
                  tabPanel(names(CutpointFiles())[3], DT::dataTableOutput("ViewTable3"))
      )
      
    }else if(numberOfTables() ==4){
      tabsetPanel(type = "tabs", 
                  tabPanel(names(CutpointFiles())[1], DT::dataTableOutput("ViewTable1")),
                  tabPanel(names(CutpointFiles())[2], DT::dataTableOutput("ViewTable2")),
                  tabPanel(names(CutpointFiles())[3], DT::dataTableOutput("ViewTable3")),
                  tabPanel(names(CutpointFiles())[4], DT::dataTableOutput("ViewTable4"))
      )
      
    }else if(numberOfTables() ==5){
      tabsetPanel(type = "tabs", 
                  tabPanel(names(CutpointFiles())[1], DT::dataTableOutput("ViewTable1")),
                  tabPanel(names(CutpointFiles())[2], DT::dataTableOutput("ViewTable2")),
                  tabPanel(names(CutpointFiles())[3], DT::dataTableOutput("ViewTable3")),
                  tabPanel(names(CutpointFiles())[4], DT::dataTableOutput("ViewTable4")),
                  tabPanel(names(CutpointFiles())[5], DT::dataTableOutput("ViewTable5"))
      )
      
    }else if(numberOfTables() ==6){
      tabsetPanel(type = "tabs", 
                  tabPanel(names(CutpointFiles())[1], DT::dataTableOutput("ViewTable1")),
                  tabPanel(names(CutpointFiles())[2], DT::dataTableOutput("ViewTable2")),
                  tabPanel(names(CutpointFiles())[3], DT::dataTableOutput("ViewTable3")),
                  tabPanel(names(CutpointFiles())[4], DT::dataTableOutput("ViewTable4")),
                  tabPanel(names(CutpointFiles())[5], DT::dataTableOutput("ViewTable5")),
                  tabPanel(names(CutpointFiles())[6], DT::dataTableOutput("ViewTable6"))
      )
      
    }else{
      return(NULL)
    }
    
  })
  
  
  output$ViewTable1 <- DT::renderDataTable({
    if(is.null(numberOfTables())) return(NULL)
    if(numberOfTables() >= 1){
      
      table <- CutpointFiles()[[1]]
      table$Subject <- gsub("\\[","",table$Subject)
      table$Subject <- gsub("\\]","",table$Subject)
      table$Subject <- gsub("\\(","",table$Subject)
      table$Subject <- gsub("\\)","",table$Subject)
      table$Subject <- as.character(table$Subject)
      
      
      
      
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      rm(selection)
      
      table[[dim(table)[2]]] <- gsub("\\[","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\]","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\(","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\)","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- as.character(table[[dim(table)[2]]])
      
      datatable(
        table,
        rownames = F,
        # selection="multiple",
        # escape=FALSE,
        extensions = c(
          'Buttons',
          # 'ColReorder'
          'Responsive'
        ),
        
        options = list(
          dom = 'Bfrtip',
          # autoWidth = T,
          # lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('csv',I('colvis')),
          # colReorder = TRUE,
          Responsive = T
        )
      )
      
    }else{
      return(NULL)
    }
  })
  
  output$ViewTable2 <- DT::renderDataTable({
    if(is.null(numberOfTables())) return(NULL)
    if(numberOfTables() >= 2){
      
      table <- CutpointFiles()[[2]]
      table$Subject <- gsub("\\[","",table$Subject)
      table$Subject <- gsub("\\]","",table$Subject)
      table$Subject <- gsub("\\(","",table$Subject)
      table$Subject <- gsub("\\)","",table$Subject)
      table$Subject <- as.character(table$Subject)
      
      
      
      
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      rm(selection)
      
      table[[dim(table)[2]]] <- gsub("\\[","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\]","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\(","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\)","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- as.character(table[[dim(table)[2]]])
      
      datatable(
        table,
        rownames = F,
        # selection="multiple",
        # escape=FALSE,
        extensions = c(
          'Buttons',
          # 'ColReorder'
          'Responsive'
        ),
        
        options = list(
          dom = 'Bfrtip',
          # autoWidth = T,
          # lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('csv',I('colvis')),
          # colReorder = TRUE,
          Responsive = T
        )
      )
      
    }else{
      return(NULL)
    }
  })
  
  output$ViewTable3 <- DT::renderDataTable({
    if(is.null(numberOfTables())) return(NULL)
    
    if(numberOfTables() >= 3){
      
      table <- CutpointFiles()[[3]]
      table$Subject <- gsub("\\[","",table$Subject)
      table$Subject <- gsub("\\]","",table$Subject)
      table$Subject <- gsub("\\(","",table$Subject)
      table$Subject <- gsub("\\)","",table$Subject)
      table$Subject <- as.character(table$Subject)
      
      
      
      
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      rm(selection)
      
      table[[dim(table)[2]]] <- gsub("\\[","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\]","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\(","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\)","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- as.character(table[[dim(table)[2]]])
      
      datatable(
        table,
        rownames = F,
        # selection="multiple",
        # escape=FALSE,
        extensions = c(
          'Buttons',
          # 'ColReorder'
          'Responsive'
        ),
        
        options = list(
          dom = 'Bfrtip',
          # autoWidth = T,
          # lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('csv',I('colvis')),
          # colReorder = TRUE,
          Responsive = T
        )
      )
      
    }else{
      return(NULL)
    }
  })
  
  output$ViewTable4 <- DT::renderDataTable({
    if(is.null(numberOfTables())) return(NULL)
    if(numberOfTables() >= 4){
      
      table <- CutpointFiles()[[4]]
      table$Subject <- gsub("\\[","",table$Subject)
      table$Subject <- gsub("\\]","",table$Subject)
      table$Subject <- gsub("\\(","",table$Subject)
      table$Subject <- gsub("\\)","",table$Subject)
      table$Subject <- as.character(table$Subject)
      
      
      
      
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      rm(selection)
      
      table[[dim(table)[2]]] <- gsub("\\[","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\]","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\(","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\)","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- as.character(table[[dim(table)[2]]])
      
      datatable(
        table,
        rownames = F,
        # selection="multiple",
        # escape=FALSE,
        extensions = c(
          'Buttons',
          # 'ColReorder'
          'Responsive'
        ),
        
        options = list(
          dom = 'Bfrtip',
          # autoWidth = T,
          # lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('csv',I('colvis')),
          # colReorder = TRUE,
          Responsive = T
        )
      )
      
    }else{
      return(NULL)
    }
    
  })
  
  output$ViewTable5 <- DT::renderDataTable({
    if(is.null(numberOfTables())) return(NULL)
    if(numberOfTables() >= 5){
      
      table <- CutpointFiles()[[5]]
      table$Subject <- gsub("\\[","",table$Subject)
      table$Subject <- gsub("\\]","",table$Subject)
      table$Subject <- gsub("\\(","",table$Subject)
      table$Subject <- gsub("\\)","",table$Subject)
      table$Subject <- as.character(table$Subject)
      
      
      
      
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      rm(selection)
      
      table[[dim(table)[2]]] <- gsub("\\[","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\]","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\(","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\)","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- as.character(table[[dim(table)[2]]])
      
      datatable(
        table,
        rownames = F,
        # selection="multiple",
        # escape=FALSE,
        extensions = c(
          'Buttons',
          # 'ColReorder'
          'Responsive'
        ),
        
        options = list(
          dom = 'Bfrtip',
          # autoWidth = T,
          # lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('csv',I('colvis')),
          # colReorder = TRUE,
          Responsive = T
        )
      )
      
    }else{
      return(NULL)
    }
  })
  
  output$ViewTable6 <- DT::renderDataTable({
    if(is.null(numberOfTables())) return(NULL)
    if(numberOfTables() >= 6){
      
      table <- CutpointFiles()[[6]]
      table$Subject <- gsub("\\[","",table$Subject)
      table$Subject <- gsub("\\]","",table$Subject)
      table$Subject <- gsub("\\(","",table$Subject)
      table$Subject <- gsub("\\)","",table$Subject)
      table$Subject <- as.character(table$Subject)
      
      
      
      
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      rm(selection)
      
      table[[dim(table)[2]]] <- gsub("\\[","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\]","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\(","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- gsub("\\)","",table[[dim(table)[2]]])
      table[[dim(table)[2]]] <- as.character(table[[dim(table)[2]]])
      
      datatable(
        table,
        rownames = F,
        # selection="multiple",
        # escape=FALSE,
        extensions = c(
          'Buttons',
          # 'ColReorder'
          'Responsive'
        ),
        
        options = list(
          dom = 'Bfrtip',
          # autoWidth = T,
          # lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('csv',I('colvis')),
          # colReorder = TRUE,
          Responsive = T
        )
      )
      
    }else{
      return(NULL)
    }
  })
  
  
  output$table <- DT::renderDataTable({
    if(is.null(dataset())) return(NULL)
    
    
    
    
    if(is.null(ApplyAscii())){
      
      table <- dataset()
      
      if (names(table)[1] %in% "V1"){
        table[,V1:= NULL]
      }
      
      colNames <- names(table)
      foreach(i=1:length(colNames)) %do%{
        colNames[i] <- stri_trim(colNames[i])
      }
      setnames(table, names(table), colNames); rm(colNames)
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      
      
    }else if(ApplyAscii()$case == 2){
      table <- dataset()
      
      if (names(table)[1] %in% "V1"){
        table[,V1:= NULL]
      }
      
      colNames <- names(table)
      foreach(i=1:length(colNames)) %do%{
        colNames[i] <- stri_trim(colNames[i])
      }
      setnames(table, names(table), colNames); rm(colNames)
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      if(length(names(table) !=0)){
        setnames(
          table,
          names(table)[which(stri_trim(tolower(names(table)))%in% stri_trim(tolower(ApplyAscii()$all.variables)))],
          ApplyAscii()$all.descriptions[which(stri_trim(tolower(ApplyAscii()$all.variables)) %in% stri_trim(tolower(names(table))))]
        )
      }
      
    }else{
      
      
      table <- dataset()
      if(names(table)[1] %in% "V1"){
        table[,V1:= NULL]
      }
      
      colNames <- names(table)
      foreach(i=1:length(colNames)) %do%{
        colNames[i] <- stri_trim(colNames[i])
      }
      setnames(table, names(table), colNames); rm(colNames)
      
      Map <- ApplyAscii()$map
      
      foreach(i=1:length(Map$variable)) %do%{
        Map$variable <- as.character(Map$variable)
        Map$variable[i] <- stri_trim(Map$variable[i])
      }
      foreach(i=1:length(Map$value)) %do%{
        Map$value <- as.character(Map$value)
        Map$value[i] <- stri_trim(Map$value[i])
      }
      foreach(i=1:length(Map$meaning)) %do%{
        Map$meaning<- as.character(Map$meaning)
        Map$meaning[i] <- stri_trim(Map$meaning[i])
      }
      
      mappedVariables <- unique(Map$variable)
      
      foreach(i = 1:length(mappedVariables)) %do% {
        map <- Map %>% filter(variable %in% mappedVariables[i])
        
        if(mappedVariables[i] %in% names(table)){
          Col <- table[[which(names(table) %in% mappedVariables[i])]]
          foreach(j= 1:length(map$value)) %do%{
            Col[which(tolower(stri_trim(as.character(Col))) %in% tolower(stri_trim(as.character(map$value[j]))))] <- map$meaning[j]
          }
          
          table[[which(names(table) %in% mappedVariables[i])]] <- Col
        }
      }
      
      table <- data.table(table)
      
      foreach(i =1:dim(table)[2], .combine = c) %dopar%{
        if(all(is.na(table[[i]])) == TRUE){
          return(i)
        }
      } -> selection
      
      if(length(selection) != 0){
        table <- data.table(table %>% select(-c(selection)))
      }
      
      
      if(length(names(table) !=0)){
        setnames(
          table,
          names(table)[which(stri_trim(tolower(names(table)))%in% stri_trim(tolower(ApplyAscii()$all.variables)))],
          ApplyAscii()$all.descriptions[which(stri_trim(tolower(ApplyAscii()$all.variables)) %in% stri_trim(tolower(names(table))))]
        )
      }
    }
    
    
    
    datatable(
      table,
      filter = 'top',
      rownames = F,
      selection="multiple", 
      escape=FALSE,
      extensions = c(
        'Buttons',
        'ColReorder',
        'Responsive'
      ),
      
      options = list(
        dom = 'Bfrtip',
        autoWidth = T,
        lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
        buttons = list('excel' ,I('colvis')),
        colReorder = TRUE,
        Responsive = T
      )
    )
    
  })
  
  
  ##=============================================================================
  ##
  ##                                  TAB 3 (COMPARING SPREADSHEETS)
  ##
  ##=============================================================================
  
  
  
  Path1 <- eventReactive(input$file1.path,{
    
    if(input$file1.path <= 0) return(NULL)
    filepath <- choose.files(default = "C:/Users",
                             caption = "Select Spreadsheet", multi = T
    )
    if(length(filepath) == 0) return(NULL)
    
    return(filepath)
  }) 
  
  Path2 <- eventReactive(input$file2.path,{
    
    if(input$file2.path <= 0) return(NULL)
    filepath <- choose.files(default = "C:/Users",
                             caption = "Select Spreadsheet", multi = T
    )
    if(length(filepath) == 0) return(NULL)
    return(filepath)
  }) 
  
  File1 <- reactive({
    
    if(is.null(Path1())) return(NULL)
    Get <- getCompareFiles(Path1())
    if(Get$case == 1) return(NULL)
    return(list("data"=Get$data, "keyOptions" = Get$keyOptions))
  })
  
  File2 <- reactive({
    
    if(is.null(Path2())) return(NULL)
    Get <- getCompareFiles(Path2())
    if(Get$case == 1) return(NULL)
    return(list("data"=Get$data, "keyOptions" = Get$keyOptions))
  })
  
  
  output$file1.status <- renderUI({
    if(is.null(File1())){
      return(tags$b(tags$em("NO SELECTION OR FILE CAN'T BE LOADED")))
    }else{
      return(tags$b(tags$em("LOADED")))
    }
  })
  
  output$file2.status <- renderUI({
    if(is.null(File2())){
      return(tags$b(tags$em("NO SELECTION OR FILE CAN'T BE LOADED")))
    }else{
      return(tags$b(tags$em("LOADED")))
    }
  })
  
  output$getKey1 <- renderUI({
    if(is.null(File1())) return(NULL)
    selectizeInput("GetKey1","Select Key",
                   multiple = F,
                   choices = c("Select",sort(File1()$keyOptions)),
                   selected = "Select"
    )
  })
  
  output$getKey2 <- renderUI({
    if(is.null(File2())) return(NULL)
    selectizeInput("GetKey2","Select Key",
                   multiple = F,
                   choices = c("Select",sort(File2()$keyOptions)),
                   selected = "Select"
    )
  })
  
  GETKEY1 <- reactive({
    if(is.null(input$GetKey1)) return(NULL)
    if("Select" %in% input$GetKey1) return(NULL)
    return(input$GetKey1)
  })
  
  GETKEY2 <- reactive({
    if(is.null(input$GetKey2)) return(NULL)
    if("Select" %in% input$GetKey2) return(NULL)
    return(input$GetKey2)
  })
  
  File.1 <- reactive({ 
    if(is.null(GETKEY1())) return(NULL)
    
    table <- data.table(File1()$data)
    names(table)[which(names(table) %in% GETKEY1())] <- "KEY"
    setkey(table,KEY)
    return(table)
    
    
  })
  
  File.2 <- reactive({
    if(is.null(GETKEY2())) return(NULL) 
    table <- data.table(File2()$data)
    names(table)[which(names(table) %in% GETKEY2())] <- "KEY"
    setkey(table,KEY)
    return(table)
    
  })
  
  
  
  
  output$relevantcols1 <- renderUI({
    if(is.null(File.1())) return(NULL)
    Options <- names(File.1())[which(names(File.1()) != "KEY")]
    selectizeInput("relevantCols1", 
                   "Relevant Columns from Spreadsheet #1",
                   multi= T,
                   selected = "",
                   choices = c("All", Options)
    )
  })
  
  
  output$relevantcols2 <- renderUI({
    if(is.null(File.2())) return(NULL)
    
    Options <- names(File.2())[which(names(File.2()) != "KEY")]
    selectizeInput("relevantCols2", 
                   "Relevant Columns from Spreadsheet #2",
                   multi= T,
                   selected = "",
                   choices = c("All", Options)
    )
    
  })
  
  
  RelevantCols1 <- reactive({
    if(is.null(input$relevantCols1)) return(NULL)
    if(sum(input$relevantCols1 %in% "") != 0) return(NULL)
    return(input$relevantCols1)
    
  })
  
  RelevantCols2 <- reactive({
    if(is.null(input$relevantCols2)) return(NULL)
    if(sum(input$relevantCols2 %in% "") != 0)return(NULL)
    return(input$relevantCols2)
    
  })
  
  
  output$whichFilterColumns1 <- renderUI({
    if(is.null(File.1())) return(NULL)
    Options <- names(File.1())[which(names(File.1()) != "KEY")]
    selectizeInput("whichFilterCols1", 
                   "Filter By:",
                   multi= T,
                   selected = "None",
                   choices = c("None",Options)
    )
  })
  
  output$whichFilterColumns2 <- renderUI({
    if(is.null(File.2())) return(NULL)
    Options <- names(File.2())[which(names(File.2()) != "KEY")]
    selectizeInput("whichFilterCols2", 
                   "Filter By:",
                   multi= T,
                   selected = "None",
                   choices = c("None",Options)
    )
  })
  
  output$whichFilterValues1 <- renderUI({
    if(is.null(input$whichFilterCols1)) return(NULL)
    if("None" %in% input$whichFilterCols1){
      return(NULL)
    }else{
      foreach(i=1:length(input$whichFilterCols1),.combine = c) %do%{
        get <- which(names(File.1()) %in% input$whichFilterCols1[i])
        get <- sort(str_trim(as.character(unique(File.1()[[get]]))))
        get <- paste0(i,"::", get[1:length(get)])
        return(get)
      } -> Options; rm(get)
    }
    
    
    selectizeInput("whichFilterVals1", 
                   "",
                   multi= T,
                   selected = "None",
                   choices = c("None",Options)
    )
    
  })
  
  output$whichFilterValues2 <- renderUI({
    if(is.null(input$whichFilterCols2)) return(NULL)
    
    if("None" %in% input$whichFilterCols2){
      return(NULL)
    }else{
      foreach(i=1:length(input$whichFilterCols2),.combine = c) %do%{
        get <- which(names(File.2()) %in% input$whichFilterCols2[i])
        get <- sort(str_trim(as.character(unique(File.2()[[get]]))))
        get <- paste0(i,"::", get[1:length(get)])
        return(get)
      } -> Options
    }
    
    selectizeInput("whichFilterVals2", 
                   "",
                   multi= T,
                   selected = "None",
                   choices = c("None",Options)
    )
    
  })
  
  output$apply.filters <- renderUI({
    if(is.null(File.1())) return(NULL)
    if(is.null(File.2())) return(NULL)
    
    radioButtons("applyFilters", 
                 label = "Apply Filter(s)",
                 choices = c("No","Yes"),
                 selected = "No",
                 inline = T
                 )
  })
  
  
  ApplyFilters <- reactive({
    if(is.null(input$applyFilters)) return(NULL)
    if("No" %in% input$applyFilters) return(NULL)
    
    condition1 <- is.null(input$whichFilterCols1) | is.null(input$whichFilterVals1) | "None" %in% input$whichFilterVals1 | length(input$whichFilterVals1) == 0
    condition2 <- is.null(input$whichFilterCols2) | is.null(input$whichFilterVals2) | "None" %in% input$whichFilterVals2 | length(input$whichFilterVals2) == 0
    flag <- condition1 & condition2
    if(flag %in% TRUE) return(NULL)
    
    
    if(condition1 %in% F & condition2 %in% T ) return(1)
    if(condition2 %in% F & condition1 %in% T ) return(2)
    if(condition1 %in% F & condition2 %in% F ) return(3)
  })
  
  
  FILE1 <- reactive({
    if(is.null(File.1())) return(NULL)
    if(!is.null(ApplyFilters())){
      if(ApplyFilters() %in% c(1,3)){
        table <- FilterBy(File.1(), input$whichFilterCols1,input$whichFilterVals1)
        if(dim(table)[1] == 0) return(NULL)
        setkey(table,KEY)
        return(table)
      }else{
        return(File.1())
      }
      
    }else{
      return(File.1())
    }
  })
  
  
  FILE2 <- reactive({
    if(is.null(File.2())) return(NULL)
    if(!is.null(ApplyFilters())){
      if(ApplyFilters() %in% c(2,3)){
        table <- FilterBy(File.2(), input$whichFilterCols2,input$whichFilterVals2) 
        if(dim(table)[1] == 0) return(NULL)
        setkey(table,KEY)
        return(table)
      }else{
        return(File.2())
      }
      
    }else{
      return(File.2())
    }
  })
  
  
  
  compare <- reactive({
    return(compareFiles(FILE1(),RelevantCols1(),FILE2(),RelevantCols2()))
  })
  
  compareGO <- eventReactive(input$compareButton, {
    if(input$compareButton <= 0) return(NULL)
    if(is.null(FILE1())) return(NULL)
    if(is.null(FILE2())) return(NULL)
    if(is.null(RelevantCols1())) return(NULL)
    if(is.null(RelevantCols2())) return(NULL)
    return(compare())
    
  })
  
  
  output$displayComparisonReport<- renderUI({
    if(is.null(compareGO)) return(NULL)
    
    if(compareGO()$case != 1 & compareGO()$case != 2 ){
      tabsetPanel(type = "tabs", 
                  tabPanel("Summary", uiOutput("reportText"))
                  )
      
    }else{
      
      if(compareGO()$case == 1){
        tabsetPanel(type = "tabs", 
                    tabPanel("Summary", uiOutput("reportText")), 
                    tabPanel("Records with Perfect Match", DT::dataTableOutput("perfect_match")),
                    tabPanel("Records in File#1 but not in File#2", DT::dataTableOutput("in_1_not_2")),
                    tabPanel("Records in File#2 but not in File#1", DT::dataTableOutput("in_2_not_1"))
                    )
        
      }else{
        
        tabsetPanel(type = "tabs", 
                    tabPanel("Summary", uiOutput("reportText")),
                    tabPanel("Records with Perfect Match", DT::dataTableOutput("perfect_match")),
                    tabPanel("Records with Mismatches", DT::dataTableOutput("mismatch_records")),
                    tabPanel("Records in File#1 but not in File#2", DT::dataTableOutput("in_1_not_2")),
                    tabPanel("Records in File#2 but not in File#1",DT::dataTableOutput("in_2_not_1"))
                    )
        
      }
      
      
    }
  
    
  })
  
  
  output$reportText <- renderUI({
    
    if(compareGO()$case == 0){
      return(tags$em(tags$b("No Common Key Values Between These Files")))
    }else if(compareGO()$case == 1){
      return( HTML(paste0(
        tags$em(tags$b(paste0("Number of Records with Perfect Match: ", dim(compareGO()$Match)[1]))),
        tags$br(),
        tags$em(tags$b(paste0("Number of Records in File#1 but not in File#2: ", dim(compareGO()$`IN-1-NOT-2`)[1]))),
        tags$br(),
        tags$em(tags$b(paste0("Number of Records in File#2 but not in File#1: ", dim(compareGO()$`IN-2-NOT-1`)[1]))))
      ))
      
      
    }else if(compareGO()$case == 2){
      return(HTML(paste0(
        tags$em(tags$b(paste0("Number of Records with Perfect Match: ", dim(compareGO()$Match)[1]))),
        tags$br(),
        tags$em(tags$b(paste0("Number of Records with Mismatch: ", dim(compareGO()$Mismatch)[1]))),
        tags$br(),
        tags$em(tags$b(paste0("Number of Records in File#1 but not in File#2: ", dim(compareGO()$`IN-1-NOT-2`)[1]))),
        tags$br(),
        tags$em(tags$b(paste0("Number of Records in File#2 but not in File#1: ", dim(compareGO()$`IN-2-NOT-1`)[1]))))
      ))
      
    }else if(compareGO()$case == 3){
      return(tags$em(tags$b("Something's wrong; I can't determine mismatching elements. FIX ME!")))
    }else if(compareGO()$case == 4){
      return(tags$em(tags$b("Relevant Columns don't match")))
    }else{
      return(tags$em(tags$b("FIX ME!")))
    }
    
    
  })
  
  
  
  output$perfect_match <- DT::renderDataTable({
    
    if(compareGO()$case == 1 | compareGO()$case == 2){
      datatable(
        data.table(compareGO()$Match),
        filter = 'top',
        rownames = F,
        selection="multiple",
        escape=FALSE,
        extensions = c(
          'Buttons',
          'ColReorder',
          'Responsive'
        ),

        options = list(
          dom = 'Bfrtip',
          autoWidth = T,
          lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('excel' ,I('colvis')),
          colReorder = TRUE,
          Responsive = T
        )
      )
    }else{
      
      return(NULL)
    }
  
  })
  
  
  output$mismatch_records <- DT::renderDataTable({
    
    if(compareGO()$case == 2){
      
      
      datatable(
        data.table(compareGO()$Mismatch),
        filter = 'top',
        rownames = F,
        selection="multiple",
        escape=FALSE,
        extensions = c(
          'Buttons',
          'ColReorder',
          'Responsive'
        ),

        options = list(
          dom = 'Bfrtip',
          autoWidth = T,
          lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('excel' ,I('colvis')),
          colReorder = TRUE,
          Responsive = T
        )
      )
    }else{
      return(NULL)
    }
  })
  
  
  output$in_1_not_2 <- DT::renderDataTable({
    
    if(compareGO()$case == 1 |compareGO()$case == 2){
      datatable(
        data.table(compareGO()$`IN-1-NOT-2`),
        filter = 'top',
        rownames = F,
        selection="multiple",
        escape=FALSE,
        extensions = c(
          'Buttons',
          'ColReorder',
          'Responsive'
        ),

        options = list(
          dom = 'Bfrtip',
          autoWidth = T,
          lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('excel' ,I('colvis')),
          colReorder = TRUE,
          Responsive = T
        )
      )
    }else{
      return(NULL)
    }
  })
  
  
  output$in_2_not_1 <-  DT::renderDataTable({
    
    if(compareGO()$case == 1 | compareGO()$case == 2){
     
      datatable(
        data.table(compareGO()$`IN-2-NOT-1`),
        filter = 'top',
        rownames = F,
        selection="multiple",
        escape=FALSE,
        extensions = c(
          'Buttons',
          'ColReorder',
          'Responsive'
        ),

        options = list(
          dom = 'Bfrtip',
          autoWidth = T,
          lengthMednu = list(c(5,10,25,50,100,-1), c("5","10","25","50","100","All")),
          buttons = list('excel' ,I('colvis')),
          colReorder = TRUE,
          Responsive = T
        )
      )
    }else{
      return(NULL)
    }
  })
  
  
  
})
