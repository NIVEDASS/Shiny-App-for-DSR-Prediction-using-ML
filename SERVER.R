shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models
  
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    m <- readRDS(file = rdsfile)  
    models[[name]] <- m
    
    # try to update the preprocessing steps with the ones that were used
    inpId <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), "Preprocess")
    steps <- m$recipe$steps
    seld <- c()
    for (step in steps) {
      s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
      seld <- c(seld, s)
    }
    if (length(seld) > 0 && seld[1] == "date") { 
      seld <- seld[2:length(seld)] #discard initial date step 
      seld <- seld[seld != "rm"] #discard rm step
    }
    updateSelectizeInput(session = session, inputId = inpId, choices = ppchoices, selected = seld)
    if (length(seld) > 0) {
      showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 3)
    }
  }
  
  # reactive getData ----
  getData <- reactive({
    # d <- read_excel("CHA-wx.xlsx", sheet = "CHA-old")
    # d<-data.frame(d)
    # myvars <- c("Temp", "RH", "WSpd", "Rain", "DSR")
    # d <- d[myvars]
    input$goButton
    isolate({
      
      suppressWarnings({
        #All the stations dataset is under the folder named 'weather stations'. Once given this main path, it automatically fetches all the dataset inside this root folder.
        half_path = file.path("C:/Users/NIVEDITHA/Desktop/Fire Season Severity prediction using Machine Learning/dataset/Weather_stations", input$station_code)
        last_path = str_c(c(input$station_code, "wx.xlsx"), collapse = "-")
        full_path = file.path(half_path, last_path)
        print(full_path)
        sheet_name = str_c(c(input$station_code, "old"), collapse = "-")
        #print(sheet_name)
        dat <- read_excel(full_path, sheet = sheet_name)
        dat<-data.frame(dat)
        #dat$Date<- as.Date(dat$Date)
        #Chosen only first 900 observations for clear understanding of charts, whole dataset can be used.
        
        myvars <- c("Temp", "RH", "WSpd", "Rain","DSR")
        
        d <- dat[myvars]
    #18993
        d <- as.data.frame(d)[1:1000,]
        d <- rename(d,c("Y" = "DSR"))
        d
        
      })
    
  })
    #d <- rename(d, replace = c("DSR" = "Y"))
    })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    dfSummary(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # #output Available ----
  # output$Available <- DT::renderDataTable({
  #    m <- getMethods()
  #    m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
  #    DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE)
  # })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode="list", length = n+1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 15, min = 1000, max = 5000)))
    }
    seeds[[n+1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    # trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "random",
    #              index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, trim = TRUE)
    trainControl(method = "repeatedcv", number = 10, repeats = 3,
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, trim = TRUE)
    
    })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    recipe <- recipes::recipe(Y ~ ., data = getTrainData())
  })
  
  # observeEvent NullGo ----
  observeEvent(
    input$NullGo,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent NullGo ----
  output$NullMetrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output NullRecipe
  output$NullRecipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------

  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>%  # add a numeric date variables
      steps(input$GlmnetPreprocess) #%>% 
      #step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent GlmnetGo ----
  observeEvent(
    input$GlmnetGo,
    {
      library(glmnet)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output GlmnetModelSummary (text) ----
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })
  
  # output GlmnetMetrics (table) ----
  output$GlmnetMetrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output GlmnetModelPlots (plot) ----
  output$GlmnetModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })
  
  # output GlmnetRecipe (print) ----
  output$GlmnetRecipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output GlmnetModelSummary2 (print) ----
  output$GlmnetModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
  # # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------

  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$PlsPreprocess) #%>%
      #step_rm(has_type("date"))   # remove original date variables
  })

  # observeEvent PlsGo ----
  observeEvent(
    input$PlsGo,
    {
      library(pls)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output PlsModelSummary0 (text) ----
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  # output PlsMetrics (table) ----
  output$PlsMetrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })

  # output PlsModelPlots (plot) ----
  output$PlsModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })

  # output PlsRecipe (print) ----
  output$PlsRecipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })

  # output PlsModelSummary2 (print) ----
  output$PlsModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })


  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------

  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>%
      steps(input$RpartPreprocess) #%>%
      #step_rm(has_type("date"))
  })
  
  # observeEvent RpartGo ----
  observeEvent(
    input$RpartGo,
    {
      library(rpart)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output RpartModelSummary0 (print) ----
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  # output RpartMetrics (table) ----
  output$RpartMetrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output RpartRecipe (print) ----
  output$RpartRecipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output RpartModelPlots (plot) ----
  output$RpartModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output RpartModelTree (plot) ----
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  
  #####################################OLS
  
  # # METHOD * ols ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # # reactive getolsRecipe ----
  # getolsRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>%
  #     steps(input$olsPreprocess) #%>%
  #   #step_rm(has_type("date"))
  # })
  # 
  # ## observeEvent olsGo ----
  # observeEvent(
  #   input$olsGo,
  #   {
  #     library(olsrr)
  #     #method <- "lm"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       train.control <- trainControl(method = "cv", number = 10)
  #       # Train the model
  #       step.model1 <- train(Y ~., data = getTrainData(),
  #                            method = "leapBackward", 
  #                            tuneGrid = data.frame(nvmax = 1:4),
  #                            trControl = train.control
  #       )
  #       
  #       # leapForward , leapSeq
  #       step.model1$results
  #       step.model1$bestTune
  #       summary(step.model1$finalModel)
  #       
  #       saveToRds(step.model1, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # # output RpartModelSummary0 (print) ----
  # output$olsModelSummary0 <- renderText({
  #   description("ols")
  # })
  # 
  # # output RpartMetrics (table) ----
  # output$olsMetrics <- renderTable({
  #   req(models$ols)
  #   models$ols$results[ which.min(models$ols$results[, "RMSE"]), ]
  # })
  # 
  # # output RpartRecipe (print) ----
  # output$olsRecipe <- renderPrint({
  #   req(models$ols)
  #   models$ols$recipe
  # })
  # 
  # # output RpartModelPlots (plot) ----
  # output$olsModelPlots <- renderPlot({
  #   req(models$ols)
  #   plot(models$ols)
  # })
  # 
  # # output RpartModelTree (plot) ----
  # output$olsModelTree <- renderPlot({
  #   #library(ols.plot)
  #   req(models$ols)
  #   rpart.plot::ols.plot(models$ols$finalModel, roundint = FALSE)
  # })
  # 


  # # METHOD * brnn ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getBrnnRecipe ----
  # getBrnnRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$BrnnPreprocess) # %>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent BrnnGo ----
  # observeEvent(
  #   input$BrnnGo,
  #   {
  #     library(brnn)
  #     method <- "brnn"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getBrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output BrnnModelSummary0 (text) ----
  # output$PlsModelSummary0 <- renderText({
  #   description("brnn")
  # })
  # 
  # # output BrnnMetrics (table) ----
  # output$BrnnMetrics <- renderTable({
  #   req(models$brnn)
  #   models$brnn$results[ which.min(models$brnn$results[, "RMSE"]), ]
  # })
  # 
  # # output BrnnModelPlots (plot) ----
  # output$BrnnModelPlots <- renderPlot({
  #   req(models$brnn)
  #   plot(models$brnn)
  # })
  # 
  # # output BrnnRecipe (print) ----
  # output$BrnnRecipe <- renderPrint({
  #   req(models$brnn)
  #   models$brnn$recipe
  # })
  # 
  # # output BrnnModelSummary2 (print) ----
  # output$BrnnModelSummary2 <- renderPrint({
  #   req(models$brnn)
  #   summary(models$brnn$finalModel)
  # })


  # # METHOD * parRF ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getBrnnRecipe ----
  # getparRFRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$parRFPreprocess) # %>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent parRFGo ----
  # observeEvent(
  #   input$parRFGo,
  #   {
  #     #library(parRF)
  #     library(e1071)
  #     library(randomForest)
  #     method <- "parRF"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getparRFRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output parRFModelSummary0 (text) ----
  # output$parRFModelSummary0 <- renderText({
  #   description("parRF")
  # })
  # 
  # # output parRFMetrics (table) ----
  # output$parRFMetrics <- renderTable({
  #   req(models$parRF)
  #   models$parRF$results[ which.min(models$parRF$results[, "RMSE"]), ]
  # })
  # 
  # # output BrnnModelPlots (plot) ----
  # output$parRFModelPlots <- renderPlot({
  #   req(models$parRF)
  #   plot(models$parRF)
  # })
  # 
  # # output parRFRecipe (print) ----
  # output$parRFRecipe <- renderPrint({
  #   req(models$parRF)
  #   models$parRF$recipe
  # })
  # 
  # # output BrnnModelSummary2 (print) ----
  # output$parRFModelSummary2 <- renderPrint({
  #   req(models$parRF)
  #   summary(models$parRF$finalModel)
  # })
  # 
  # # METHOD * BstLm ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getBstLmRecipe ----
  # getBstLmRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$BstLmPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent BstLmGo ----
  # observeEvent(
  #   input$BstLmGo,
  #   {
  #     library(plyr)
  #     library(bst)
  #     method <- "BstLm"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getBstLmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output BstLmModelSummary0 (text) ----
  # output$BstLmModelSummary0 <- renderText({
  #   description("BstLm")
  # })
  # 
  # # output BstLmMetrics (table) ----
  # output$BstLmMetrics <- renderTable({
  #   req(models$BstLm)
  #   models$BstLm$results[ which.min(models$BstLm$results[, "RMSE"]), ]
  # })
  # 
  # # output BstLmModelPlots (plot) ----
  # output$BstLmModelPlots <- renderPlot({
  #   req(models$BstLm)
  #   plot(models$BstLm)
  # })
  # 
  # # output BstLmRecipe (print) ----
  # output$BstLmRecipe <- renderPrint({
  #   req(models$BstLm)
  #   models$BstLm$recipe
  # })
  # 
  # # output BstLmModelSummary2 (print) ----
  # output$BstLmModelSummary2 <- renderPrint({
  #   req(models$BstLm)
  #   summary(models$BstLm$finalModel)
  # })
  # 
  # 
  # # METHOD * icr ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive geticrRecipe ----
  # geticrRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$icrPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent icrGo ----
  # observeEvent(
  #   input$icrGo,
  #   {
  #     library(fastICA)
  # 
  #     method <- "icr"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(geticrRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output icrModelSummary0 (text) ----
  # output$icrModelSummary0 <- renderText({
  #   description("icr")
  # })
  # 
  # # output icrMetrics (table) ----
  # output$icrMetrics <- renderTable({
  #   req(models$icr)
  #   models$icr$results[ which.min(models$icr$results[, "RMSE"]), ]
  # })
  # 
  # # output icrModelPlots (plot) ----
  # output$icrModelPlots <- renderPlot({
  #   req(models$icr)
  #   plot(models$icr)
  # })
  # 
  # # output icrRecipe (print) ----
  # output$icrRecipe <- renderPrint({
  #   req(models$icr)
  #   models$icr$recipe
  # })
  # 
  # # output icrModelSummary2 (print) ----
  # output$icrModelSummary2 <- renderPrint({
  #   req(models$icr)
  #   summary(models$icr$finalModel)
  # })
  # 
  # # METHOD * lasso ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getlassoRecipe ----
  # getlassoRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$lassoPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent lassoGo ----
  # observeEvent(
  #   input$lassoGo,
  #   {
  #     library(elasticnet)
  # 
  #     method <- "lasso"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getlassoRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output lassoModelSummary0 (text) ----
  # output$lassoModelSummary0 <- renderText({
  #   description("lasso")
  # })
  # 
  # # output lassoMetrics (table) ----
  # output$lassoMetrics <- renderTable({
  #   req(models$lasso)
  #   models$lasso$results[ which.min(models$lasso$results[, "RMSE"]), ]
  # })
  # 
  # # output lassoModelPlots (plot) ----
  # output$lassoModelPlots <- renderPlot({
  #   req(models$lasso)
  #   plot(models$lasso)
  # })
  # 
  # # output lassoRecipe (print) ----
  # output$lassoRecipe <- renderPrint({
  #   req(models$lasso)
  #   models$lasso$recipe
  # })
  # 
  # # output lassoModelSummary2 (print) ----
  # output$lassoModelSummary2 <- renderPrint({
  #   req(models$lasso)
  #   summary(models$lasso$finalModel)
  # })

  # # METHOD * pcr ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getpcrRecipe ----
  # getpcrRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$pcrPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent pcrGo ----
  # observeEvent(
  #   input$pcrGo,
  #   {
  #     library(pls)
  # 
  #     method <- "pcr"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getpcrRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output pcrModelSummary0 (text) ----
  # output$pcrModelSummary0 <- renderText({
  #   description("pcr")
  # })
  # 
  # # output pcrMetrics (table) ----
  # output$pcrMetrics <- renderTable({
  #   req(models$pcr)
  #   models$pcr$results[ which.min(models$pcr$results[, "RMSE"]), ]
  # })
  # 
  # # output pcrModelPlots (plot) ----
  # output$pcrModelPlots <- renderPlot({
  #   req(models$pcr)
  #   plot(models$pcr)
  # })
  # 
  # # output pcrRecipe (print) ----
  # output$pcrRecipe <- renderPrint({
  #   req(models$pcr)
  #   models$pcr$recipe
  # })
  # 
  # # output pcrModelSummary2 (print) ----
  # output$pcrModelSummary2 <- renderPrint({
  #   req(models$pcr)
  #   summary(models$pcr$finalModel)
  # })

  
  # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------

  # reactive getsvmLinearRecipe ----
  getsvmLinearRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$svmLinearPreprocess) #%>%
      #step_rm(has_type("date"))   # remove original date variables
  })

  # observeEvent svmLinearGo ----
  observeEvent(
    input$svmLinearGo,
    {
      library(kernlab)

      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getsvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output svmLinearModelSummary0 (text) ----
  output$svmLinearModelSummary0 <- renderText({
    description("svmLinear")
  })

  # output svmLinearMetrics (table) ----
  output$svmLinearMetrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })

  # output svmLinearModelPlots (plot) ----
  output$svmLinearModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })

  # output svmLinearRecipe (print) ----
  output$svmLinearRecipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })

  # output svmLinearModelSummary2 (print) ----
  output$svmLinearModelSummary2 <- renderPrint({
    req(models$svmLinear)
    summary(models$svmLinear$finalModel)
  })


  # # METHOD * kknn ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getkknnRecipe ----
  # getkknnRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$kknnPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })

  #  # observeEvent kknnGo ----
  # observeEvent(
  #   input$kknnGo,
  #   {
  #     library(kknn)
  # 
  #     method <- "kknn"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getkknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output kknnModelSummary0 (text) ----
  # output$kknnModelSummary0 <- renderText({
  #   description("kknn")
  # })
  # 
  # # output kknnMetrics (table) ----
  # output$kknnMetrics <- renderTable({
  #   req(models$kknn)
  #   models$kknn$results[ which.min(models$kknn$results[, "RMSE"]), ]
  # })
  # 
  # # output kknnModelPlots (plot) ----
  # output$kknnModelPlots <- renderPlot({
  #   req(models$kknn)
  #   plot(models$kknn)
  # })
  # 
  # # output kknnRecipe (print) ----
  # output$kknnRecipe <- renderPrint({
  #   req(models$kknn)
  #   models$kknn$recipe
  # })
  # 
  # # output kknnModelSummary2 (print) ----
  # output$kknnModelSummary2 <- renderPrint({
  #   req(models$kknn)
  #   summary(models$kknn$finalModel)
  # })


  # METHOD * rf ---------------------------------------------------------------------------------------------------------------------------

  # reactive getrfRecipe ----
  getrfRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$rfPreprocess) #%>%
      #step_rm(has_type("date"))   # remove original date variables
  })

  # observeEvent rfGo ----
  observeEvent(
    input$rfGo,
    {
      library(randomForest)

      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        # grid <- expand.grid(size=c(5,10,20,50), k=c(1,2,3,4,5))
        # model <- caret::train(getrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneGrid = grid)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output rfModelSummary0 (text) ----
  output$rfModelSummary0 <- renderText({
    description("rf")
  })

  # output rfMetrics (table) ----
  output$rfMetrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })

  # output rfModelPlots (plot) ----
  output$rfModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })

  # output rfRecipe (print) ----
  output$rfRecipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })

  # output rfModelSummary2 (print) ----
  output$rfModelSummary2 <- renderPrint({
    req(models$rf)
    summary(models$rf$finalModel)
  })

  
  # # METHOD * lmStepAIC ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getlmStepAICRecipe ----
  # getlmStepAICRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$lmStepAICPreprocess) # %>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent lmStepAICGo ----
  # observeEvent(
  #   input$lmStepAICGo,
  #   {
  #     library(MASS)
  # 
  #     method <- "lmStepAIC"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getlmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output lmStepAICModelSummary0 (text) ----
  # output$lmStepAICModelSummary0 <- renderText({
  #   description("lmStepAIC")
  # })
  # 
  # # output lmStepAICMetrics (table) ----
  # output$lmStepAICMetrics <- renderTable({
  #   req(models$lmStepAIC)
  #   models$lmStepAIC$results[ which.min(models$lmStepAIC$results[, "RMSE"]), ]
  # })
  # 
  # # output lmStepAICModelPlots (plot) ----
  # output$lmStepAICModelPlots <- renderPlot({
  #   req(models$lmStepAIC)
  #   plot(models$lmStepAIC)
  # })
  # 
  # # output lmStepAICRecipe (print) ----
  # output$lmStepAICRecipe <- renderPrint({
  #   req(models$lmStepAIC)
  #   models$lmStepAIC$recipe
  # })
  # 
  # # output lmStepAICModelSummary2 (print) ----
  # output$lmStepAICModelSummary2 <- renderPrint({
  #   req(models$lmStepAIC)
  #   summary(models$lmStepAIC$finalModel)
  # })


  #################################################################################################################################
  
  # # METHOD * bagEarthGCV ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getbagEarthGCVRecipe ----
  # getbagEarthGCVRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$bagEarthGCVPreprocess) # %>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent bagEarthGCVGo ----
  # observeEvent(
  #   input$bagEarthGCVGo,
  #   {
  #     library(earth)
  # 
  #     method <- "bagEarthGCV"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getbagEarthGCVRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output bagEarthGCVModelSummary0 (text) ----
  # output$bagEarthGCVModelSummary0 <- renderText({
  #   description("bagEarthGCV")
  # })
  # 
  # # output bagEarthGCVMetrics (table) ----
  # output$bagEarthGCVMetrics <- renderTable({
  #   req(models$bagEarthGCV)
  #   models$bagEarthGCV$results[ which.min(models$bagEarthGCV$results[, "RMSE"]), ]
  # })
  # 
  # # output bagEarthGCVModelPlots (plot) ----
  # output$bagEarthGCVModelPlots <- renderPlot({
  #   req(models$bagEarthGCV)
  #   plot(models$bagEarthGCV)
  # })
  # 
  # # output bagEarthGCVRecipe (print) ----
  # output$bagEarthGCVRecipe <- renderPrint({
  #   req(models$bagEarthGCV)
  #   models$bagEarthGCV$recipe
  # })
  # 
  # # output bagEarthGCVModelSummary2 (print) ----
  # output$bagEarthGCVModelSummary2 <- renderPrint({
  #   req(models$bagEarthGCV)
  #   summary(models$bagEarthGCV$finalModel)
  # })

  
  #################################################################################################################################
  
  # # METHOD * bridge ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getbridgeRecipe ----
  # getbridgeRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$bridgePreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent bridgeGo ----
  # observeEvent(
  #   input$bridgeGo,
  #   {
  #     library(monomvn)
  # 
  #     method <- "bridge"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getbridgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output bridgeModelSummary0 (text) ----
  # output$bridgeModelSummary0 <- renderText({
  #   description("bridge")
  # })
  # 
  # # output bridgeMetrics (table) ----
  # output$bridgeMetrics <- renderTable({
  #   req(models$bridge)
  #   models$bridge$results[ which.min(models$bridge$results[, "RMSE"]), ]
  # })
  # 
  # # output bridgeModelPlots (plot) ----
  # output$bridgeModelPlots <- renderPlot({
  #   req(models$bridge)
  #   plot(models$bridge)
  # })
  # 
  # # output bridgeRecipe (print) ----
  # output$bridgeRecipe <- renderPrint({
  #   req(models$bridge)
  #   models$bridge$recipe
  # })
  # 
  # # output bridgeModelSummary2 (print) ----
  # output$bridgeModelSummary2 <- renderPrint({
  #   req(models$bridge)
  #   summary(models$bridge$finalModel)
  # })
  # 
  # 
  # #################################################################################################################################
  # 
  # # METHOD * gaussprPoly ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getgaussprPolyRecipe ----
  # getgaussprPolyRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$gaussprPolyPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent gaussprPolyGo ----
  # observeEvent(
  #   input$gaussprPolyGo,
  #   {
  #     library(kernlab)
  # 
  #     method <- "gaussprPoly"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output gaussprPolyModelSummary0 (text) ----
  # output$gaussprPolyModelSummary0 <- renderText({
  #   description("gaussprPoly")
  # })
  # 
  # # output gaussprPolyMetrics (table) ----
  # output$gaussprPolyMetrics <- renderTable({
  #   req(models$gaussprPoly)
  #   models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  # })
  # 
  # # output gaussprPolyModelPlots (plot) ----
  # output$gaussprPolyModelPlots <- renderPlot({
  #   req(models$gaussprPoly)
  #   plot(models$gaussprPoly)
  # })
  # 
  # # output gaussprPolyRecipe (print) ----
  # output$gaussprPolyRecipe <- renderPrint({
  #   req(models$gaussprPoly)
  #   models$gaussprPoly$recipe
  # })
  # 
  # # output gaussprPolyModelSummary2 (print) ----
  # output$gaussprPolyModelSummary2 <- renderPrint({
  #   req(models$gaussprPoly)
  #   summary(models$gaussprPoly$finalModel)
  # })
  # 
  # 
  # # METHOD * gaussprRadial ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getgaussprRadialRecipe ----
  # getgaussprRadialRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$gaussprRadialPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent gaussprRadialGo ----
  # observeEvent(
  #   input$gaussprRadialGo,
  #   {
  #     library(kernlab)
  # 
  #     method <- "gaussprRadial"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getgaussprRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output gaussprRadialModelSummary0 (text) ----
  # output$gaussprRadialModelSummary0 <- renderText({
  #   description("gaussprRadial")
  # })
  # 
  # # output gaussprRadialMetrics (table) ----
  # output$gaussprRadialMetrics <- renderTable({
  #   req(models$gaussprRadial)
  #   models$gaussprRadial$results[ which.min(models$gaussprRadial$results[, "RMSE"]), ]
  # })
  # 
  # # output gaussprRadialModelPlots (plot) ----
  # output$gaussprRadialModelPlots <- renderPlot({
  #   req(models$gaussprRadial)
  #   plot(models$gaussprRadial)
  # })
  # 
  # # output gaussprRadialRecipe (print) ----
  # output$gaussprRadialRecipe <- renderPrint({
  #   req(models$gaussprRadial)
  #   models$gaussprRadial$recipe
  # })
  # 
  # # output gaussprRadialModelSummary2 (print) ----
  # output$gaussprRadialModelSummary2 <- renderPrint({
  #   req(models$gaussprRadial)
  #   summary(models$gaussprRadial$finalModel)
  # })
  # 
  # # METHOD * bam ---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getbamRecipe ----
  # getbamRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$bamPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent bamGo ----
  # observeEvent(
  #   input$bamGo,
  #   {
  #     library(mgcv)
  # 
  #     method <- "bam"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getbamRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output bamModelSummary0 (text) ----
  # output$bamModelSummary0 <- renderText({
  #   description("bam")
  # })
  # 
  # # output bamMetrics (table) ----
  # output$bamMetrics <- renderTable({
  #   req(models$bam)
  #   models$bam$results[ which.min(models$bam$results[, "RMSE"]), ]
  # })
  # 
  # # output bamModelPlots (plot) ----
  # output$bamModelPlots <- renderPlot({
  #   req(models$bam)
  #   plot(models$bam)
  # })
  # 
  # # output bamRecipe (print) ----
  # output$bamRecipe <- renderPrint({
  #   req(models$bam)
  #   models$bam$recipe
  # })
  # 
  # # output bamModelSummary2 (print) ----
  # output$bamModelSummary2 <- renderPrint({
  #   req(models$bam)
  #   summary(models$bam$finalModel)
  # })

  
  # METHOD * cubist ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getcubistRecipe ----
  getcubistRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$cubistPreprocess) #%>% 
      #step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent cubistGo ----
  observeEvent(
    input$cubistGo,
    {
      library(Cubist)
      
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output cubistModelSummary0 (text) ----
  output$cubistModelSummary0 <- renderText({
    description("cubist")
  })
  
  # output cubistMetrics (table) ----
  output$cubistMetrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  # output cubistModelPlots (plot) ----
  output$cubistModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })     
  
  # output cubistRecipe (print) ----
  output$cubistRecipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  # output cubistModelSummary2 (print) ----
  output$cubistModelSummary2 <- renderPrint({
    req(models$cubist)
    summary(models$cubist$finalModel)
  })
  
  
  # METHOD * xgbLinear ---------------------------------------------------------------------------------------------------------------------------

  # reactive getxgbLinearRecipe ----
  getxgbLinearRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$xgbLinearPreprocess) #%>%
      #step_rm(has_type("date"))   # remove original date variables
  })
  # 
  # observeEvent xgbLinearGo ----
  observeEvent(
    input$xgbLinearGo,
    {
      library(xgboost)

      method <- "xgbLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getxgbLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output xgbLinearModelSummary0 (text) ----
  output$xgbLinearModelSummary0 <- renderText({
    description("xgbLinear")
  })

   # output xgbLinearMetrics (table) ----
  output$xgbLinearMetrics <- renderTable({
    req(models$xgbLinear)
    models$xgbLinear$results[ which.min(models$xgbLinear$results[, "RMSE"]), ]
  })

   # output xgbLinearModelPlots (plot) ----
  output$xgbLinearModelPlots <- renderPlot({
    req(models$xgbLinear)
    plot(models$xgbLinear)
  })

  # output xgbLinearRecipe (print) ----
  output$xgbLinearRecipe <- renderPrint({
    req(models$xgbLinear)
    models$xgbLinear$recipe
  })

  # output xgbLinearModelSummary2 (print) ----
  output$xgbLinearModelSummary2 <- renderPrint({
    req(models$xgbLinear)
    summary(models$xgbLinear$finalModel)
  })

  #################################################################################################################################
 
  # METHOD * gbm_h2o---------------------------------------------------------------------------------------------------------------------------

  # reactive getgbm_h2oRecipe ----
  getgbm_h2oRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$gbm_h2oPreprocess) #%>%
      #step_rm(has_type("date"))   # remove original date variables
  })
  # 
  # observeEvent gbm_h2oGo ----
  observeEvent(
    input$gbm_h2oGo,
    {
      library(h2o)

      method <- "gbm_h2o"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getgbm_h2oRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output gbm_h2oModelSummary0 (text) ----
  output$gbm_h2oModelSummary0 <- renderText({
    description("gbm_h2o")
  })

  # output gbm_h2oMetrics (table) ----
  output$gbm_h2oMetrics <- renderTable({
    req(models$gbm_h2o)
    models$gbm_h2o$results[ which.min(models$gbm_h2o$results[, "RMSE"]), ]
  })

  # output gbm_h2oModelPlots (plot) ----
  output$gbm_h2oModelPlots <- renderPlot({
    req(models$gbm_h2o)
    plot(models$gbm_h2o)
  })

  # output gbm_h2oRecipe (print) ----
  output$gbm_h2oRecipe <- renderPrint({
    req(models$gbm_h2o)
    models$gbm_h2o$recipe
  })

  # output gbm_h2oModelSummary2 (print) ----
  output$gbm_h2oModelSummary2 <- renderPrint({
    req(models$gbm_h2o)
    summary(models$gbm_h2o$finalModel)
  })
  # 
  # # METHOD * spls---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getsplsRecipe ----
  # getsplsRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$splsPreprocess) #%>% 
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent splsGo ----
  # observeEvent(
  #   input$splsGo,
  #   {
  #     library(spls)
  #     
  #     method <- "spls"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getsplsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     }, 
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output splsModelSummary0 (text) ----
  # output$splsModelSummary0 <- renderText({
  #   description("spls")
  # })
  # 
  # # output splsMetrics (table) ----
  # output$splsMetrics <- renderTable({
  #   req(models$spls)
  #   models$spls$results[ which.min(models$spls$results[, "RMSE"]), ]
  # })
  # 
  # # output splsModelPlots (plot) ----
  # output$splsModelPlots <- renderPlot({
  #   req(models$spls)
  #   plot(models$spls)
  # })     
  # 
  # # output splsRecipe (print) ----
  # output$splsRecipe <- renderPrint({
  #   req(models$spls)
  #   models$spls$recipe
  # })  
  # 
  # # output splsModelSummary2 (print) ----
  # output$splsModelSummary2 <- renderPrint({
  #   req(models$spls)
  #   summary(models$spls$finalModel)
  # })
  # 
  # # METHOD * krlsPoly---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getkrlsPolyRecipe ----
  # getkrlsPolyRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$krlsPolyPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent krlsPolyGo ----
  # observeEvent(
  #   input$krlsPolyGo,
  #   {
  #     library(KRLS)
  # 
  #     method <- "krlsPoly"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getkrlsPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output krlsPolyModelSummary0 (text) ----
  # output$krlsPolyModelSummary0 <- renderText({
  #   description("krlsPoly")
  # })
  # 
  # # output krlsPolyMetrics (table) ----
  # output$krlsPolyMetrics <- renderTable({
  #   req(models$krlsPoly)
  #   models$krlsPoly$results[ which.min(models$krlsPoly$results[, "RMSE"]), ]
  # })
  # 
  # # output krlsPolyModelPlots (plot) ----
  # output$krlsPolyModelPlots <- renderPlot({
  #   req(models$krlsPoly)
  #   plot(models$krlsPoly)
  # })
  # 
  # # output krlsPolyRecipe (print) ----
  # output$krlsPolyRecipe <- renderPrint({
  #   req(models$krlsPoly)
  #   models$krlsPoly$recipe
  # })
  # 
  # # output krlsPolyModelSummary2 (print) ----
  # output$krlsPolyModelSummary2 <- renderPrint({
  #   req(models$krlsPoly)
  #   summary(models$krlsPoly$finalModel)
  # })
  # 
  # # METHOD * rvmPoly---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getrvmPolyRecipe ----
  # getrvmPolyRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$rvmPolyPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent rvmPolyGo ----
  # observeEvent(
  #   input$rvmPolyGo,
  #   {
  #     library(kernlab)
  # 
  #     method <- "rvmPoly"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getrvmPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output rvmPolyModelSummary0 (text) ----
  # output$rvmPolyModelSummary0 <- renderText({
  #   description("rvmPoly")
  # })
  # 
  # # output rvmPolyMetrics (table) ----
  # output$rvmPolyMetrics <- renderTable({
  #   req(models$rvmPoly)
  #   models$rvmPoly$results[ which.min(models$rvmPoly$results[, "RMSE"]), ]
  # })
  # 
  # # output rvmPolyModelPlots (plot) ----
  # output$rvmPolyModelPlots <- renderPlot({
  #   req(models$rvmPoly)
  #   plot(models$rvmPoly)
  # })
  # 
  # # output rvmPolyRecipe (print) ----
  # output$rvmPolyRecipe <- renderPrint({
  #   req(models$rvmPoly)
  #   models$rvmPoly$recipe
  # })
  # 
  # # output rvmPolyModelSummary2 (print) ----
  # output$rvmPolyModelSummary2 <- renderPrint({
  #   req(models$rvmPoly)
  #   summary(models$rvmPoly$finalModel)
  # })
  #  
  # # METHOD * ANFIS---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getANFISRecipe ----
  # getANFISRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$ANFISPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent ANFISGo ----
  # observeEvent(
  #   input$ANFISGo,
  #   {
  #     library(frbs)
  # 
  #     method <- "ANFIS"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getANFISRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output ANFISModelSummary0 (text) ----
  # output$ANFISModelSummary0 <- renderText({
  #   description("ANFIS")
  # })
  # 
  # # output ANFISMetrics (table) ----
  # output$ANFISMetrics <- renderTable({
  #   req(models$ANFIS)
  #   models$ANFIS$results[ which.min(models$ANFIS$results[, "RMSE"]), ]
  # })
  # 
  # # output ANFISModelPlots (plot) ----
  # output$ANFISModelPlots <- renderPlot({
  #   req(models$ANFIS)
  #   plot(models$ANFIS)
  # })
  # 
  # # output ANFISRecipe (print) ----
  # output$ANFISRecipe <- renderPrint({
  #   req(models$ANFIS)
  #   models$ANFIS$recipe
  # })
  # 
  # # output ANFISModelSummary2 (print) ----
  # output$ANFISModelSummary2 <- renderPrint({
  #   req(models$ANFIS)
  #   summary(models$ANFIS$finalModel)
  # })
  # 
  # # METHOD * DENFIS---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getDENFISRecipe ----
  # getDENFISRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$DENFISPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent DENFISGo ----
  # observeEvent(
  #   input$DENFISGo,
  #   {
  #     library(frbs)
  # 
  #     method <- "DENFIS"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getDENFISRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output DENFISModelSummary0 (text) ----
  # output$DENFISModelSummary0 <- renderText({
  #   description("DENFIS")
  # })
  # 
  # # output DENFISMetrics (table) ----
  # output$DENFISMetrics <- renderTable({
  #   req(models$DENFIS)
  #   models$DENFIS$results[ which.min(models$DENFIS$results[, "RMSE"]), ]
  # })
  # 
  # # output DENFISModelPlots (plot) ----
  # output$DENFISModelPlots <- renderPlot({
  #   req(models$DENFIS)
  #   plot(models$DENFIS)
  # })
  # 
  # # output DENFISRecipe (print) ----
  # output$DENFISRecipe <- renderPrint({
  #   req(models$DENFIS)
  #   models$DENFIS$recipe
  # })
  # 
  # # output DENFISModelSummary2 (print) ----
  # output$DENFISModelSummary2 <- renderPrint({
  #   req(models$DENFIS)
  #   summary(models$DENFIS$finalModel)
  # })
  # 
  #  # METHOD * SBC---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getSBCRecipe ----
  # getSBCRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$SBCPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent SBCGo ----
  # observeEvent(
  #   input$SBCGo,
  #   {
  #     library(frbs)
  # 
  #     method <- "SBC"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getSBCRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output SBCModelSummary0 (text) ----
  # output$SBCModelSummary0 <- renderText({
  #   description("SBC")
  # })
  # 
  # # output SBCMetrics (table) ----
  # output$SBCMetrics <- renderTable({
  #   req(models$SBC)
  #   models$SBC$results[ which.min(models$SBC$results[, "RMSE"]), ]
  # })
  # 
  # # output SBCModelPlots (plot) ----
  # output$SBCModelPlots <- renderPlot({
  #   req(models$SBC)
  #   plot(models$SBC)
  # })
  # 
  # # output SBCRecipe (print) ----
  # output$SBCRecipe <- renderPrint({
  #   req(models$SBC)
  #   models$SBC$recipe
  # })
  # 
  # # output SBCModelSummary2 (print) ----
  # output$SBCModelSummary2 <- renderPrint({
  #   req(models$SBC)
  #   summary(models$SBC$finalModel)
  # })
  # # 
  # # METHOD * rqnc---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getrqncRecipe ----
  # getrqncRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$rqncPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent rqncGo ----
  # observeEvent(
  #   input$rqncGo,
  #   {
  #     library(rqPen)
  # 
  #     method <- "rqnc"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getrqncRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output rqncModelSummary0 (text) ----
  # output$rqncModelSummary0 <- renderText({
  #   description("rqnc")
  # })
  # 
  # # output rqncMetrics (table) ----
  # output$rqncMetrics <- renderTable({
  #   req(models$rqnc)
  #   models$rqnc$results[ which.min(models$rqnc$results[, "RMSE"]), ]
  # })
  # 
  # # output rqncModelPlots (plot) ----
  # output$rqncModelPlots <- renderPlot({
  #   req(models$rqnc)
  #   plot(models$rqnc)
  # })
  # 
  # # output rqncRecipe (print) ----
  # output$rqncRecipe <- renderPrint({
  #   req(models$rqnc)
  #   models$rqnc$recipe
  # })
  # 
  # # output rqncModelSummary2 (print) ----
  # output$rqncModelSummary2 <- renderPrint({
  #   req(models$rqnc)
  #   summary(models$rqnc$finalModel)
  # })
  # 
  # # METHOD * mlpML---------------------------------------------------------------------------------------------------------------------------
  # 
  # # reactive getmlpMLRecipe ----
  # getmlpMLRecipe <- reactive({
  #   recipes::recipe(Y ~ ., data = getTrainData()) %>%
  #     #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
  #     steps(input$mlpMLPreprocess) #%>%
  #     #step_rm(has_type("date"))   # remove original date variables
  # })
  # 
  # # observeEvent mlpMLGo ----
  # observeEvent(
  #   input$mlpMLGo,
  #   {
  #     library(RSNNS)
  # 
  #     method <- "mlpML"
  #     models[[method]] <- NULL
  #     showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
  #     clus <- startMode(input$Parallel)
  #     tryCatch({
  #       deleteRds(method)
  #       model <- caret::train(getmlpMLRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
  #       saveToRds(model, method)
  #       models[[method]] <- model
  #     },
  #     finally = {
  #       removeNotification(id = method)
  #       stopMode(clus)
  #     })
  #   }
  # )
  # 
  # # output mlpMLModelSummary0 (text) ----
  # output$mlpMLModelSummary0 <- renderText({
  #   description("mlpML")
  # })
  # 
  # # output mlpMLMetrics (table) ----
  # output$mlpMLMetrics <- renderTable({
  #   req(models$mlpML)
  #   models$mlpML$results[ which.min(models$mlpML$results[, "RMSE"]), ]
  # })
  # 
  # # output mlpMLModelPlots (plot) ----
  # output$mlpMLModelPlots <- renderPlot({
  #   req(models$mlpML)
  #   plot(models$mlpML)
  # })
  # 
  # # output mlpMLRecipe (print) ----
  # output$mlpMLRecipe <- renderPrint({
  #   req(models$mlpML)
  #   models$mlpML$recipe
  # })
  # 
  # # output mlpMLModelSummary2 (print) ----
  # output$mlpMLModelSummary2 <- renderPrint({
  #   req(models$mlpML)
  #   summary(models$mlpML$finalModel)
  # })

  #################################################################################################################################
  
  # METHOD * M5---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getM5Recipe ----
  getM5Recipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$M5Preprocess) #%>% 
      #step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent M5Go ----
  observeEvent(
    input$M5Go,
    {
      library(RWeka)
      
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output M5ModelSummary0 (text) ----
  output$M5ModelSummary0 <- renderText({
    description("M5")
  })
  
  # output M5Metrics (table) ----
  output$M5Metrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })
  
  # output M5ModelPlots (plot) ----
  output$M5ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })     
  
  # output M5Recipe (print) ----
  output$M5Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  
  
  # output M5ModelSummary2 (print) ----
  output$M5ModelSummary2 <- renderPrint({
    req(models$M5)
    summary(models$M5$finalModel)
  })
  
  
  # METHOD * M5Rules---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getM5RulesRecipe ----
  getM5RulesRecipe <- reactive({
    recipes::recipe(Y ~ ., data = getTrainData()) %>%
      #step_date(has_type("date"), features = "decimal") %>% # add a numeric date variables
      steps(input$M5RulesPreprocess) #%>% 
      #step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent M5RulesGo ----
  observeEvent(
    input$M5RulesGo,
    {
      library(RWeka)
      
      method <- "M5Rules"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getM5RulesRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output M5RulesModelSummary0 (text) ----
  output$M5RulesModelSummary0 <- renderText({
    description("M5Rules")
  })
  
  # output M5RulesMetrics (table) ----
  output$M5RulesMetrics <- renderTable({
    req(models$M5Rules)
    models$M5Rules$results[ which.min(models$M5Rules$results[, "RMSE"]), ]
  })
  
  # output M5RulesModelPlots (plot) ----
  output$M5RulesModelPlots <- renderPlot({
    req(models$M5Rules)
    plot(models$M5Rules)
  })     
  
  # output M5RulesRecipe (print) ----
  output$M5RulesRecipe <- renderPrint({
    req(models$M5Rules)
    models$M5Rules$recipe
  })  
  
  # output M5RulesModelSummary2 (print) ----
  output$M5RulesModelSummary2 <- renderPrint({
    req(models$M5Rules)
    summary(models$M5Rules$finalModel)
  })

  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getResamples ----
  getResamples <- reactive({
    models <- reactiveValuesToList(models)
    results <- caret::resamples(models)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }

    #hide results worse than null model
    subset <- rep(TRUE, length(models))
    if (input$HideWorse & NullModel %in% names(models)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })

  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })

  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  output$H1 <- renderPrint({
    
    dat1 <- read_excel("C:/Users/NIVEDITHA/Desktop/prediction_DSR/CHA-wx.xlsx", sheet = 'CHA-old')
    dat1 <-data.frame(dat1)
    #dat$Date<- as.Date(dat$Date)
    #Chosen only first 900 observations for clear understanding of charts, whole dataset can be used.
    
    myvars <- c("Temp", "RH", "WSpd", "Rain","DSR")
    
    d <- dat1[myvars]
    #18993
    d <- as.data.frame(d)[1:18000,]
    d <- rename(d,c("Y" = "DSR"))
    
    
    #myvars <- c("Temp", "RH", "WS", "Rain","DSR")

    #data_comp <- data_comp[myvars]
    #18993
    
    #data_comp <- rename(data_comp,c("Y" = "DSR"))
    data_comp_scaled <- d
    row.number <- sample(1:nrow(data_comp_scaled), 0.8*nrow(data_comp_scaled))
    x <- data_comp_scaled
    train = x[row.number,]
    test = x[-row.number,]
    model_more <- lm(Y~RH*Temp*WSpd*Rain, data = train)
    step.model <- stepAIC(model_more, direction = "both", 
                          trace = FALSE) #method can take backward, forward and both
    summary(step.model)
    # ols.coef <- coef(model_more)
    # #ols.coef
    # # pred1 <- predict(model_more, newdata = test)
    # # #pred1
    # # r = rmse(test$Y, pred1)
    # # #rmse <- sqrt(sum((exp(pred1) - test$Y)^2)/length(test$Y))
    # # c(RMSE = r, R2=summary(model_more)$r.squared)
    # 

  })
  
  output$H2 <- renderPrint({
    dat2 <- read_excel("C:/Users/NIVEDITHA/Desktop/prediction_DSR/CHA-wx.xlsx", sheet = 'CHA-old')
    dat2 <-data.frame(dat2)
    #dat$Date<- as.Date(dat$Date)
    #Chosen only first 900 observations for clear understanding of charts, whole dataset can be used.
    
    myvars <- c("Temp", "RH", "WSpd", "Rain","DSR")
    
    d <- dat2[myvars]
    #18993
    d <- as.data.frame(d)[1:18000,]
    d <- rename(d,c("Y" = "DSR"))
    
    
    #myvars <- c("Temp", "RH", "WS", "Rain","DSR")
    
    #data_comp <- data_comp[myvars]
    #18993
    
    #data_comp <- rename(data_comp,c("Y" = "DSR"))
    data_comp_scaled <- d
    row.number <- sample(1:nrow(data_comp_scaled), 0.8*nrow(data_comp_scaled))
    x <- data_comp_scaled
    train = x[row.number,]
    test = x[-row.number,]
    model_more <- lm(Y~RH*Temp*WSpd*Rain, data = train)
    step.model <- stepAIC(model_more, direction = "both", 
                          trace = FALSE) #method can take backward, forward and both
    #summary(step.model)
    pred1 <- predict(step.model, newdata = test)
    #pred1
    r = rmse(test$Y, pred1)
    #rmse <- sqrt(sum((exp(pred1) - test$Y)^2)/length(test$Y))
    c(RMSE = r, R2=summary(step.model)$r.squared)
  })
  
  
  
  output$H6 <- renderPrint({
    dat2 <- read_excel("C:/Users/NIVEDITHA/Desktop/prediction_DSR/CHA-wx.xlsx", sheet = 'CHA-old')
    dat2 <-data.frame(dat2)
    #dat$Date<- as.Date(dat$Date)
    #Chosen only first 900 observations for clear understanding of charts, whole dataset can be used.
    
    myvars <- c("Temp", "RH", "WSpd", "Rain","DSR")
    
    d <- dat2[myvars]
    #18993
    d <- as.data.frame(d)[1:18000,]
    d <- rename(d,c("Y" = "DSR"))
    
    
    #myvars <- c("Temp", "RH", "WS", "Rain","DSR")
    
    #data_comp <- data_comp[myvars]
    #18993
    
    #data_comp <- rename(data_comp,c("Y" = "DSR"))
    data_comp_scaled <- d
    row.number <- sample(1:nrow(data_comp_scaled), 0.8*nrow(data_comp_scaled))
    x <- data_comp_scaled
    train = x[row.number,]
    test = x[-row.number,]
    model_more <- lm(Y~RH*Temp*WSpd*Rain, data = train)
    step.model <- stepAIC(model_more, direction = "both", 
                          trace = FALSE) #method can take backward, forward and both
    #summary(step.model)
    pred1 <- predict(step.model, newdata = test)
    #pred1
    #r = rmse(test$Y, pred1)
    #rmse <- sqrt(sum((exp(pred1) - test$Y)^2)/length(test$Y))
    y = varImp(step.model)
    y
   
  })
  
  
  output$H3 <- renderPrint({
    dat3 <- read_excel("C:/Users/NIVEDITHA/Desktop/prediction_DSR/CHA-wx.xlsx", sheet = 'CHA-old')
    dat3 <-data.frame(dat3)
    #dat$Date<- as.Date(dat$Date)
    #Chosen only first 900 observations for clear understanding of charts, whole dataset can be used.
    
    myvars <- c("Temp", "RH", "WSpd", "Rain","DSR")
    
    d <- dat3[myvars]
    #18993
    d <- as.data.frame(d)[1:18000,]
    d <- rename(d,c("Y" = "DSR"))
    
    
    #myvars <- c("Temp", "RH", "WS", "Rain","DSR")
    
    #data_comp <- data_comp[myvars]
    #18993
    
    #data_comp <- rename(data_comp,c("Y" = "DSR"))
    data_comp_scaled <- d
    row.number <- sample(1:nrow(data_comp_scaled), 0.8*nrow(data_comp_scaled))
    x <- data_comp_scaled
    train = x[row.number,]
    test = x[-row.number,]
  set.seed(123)
  # Set up repeated k-fold cross-validation
  train.control <- trainControl(method = "cv", number = 10)
  # Train the model
  step.model1 <- train(Y ~., data = train,
                       method = "leapBackward", 
                       tuneGrid = data.frame(nvmax = 1:4),
                       trControl = train.control
  )
  
  # leapForward , leapSeq
  step.model1$results
  step.model1$bestTune
  summary(step.model1$finalModel)
  
  })
  
  
  
  output$H4 <- renderPrint({
    dat4 <- read_excel("C:/Users/NIVEDITHA/Desktop/prediction_DSR/CHA-wx.xlsx", sheet = 'CHA-old')
    da4 <-data.frame(dat4)
    #dat$Date<- as.Date(dat$Date)
    #Chosen only first 900 observations for clear understanding of charts, whole dataset can be used.
    
    myvars <- c("Temp", "RH", "WSpd", "Rain","DSR")
    
    d <- dat4[myvars]
    #18993
    d <- as.data.frame(d)[1:18000,]
    d <- rename(d,c("Y" = "DSR"))
    
    
    #myvars <- c("Temp", "RH", "WS", "Rain","DSR")
    
    #data_comp <- data_comp[myvars]
    #18993
    
    #data_comp <- rename(data_comp,c("Y" = "DSR"))
    data_comp_scaled <- d
    row.number <- sample(1:nrow(data_comp_scaled), 0.8*nrow(data_comp_scaled))
    x <- data_comp_scaled
    train = x[row.number,]
    test = x[-row.number,]
    set.seed(123)
    # Set up repeated k-fold cross-validation
    train.control <- trainControl(method = "cv", number = 10)
    # Train the model
    step.model2 <- train(Y ~., data = train,
                         method = "leapBackward", 
                         tuneGrid = data.frame(nvmax = 1:4),
                         trControl = train.control
    )
    
    # leapForward , leapSeq
  pred4 <- predict(step.model2, newdata = test)
  
  #rmse <- sqrt(sum((exp(pred4) - test$Y)^2)/length(test$Y))
  rms = rmse(test$Y, pred4)
  c(RMSE = rms, R2=summary(step.model2)$r.squared)
  
  })
    
  
  
  
  
  output$H5 <- renderPlot({
    dat4 <- read_excel("C:/Users/NIVEDITHA/Desktop/prediction_DSR/CHA-wx.xlsx", sheet = 'CHA-old')
    da4 <-data.frame(dat4)
    #dat$Date<- as.Date(dat$Date)
    #Chosen only first 900 observations for clear understanding of charts, whole dataset can be used.
    
    myvars <- c("Temp", "RH", "WSpd", "Rain","DSR")
    
    d <- dat4[myvars]
    #18993
    d <- as.data.frame(d)[1:18000,]
    d <- rename(d,c("Y" = "DSR"))
    
    
    #myvars <- c("Temp", "RH", "WS", "Rain","DSR")
    
    #data_comp <- data_comp[myvars]
    #18993
    
    #data_comp <- rename(data_comp,c("Y" = "DSR"))
    data_comp_scaled <- d
    row.number <- sample(1:nrow(data_comp_scaled), 0.8*nrow(data_comp_scaled))
    x <- data_comp_scaled
    train = x[row.number,]
    test = x[-row.number,]
    set.seed(123)
    # Set up repeated k-fold cross-validation
    train.control <- trainControl(method = "cv", number = 10)
    # Train the model
    step.model2 <- train(Y ~., data = train,
                         method = "leapBackward", 
                         tuneGrid = data.frame(nvmax = 1:4),
                         trControl = train.control
    )
    
    # leapForward , leapSeq
    pred4 <- predict(step.model2, newdata = test)
    
    #rmse <- sqrt(sum((exp(pred4) - test$Y)^2)/length(test$Y))
    rms = rmse(test$Y, pred4)
    #c(RMSE = rms, R2=summary(step.model2)$r.squared)
  a = varImp(step.model2)

  plot(a)
  
  })
})




