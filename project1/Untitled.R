library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggthemes)
library(numform)
library(timeDate)
library(lubridate)
library(reshape2)
library(ca)
library(tidyr)
library(ape)
library(knitr)
library(ROCR)
library(ROCit)
library(rpart)
library(rpart.plot)
library(grid)
library(gridExtra)
library(tidyverse)
library(data.table)
library(vtreat)
library(caret)
library(stats)
library(caTools)
library(lime)
library(DALEX)
library(e1071)
library(fpc)
library(grDevices)
library(cluster)


# Define UI
ui <- fluidPage(
  titlePanel("Project #2 Modeling"),
  
  navlistPanel(
    "Classification",
    tabPanel("Univariate Models and Bivariate Models - average_monthly_earnings_category", 
             h2("Univariate Models and Bivariate Models - average_monthly_earnings_category"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable",
                             "Univariate Model —— Select a variable to display a ROC curve:",
                             choices = c("subscribers", "video.views", "uploads", "created_month", "channel_type"),
                             selected = "subscribers"),
                 selectInput("variableDensity",
                             "Univariate Model —— Select a variable to display a double density plot:",
                             choices = c("subscribers", "video.views", "uploads", "created_month", "channel_type"),
                             selected = "subscribers"),
                 radioButtons("modelChoice", "Select a model to display a ROC curve:",  # Corrected the input name
                              choices = c("Decision Tree", "k-NN"), selected = "Decision Tree")
               ),
               mainPanel(
                 plotOutput("rocPlot"),
                 plotOutput("densityPlot"),
                 plotOutput("modelROCPlot")
               )
             )
    ),
    
    tabPanel("Binary Classification Model - Unemployment.rate", 
             h2("Binary Classification Model - Unemployment.rate"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("model_type", "Select Model", 
                             c("Single Variable", "Decision Tree", "Naive Bayes")),
                 conditionalPanel(
                   condition = "input.model_type == 'Single Variable'",
                   selectInput("single_features", "Select Features", 
                               c("Population", "Gross.tertiary.education.enrollment", "Urban_population")),
                 ),
                 conditionalPanel(
                   condition = "input.model_type == 'Decision Tree'",
                   selectInput("tree_features", "Select Features for Decision Tree", 
                               c('"Population", "Gross.tertiary.education.enrollment", "Urban_population"', 
                                 '"Population", "Gross.tertiary.education.enrollment"', 
                                 '"Population", "Urban_population"', 
                                 '"Gross.tertiary.education.enrollment", "Urban_population"')
                   ),
                   sliderInput("tree_minsplit", "Minsplit", min = 2, max = 16, value = 2
                   ),
                   sliderInput("tree_minbucket", "Minbucket", min = 2, max = 13, value = 2)
                 ),
                 conditionalPanel(
                   condition = "input.model_type == 'Naive Bayes'",
                   selectInput("nb_features", "Select Features for Naive Bayes", 
                               c('"Population", "Gross.tertiary.education.enrollment", "Urban_population"', 
                                 '"Population", "Gross.tertiary.education.enrollment"', 
                                 '"Population", "Urban_population"', 
                                 '"Gross.tertiary.education.enrollment", "Urban_population"')
                   )
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("ROC Curve", plotOutput("roc_plot")),
                   tabPanel("Density Plot", plotOutput("density_plot")),
                   tabPanel("Measures Table", tableOutput("measures_table"))
                 )
               )
             )
    ),
    
    "Clustering",
    tabPanel("Hierarchical Clustering Analysis - Country",
             h2("Hierarchical Clustering Analysis - Country"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dist_method", "Select Distance Method", 
                             c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski")),
                 selectInput("hclust_method", "Select Hierarchical Clustering Method",
                             c("ward.D2", "single", "centroid","ward.D", "complete", "average")),
                 sliderInput("k_value", "Select k Value", min = 2, max = 10, value = 5),
                 actionButton("run_analysis", "Run Analysis")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Dendrogram Plot", plotOutput("dendrogram_plot")),
                   tabPanel("CH Index & WSS Plot", plotOutput("ch_wss_plot")),
                   tabPanel("Scatter Plot", plotOutput("scatter_plot")),
                   tabPanel("Cluster Stability", tableOutput("cluster_stability"))
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # 文件读取，数据处理
  youtubedata <- read.csv("Global YouTube Statistics.csv")
  youtubedata[youtubedata == "nan"] <- NA
  youtubedata[youtubedata == "NaN"] <- NA
  
  month<- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
            Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
  youtubedata$created_month <- month[youtubedata$created_month]
  youtubedata$created_month <- as.numeric(youtubedata$created_month)
  
  
  youtubedata$Gross.tertiary.education.enrollment....[youtubedata$Gross.tertiary.education.enrollment.... > 100] <- NA
  apply(is.na(youtubedata), 2, sum)
  
  datanew <- youtubedata %>%
    select(Country, Population, Gross.tertiary.education.enrollment...., Unemployment.rate, Urban_population) %>%
    filter(!is.na(Unemployment.rate))
  
  median_GTEE <- median(datanew$Gross.tertiary.education.enrollment...., na.rm = TRUE)
  
  datanew$Gross.tertiary.education.enrollment....[is.na(datanew$Gross.tertiary.education.enrollment....)] <- median_GTEE
  
  data <- unique(datanew)
  
  df <- data
  
  all_zero_rows <- which(youtubedata$lowest_monthly_earnings == 0 & 
                           youtubedata$highest_monthly_earnings == 0 & 
                           youtubedata$lowest_yearly_earnings == 0 & 
                           youtubedata$highest_yearly_earnings == 0)
  
  median_lowest_monthly <- median(youtubedata$lowest_monthly_earnings[youtubedata$lowest_monthly_earnings != 0])
  median_highest_monthly <- median(youtubedata$highest_monthly_earnings[youtubedata$highest_monthly_earnings != 0])
  median_lowest_yearly <- median(youtubedata$lowest_yearly_earnings[youtubedata$lowest_yearly_earnings != 0])
  median_highest_yearly <- median(youtubedata$highest_yearly_earnings[youtubedata$highest_yearly_earnings != 0])
  
  youtubedata$lowest_monthly_earnings[all_zero_rows] <- median_lowest_monthly
  youtubedata$highest_monthly_earnings[all_zero_rows] <- median_highest_monthly
  youtubedata$lowest_yearly_earnings[all_zero_rows] <- median_lowest_yearly
  youtubedata$highest_yearly_earnings[all_zero_rows] <- median_highest_yearly
  
  all_NA_rows <- which(is.na(youtubedata$created_year) & 
                         is.na(youtubedata$created_month) & 
                         is.na(youtubedata$created_date))
  youtubedata <- youtubedata[-all_NA_rows, ]
  
  median_views <- median(youtubedata$video_views_for_the_last_30_days, na.rm = TRUE)
  youtubedata$video_views_for_the_last_30_days[is.na(youtubedata$video_views_for_the_last_30_days)] <- median_views
  median_subscribers <- median(youtubedata$subscribers_for_last_30_days, na.rm = TRUE)
  youtubedata$subscribers_for_last_30_days[is.na(youtubedata$subscribers_for_last_30_days)] <- median_subscribers
  
  youtubedata <- youtubedata %>%
    mutate(
      average_monthly_earnings = apply(select(., c("lowest_monthly_earnings", "highest_monthly_earnings")), 1, median),
      average_monthly_earnings_category = cut(
        average_monthly_earnings,
        breaks = c(0, 138975 , Inf),
        labels = c("low", "high"),
        right = FALSE
      )
    )
  
  youtubedata1 <- youtubedata[, c("subscribers","video.views", "uploads", "channel_type", "created_year", "created_month", "created_date", "average_monthly_earnings_category")]
  youtubedata1 <- youtubedata1[!is.na(youtubedata1$channel_type), ]
  youtubedata1$channel_type <- as.factor(youtubedata1$channel_type)
  
  d1 <- youtubedata1
  set.seed(2333)
  d1$rg <- runif(dim(d1)[1])
  d1TrainAll <- subset(d1, rg<=0.7)
  d1Test <- subset(d1, rg>0.7)
  vars1 <- setdiff(colnames(d1TrainAll), c("rg","average_monthly_earnings_category"))
  catvars1 <- vars1[sapply(d1TrainAll[, vars1], class) %in%
                      c('factor', 'character')]
  numericvars1 <- vars1[sapply(d1TrainAll[, vars1], class) %in%
                          c('numeric', 'integer')]
  
  useForCal1 <- rbinom(n=dim(d1TrainAll)[1], size=1, prob=0.1)>0
  d1Cal <- subset(d1TrainAll, useForCal1)
  
  d1Train <- subset(d1TrainAll, !useForCal1)
  
  outcome1 <- 'average_monthly_earnings_category'
  pos1 <- 'high'
  
  mkPredC1 <- function(outCol, varCol, appCol) {
    pPos1 <- sum(outCol == pos1) / length(outCol)
    naTab <- table(as.factor(outCol[is.na(varCol)]))
    pPos1Wna <- (naTab/sum(naTab))[pos1]
    vTab <- table(as.factor(outCol), varCol)
    
    pPos1Wv <- (vTab[pos1, ] + 1.0e-3*pPos1) / (colSums(vTab) + 1.0e-3)
    pred <- pPos1Wv[appCol]
    pred[is.na(appCol)] <- pPos1Wna
    pred[is.na(pred)] <- pPos1
    pred
  }
  
  mkPredN1 <- function(outCol, varCol, appCol) {
    
    cuts <- unique(
      quantile(varCol, probs=seq(0, 1, 0.1), na.rm=T))
    
    varC <- cut(varCol,cuts)
    appC <- cut(appCol,cuts)
    mkPredC1(outCol,varC,appC)
  }
  
  calcAUC1 <- function(predcol,outcol) {
    perf1 <- performance(prediction(predcol,outcol == pos1),'auc')
    as.numeric(perf1@y.values)
  }
  
  for (v in catvars1) {
    pi <- paste('pred', v, sep='')
    d1Train[,pi] <- mkPredC1(d1Train[[outcome1]], d1Train[[v]], d1Train[[v]])
    d1Cal[,pi] <- mkPredC1(d1Cal[[outcome1]], d1Cal[[v]], d1Cal[[v]])
    d1Test[,pi] <- mkPredC1(d1Test[[outcome1]], d1Test[[v]], d1Test[[v]])
    aucTrain1 <- roc(d1Train[[outcome1]], d1Train[[pi]])$auc
    if (aucTrain1 >= 0.55) {
      aucCal1 <- roc(d1Cal[[outcome1]], d1Cal[[pi]])$auc
    }
  }
  
  for (v in numericvars1) {
    pi <- paste('pred', v, sep='')
    d1Train[,pi] <- mkPredN1(d1Train[[outcome1]], d1Train[[v]], d1Train[[v]])
    d1Cal[,pi] <- mkPredN1(d1Cal[[outcome1]], d1Cal[[v]], d1Cal[[v]])
    d1Test[,pi] <- mkPredN1(d1Test[[outcome1]], d1Test[[v]], d1Test[[v]])
    aucTrain1 <- roc(d1Train[[outcome1]], d1Train[[pi]])$auc
    if (aucTrain1 >= 0.55) {
      aucCal1 <- roc(d1Cal[[outcome1]], d1Cal[[pi]])$auc
    }
  }
  
  # colour_id 1-7 are: black,red,green,blue,cyan,purple,gold
  plot_roc1 <- function(predcol, outcol, colour_id=2, overlaid=F) {
    ROCit_obj <- rocit(score=predcol, class=outcol==pos1)
    par(new=overlaid)
    plot(ROCit_obj, col = c(colour_id, 1),
         legend = FALSE, YIndex = FALSE, values = FALSE)
  }
  
  set.seed(123)
  split <- sample.split(youtubedata1$average_monthly_earnings_category, SplitRatio = 0.7)
  training_data1 <- youtubedata1[split, ]
  testing_data1 <- youtubedata1[!split, ]
  
  train_levels <- levels(training_data1$channel_type)
  test_levels <- levels(testing_data1$channel_type)
  
  channel_type_data <- youtubedata1[,c("channel_type","video.views","average_monthly_earnings_category")]
  
  tree_model <- rpart(average_monthly_earnings_category ~ ., data = channel_type_data)
  
  predictions <- predict(tree_model, testing_data1, type = "class")
  confusion_matrix <- table(predictions, testing_data1$average_monthly_earnings_category)
  
  
  predictions <- predict(tree_model, testing_data1, type = "vector")
  true_labels <- ifelse(testing_data1$average_monthly_earnings_category == "high", 1, 0)
  pred_obj <- prediction(predictions, true_labels)
  perf1 <- performance(pred_obj, "tpr", "fpr")
  predictions <- predict(tree_model, testing_data1, type = "vector")
  true_labels <- ifelse(testing_data1$average_monthly_earnings_category == "high", 1, 0)
  pred_obj <- prediction(predictions, true_labels)
  perf1 <- performance(pred_obj, "tpr", "fpr")
  
  # Plot the ROC curve
  plot(perf1, colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
  
  legend("bottomright", legend = c("Model"), col = 1, lwd = 2)
  
  
  k <- 15
  features <- c("subscribers", "video.views","uploads")
  knn_model <- knn(train = training_data1[, features], test = testing_data1[, features], cl = training_data1$average_monthly_earnings_category, k = k)
  confusion_matrix <- table(knn_model, testing_data1$average_monthly_earnings_category)
  
  predictions2 <- as.numeric(knn_model == "high") 
  true_labels <- as.numeric(testing_data1$average_monthly_earnings_category == "high")
  pred_obj <- prediction(predictions2, true_labels)
  perf1 <- performance(pred_obj, "tpr", "fpr")
  auc_value <- performance(pred_obj, "auc")
  
  
  normalized_predictions <- (predictions - min(predictions)) / (max(predictions) - min(predictions))
  roc_data <- data.frame(Predicted = normalized_predictions, Actual = true_labels)
  normalized_predictions <- (predictions - min(predictions)) / (max(predictions) - min(predictions))
  roc_data$Predicted <- as.numeric(normalized_predictions)
  fig1 <- ggplot(roc_data) +
    geom_density(aes(x = Predicted, fill = factor(Actual), color = factor(Actual)), alpha = 0.5) +
    xlab("Predicted Probabilities") +
    ylab("Density") 
  print(fig1)
  
  fig1 <- ggplot(roc_data) +
    geom_density(aes(x = Predicted, fill = factor(Actual), color = factor(Actual)), alpha = 0.5) +
    xlab("Predicted Probabilities") +
    ylab("Density") +
    scale_fill_manual(values = c("blue", "red")) +
    scale_color_manual(values = c("blue", "red")) +
    theme_minimal()
  print(fig1)
  
  # 单变量模型，分割数据集
  
  mean_unemployment_rate <- mean(data$Unemployment.rate)
  
  data$Unemployment <- ifelse(data$Unemployment.rate > mean_unemployment_rate, "1", "-1")
  data <- data %>% select(-Unemployment.rate)
  
  d <- data %>%
    select(-Country)
  set.seed(2333)
  d$rg <- runif(dim(d)[1])
  dTrainAll <- subset(d, rg <= 0.7)
  dTest <- subset(d, rg > 0.7)
  vars <- setdiff(colnames(dTrainAll), c('rg', 'Unemployment'))
  useForCal <- rbinom(n = dim(dTrainAll)[1], size = 1, prob = 0.1) > 0
  dCal <- subset(dTrainAll, useForCal)
  dTrain <- subset(dTrainAll, !useForCal)
  
  # 建立单变量模型
  
  outcome <- 'Unemployment'
  pos <- '1'
  
  mkPredC <- function(outCol, varCol, appCol) {
    pPos <- sum(outCol == pos) / length(outCol)
    naTab <- table(as.factor(outCol[is.na(varCol)]))
    pPosWna <- (naTab/sum(naTab))[pos]
    vTab <- table(as.factor(outCol), varCol)
    pPosWv <- (vTab[pos, ] + 1.0e-3 * pPos) / (colSums(vTab) + 1.0e-3)
    pred <- pPosWv[appCol]
    pred[is.na(appCol)] <- pPosWna
    pred[is.na(pred)] <- pPos
    pred
  }
  
  mkPredN <- function(outCol, varCol, appCol) {
    cuts <- unique(
      quantile(varCol, probs = seq(0, 1, 0.1), na.rm = TRUE))
    varC <- cut(varCol, cuts)
    appC <- cut(appCol, cuts)
    mkPredC(outCol, varC, appC)
  }
  
  for (v in vars) {
    pi <- paste('pred', v, sep='')
    dTrain[, pi] <- mkPredN(dTrain[, outcome], dTrain[, v], dTrain[, v])
    dCal[, pi] <- mkPredN(dTrain[, outcome], dTrain[, v], dCal[, v])
    dTest[, pi] <- mkPredN(dTrain[, outcome], dTrain[, v], dTest[, v])
  }
  
  calcAUC <- function(predcol, outcol) {
    perf <- performance(prediction(predcol, outcol == pos), 'auc')
    as.numeric(perf@y.values)
  }
  
  logLikelihood <- function(ytrue, ypred, epsilon=1e-6) {
    sum(ifelse(ytrue==pos, log(ypred+epsilon), log(1-ypred-epsilon)), na.rm=T)
  }
  
  performanceMeasures <- function(ytrue, ypred, model.name = "model", threshold = 0.5) {
    # compute the normalised deviance
    dev.norm <- -2 * logLikelihood(ytrue, ypred)/length(ypred)
    # compute the confusion matrix
    cmat <- table(actual = ytrue, predicted = ypred >= threshold)
    accuracy <- sum(diag(cmat)) / sum(cmat)
    precision <- cmat[2, 2] / sum(cmat[, 2])
    recall <- cmat[2, 2] / sum(cmat[2, ])
    f1 <- 2 * precision * recall / (precision + recall)
    data.frame(model = model.name, precision = precision,
               recall = recall, f1 = f1, dev.norm = dev.norm)
  }
  
  panderOpt <- function() {
    library(pander)
    # setting up Pander Options
    panderOptions("plain.ascii", TRUE)
    panderOptions("keep.trailing.zeros", TRUE)
    panderOptions("table.style", "simple")
  }
  
  observe({
    # Create a reactive expression for plotting ROC curves
    plotROC <- reactive({
      selected_variable <- input$variable
      colour_id <- switch(
        selected_variable,
        "subscribers" = 2, # Use red for subscribers
        "video.views" = 3,  # Use green for video views
        "uploads" = 4,     # Use blue for uploads
        "created_month" = 5,  # Use light blue for created month
        "channel_type" = 6  # Use purple for channel type
      )
      
      plot_roc1(d1Test[[paste0("pred", selected_variable)]], d1Test[, outcome1], colour_id = colour_id)
    })
    
    # Render the ROC plot for the selected variable
    output$rocPlot <- renderPlot({
      plotROC()
    })
    
    # Create a reactive expression for plotting double density plots
    plotDensity <- reactive({
      selected_variable <- input$variableDensity
      
      fig <- ggplot(d1Cal) +
        geom_density(aes(x = .data[[paste0("pred", selected_variable)]], color = as.factor(average_monthly_earnings_category)))
      
      print(fig)
    })
    
    # Render the density plot for the selected variable
    output$densityPlot <- renderPlot({
      plotDensity()
    })
    
    # Create a reactive expression for plotting model ROC curves
    selected_modelROC <- reactive({
      if (input$modelChoice == "Decision Tree") {
        # 使用决策树模型的代码
        predictions <- predict(tree_model, testing_data1, type = "vector")
        true_labels <- ifelse(testing_data1$average_monthly_earnings_category == "high", 1, 0)
      } else {
        # 使用 k-NN 模型的代码
        k <- 15
        features <- c("subscribers", "video.views", "uploads")
        knn_model <- knn(train = training_data1[, features], test = testing_data1[, features], cl = training_data1$average_monthly_earnings_category, k = k)
        predictions <- as.numeric(knn_model == "high")
        true_labels <- as.numeric(testing_data1$average_monthly_earnings_category == "high")
      }
      
      # 计算 ROC 曲线
      pred_obj <- prediction(predictions, true_labels)
      perf1 <- performance(pred_obj, "tpr", "fpr")
      
      perf1
    })
    
    # Render the model ROC plot
    output$modelROCPlot <- renderPlot({
      perf1 <- selected_modelROC()
      
      # 绘制 ROC 曲线
      plot(perf1, colorize = TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      
      # 添加一个 legend
      legend("bottomright", legend = c(input$modelChoice), col = 1, lwd = 2)
    })
    
    
    # 读取参数
    model_type <- input$model_type
    
    if (input$single_features == "Population") {
      single_features <- "predPopulation"
    } else if (input$single_features == "Gross.tertiary.education.enrollment") {
      single_features <- "predGross.tertiary.education.enrollment...."
    }  else {
      single_features <- "predUrban_population"
    }
    
    if (input$tree_features == '"Population", "Gross.tertiary.education.enrollment", "Urban_population"') {
      tree_features <- c("predPopulation", "predGross.tertiary.education.enrollment....", "predUrban_population")
    } else if (input$tree_features == '"Population", "Gross.tertiary.education.enrollment"') {
      tree_features <- c("predPopulation", "predGross.tertiary.education.enrollment....")
    } else if (input$tree_features == '"Population", "Urban_population"') {
      tree_features <- c("predPopulation", "predUrban_population")
    } else {
      tree_features <- c("predGross.tertiary.education.enrollment....", "predUrban_population")
    }
    tree_minsplit <- input$tree_minsplit
    tree_minbucket <- input$tree_minbucket
    
    if (input$nb_features == '"Population", "Gross.tertiary.education.enrollment", "Urban_population"') {
      nb_features <- c("predPopulation", "predGross.tertiary.education.enrollment....", "predUrban_population")
    } else if (input$nb_features == '"Population", "Gross.tertiary.education.enrollment"') {
      nb_features <- c("predPopulation", "predGross.tertiary.education.enrollment....")
    } else if (input$nb_features == '"Population", "Urban_population"') {
      nb_features <- c("predPopulation", "predUrban_population")
    } else {
      nb_features <- c("predGross.tertiary.education.enrollment....", "predUrban_population")
    }
    dist_method <- input$dist_method
    hclust_method <- input$hclust_method
    k_value <- input$k_value
    
    pretty_perf_table <- function(model, xtrain, ytrain, xcal, ycal,
                                  xtest, ytest, threshold = 0.5) {
      # Option setting for Pander
      panderOpt()
      perf_justify <- "lrrrr"
      # call the predict() function to do the predictions
      if (model_type == "Decision Tree") {
        pred_train <- predict(model, newdata = xtrain)
        pred_cal <- predict(model, newdata = xcal)
        pred_test <- predict(model, newdata = xtest)
      } else if (model_type == 'Naive Bayes') {
        pred_train <- predict(model, newdata = xtrain, type = 'raw')[, 'TRUE']
        pred_cal <- predict(model, newdata = xcal, type = 'raw')[, 'TRUE']
        pred_test <- predict(model, newdata = xtest, type = 'raw')[, 'TRUE']
      } else {
        pred_train <- xtrain
        pred_cal <- xcal
        pred_test <- xtest
      }
      # comparing performance on training, calibration,test
      trainperf_df <- performanceMeasures(
        ytrain, pred_train, model.name = "training", threshold = threshold)
      calperf_df <- performanceMeasures(
        ycal, pred_cal, model.name = "calibration", threshold = threshold)
      testperf_df <- performanceMeasures(
        ytest, pred_test, model.name = "test", threshold = threshold)
      # combine the three performance data frames using rbind()
      perftable <- rbind(trainperf_df, calperf_df, testperf_df)
      return(perftable)
      # pandoc.table(perftable, justify = perf_justify)
    }
    
    # 做双密度图
    
    if (model_type == 'Single Variable') {
      output$density_plot <- renderPlot({
        ggplot(dCal) + geom_density(aes(x = dCal[, single_features], color = as.factor(Unemployment)))
      })
    } else if (model_type == 'Decision Tree') {
      (fV <- paste(outcome, '> 0 ~ ',
                   paste(tree_features, collapse = ' + '),
                   sep = ''))
      tmodel <- rpart(fV, data = dTrain,
                      control = rpart.control(minsplit = tree_minsplit,
                                              minbucket = tree_minbucket))
      
      tpred <- predict(tmodel, newdata = dCal)
      output$density_plot <- renderPlot({
        ggplot(dCal) + geom_density(aes(x = tpred, color = as.factor(Unemployment)))
      })
    } else {
      (f <- paste(outcome, '> 0 ~ ',
                  paste(nb_features, collapse = ' + '),
                  sep = ''))
      nbmodel <- naiveBayes(as.formula(f), data = dTrain)
      dCal$nbpred <- predict(nbmodel, newdata = dCal, type = 'raw')[, 'TRUE']
      ggplot(dCal) + geom_density(aes(x = nbpred, color = as.factor(Unemployment)))
    }
    
    # 做ROC图
    plot_roc <- function(predcol1, outcol1, predcol2, outcol2) {
      roc_1 <- rocit(score = predcol1, class = outcol1 == pos)
      roc_2 <- rocit(score = predcol2, class = outcol2 == pos)
      plot(roc_1, col = c("blue", "green"), lwd = 3,
           legend = FALSE, YIndex = FALSE, values = TRUE, asp = 1)
      lines(roc_2$TPR ~ roc_2$FPR, lwd = 3,
            col = c("red", "green"), asp = 1)
      legend("bottomright", col = c("blue", "red", "green"),
             c("Test Data", "Training Data", "Null Model"), lwd = 2)
    }
    
    if (model_type == 'Single Variable') {
      output$roc_plot <- renderPlot({
        plot_roc(dTest[, single_features], dTest[[outcome]],
                 dTrain[, single_features], dTrain[[outcome]])
      })
      
    } else if (model_type == 'Decision Tree') {
      t_testpred <- predict(tmodel, newdata = dTest)
      t_trainpred <- predict(tmodel, newdata = dTrain)
      output$roc_plot <- renderPlot({
        plot_roc(t_testpred, dTest[[outcome]],
                 t_trainpred, dTrain[[outcome]])
      })
      
    } else {
      nb_testpred <- predict(nbmodel, newdata = dTest, type = 'raw')[, 'TRUE']
      nb_trainpred <- predict(nbmodel, newdata = dTrain, type = 'raw')[, 'TRUE']
      output$roc_plot <- renderPlot({
        plot_roc(nb_testpred, dTest[[outcome]],
                 nb_trainpred, dTrain[[outcome]])
      })
      
    }
    
    # 做参数表格
    
    if (model_type == 'Single Variable') {
      output$measures_table <- renderTable({
        pretty_perf_table(single_features,
                          dTrain[, single_features], dTrain[, outcome] == pos,
                          dCal[, single_features], dCal[, outcome] == pos,
                          dTest[, single_features], dTest[, outcome] == pos)
      })
    } else if (model_type == 'Decision Tree') {
      output$measures_table <- renderTable({
        pretty_perf_table(tmodel,
                          dTrain[c(tree_features)], dTrain[, outcome] == pos,
                          dCal[c(tree_features)], dCal[, outcome] == pos,
                          dTest[c(tree_features)], dTest[, outcome] == pos)
      })
    } else {
      output$measures_table <- renderTable({
        pretty_perf_table(nbmodel,
                          dTrain[c(tree_features)], dTrain[, outcome] == pos,
                          dCal[c(tree_features)], dCal[, outcome] == pos,
                          dTest[c(tree_features)], dTest[, outcome] == pos)
      })
    }
    
    # 聚类分析
    
    country_lon_lat <- youtubedata %>%
      select(Country, Latitude, Longitude) %>%
      filter(!is.na(Latitude))
    
    country_lon_lat <- unique(country_lon_lat)
    
    df <- left_join(df, country_lon_lat, by = "Country")
    
    vars.to.use <- colnames(df)[-1]
    scaled_df <- scale(df[, vars.to.use])
    
    # Perform hierarchical clustering
    d <- dist(scaled_df, method = dist_method)
    pfit <- hclust(d, method = hclust_method)
    
    # Generate dendrogram plot
    output$dendrogram_plot <- renderPlot({
      plot(pfit, labels = df$Country, main = "Cluster Dendrogram for Country",
           cex = 0.8)
      rect.hclust(pfit, k = k_value)
    })
    
    # Identify clusters
    groups <- cutree(pfit, k = k_value)
    
    # Calculate CH Index
    sqr_euDist <- function(x, y) {
      sum((x - y) ^ 2)
    }
    wss <- function(clustermat) {
      c0 <- colMeans(clustermat)
      sum(apply(clustermat, 1, FUN = function(row) {
        sqr_euDist(row, c0)
      }))
    }
    wss_total <- function(scaled_df, labels) {
      wss.sum <- 0
      k <- length(unique(labels))
      for (i in 1:k)
        wss.sum <- wss.sum + wss(subset(scaled_df, labels == i))
      wss.sum
    }
    tss <- function(scaled_df) {
      wss(scaled_df)
    }
    CH_index <- function(scaled_df, kmax, method = "kclust") {
      npts <- nrow(scaled_df)
      wss.value <- numeric(kmax) # create a vector of numeric type
      wss.value[1] <- wss(scaled_df)
      
      d <- dist(scaled_df, method = dist_method)
      pfit <- hclust(d, method = hclust_method)
      for (k in 2:kmax) {
        labels <- cutree(pfit, k = k)
        wss.value[k] <- wss_total(scaled_df, labels)
      }
      bss.value <- tss(scaled_df) - wss.value # this is a vector
      B <- bss.value / (0:(kmax - 1)) # also a vector
      W <- wss.value / (npts - 1:kmax) # also a vector
      data.frame(k = 1:kmax, CH_index = B/W, WSS = wss.value)
    }
    
    # calculate the CH criterion
    crit.df <- CH_index(scaled_df, 10, method = "hclust")
    fig1 <- ggplot(crit.df, aes(x = k, y = CH_index)) +
      geom_point() + geom_line(colour = "red") +
      scale_x_continuous(breaks = 1:10, labels = 1:10) +
      labs(y = "CH index") + theme(text = element_text(size = 20))
    fig2 <- ggplot(crit.df, aes(x = k, y = WSS), color = "blue") +
      geom_point() + geom_line(colour = "blue") +
      scale_x_continuous(breaks = 1:10, labels = 1:10) +
      theme(text = element_text(size = 20))
    
    output$ch_wss_plot <- renderPlot({
      grid.arrange(fig1, fig2, nrow = 1)
    })
    
    # Scatter plot
    princ <- prcomp(scaled_df)
    nComp <- 2
    find_convex_hull <- function(proj2Ddf, groups) {
      do.call(rbind,
              lapply(unique(groups),
                     FUN = function(c) {
                       f <- subset(proj2Ddf, cluster == c);
                       f[chull(f), ]
                     }
              )
      )
    }
    
    project2D <- as.data.frame(predict(princ, newdata = scaled_df)[, 1:nComp])
    hclust.project2D <- cbind(project2D, cluster = as.factor(groups), country = df$Country)
    hclust.hull <- find_convex_hull(hclust.project2D, groups)
    
    output$scatter_plot <- renderPlot({
      ggplot(hclust.project2D, aes(x = PC1, y = PC2)) +
        geom_point(aes(shape = cluster, color = cluster)) +
        geom_polygon(data = hclust.hull, aes(group = cluster, fill = as.factor(cluster)),
                     alpha = 0.4, linetype = 0) +
        scale_x_continuous(limits = c(-0.1, 0.07)) +
        scale_y_continuous(limits = c(-0.3, 0.4)) +
        labs(title = sprintf("k = %d", k_value)) +
        theme(legend.position = "none", text = element_text(size = 10))
    })
    
    # Cluster stability
    observeEvent(input$run_analysis, {
      dist_method <- input$dist_method
      hclust_method <- input$hclust_method
      k_value <- input$k_value
      
      cboot.hclust <- clusterboot(scaled_df, clustermethod = hclustCBI,
                                  method = hclust_method, k = k_value)
      
      groups.cboot <- cboot.hclust$result$partition
      
      values <- 1 - cboot.hclust$bootbrd / 100
      #cat("So clusters", order(values)[5], "and", order(values)[4], "are highly stable")
      cluster_numbers <- 1:k_value
      cluster_counts <- sapply(cluster_numbers, function(cluster_num) {
        sum(groups.cboot == cluster_num)
      })
      output$cluster_stability <- renderTable({
        stability <- data.frame(Cluster = paste0('cluster ', 1:k_value), Number = cluster_counts, Value = values)
        stability
      })
    })
  })
}


shinyApp(ui = ui, server = server)
