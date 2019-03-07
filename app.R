#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# List of required packages 
packages <- c('shiny', 'ggplot2', 'tidyverse', 'shinythemes', 'ggfortify', 
              'reshape2', 'broom')

# Install necessary packages
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(ggfortify)
library(reshape2)
library(broom)

# Read in index data
indexes <- read_csv('indexes_monthly.csv')


# Define UI for app that plots histograms, regression plots, and tests
# equalities of mean to investigate the October Effect
ui <- navbarPage(title = "",
                 theme = shinytheme("flatly"),
                 
                 # Introduction page with history, aims, and layout of the web app 
                 
                 tabPanel("Introduction", verticalLayout(
                   withTags ({
                     div(class="desc",
                         h3("The Mark Twain Effect"),
                         p("In this web app, we would like to investigate the Mark Twain Effect,
                            also known as the “October Effect”, that stock returns on October are
                            lower than for the rest of the months. First, we chose 10 global stock
                            indices to focus our research on. We then perform a stock index analysis 
                            to find their density plots. Next, we compare their log-returns against
                            another stock index. Finally, we examine whether there are statistical
                            evidence to support that stock indices returns are significantly lower in
                            October, as the Mark Twain Effect suggests. This web app also provides
                            the ability to check similar hypotheses for other monthly trends."),
                         p("The contents for each of the following tabs can be found below.")
                     )
                   }),
                   
                   withTags ({
                     div(class="desc",
                         h3("Stock Index Analysis"),
                         ul(li("A histogram of log-returns over all 30 years for each stock index"),
                           li("The expected mean, variance, and confidence interval for the given stock index and confidence level"),
                           li("A regression of log returns over time for the given stock, including residuals"),
                           li("A QQ plot to assess closeness of log returns data to a normal distribution"))
                     )
                   }),
                   withTags ({
                     div(class="desc",
                         h3("Compare Stock Indices"),
                         ul(li("A density plot comparing the log-returns distribution of 2 stock indices"),
                           li("A t-test equality of the means of the 2 stock indices log-returns"),
                           li("A regression of log returns of 2 stock indices over 30 years"))
                     )
                   }),
                   withTags ({
                     div(class="desc",
                         h3("Compare Months for Stock Index"),
                         ul(li("A density plot comparing the distribution of monthly log-returns for 2 months of a given stock index"),
                           li("A t-test equality of the means between the 2 months of log-returns"))
                     )
                   }),
                   
                   withTags ({
                     div(class="titles",
                         h3("Legend"))
                   }),
                   tableOutput("stockIndex")
                 )),
                 
                 # Single Stock Index Fund Analysis
                 tabPanel("Stock Index Analysis", sidebarLayout(
                   sidebarPanel(
                     selectizeInput('Stock', label = 'Stock Index', 
                                    choices=c("DJI", "FTSE", "GDAXI", "GSPC", "HSI", "IXIC", "N225", "RUT", "STI", 
                                              "STOXX50E")),
                     numericInput("ConfidenceInterval", "Confidence Interval:",
                                  min = 0.0, max = 1.0, value = 0.95, step = 0.05),
                     sliderInput("NumBins",
                                 "Number of Bins:",
                                 min = 1,
                                 max = 40,
                                 value = 30),
                     selectizeInput('RegRes', label = 'Regression Plot', 
                                    choices=c("Regression", "Residuals"))
                   ), 
                   mainPanel(
                     plotOutput("ggHistPlot"),
                     tableOutput("confTable"), 
                     plotOutput("regressionPlot"),
                     htmlOutput("regressionEQ"),
                     plotOutput("QQplot")
                   ))),
                
                 # Two Stock Analysis Tab 
                 tabPanel("Compare Stock Indices", sidebarLayout(
                   sidebarPanel(
                     selectizeInput('Stock1', label = 'Stock Index 1', 
                                    choices=c("DJI", "FTSE", "GDAXI", "GSPC", "HSI", "IXIC", "N225", "RUT", "STI", 
                                              "STOXX50E")),
                     selectizeInput('Stock2', label = 'Stock Index 2', 
                                    choices=c("DJI", "FTSE", "GDAXI", "GSPC", "HSI", "IXIC", "N225", "RUT", "STI", 
                                              "STOXX50E")),
                     numericInput("Alpha", "Alpha:",
                                  min = 0.0, max = 1.0, value = 0.05, step = 0.05),
                     selectizeInput('RegResStocks', label = 'Regression Plot', 
                                    choices=c("Regression", "Residuals"))
                     
                     ), 
                   mainPanel(
                     plotOutput("compareDistPlot"),
                     htmlOutput("meanTestingStocks"),
                     plotOutput("compareRegressionPlot"),
                     htmlOutput("regressionEQ2Stocks")
                   ))),
                 
                 # Month Comparison of Single Stock 
                 tabPanel("Compare Months for Stock Index", sidebarLayout(
                   sidebarPanel(
                     selectizeInput('Stock3', label = 'Stock Index', 
                                    choices=c("All Stocks", "DJI", "FTSE", "GDAXI", "GSPC", "HSI", "IXIC", "N225", "RUT", "STI", 
                                              "STOXX50E")),
                     numericInput("AlphaMonths", "Alpha:",
                                  min = 0.0, max = 1.0, value = 0.05, step = 0.05),
                     selectizeInput('Month1', label = 'Month 1', 
                                    choices=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                     selectizeInput('Month2', label = 'Month 2', 
                                    choices=c("All Other Months","Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))), 
                   mainPanel(
                     plotOutput("compareMonths", height = "700px"),
                     htmlOutput("meanTestingMonths"),
                     tableOutput("meanTestingMonthsTable"),
                     plotOutput("monthQQ")
                   ))),
             
                 # Panel about our team 
                 tabPanel("About Us",verticalLayout(
                   withTags ({
                     div(class="welcome",
                         p("Welcome to our web app!"),
                         p("We have put a lot of heart and effort into this web app. Please enjoy!"),
                         br(),
                         p("XOXO,"),
                         p("Sunny, Tiffany, and Huda"))
                   }),
                   img(src='ourImage.jpeg', width=400, height=300, align = "center")
                 ))
) 

# Server containing 
server <- function(input, output, session) {
  
  # Plot histogram of the log returns for one stock
  # with the density plot overlayed 
  output$ggHistPlot <- renderPlot({
    
    # Define the stock
    stock = input$Stock
    
    # Plot single histogram
    ggplot(indexes, aes(x = !!ensym(stock))) +
      geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5, bins = input$NumBins, color = 'royalblue', fill = 'skyblue') +
      geom_density(alpha=0.6) +
      xlab('Monthly Log Returns') +
      ylab('Density') + 
      ggtitle(label=stock, subtitle = "All Months 1988-2018") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 30), plot.subtitle = element_text(hjust = 0.5, size = 15)) 
  })
  
  # Render an table with estimates of the mean and variance
  # and include the confidence interval of the true mean 
  output$confTable<-renderTable({
    
    # Define the stock
    stock = input$Stock
    
    summary = summarise(indexes, Average = mean(!!ensym(stock)), Variance = var(!!ensym(stock)), Sample = length(!!ensym(stock)))

    # Calculate mean, variance, and number of samples 
    xbar = summary$Average 
    sbar = summary$Variance
    nsamp = summary$Sample
    
    # Calculate mean confidence interval
    alpha = 1.0 - input$ConfidenceInterval 
    t = qt(alpha/2, df=nsamp-1)
    
    xbar_conf_lower = round(xbar+t*sbar/sqrt(nsamp), 6)
    xbar_conf_upper = round(xbar-t*sbar/sqrt(nsamp), 6)
    
    # Calculate confidence interval for variance 
    chi_lower = qchisq(alpha/2, df=nsamp-1)
    sbar_conf_lower = round(((nsamp - 1)*sbar^2)/chi_lower, 6)

    chi_upper = qchisq(1-alpha/2, df=nsamp-1)
    sbar_conf_upper = round(((nsamp - 1)*sbar^2)/chi_upper, 6)
    
    xbar_ci = paste("[", xbar_conf_lower, ", ", xbar_conf_upper, "]", sep="")
    sbar_ci = paste("[", sbar_conf_lower, ", ", sbar_conf_upper, "]", sep="")
    
    # Put all values into a data frame to display as table 
    tibble("Parameter" = c("Mean", "Variance"), "Estimate" = c(xbar, sbar), "Confidence Interval" = c(xbar_ci, sbar_ci))
    
  }, 
  width = "100%",
  digits = 4, 
  striped = TRUE)
  
  # Plot the regression of the log returns 
  output$regressionPlot <- renderPlot({
    
    # Define the stock 
    stock = input$Stock

    # Calculate the regression of the selcted stock over time
    fit <- lm(get(stock) ~ DATE, data = indexes)
    indexes$predicted <- predict(fit)
    indexes$residuals <- residuals(fit)
    
    rsq <- round(summary(fit)$r.squared, 3)
    
    # Function to decide which type of plot to show based on selection
    # from user. User can choose to display regression or residuals 
    reg.plot <- function(type = c('Regression', 'Residuals')) {
      if (type == 'Regression') {
        ggplot(indexes, aes(x = DATE, y = !!ensym(stock))) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
          xlab('Date') + 
          ggtitle(label="Regression over Time") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 20)) +
          geom_text(x=Inf,y=+Inf, hjust=2, vjust = 2, label = paste0("R2=", rsq), size = 5)
      } else if (type == 'Residuals'){
        ggplot(indexes, aes(x = DATE, y = !!ensym(stock))) +
          geom_segment(aes(xend = DATE, yend = predicted), alpha = .1) +
          geom_point(aes(color = residuals)) +  # Color mapped here
          scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
          xlab('Date') + 
          guides(color = FALSE) +
          geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
          ggtitle(label="Regression over time (with Residuals)") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 20)) +
          geom_text(x=Inf,y=+Inf, hjust= 2, vjust = 2, label = paste0("R2=", rsq), size = 5)
      }
    }
    
    reg.plot(type = input$RegRes)
  })
  
  # Display the results of the regression for a particular stock  
  output$regressionEQ <- renderText({
    
    stock = input$Stock
    
    # Get estimate information from the linear model
    fit <- lm(get(stock) ~ DATE, data = indexes)
    indexes$predicted <- predict(fit)
    indexes$residuals <- residuals(fit)
    fit <- tidy(fit)
    beta0 <- signif(fit[1,2],3)
    beta1 <- signif(fit[2,2],3)
    sign <- if_else(sign(beta1) == 1, "+", "-")
    
    paste("<center>", "<h5>", "<br>", "The regression of", "<b>", stock, "</b>", "over time is modeled by", "</h5>",
          "<h4>",stock, "=", "<b>", beta0, "</b>", sign , "<b>", abs(beta1), "</b>", "&#215", "time", "</h4>", "</center>", "<br>", "<br>",  sep= ' ')
  })
  
  # Plot QQplot to determine normality of distributions 
  output$QQplot <- renderPlot({
    
    # Define the stock 
    stock = input$Stock
    
    # Print QQ Plot
    ggplot(indexes, aes(sample = !!ensym(stock))) +
      stat_qq() + stat_qq_line() +
      xlab('Theoretical') + 
      ylab('Sample') + 
      ggtitle(label="QQ Plot") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 20))
      
    
  })
  
  # Density plot comparing the distribution of two stocks
  # chosen by the user 
  output$compareDistPlot <- renderPlot({
    
    # Define stock 1 
    stock1 = input$Stock1
    
    # Define stock 2
    stock2 = input$Stock2
    
    # Subset data based on two chosen stocks to compare and melt dataframe for plotting
    indexes <- indexes %>% select(stock1, stock2)
    indexes <- melt(indexes)
    
    # Calculate mean for plotting
    m.indexes <- indexes %>% 
      group_by(variable) %>% 
      summarise_all(mean)
    
    # Density plot  
    ggplot(indexes, aes(x = value, fill=variable)) +
      geom_density(alpha=0.5, position="identity") +
      geom_vline(data=m.indexes, aes(xintercept=value, colour=variable), 
                 linetype='dashed', size=1) +
      ggtitle(label=paste(stock1, "vs", stock2, sep = " "), subtitle = "All Months 1988-2018") +
      theme_bw() +
      xlab('Value') + 
      ylab('Density') + 
      theme(plot.title = element_text(hjust = 0.5, size = 30), plot.subtitle = element_text(hjust = 0.5, size = 15)) +
      geom_text(x=+Inf, y = Inf, hjust=1, vjust = 3, label = paste(stock1, signif(m.indexes$value[1],3), sep = " : "), size = 5) +
      geom_text(x=-Inf, y = Inf, hjust=0, vjust = 3, label = paste(stock2, signif(m.indexes$value[2],3), sep = " : "), size = 5)
      
  })
  
  # Compare the means of stock twos and conduct a t-test 
  output$meanTestingStocks<-renderText({
    
    # Define stock 1 
    stock1 = input$Stock1
    
    # Define stock 2
    stock2 = input$Stock2
    
    s1 <- indexes %>% 
      select(stock1) %>%
      unlist(use.names = F)
    
    s2 <- indexes %>% 
      select(stock2) %>%
      unlist(use.names = F)
    
    ttest = t.test(s1, s2, conf.level= 1-input$Alpha)
    tstatistic = ttest$statistic
    tpvalue = signif(ttest$p.value,2) 
    
    answer <- if_else(tpvalue > input$Alpha, "No", "Yes")
    
    paste("<center>", "<h5>",
          "Are the means of indexes", "<b>",stock1, "</b>", "and", "<b>", stock2, "</b>", "statistically different", "<br>", 
          "based on your chosen alpha of", "<b>", input$Alpha, "</b>", "?", "</h5>", 
          "<h4>", answer, "because the p-value is", "<b>", tpvalue, "</b>", "</h4>", "<br>", sep = " ")
  })
  
  # Regression plot comparing stocks 
  output$compareRegressionPlot <- renderPlot({
    
    # Define stock 1 
    stock1 = input$Stock1
    
    # Define stock 2
    stock2 = input$Stock2
    
    # Calculate regression of one stock on the other
    fit <- lm(get(stock2) ~ get(stock1), data = indexes)
    indexes$predicted <- predict(fit)
    indexes$residuals <- residuals(fit)
    
    rsq <- round(summary(fit)$r.squared, 3)
    
    # User gives choice of regression or residuals
    reg.plot <- function(type = c('Regression', 'Residuals')) {
      if (type == 'Regression') {
        ggplot(indexes, aes(x = !!ensym(stock1), y = !!ensym(stock2))) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
          theme_bw() +
          geom_text(x=Inf,y=+Inf, hjust=2, vjust = 2, label = paste0("R2=", rsq), size = 7)
      } else if (type == 'Residuals'){
        ggplot(indexes, aes(x = !!ensym(stock1), y = !!ensym(stock2))) +
          geom_segment(aes(xend = !!ensym(stock1), yend = predicted), alpha = .1) +
          geom_point(aes(color = residuals)) +  # Color mapped here
          scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
          guides(color = FALSE) +
          geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
          theme_bw() +
          geom_text(x=Inf,y=+Inf, hjust=2, vjust = 2, label = paste0("R2=", rsq), size = 7)
      }
    }
    reg.plot(type = input$RegResStocks)
    
  })
  
  # Regression equation for 2 stocks 
  output$regressionEQ2Stocks <- renderText({
    
    # Define stock 1 
    stock1 = input$Stock1
    
    # Define stock 2
    stock2 = input$Stock2
    
    # Get estimate information from the linear model
    fit <- lm(get(stock2) ~ get(stock1), data = indexes)
    indexes$predicted <- predict(fit)
    indexes$residuals <- residuals(fit)
    fit <- tidy(fit)
    beta0 <- signif(fit[1,2],3)
    beta1 <- signif(fit[2,2],3)
    sign <- if_else(sign(beta1) == 1, "+", "-")
    
    
    paste("<center>", "<h5>", 
          "The regression of","<b>", stock2, "</b>", "on", "<b>", stock1, "</b>", "is modeled by", "</h5>", 
          "<h4>", stock2, "=", "<b>",  beta0, "</b>", sign, "<b>", abs(beta1), "</b>", "&#215", stock1, "</h4>", sep= ' ')
  })

  # Density plot to compare means of a stock
  # on one month versus another month 
  output$compareMonths <- renderPlot({
    
    #Define Stock and Months
    stock = input$Stock3
    
    month1 = input$Month1
    month2 = input$Month2
    
    # Convert Month variable depending on input
    if (month2 == "All Other Months") {
      
      indexes$month <- if_else(indexes$month == month1, month1, "other")
      indexes <- indexes %>% select(-DATE)
      
    } else { 
      
      months_view = c(month1, month2)
      
      indexes <- indexes %>%
        filter(month %in% months_view) %>%
        select(-DATE)
    }
    
    
    # Select stock(s) to view and plot
    if (stock == "All Stocks") {
      
      melt.indexes <- indexes %>% melt()
      
      avg.indexes <- melt.indexes %>%
        group_by(variable, month) %>%
        summarise_all(mean)
      
      ggplot(melt.indexes, aes(x=value, fill=month))+
        geom_density(alpha = 0.4, position='identity')+
        geom_vline(data=avg.indexes, aes(xintercept=value, color=month), linetype='dashed', size=1)+
        theme_bw() +
        ylab('Density') + 
        xlab('Value') + 
        theme(plot.title = element_text(hjust = 0.5, size = 20), plot.subtitle = element_text(hjust = 0.5)) + 
        facet_wrap(~variable, ncol=2)
      
    } else {
      
      indexes <- indexes %>% 
        select(stock, month)
      
      m.indexes <- indexes %>%
        group_by(month) %>%
        summarise_all(mean)
      
      
      ggplot(indexes, aes(x = !!ensym(stock), fill=month)) +
        geom_density(alpha=0.2, position="identity") +
        geom_vline(data=m.indexes, aes(xintercept=!!ensym(stock), colour=month), 
                   linetype='dashed', size=1) +
        ggtitle(label=stock, subtitle = "Month Comparison") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 30), plot.subtitle = element_text(hjust = 0.5, size = 15))+
        geom_text(x=+Inf, y = Inf, hjust=1, vjust = 3, label = paste(month1, signif(m.indexes[1,2],3), sep = " : "), size = 5) +
        geom_text(x=-Inf, y = Inf, hjust=0, vjust = 3, label = paste(month2, signif(m.indexes[2,2],3), sep = " : "), size = 5)
    }
    
  })
  
  ########
  # Make this an output of whether or not the means are 
  # different for the selected months/the means/ 
  # a list of which stocks do have significant 
  # differences for a particular month 
  #####
  output$meanTestingMonths<-renderText({
    
    # Define the stock 
    stock = input$Stock3
    
    # Choose the months to compare 
    month1 = input$Month1
    month2 = input$Month2
    
    # Compare all stocks 
    if (stock == "All Stocks") {
      
      paste("<center>", "<h4>", "See table for the means and p-values for every comparison above")
      
    # Conduct no comparison if months are the same 
    } else if (month1 == month2) {
      return(NULL)
      
    # Comp
    } else if (month2 == "All Other Months") {
      indexes$month <- if_else(indexes$month == month1, month1, "other")
      
      m1 <- indexes %>% 
        filter(month == month1) %>%
        select(stock) %>%
        unlist(use.names = F)
      
      
      m2 <- indexes %>%
        filter(month == "other") %>%
        select(stock) %>%
        unlist(use.names = F)
      
      # t-test on the means of the selected month versus all the rest
      ttest = t.test(m1, m2, conf.level= 1-input$Alpha, alternative = "greater")
      tstatistic = ttest$statistic
      tpvalue = signif(ttest$p.value,2) 
      
      answer <- if_else(tpvalue > input$Alpha, "No", "Yes")
      
      paste("<center>", "<h5>", "Are the mean log returns for", "<b>", stock, "</b>", "statistically higher in", "<br>",
            "<b>", month1, "</b>", "as compared to", "<b>", month2, "</b>", "<br>",
            "based on your chosen alpha of", "<b>", input$Alpha, "</b>","?", "</h5>",
            "<h4>", answer, "because the p-value is", "<b>", tpvalue, "</b>", "</h4>", sep = " ")
  
    } else {
      
      m1 <- indexes %>% 
        filter(month == month1) %>%
        select(stock) %>%
        unlist(use.names = F)
      
      m2 <- indexes %>% 
        filter(month == month2) %>%
        select(stock) %>%
        unlist(use.names = F)
      
      # t-test on the means of the two selected months
      ttest = t.test(m1, m2, conf.level= 1-input$Alpha, alternative = "greater")
      tstatistic = ttest$statistic
      tpvalue = signif(ttest$p.value,2) 
      
      answer <- if_else(tpvalue > input$Alpha, "No", "Yes")
      
      paste("<center>", "<h5>", "Are the mean log returns for", "<b>", stock, "</b>", "statistically higher in", "<br>",
            "<b>", month1, "</b>", "as compared to", "<b>", month2, "</b>", "<br>",
            "based on your chosen alpha of", "<b>", input$Alpha, "</b>","?", "</h5>",
            "<h4>", answer, "because the p-value is", "<b>", tpvalue, "</b>", "</h4>", sep = " ")
    }
  })
  
  # Display the means of one month versus the means of 
  # all the other months with the p-values 
  output$meanTestingMonthsTable<-renderTable({
    
    stock = input$Stock3
    month1 = input$Month1
    month2 = input$Month2
    
    
    info.table <- c('Ticker', paste(month1, "vs", month2, sep = " "), 'Signifcant', 'P-value')
    
    # Convert Month variable depending on input
    if (month1 == month2) {
      return(NULL) #only print anything if looking at different months
      
    } else if (month2 == "All Other Months") {
      indexes$month <- if_else(indexes$month == month1, month1, "other")
      indexes <- indexes %>% select(-DATE)
      
    } else { 
      months_in_view <- c(month1, month2)
      indexes <- indexes %>%
        filter(month %in% months_in_view) %>%
        select(-DATE)
    }
    
    if (stock == "All Stocks") {
      
      print(colnames(indexes))
      for (i in 1:10) {
        tick <- colnames(indexes)[i]
        m1 <- indexes %>% 
          filter(month == month1) %>%
          select(tick) %>%
          unlist(use.names = F)
        
        m2 <- indexes %>% 
          filter(month != month1) %>%
          select(tick) %>%
          unlist(use.names = F)
        
        # t-test on the means of the two selected months
        ttest = t.test(m1, m2, conf.level= 1-input$Alpha)
        tstatistic = ttest$statistic
        tpvalue = signif(ttest$p.value,2) 
        
        answer <- if_else(tpvalue > input$Alpha, "No", "Yes")
        
        info <- c(tick, paste(signif(mean(m1),2), "vs", signif(mean(m2),2), sep = " "), answer, tpvalue)
        
        info.table <- rbind(info.table, info)
        
      }
      
    } else {
     return(NULL) #don't show table unless viewing all stocks
    }
    
    
    # Log all values into data frame to display as table 
    colnames(info.table) <- c('Ticker', paste(month1, "vs", month2, sep = " "), 'Signifcant', 'P-value')
    info.table <- info.table[-1,]
    
  },
  width = "100%",
  digits = 4, 
  striped = TRUE) 
  
  output$monthQQ <- renderPlot({
    
    stock = input$Stock3
    month1 = input$Month1
    month2 = input$Month2
    
    if (stock == "All Stocks") {
      return(NULL)
    }
    
    indexes <- indexes %>%
      select(stock, month) %>% 
      filter(month == month1)
    
    if (month1 == month2) {

      # Print QQ Plot
      ggplot(indexes, aes(sample = !!ensym(stock))) +
        stat_qq() + stat_qq_line() +
        ggtitle(label="QQ Plot") +
        ylab('Density')
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size = 15))
      
    } else {
      return(NULL)
    }
    
  })
  
  # Table displaying all stocks we used for data analysis 
  output$stockIndex<-renderTable({
    
    indexes <- indexes[,2:11]
    
    # Log all values into data frame to display as table 
    tibble("Stock Index Ticker" = colnames(indexes),
               "Stock Index Name" = c("Dow Jones Industrial Average","FTSE 100","DAX","S&P 500","Hang Seng Index","NASDAQ Composite","Nikkei 225","Russell 2000","Strait Times Index","Euro Stoxx 50"),
               "Stock Market Location" = c('USA','United Kingdom','Germany','USA','Hong Kong','USA','Japan','USA','Singapore','Eurozone'))
    
  }
  , 
  width = "100%",
  digits = 4, 
  striped = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

