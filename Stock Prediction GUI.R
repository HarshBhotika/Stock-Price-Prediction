library(shinydashboard)
library(forecast)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"


sidebar <- dashboardSidebar(
  sidebarSearchForm(label = "Search...", "searchText", "searchButton"),
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            
            # Boxes with solid headers
            fluidRow(
              
              box(
                title = "Enter Stock Ticker", width = 4, solidHeader = TRUE, status = "primary",
                textInput("StockCode", "Stock Symbol", value = ""),
                actionButton(inputId = "click", label = "Predict")
              ),
              
              box(
                title = "Date Range",
                tableOutput("date.range"), width = 4, 
                height = 125
              ),
              
              box(
                title = "% Error in Prediction",
                status = "primary",
                tableOutput("error.stock"), width = 4, 
                height = 125
              )
              
            ),
            
            fluidRow(
              
              box(
                title = "Arima plot of Stock Prices",
                status = "primary",
                plotOutput("stock.arima", height = 700),
                height = 800
              ),
              box(
                title = "Prices (First 20 days Predicted)",
                tableOutput("arima.table"),
                height = 800, width = 3
              ),
              box(
                title = "Prices (Last 20 days Predicted)",
                tableOutput("arima.table1"),
                height = 800, width = 3
              )
              
            ),
            
          
            fluidRow(
              
              box(
                title = "Box Plot Monthly",
                status = "primary",
                plotOutput("box.month", height = 700),
                height = 800
              ),
              
              box(
                title = "Box Plot Yearly",
                status = "primary",
                plotOutput("box.year", height = 700),
                height = 800
              )
              
            )
            
    )
  )
)


header <- dashboardHeader(
  title = "Harsh's App"
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) {
  
    output$date.range <- renderTable({
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    ticker <- as.character(data())
    print(ticker)
    library(quantmod)
    
    stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
    View(stock_name)
    class(stock_name)
    stock <- get(ticker)
    View(stock)
    class(stock)
    names(stock)
    names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    View(stock)
    class(stock)
    
    #Format the Date column to get a Date column
    stock <- as.data.frame(stock)
    View(stock)
    class(stock)
    
    stock$Date <- row.names(stock)
    View(stock)
    cln <- ncol(stock) # 7
    stock <- stock[, c(cln, 1:(cln-1))]
    row.names(stock) <- NULL
    head(stock)
    View(stock)
    class(stock)
    class(stock$Date)
    class(stock$stock.Adjusted)
    
    start_date <- min(stock$Date)
    View(start_date)
    end_date <- max(stock$Date)
    date_range <- paste(start_date, "to", end_date)
    View(date_range)
    range <- as.data.frame(date_range)
    colnames(range) <- "Range"
    (range)
  })
  
    output$error.stock <- renderTable({
      data <- eventReactive(input$click, {
        (input$StockCode) 
      })
      ticker <- as.character(data())
      print(ticker)
      library(quantmod)
      
      stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
      View(stock_name)
      class(stock_name)
      stock <- get(ticker)
      View(stock)
      class(stock)
      names(stock)
      names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
      View(stock)
      class(stock)
      
      #Format the Date column to get a Date column
      stock <- as.data.frame(stock)
      View(stock)
      class(stock)
      
      stock$Date <- row.names(stock)
      View(stock)
      cln <- ncol(stock) # 7
      stock <- stock[, c(cln, 1:(cln-1))]
      row.names(stock) <- NULL
      head(stock)
      View(stock)
      class(stock)
      class(stock$Date)
      class(stock$stock.Adjusted)
      pricearima <- ts(log(stock$stock.Adjusted), start = c(1,02),frequency = 1)
      View(pricearima)
      fitlnstock <- auto.arima(pricearima)
      fitlnstock
      View(fitlnstock)
      #plot(pricearima, type="l", main="Comparison of Fitted Prices")
      #lines(fitted(fitlnstock), col="red")
      
      forecasted <- forecast(fitlnstock, h=100)
      #forecasted
      View(forecasted)
      #plot(forecasted, xlab="Days")
      forecastedvalues <- as.numeric(forecasted$mean)
      finalvalues <- exp(forecastedvalues)
      #finalvalues
      
      df <- getSymbols(ticker,src = 'yahoo', from='2018-01-01', to='2018-05-25')
      stock_pred <- get(ticker)
      View(stock_pred)
      names(stock_pred) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
      
      df<-data.frame(stock_pred$stock.Adjusted,finalvalues)
      col_headings<-c("Actual Price","Forecasted Price")
      names(df)<-col_headings
      attach(df)
      View(df)
      
      percentage_error=((df$`Actual Price`-df$`Forecasted Price`)/(df$`Actual Price`))
      #percentage_error
      Prediction_Error <- round((mean(percentage_error)*100),digits = 2)
      error <- as.data.frame(Prediction_Error)
      colnames(error) <- "Error"
      (error)
      
    })
    
    
    output$stock.arima <- renderPlot({
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    ticker <- as.character(data())
    print(ticker)
    library(quantmod)
    
    stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
    View(stock_name)
    class(stock_name)
    stock <- get(ticker)
    View(stock)
    class(stock)
    names(stock)
    names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    View(stock)
    class(stock)
    
    #Format the Date column to get a Date column
    stock <- as.data.frame(stock)
    View(stock)
    class(stock)
    
    stock$Date <- row.names(stock)
    View(stock)
    cln <- ncol(stock) # 7
    stock <- stock[, c(cln, 1:(cln-1))]
    row.names(stock) <- NULL
    head(stock)
    View(stock)
    class(stock)
    class(stock$Date)
    class(stock$stock.Adjusted)
    stock$Date <- as.Date(stock$Date)
    
    pricearima <- ts(log(stock$stock.Adjusted), start = c(1,02),frequency = 1)
    View(pricearima)
    fitlnstock <- auto.arima(pricearima)
    fitlnstock
    View(fitlnstock)
    #plot(pricearima, type="l", main="Comparison of Fitted Prices")
    #lines(fitted(fitlnstock), col="red")
    
    forecasted <- forecast(fitlnstock, h=100)
    forecasted
    View(forecasted)
    plot(forecasted, xlab="Days")
    
  })

  output$arima.table <- renderTable({
    
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    ticker <- as.character(data())
    print(ticker)
    library(quantmod)
    
    stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
    View(stock_name)
    class(stock_name)
    stock <- get(ticker)
    View(stock)
    class(stock)
    names(stock)
    names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    View(stock)
    class(stock)
    
    #Format the Date column to get a Date column
    stock <- as.data.frame(stock)
    View(stock)
    class(stock)
    
    stock$Date <- row.names(stock)
    View(stock)
    cln <- ncol(stock) # 7
    stock <- stock[, c(cln, 1:(cln-1))]
    row.names(stock) <- NULL
    head(stock)
    View(stock)
    class(stock)
    class(stock$Date)
    class(stock$stock.Adjusted)
    stock$Date <- as.Date(stock$Date)
    
    pricearima <- ts(log(stock$stock.Adjusted), start = c(1,02),frequency = 1)
    View(pricearima)
    fitlnstock <- auto.arima(pricearima)
    fitlnstock
    View(fitlnstock)
    #plot(pricearima, type="l", main="Comparison of Fitted Prices")
    #lines(fitted(fitlnstock), col="red")
    
    forecasted <- forecast(fitlnstock, h=100)
    forecasted
    View(forecasted)
    #plot(forecasted, xlab="Days")
    
    forecastedvalues <- as.numeric(forecasted$mean)
    finalvalues <- exp(forecastedvalues)
    #finalvalues
    
    df <- getSymbols(ticker,src = 'yahoo', from='2018-01-01', to='2018-05-25')
    stock_pred <- get(ticker)
    View(stock_pred)
    names(stock_pred) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    
    df<-data.frame(stock_pred$stock.Adjusted,finalvalues)
    col_headings<-c("Actual Price","Forecasted Price")
    names(df)<-col_headings
    attach(df)
    View(df)
    
    stock1 <- as.data.frame(df)
    View(stock1)
    class(stock1)
    
    stock1$Date <- row.names(stock1)
    View(stock1)
    cln <- ncol(stock1) # 3
    stock1 <- stock1[, c(cln, 1:(cln-1))]
    row.names(stock1) <- NULL
    head(stock1)
    View(stock1)
    class(stock1)
    Display_prices<- stock1[1:20,]
    (Display_prices)
  })
  
  output$arima.table1 <- renderTable({
    
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    ticker <- as.character(data())
    print(ticker)
    library(quantmod)
    
    stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
    View(stock_name)
    class(stock_name)
    stock <- get(ticker)
    View(stock)
    class(stock)
    names(stock)
    names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    View(stock)
    class(stock)
    
    #Format the Date column to get a Date column
    stock <- as.data.frame(stock)
    View(stock)
    class(stock)
    
    stock$Date <- row.names(stock)
    View(stock)
    cln <- ncol(stock) # 7
    stock <- stock[, c(cln, 1:(cln-1))]
    row.names(stock) <- NULL
    head(stock)
    View(stock)
    class(stock)
    class(stock$Date)
    class(stock$stock.Adjusted)
    stock$Date <- as.Date(stock$Date)
    
    pricearima <- ts(log(stock$stock.Adjusted), start = c(1,02),frequency = 1)
    View(pricearima)
    fitlnstock <- auto.arima(pricearima)
    fitlnstock
    View(fitlnstock)
    #plot(pricearima, type="l", main="Comparison of Fitted Prices")
    #lines(fitted(fitlnstock), col="red")
    
    forecasted <- forecast(fitlnstock, h=100)
    forecasted
    View(forecasted)
    #plot(forecasted, xlab="Days")
    
    forecastedvalues <- as.numeric(forecasted$mean)
    finalvalues <- exp(forecastedvalues)
    #finalvalues
    
    df <- getSymbols(ticker,src = 'yahoo', from='2018-01-01', to='2018-05-25')
    stock_pred <- get(ticker)
    View(stock_pred)
    names(stock_pred) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    
    df<-data.frame(stock_pred$stock.Adjusted,finalvalues)
    col_headings<-c("Actual Price","Forecasted Price")
    names(df)<-col_headings
    attach(df)
    View(df)
    
    stock1 <- as.data.frame(df)
    View(stock1)
    class(stock1)
    
    stock1$Date <- row.names(stock1)
    View(stock1)
    cln <- ncol(stock1) # 3
    stock1 <- stock1[, c(cln, 1:(cln-1))]
    row.names(stock1) <- NULL
    head(stock1)
    View(stock1)
    class(stock1)
    Display_prices<- stock1[80:100,]
    (Display_prices)
  })
  
  output$box.month <- renderPlot({
    
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    ticker <- as.character(data())
    print(ticker)
    library(quantmod)
    
    stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
    View(stock_name)
    class(stock_name)
    stock <- get(ticker)
    View(stock)
    class(stock)
    names(stock)
    names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    View(stock)
    class(stock)
    
    #Format the Date column to get a Date column
    stock <- as.data.frame(stock)
    View(stock)
    class(stock)
    
    stock$Date <- row.names(stock)
    View(stock)
    cln <- ncol(stock) # 7
    stock <- stock[, c(cln, 1:(cln-1))]
    row.names(stock) <- NULL
    head(stock)
    View(stock)
    class(stock)
    class(stock$Date)
    class(stock$stock.Adjusted)
    stock$Date <- as.Date(stock$Date)
    
    
    #Aggregate data by Month
    library(lubridate)
    bymonth <- aggregate(cbind(stock.Adjusted)~month(Date),data = stock, FUN = mean)
    class(bymonth$`month(Date)`)
    colnames(bymonth)[colnames(bymonth)=="month(Date)"] <- "Month"
    View(bymonth)
    
    #Name months
    bymonth$`Month`[1:12] <- c("January","February","March","April","May","June","July","August",
                               "September","October","November","December")
    View(bymonth)
    
    #Boxplot by Month
    boxplot(stock$stock.Adjusted ~ month(stock$Date), data = stock, col="Yellow", staplewex = 1, names=bymonth$Month, ylab="Stock Price",
            xlab="Month", main="Monthly Stock Prices")
    bp <- boxplot(stock$stock.Adjusted ~ month(stock$Date), data = stock, col="Yellow", staplewex = 1, names=bymonth$Month,
                  ylab=" Adjusted Stock Price", xlab="Month", main= stock_name, col.main="blue")
    bp$stats
    bp$stats<- round(bp$stats, digits = 2)
    text(x = col(bp$stats) - .5, y = bp$stats, labels = bp$stats)
    
  })
  
  output$box.year <- renderPlot({
    
    data <- eventReactive(input$click, {
      (input$StockCode) 
    })
    ticker <- as.character(data())
    print(ticker)
    library(quantmod)
    
    stock_name <- getSymbols(ticker,src = 'yahoo', from='2013-01-01', to='2017-12-31')
    View(stock_name)
    class(stock_name)
    stock <- get(ticker)
    View(stock)
    class(stock)
    names(stock)
    names(stock) <- c("stock.Open","stock.High","stock.Low","stock.Close","stock.Volume","stock.Adjusted")
    View(stock)
    class(stock)
    
    #Format the Date column to get a Date column
    stock <- as.data.frame(stock)
    View(stock)
    class(stock)
    
    stock$Date <- row.names(stock)
    View(stock)
    cln <- ncol(stock) # 7
    stock <- stock[, c(cln, 1:(cln-1))]
    row.names(stock) <- NULL
    head(stock)
    View(stock)
    class(stock)
    class(stock$Date)
    class(stock$stock.Adjusted)
    stock$Date <- as.Date(stock$Date)
    
    boxplot(stock$stock.Adjusted ~ year(stock$Date), data = stock, col="Green", staplewex = 1)
    bp <- boxplot(stock$stock.Adjusted ~ year(stock$Date), data = stock, col="Green", staplewex = 1, ylab="Adjusted Stock Price",
                  xlab="Year", main= stock_name, col.main="blue")
    bp$stats
    bp$stats<- round(bp$stats, digits = 2)
    text(x = col(bp$stats) - .5, y = bp$stats, labels = bp$stats)
    
  })
  
}
shinyApp(ui, server)