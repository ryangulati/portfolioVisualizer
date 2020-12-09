library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(readr)
library(purrr)
library(registry)
library(codetools)
library(iterators)
library(lattice)
library(methods)
library(MarkowitzR)
library(markdown)
library(lmtest)
library(graphics)
library(grDevices)
library(ggplot.multistats)
library(foreach)
library(dbplyr)
library(base)
library(highcharter)
library(tidyquant)
library(timetk)
library(shiny)
library(scales)
library(PortfolioAnalytics)
library(frontier)
library(fPortfolio)
library(forcats)
library(fAssets)
library(caTools)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(PerformanceAnalytics)
library(pkgconfig)
library(pkgbuild)
library(pkgload)
library(quadprog)
library(quantmod)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(slam)
library(timeSeries)
library(timeDate)
library(TTR)
library(tseries)
library(utils)
library(timeDate)
library(tibble)
library(splines)
library(xts)
library(zoo)


ui <- shinyUI( dashboardPage(
    dashboardHeader(title = "portfolioAnalytics"),
    dashboardSidebar(

fluidRow(
    column(6,
           textInput("stock1", "Stock 1", "SPY")),
    column(6,
           numericInput("w1", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock2", "Stock 2", "AMZN")),
    column(6,
           numericInput("w2", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock3", "Stock 3", "TSLA")),
    column(6,
           numericInput("w3", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock4", "Stock 4", "BAC")),
    column(6,
           numericInput("w4", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock5", "Stock 5", "JPM")),
    column(6,
           numericInput("w5", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock6", "Stock 6", "BA")),
    column(6,
           numericInput("w6", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock7", "Stock 7", "QQQ")),
    column(6,
           numericInput("w7", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock8", "Stock 8", "T")),
    column(6,
           numericInput("w8", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock9", "Stock 9", "V")),
    column(6,
           numericInput("w9", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock10", "Stock 10", "GS")),
    column(6,
           numericInput("w10", "Portf. %", 2, min = 0, max = 100))
),





fluidRow(
    column(6,
           textInput("stock11", "Stock 11", "CRM")),
    column(6,
           numericInput("w11", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock12", "Stock 12", "IWM")),
    column(6,
           numericInput("w12", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock13", "Stock 13", "PG")),
    column(6,
           numericInput("w13", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock14", "Stock 14", "PNC")),
    column(6,
           numericInput("w14", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock15", "Stock 15", "AVGO")),
    column(6,
           numericInput("w15", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock16", "Stock 16", "ULTA")),
    column(6,
           numericInput("w16", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock17", "Stock 17", "NKE")),
    column(6,
           numericInput("w17", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock18", "Stock 18", "NVDA")),
    column(6,
           numericInput("w18", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock19", "Stock 19", "URI")),
    column(6,
           numericInput("w19", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock20", "Stock 20", "CTAS")),
    column(6,
           numericInput("w20", "Portf. %", 2, min = 0, max = 100))
),





fluidRow(
    column(6,
           textInput("stock21", "Stock 21", "ADBE")),
    column(6,
           numericInput("w21", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock22", "Stock 22", "MSFT")),
    column(6,
           numericInput("w22", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock23", "Stock 23", "AAPL")),
    column(6,
           numericInput("w23", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock24", "Stock 24", "FB")),
    column(6,
           numericInput("w24", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock25", "Stock 25", "KO")),
    column(6,
           numericInput("w25", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock26", "Stock 26", "PEP")),
    column(6,
           numericInput("w26", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock27", "Stock 27", "PFE")),
    column(6,
           numericInput("w27", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock28", "Stock 28", "INTC")),
    column(6,
           numericInput("w28", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock29", "Stock 29", "AXP")),
    column(6,
           numericInput("w29", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock30", "Stock 30", "STZ")),
    column(6,
           numericInput("w30", "Portf. %", 2, min = 0, max = 100))
),




fluidRow(
    column(6,
           textInput("stock31", "Stock 31", "MA")),
    column(6,
           numericInput("w31", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock32", "Stock 32", "DPZ")),
    column(6,
           numericInput("w32", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock33", "Stock 33", "WTRG")),
    column(6,
           numericInput("w33", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock34", "Stock 34", "WW")),
    column(6,
           numericInput("w34", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock35", "Stock 35", "TRIP")),
    column(6,
           numericInput("w35", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock36", "Stock 36", "AMD")),
    column(6,
           numericInput("w36", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock37", "Stock 37", "ORCL")),
    column(6,
           numericInput("w37", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock38", "Stock 38", "TXRH")),
    column(6,
           numericInput("w38", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock39", "Stock 39", "WEN")),
    column(6,
           numericInput("w39", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock40", "Stock 40", "MCD")),
    column(6,
           numericInput("w40", "Portf. %", 2, min = 0, max = 100))
),






fluidRow(
    column(6,
           textInput("stock41", "Stock 41", "O")),
    column(6,
           numericInput("w41", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock42", "Stock 42", "YUM")),
    column(6,
           numericInput("w42", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock43", "Stock 43", "NOW")),
    column(6,
           numericInput("w43", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock44", "Stock 44", "DIS")),
    column(6,
           numericInput("w44", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock45", "Stock 45", "TER")),
    column(6,
           numericInput("w45", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock46", "Stock 46", "WMT")),
    column(6,
           numericInput("w46", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock47", "Stock 47", "COST")),
    column(6,
           numericInput("w47", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock48", "Stock 48", "SAM")),
    column(6,
           numericInput("w48", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock49", "Stock 49", "WYND")),
    column(6,
           numericInput("w49", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(6,
           textInput("stock50", "Stock 50", "ALB")),
    column(6,
           numericInput("w50", "Portf. %", 2, min = 0, max = 100))
),

fluidRow(
    column(8,
           dateInput("date", "Starting Date", "2015-01-01", format = "yyyy-mm-dd"))
    ),

# fluidRow(
#     column(7,
#            selectInput("rebalance", "rebalance freq",
#                        c("Yearly" = "years",
#                          "Monthly" = "months",
#                          "Weekly" = "weeks"))
#     ) ), 

fluidRow(
column(7,
       numericInput("portsnum", "No. of Portf", 25, min = 1)
    )),

actionButton("go", "Submit")


), #sidebar closer 

dashboardBody(
    
    fluidPage(
        fluidRow(
        tabBox(
            title = "Portfolio Analysis",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset1", height = "500px", width = "250px",
            tabPanel("Returns", box(highchartOutput("returnsPlot"), width = 12)),
            tabPanel("Efficient Frontier", (box(plotOutput("efPlot"), width = 12))),
            tabPanel("Plot2", (box(plotOutput("efPlot2"), width = 12))),
            tabPanel("Frontier Results", (box(verbatimTextOutput("efBox"), width = 12)))
        ), 
                ), #fluidrow closer
        fluidRow(
            # print("Sharpe Ratio of this portfolio is: "),
            valueBoxOutput("portfSharpe", width = 12)
            # valueBoxOutput("marketSharpe", width = 5)
        )
        
        ) #fluidpage closer
        
    # fluidPage(
    #             h4("Returns"),
    #             box(highchartOutput("returnsPlot"), width = 12)
    #          ),
    # fluidPage(
    #     h4("Efficient Frontier"),
    #     box(plotOutput("efPlot"), width = 12)
    # ),
    # fluidPage(
    #     h4("Frontier Results"),
    #     box(verbatimTextOutput("efBox"), width = 12)
    # )
    
),
tags$head(tags$style(HTML("
     .sidebar {
        height: 90vh; overflow-y: auto;
                            }
                           ")))
))




server <- function(input, output) {
    
    
    
    prices <- eventReactive(input$go, {
                        
    symbols <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5, input$stock6, input$stock7, input$stock8, input$stock9, input$stock10,
                 input$stock11, input$stock12, input$stock13, input$stock14, input$stock15, input$stock16, input$stock17, input$stock18, input$stock19, input$stock20,
                 input$stock21, input$stock22, input$stock23, input$stock24, input$stock25, input$stock26, input$stock27, input$stock28, input$stock29, input$stock30,
                 input$stock31, input$stock32, input$stock33, input$stock34, input$stock35, input$stock36, input$stock37, input$stock38, input$stock39, input$stock40,
                 input$stock41, input$stock42, input$stock43, input$stock44, input$stock45, input$stock46, input$stock47, input$stock48, input$stock49, input$stock50)
    
    prices <- getSymbols(symbols, src = 'yahoo', from = input$date, 
                         auto.assign = TRUE, warnings = FALSE) %>% 
        # map(~Ad(get(.))) %>% 
        # reduce(merge) %>%
        # `colnames<-`(symbols)
    
    map(~Ad(get(.))) %>% 
        reduce(merge) %>%
        `colnames<-`(symbols) %>% 
        to.monthly(indexAt = "lastof", 
                   OHLC = FALSE) %>% 
        Return.calculate(method = "log") %>% 
        na.omit()  
                                        
    }) 
    
    portfolio_growth_xts <- eventReactive(input$go, {
        
        prices <- prices()
        
        validate(need(input$w1 + input$w2 + input$w3 + input$w4 + input$w5 + input$w6 + input$w7 + input$w8 + input$w9 + input$w10 + 
                      input$w11 + input$w12 + input$w13 + input$w14 + input$w15 + input$w16 + input$w17 + input$w18 + input$w19 + input$w20 +
                          input$w21 + input$w22 + input$w23 + input$w24 + input$w25 + input$w26 + input$w27 + input$w28 + input$w29 + input$w30 +
                          input$w31 + input$w32 + input$w33 + input$w34 + input$w35 + input$w36 + input$w37 + input$w38 + input$w39 + input$w40 +
                          input$w41 + input$w42 + input$w43 + input$w44 + input$w45 + input$w46 + input$w47 + input$w48 + input$w49 + input$w50 == 100, 
                      "The portfolio weights must sum to 100%!"))
        
        w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100, input$w5/100, input$w6/100, input$w7/100, input$w8/100, input$w9/100, input$w10/100,
               input$w11/100, input$w12/100, input$w13/100, input$w14/100, input$w15/100, input$w16/100, input$w17/100, input$w18/100, input$w19/100, input$w20/100,
               input$w21/100, input$w22/100, input$w23/100, input$w24/100, input$w25/100, input$w26/100, input$w27/100, input$w28/100, input$w29/100, input$w30/100,
               input$w31/100, input$w32/100, input$w33/100, input$w34/100, input$w35/100, input$w36/100, input$w37/100, input$w38/100, input$w39/100, input$w40/100,
               input$w41/100, input$w42/100, input$w43/100, input$w44/100, input$w45/100, input$w46/100, input$w47/100, input$w48/100, input$w49/100, input$w50/100)
        
        # prices_monthly <- to.monthly(prices,indexAt = "last",OHLC = FALSE)
        # asset_returns_xts <- na.omit(Return.calculate(prices_monthly), method = "log")
        
        portfolio_growth_xts <- 
            Return.portfolio(prices, 
                             wealth.index = 1, 
                             weights = w )  %>% 
                             # rebalance_on = input$rebalance) %>% 
            `colnames<-`("growth")
        
    })
    
    
    
     market_growth_xts <- eventReactive(input$go, {
            

    # sp500prices <- getSymbols.yahoo("SPY", from=input$date, periodicity = 'daily', auto.assign=FALSE)[,4]
    # sp500Rets <- na.omit(ROC(sp500prices))
    # market_growth_xts <- as.xts(sp500Rets)
        prices <- getSymbols("SPY", src = 'yahoo',
                   from = input$date,
                   auto.assign = TRUE,
                   warnings = FALSE) %>%
            map(~Ad(get(.))) %>%
            reduce(merge) %>%
            `colnames<-`("SPY") %>% 
            to.monthly(indexAt = "lastof", 
                       OHLC = FALSE) %>% 
            Return.calculate(method = "log") %>% 
            na.omit()  

        # prices_daily <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
        # market_returns_xts <- na.omit(Return.calculate(prices_daily), method = "log")

        market_growth_xts <- Return.portfolio(prices, wealth.index = 1) %>%
            `colnames<-`("growth")


     })
    
    
    
    portfolio_sharpe <- eventReactive(input$go, {
        portfolio_growth_xts <- portfolio_growth_xts()
        
            portfolio_sharpes <-SharpeRatio(portfolio_growth_xts,
                    Rf = 0, 
                    FUN = "StdDev")
            
            portfolio_sharpe <-  portfolio_sharpes*.5
        
    })
    market_returns <- eventReactive(input$go, {
        
        getSymbols("SPY", src = 'yahoo', 
                   from = input$date, 
                   auto.assign = TRUE, 
                   warnings = FALSE) %>% 
            map(~Ad(get(.))) %>% 
            reduce(merge) %>%
            `colnames<-`("SPY") %>% 
            to.monthly(indexAt = "lastof", 
                       OHLC = FALSE) %>% 
            Return.calculate(method = "log") %>% 
            na.omit()  
    })
    
    # market_sharpe <- eventReactive(input$go, {
    #     market_returns <- market_returns()
    # 
    #     market_sharpe <- SharpeRatio(market_returns(),
    #                 Rf = 0,
    #                 FUN = "StdDev")
    # })

    
    
    #original portfolio analytics frontier
    efficientFront <- eventReactive(input$go, {
        # tickers <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5, input$stock6, input$stock7, input$stock8, input$stock9, input$stock10)
        # 
        # portfolioPrices <- NULL
        # for(ticker in tickers) {
        #     portfolioPrices <- cbind(portfolioPrices,
        #                              getSymbols.yahoo(ticker, from=input$date, periodicity = 'monthly', auto.assign=FALSE)[,4])
        # }
        # 
        # portfolioReturns <- na.omit(ROC(portfolioPrices))
        # 
        # portf <- portfolio.spec(colnames(portfolioReturns))
        # 
        
        
        prices <- prices()
        #Efficient Frontier 
    # prices_daily <- to.monthly(prices, indexAt = "last", OHLC = FALSE)
    # portfolioReturns <- na.omit(ROC(prices_daily))
    portf <- portfolio.spec(colnames(prices))
    portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1) #full investment
    portf <- add.constraint(portf, type="box", min=.01, max=.99) #long only
    # portf <- add.constraint(portf, type = "diversification")
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev")
    optPort <- optimize.portfolio(prices, portf, optimize_method = "ROI", maxSR=TRUE, trace=TRUE)
    ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = input$portsnum,
                                   risk_aversion = NULL)


    
})
    
efPrint <- eventReactive(input$go, {
    
    prices <- prices()

    portf <- portfolio.spec(colnames(prices))
    portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
    portf <- add.constraint(portf, type="box", min=0.01, max=1)
    # portf <- add.constraint(portf, type = "diversification")
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev")
    optPort <- optimize.portfolio(prices, portf, optimize_method = "ROI", trace=TRUE)

    
})
    



















    # Output Plots
    
    output$returnsPlot <- renderHighchart({
        highchart(type = "stock") %>%
            hc_title(text = "Portfolio Returns") %>%
            hc_add_series(portfolio_growth_xts(), name = "Portfolio Growth", color = "cornflowerblue") %>%
            hc_add_series(market_growth_xts(), name = "SPY Growth", color = "green") %>%
            hc_navigator(enabled = FALSE) %>% 
            hc_scrollbar(enabled = FALSE) %>% 
            hc_exporting(enabled = TRUE) %>%
            hc_legend(enabled = TRUE, align = "right", verticalAlign = "middle",
                  layout = "vertical")
        
    })
    
    output$efPlot <- renderPlot({
        efficientFront() %>% 
        chart.EfficientFrontier(efficientFront()$ef,
                                match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                                cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                                RAR.text = "Risk adj Return", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                                chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                                cex.assets = 0.8)

    })
    
    
    output$efPlot2 <- renderPlot({
        efficientFront() %>%
            chart.EfficientFrontier(efficientFront()$ef,
                                    match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                                    cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                                    RAR.text = "Risk adj Return", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                                    chart.assets = FALSE, labels.assets = FALSE, pch.assets = 21,
                                    cex.assets = 0.8)

    })

    
    output$efBox <- renderPrint({
        efPrint() %>%
             paste( print(efPrint()$optPort.weights))
    })
    
    output$portfSharpe <- renderValueBox({
        portfolio_sharpe() %>%
            valueBox(value = portfolio_sharpe(), 
                     icon("arrow-up"),
                     color = "blue") 
    })
    
    # output$marketSharpe <- renderValueBox({
    #     market_sharpe() %>%
    #         valueBox(value = round(market_sharpe(), 4), 
    #                  icon("arrow-down"),
    #                  color = "red") 
    # })

    
}

shinyApp(ui = ui, server = server)

