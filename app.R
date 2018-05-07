library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(httr)
library(htmltab)
library(Quandl)
library(padr)

#### Formulas:
pct <- function(x) (x/lead(x)) - 1
value <- function(buy, sell, inv) (inv * sell / buy)

#### API: Top 10 Coins Load, Convert Market Cap to Billions
api <- GET("https://api.coinmarketcap.com/v1/ticker/?limit=10")
top <- as.tbl(fromJSON(content(api, "text", encoding = "UTF-8")))
top <- top %>%
  rename(mktcap = market_cap_usd) %>% 
  mutate(mktcap = sub("*\\.[0-9]", "", mktcap)) %>% 
  mutate(mktcap = substr(mktcap, 1, nchar(mktcap) - 3)) %>% 
  mutate(mktcap = as.double(mktcap) / 1000000)

rm(api)

#### Load Crypto Data
url1 <- "https://coinmarketcap.com/currencies/"
url2 <- "/historical-data/?start=20130428&end="
today <- str_replace_all(Sys.Date(), "-", "")

N <- 10

for (i in 1:N){
  url <- str_c(url1, top$id[i], url2, today, sep = "")
  temp <- htmltab(doc = url, which = 1)
  temp <- temp %>% 
    mutate(Close = as.double(Close)) %>%
    mutate(PctChange = pct(Close) * 100) %>% 
    mutate(Coin = top$symbol[i])
  
  if (i == 1) {
    data <- temp
  } else {
    data <- rbind.data.frame(data, temp)
  }
  
  if (i == N) {
    rm(temp, url1, url2, today)
  }
}

#### Load FOREX Data
names <- list("MXN", "EUR", "JPY", "GBP", "CNY", "CHF")
link <- list("N_B_MX", "US_N_B_EU", "N_B_JA", "US_N_B_UK", "N_B_CH", "N_B_SZ")

for (i in 1:6){
  url <- str_c("FED/RXI_", link[[i]], sep = "")
  temp <- Quandl(url, api_key="36PTSjJK8L2rhKNn_bQ5", start_date="2013-04-01")
  temp <- temp %>% 
    as.tbl() %>%
    pad() %>%
    mutate(Coin = names[[i]]) %>% 
    mutate(PctChange = pct(Value) * 100)
  
  if (i == 1) {
    forex <- temp
  } else {
    forex <- rbind.data.frame(forex, temp)
  }
  
  if (i == 6) {
    rm(temp)
    rm(names)
    rm(link)
    forex <- arrange(forex, desc(Date))
  }
}

#### Load Stocks Data (API refused to work inside a loop)

api <- GET("https://api.iextrading.com/1.0/stock/aapl/chart/5y")
appl <- as.tbl(fromJSON(content(api, "text", encoding = "UTF-8")))
appl <- appl %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Date = date, PctChange = changePercent) %>% 
  pad() %>% 
  mutate(Coin = "APPL")

api <- GET("https://api.iextrading.com/1.0/stock/googl/chart/5y")
googl <- as.tbl(fromJSON(content(api, "text", encoding = "UTF-8")))
googl <- googl %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Date = date, PctChange = changePercent) %>% 
  pad() %>% 
  mutate(Coin = "GOOGL")

api <- GET("https://api.iextrading.com/1.0/stock/tsla/chart/5y")
tsla <- as.tbl(fromJSON(content(api, "text", encoding = "UTF-8")))
tsla <- tsla %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Date = date, PctChange = changePercent) %>% 
  pad() %>% 
  mutate(Coin = "TSLA")

api <- GET("https://api.iextrading.com/1.0/stock/lmt/chart/5y")
lmt <- as.tbl(fromJSON(content(api, "text", encoding = "UTF-8")))
lmt <- lmt %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Date = date, PctChange = changePercent) %>% 
  pad() %>% 
  mutate(Coin = "LMT")

api <- GET("https://api.iextrading.com/1.0/stock/ceix/chart/5y")
ceix <- as.tbl(fromJSON(content(api, "text", encoding = "UTF-8")))
ceix <- ceix %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Date = date, PctChange = changePercent) %>% 
  pad() %>% 
  mutate(Coin = "CEIX")

api <- GET("https://api.iextrading.com/1.0/stock/nktr/chart/5y")
nktr <- as.tbl(fromJSON(content(api, "text", encoding = "UTF-8")))
nktr <- nktr %>% 
  mutate(date = as.Date(date)) %>% 
  rename(Date = date, PctChange = changePercent) %>% 
  pad() %>% 
  mutate(Coin = "NKTR")

stocks <- bind_rows(appl, googl, tsla, lmt, ceix, nktr)
stocks <- arrange(stocks, desc(Date))
rm(api, appl, googl, tsla, lmt, ceix, nktr)

#### Tidy Crypto Data
# Convert each column to its correct data type, convert last 2 columns to millions,
# fix the date format.

data <- as.tbl(data)
data <- data[c(9, 1:8)]
colnames(data)[8] <- "MktCap"
tidy <- data %>% 
  mutate(Open = as.double(Open)) %>%
  mutate(High = as.double(High)) %>%
  mutate(Low = as.double(Low)) %>% 
  mutate(Volume = str_replace_all(Volume, ",", "")) %>% 
  mutate(MktCap = str_replace_all(MktCap, ",", "")) %>% 
  mutate(Volume = substr(Volume, 1, nchar(Volume) - 3)) %>% 
  mutate(MktCap = substr(MktCap, 1, nchar(MktCap) - 3)) %>%
  mutate(Volume = as.double(Volume) / 1000) %>% 
  mutate(MktCap = as.double(MktCap) / 1000) %>% 
  mutate(Date = as.Date(Date, "%b %d, %Y"))
rm(data)

tidy_dates <- mutate(tidy, Date2 = Date) %>% 
  separate(Date2, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
  mutate(MM = as.integer(MM)) %>%
  mutate(YYYY = as.integer(YYYY)) 


ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Cryptocurrencies", titleWidth = 350),
  dashboardSidebar(width = 350,
    sidebarMenu(
      menuItem("Liquidity", tabName = "liquidity",
               menuSubItem("Top 10 Coins", tabName = "tab1"),
               menuSubItem("Market Capitalization", tabName = "tab2"),
               menuSubItem("Volume", tabName = "tab3")),
      menuItem("Correlation", tabName = "correlation",
               menuSubItem("Covariation Between Price and Volume", tabName = "tab4"),
               menuSubItem("Covariation Between Price and Circulating Supply", tabName = "tab5"),
               menuSubItem("Correlation Between Two Coins", tabName = "tab6"),
               menuSubItem("Correlation Between Coin and Forex", tabName = "tab7")),
      menuItem("Performance", tabName = "performance",
               menuSubItem("Range of a Coin in a Year", tabName = "tab8"),
               menuSubItem("Yearly Performance ($)", tabName = "tab9"),
               menuSubItem("Yearly Performance (%)", tabName = "tab10"),
               menuSubItem("Comparison (%) Against Stock", tabName = "tab11"),
               menuSubItem("Density Plot of Percentage Change", tabName = "tab12")),
      menuItem("Investment Calculator", tabName = "tab13")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab1",
              fluidRow(
                box(plotOutput("plot1"), width = 12)
              )),
      tabItem(tabName = "tab2",
              fluidRow(
                box(selectInput(inputId = "slot1_coin", label = "Coin:",
                                choices = top$symbol),
                    plotOutput("plot2"), width = 12)
              )),
      tabItem(tabName = "tab3",
              fluidRow(
                box(selectInput(inputId = "slot2_coin", label = "Coin:",
                                choices = top$symbol),
                    plotOutput("plot3"), width = 12)
              )),
      tabItem(tabName = "tab4",
              fluidRow(
                box(selectInput(inputId = "slot3_coin", label = "Coin:",
                                choices = top$symbol),
                    plotOutput("plot4"), width = 12)
              )),
      tabItem(tabName = "tab5",
              fluidRow(
                box(selectInput(inputId = "slot4_coin", label = "Coin:",
                                choices = top$symbol),
                    plotOutput("plot5"), width = 12)
              )),
      tabItem(tabName = "tab6",
              fluidRow(
                box(selectInput(inputId = "slot5_coin1", label = "Coin 1:",
                                choices = top$symbol),
                    selectInput(inputId = "slot5_coin2", label = "Coin 2:",
                                choices = top$symbol, selected = top$symbol[2]),
                    plotOutput("plot6"), width = 12)
              )),
      tabItem(tabName = "tab7",
              fluidRow(
                box(
                  selectInput(inputId = "slot6_coin1", label = "Coin:",
                              choices = top$symbol),
                  selectInput(inputId = "slot6_coin2", label = "Currency:",
                              choices = unique(forex$Coin)),
                  plotOutput("plot7"), width = 12
                )
              )),
      tabItem(tabName = "tab8",
              fluidRow(
                box(selectInput(inputId = "slot7_coin", label = "Coin:",
                                choices = top$symbol),
                    uiOutput("ui1"),
                    plotOutput("plot8"), width = 12)
              )),
      tabItem(tabName = "tab9",
              fluidRow(
                box(selectInput(inputId = "slot8_coin", label = "Coin:",
                                choices = top$symbol),
                    uiOutput("ui2"),
                    plotOutput("plot9"), width = 12)
              )),
      tabItem(tabName = "tab10",
              fluidRow(
                box(selectInput(inputId = "slot9_coin", label = "Coin:",
                                choices = top$symbol),
                    uiOutput("ui3"),
                    plotOutput("plot10"), width = 12)
              )),
      tabItem(tabName = "tab11",
              fluidRow(
                box(selectInput(inputId = "slot10_coin1", label = "Coin:",
                                choices = top$symbol),
                    selectInput(inputId = "slot10_coin2", label = "Ticker:",
                                choices = unique(stocks$Coin)),
                    plotOutput("plot11"), width = 12)
              )),
      tabItem(tabName = "tab12",
              fluidRow(
                box(selectInput(inputId = "slot11_coin", label = "Coin:",
                                choices = top$symbol),
                    uiOutput("ui5"),
                    plotOutput("plot12"), width = 12)
              )),
      tabItem(tabName = "tab13",
              fluidRow(
                box(
                  selectInput(inputId = "slot12_coin1", label = "Coin 1:",
                              choices = top$symbol),
                  selectInput(inputId = "slot12_coin2", label = "Coin 2:",
                              choices = top$symbol, selected = top$symbol[2]),
                  numericInput(inputId = "slot12_inv", label = "Investment:", 
                               min = 100, max = 100000, value = 1000),
                  uiOutput("ui6"),
                  plotOutput("plot13"), width = 12)
              ))
    )
  )
)


##### Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### Liquidity
  
  ## Top N Coins
  # Plot
  output$plot1 <- renderPlot({
    
    ggplot(top, aes(symbol, mktcap)) + 
      geom_col() +
      scale_x_discrete(name ="Coin", limits=c(top$symbol[1:N])) +
      labs(y = "Billions of US Dollars",
           title = str_c("Today's Top", N, "Coins by Market Capitalization", sep = " "))
    
  })
  
  ## Market Cap
  output$plot2 <- renderPlot({
    
    # Inputs
    name <- filter(top, symbol == input$slot1_coin)$name
    
    # Plot
    ggplot(filter(tidy, Coin == input$slot1_coin)) +
      geom_line(mapping = aes(x = Date, y = MktCap)) +
      labs(y = "Market Capitalization (Millions of USD)", 
           title = str_c("Market Capitalization of", name, sep = " "))
  })
  
  ## Volume
  output$plot3 <- renderPlot({
    
    # Inputs
    name <- filter(top, symbol == input$slot2_coin)$name
    
    # Plot
    ggplot(filter(tidy, Coin == input$slot2_coin)) +
      geom_line(mapping = aes(x = Date, y = Volume)) +
      labs(y = "Volume (Millions of USD)", 
           title = str_c("Volume of Transactions of", name, sep = " "))
    
  })
  
  ### Correlation
  
  ## Covariation Between Price and Volume
  output$plot4 <- renderPlot({
    
    # Inputs
    name <- filter(top, symbol == input$slot3_coin)$name
    
    # Plot
    ggplot(filter(tidy, Coin == input$slot3_coin)) +
      geom_point(mapping = aes(x = Volume/Close, y = Close)) +
      labs(x = str_c("Volume of Transactions (Millions of ", input$slot3_coin, ")"),
           y = "Price (USD)", 
           title = str_c("Covariation of Volume and Price for", name, sep = " "))
    
  })
  
  ## Covariation Between Price and Circulating Supply
  output$plot5 <- renderPlot({
    
    # Inputs
    name <- filter(top, symbol == input$slot4_coin)$name
    
    # Plot
    ggplot(filter(tidy, Coin == input$slot4_coin)) +
      geom_point(mapping = aes(x = MktCap/Close, y = Close)) +
      labs(x = str_c("Circulating Supply (Millions of ", input$slot4_coin, ")"),
           y = "Price (USD)", 
           title = str_c("Covariation of Circulating Supply and Price for", name, sep = " "))
    
  })
  
  ## Correlation Between Two Coins
  output$plot6 <- renderPlot({
    
    # Calculations
    name1 <- filter(top, symbol == input$slot5_coin1)$name
    name2 <- filter(top, symbol == input$slot5_coin2)$name
    min_date <- filter(tidy, Coin == input$slot5_coin1)$Date[min(nrow(filter(tidy, Coin == input$slot5_coin1)),
                                                                 nrow(filter(tidy, Coin == input$slot5_coin2)))]
    
    # Correlation
    corr <- 100 * cor(c(filter(tidy, Coin == input$slot5_coin1 & Date >= min_date)$Close, NA),
                      c(filter(tidy, Coin == input$slot5_coin2 & Date >= min_date)$Close, NA), use = "complete.obs")
    
    # Plot
    ggplot(NULL, aes(Date, PctChange, colour = Coin)) + 
      geom_line(data = filter(tidy, Coin == input$slot5_coin1)) +
      geom_line(data = filter(tidy, Coin == input$slot5_coin2), alpha = 0.7) +
      scale_color_manual(values = c("red", "black")) +
      labs(y = "Percent", 
           title = str_c("Daily Percentage Change of", name1, "vs.", name2, sep = " "),
           subtitle = str_c("Estimated Correlation of these Coins: ",
                            format(round(corr, 2), nsmall = 2), "%")) +
      scale_x_date(limits = as.Date(c(min_date, NA))) +
      scale_y_continuous(limits = c(-75,100))
    
  })
  
  ## Correlation Between Coin and Forex
  output$plot7 <- renderPlot({
    
    #Inputs
    coin1 = "BTC"
    coin2 = "CHF"
    
    min_date = filter(tidy, Coin == input$slot6_coin1)$Date[min(nrow(filter(tidy, Coin == input$slot6_coin1)),
                                                    nrow(filter(forex, Coin == input$slot6_coin2)))]
    
    max_date = filter(forex, Coin == input$slot6_coin2)$Date[1]
    
    # Correlation
    corr <- 100 * cor(c(filter(tidy, Coin == input$slot6_coin1 & Date >= min_date & Date <= max_date)$Close, NA),
                      c(filter(forex, Coin == input$slot6_coin2 & Date >= min_date)$Value, NA), use = "complete.obs")
    
    # Plot
    ggplot(NULL, aes(Date, PctChange, colour = Coin)) +
      geom_line(data = filter(tidy, Coin == input$slot6_coin1)) +
      geom_line(data = filter(forex, Coin == input$slot6_coin2), alpha = 0.7) +
      scale_color_manual(values = c("orange", "black")) +
      labs(y = "Percent",
           title = str_c("Daily Percentage Change of", input$slot6_coin1, "vs.", input$slot6_coin2, sep = " "),
           subtitle = str_c("Estimated Correlation of these Assets: ", format(round(corr, 2), nsmall = 2), "%")) +
      scale_x_date(limits = as.Date(c(min_date, max_date)))
    
  })
  
  ### Performance
  
  ## Range of a Coin in a Year
  output$plot8 <- renderPlot({
    
    # Inputs
    name <- filter(top, symbol == input$slot7_coin)$name
    
    # Data Format
    by_year <- filter(tidy, Coin == input$slot7_coin) %>% 
      separate(Date, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      mutate(MM = as.integer(MM)) %>%
      mutate(YYYY = as.integer(YYYY)) %>% 
      filter(YYYY == input$slot7_year) %>% 
      mutate(MM = month.abb[MM])
    
    # Calculations
    low <- min(by_year$Close)
    high <- max(by_year$Close)
    avg <- mean(by_year$Close)
    
    # Plot
    ggplot(data = by_year, mapping = aes(x = MM, y = Close)) + 
      geom_boxplot() +
      coord_flip() +
      scale_x_discrete (name ="Month", 
                        limits=c("Jan","Feb","Mar", "Apr","May","Jun",
                                 "Jul","Aug","Sep", "Oct","Nov","Dec")) +
      labs(title = str_c("Monthly Range of Prices for", name, "in", input$slot7_year, sep = " "),
           subtitle = str_c("Low:", format(round(low, 2), nsmall = 2), 
                            "    High:", format(round(high, 2), nsmall = 2),
                            "    Average:", format(round(avg, 2), nsmall = 2),
                            "    Yearly Range:", format(round(high - low, 2), nsmall = 2),
                            sep = " "),
           y = "Price (USD)")
  })
  
  ## Yearly Performance ($)
  output$plot9 <- renderPlot({
    
    # Inputs
    name <- filter(top, symbol == input$slot8_coin)$name
    
    #Data Format
    by_year <- filter(tidy, Coin == input$slot8_coin) %>% 
      mutate(Date2 = Date) %>% 
      separate(Date2, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      mutate(MM = as.integer(MM)) %>%
      mutate(YYYY = as.integer(YYYY)) %>% 
      filter(YYYY == input$slot8_year) %>% 
      mutate(MM = month.abb[MM])
    
    # Calculations
    avg <- mean(by_year$Close)
    sd <- sd(by_year$Close)
    growth <- 100* ((by_year$Close[1] - by_year$Close[nrow(by_year)]) /
                      by_year$Close[nrow(by_year)])
    
    # Plot
    ggplot(by_year) +
      geom_line(mapping = aes(x = Date, y = Close)) +
      labs(y = "Price (Millions of USD)",
           title = str_c("Price of", name, "in", input$slot8_year, sep = " "),
           subtitle = str_c("Average:", format(round(avg, 2), nsmall = 2), 
                            "    Standard Deviation:", format(round(sd, 2), nsmall = 2),
                            "    Period Growth:", str_c(format(round(growth, 2), nsmall = 2), "%"),
                            sep = " "))
    
  })
  
  ## Yearly Performance (%)
  output$plot10 <- renderPlot({
    
    # Inputs
    name <- filter(top, symbol == input$slot9_coin)$name
    
    #Data Format
    by_year <- filter(tidy, Coin == input$slot9_coin) %>% 
      mutate(Date2 = Date) %>% 
      separate(Date2, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      mutate(MM = as.integer(MM)) %>%
      mutate(YYYY = as.integer(YYYY)) %>% 
      filter(YYYY == input$slot9_year) %>% 
      mutate(MM = month.abb[MM])
    
    # Calculations
    avg <- mean(by_year$PctChange, na.rm = T)
    sd <- sd(by_year$PctChange, na.rm = T)
    
    # Plot
    ggplot(by_year) +
      geom_line(mapping = aes(x = Date, y = PctChange)) +
      labs(y = "Percent",
           title = str_c("Daily Percentage Change of", name, "in", input$slot9_year, sep = " "),
           subtitle = str_c("Expected Percentage Change: ", 
                            str_c(format(round(avg, 2), nsmall = 2), "%"), 
                            "    Standard Deviation: ",
                            str_c(format(round(sd, 2), nsmall = 2), "%")))
    
  })
  
  ## Comparison (%) Against Stock
  output$plot11 <- renderPlot({
    
    #Inputs
    min_date = filter(tidy, Coin == input$slot10_coin1)$Date[min(nrow(filter(tidy, Coin == input$slot10_coin1)),
                                                   nrow(filter(stocks, Coin == input$slot10_coin2)))]
    
    max_date = filter(stocks, Coin == input$slot10_coin2)$Date[1]
    
    # Plot
    ggplot(NULL, aes(Date, PctChange)) +
      geom_line(data = filter(tidy, Coin == input$slot10_coin1), aes(colour = Coin)) +
      geom_line(data = filter(stocks, Coin == input$slot10_coin2), aes(colour = Coin), alpha = 0.7) +
      scale_color_manual(values = c("black", "blue")) +
      labs(x = "Year", y = "Percent",
           title = str_c("Percentage Change of", input$slot10_coin1, "vs.", input$slot10_coin2, sep = " ")) +
      scale_x_date(limits = as.Date(c(min_date, max_date)))
    
  })
  
  ## Density Plot of Percentage Change
  output$plot12 <- renderPlot({
    
    #Inputs
    name <- filter(top, symbol == input$slot11_coin)$name
    
    #Data Format
    by_year <- filter(tidy, Coin == input$slot11_coin) %>% 
      mutate(Date2 = Date) %>% 
      separate(Date2, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      mutate(MM = as.integer(MM)) %>%
      mutate(YYYY = as.integer(YYYY)) %>% 
      filter(YYYY == input$slot11_year) %>% 
      mutate(MM = month.abb[MM])
    
    # Plot
    ggplot(by_year, aes(x = PctChange)) + 
      geom_histogram(aes(y = ..density..),      
                     binwidth = .5,
                     colour = "black", fill = "white") +
      geom_density(alpha = .2, fill = "#FF6666")  +
      geom_vline(aes(xintercept = mean(PctChange, na.rm = T)),   
                 color="red", linetype="dashed", size = 1) +
      labs(y = "Density", x = "Percentage Change",
           title = str_c("Histogram of Daily Percentage Change of", name, "in", input$slot11_year, sep = " "),
           subtitle = "Overlaid with Kernel Density Curve",
           caption = "(Vertical Red Line Indicates Average Percentage Change)")
    
  })
  
  ### Investment Calculator
  output$plot13 <- renderPlot({
    
    # Inputs
    name1 <- filter(top, symbol == input$slot12_coin1)$name
    name2 <- filter(top, symbol == input$slot12_coin2)$name
    
    # Data Format
    by_year <- filter(tidy, Coin == input$slot12_coin1) %>% 
      mutate(Date2 = Date) %>% 
      separate(Date2, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      mutate(MM = as.integer(MM)) %>%
      mutate(YYYY = as.integer(YYYY)) %>% 
      filter(Date >= input$slot12_date[1] & Date <= input$slot12_date[2])
    by_year <- by_year %>% 
      mutate(Value = value(Close[nrow(by_year)], Close, input$slot12_inv))
    
    by_year2 <- filter(tidy, Coin == input$slot12_coin2) %>% 
      mutate(Date2 = Date) %>% 
      separate(Date2, into = c("YYYY", "MM", "DD"), sep = "-") %>% 
      mutate(MM = as.integer(MM)) %>%
      mutate(YYYY = as.integer(YYYY)) %>% 
      filter(Date >= input$slot12_date[1] & Date <= input$slot12_date[2])
    by_year2 <- by_year2 %>% 
      mutate(Value = value(Close[nrow(by_year2)], Close, input$slot12_inv))
    
    
    final1 <- by_year$Value[1]
    final2 <- by_year2$Value[1]
    
    # Plot
    ggplot(NULL, aes(x = Date, y = Value)) + 
      geom_line(data = by_year) + 
      geom_line(data = by_year2) +
      facet_wrap(~ Coin, nrow = 2) +
      geom_hline(yintercept = input$slot12_inv, colour = "red", alpha = 0.5) +
      labs(y = "Value (USD)",
           title = "Value of an Investment Made During Your Chosen Date Range",
           subtitle = str_c("Initial Investment: $", format(round(input$slot12_inv, 2), nsmall = 2), 
                            str_c("     ", name1, ": $"), format(round(final1, 2), nsmall = 2),
                            str_c("     ", name2, ": $"), format(round(final2, 2), nsmall = 2),
                            sep = ""),
           caption = "(Red Line Indicates Initial Investment)")
    
    
  })
  
  output$ui1 <- renderUI({
    selectInput(inputId = "slot7_year", label = "Year:",
                choices = unique(filter(tidy_dates, Coin == input$slot7_coin)$YYYY)
                [length(unique(filter(tidy_dates, Coin == input$slot7_coin)$YYYY))]:2018)
  })
  
  output$ui2 <- renderUI({
    selectInput(inputId = "slot8_year", label = "Year:",
                choices = unique(filter(tidy_dates, Coin == input$slot8_coin)$YYYY)
                [length(unique(filter(tidy_dates, Coin == input$slot8_coin)$YYYY))]:2018)
  })
  
  output$ui3 <- renderUI({
    selectInput(inputId = "slot9_year", label = "Year:",
                choices = unique(filter(tidy_dates, Coin == input$slot9_coin)$YYYY)
                [length(unique(filter(tidy_dates, Coin == input$slot9_coin)$YYYY))]:2018)
  })
  
  output$ui5 <- renderUI({
    selectInput(inputId = "slot11_year", label = "Year:",
                choices = unique(filter(tidy_dates, Coin == input$slot11_coin)$YYYY)
                [length(unique(filter(tidy_dates, Coin == input$slot11_coin)$YYYY))]:2018)
  })
  
  output$ui6 <- renderUI({
    dateRangeInput(inputId = "slot12_date", label = "Period of Investment:", 
                   min = as.character(max(min(filter(tidy_dates, Coin == input$slot12_coin1)$Date), min(filter(tidy_dates, Coin == input$slot12_coin2)$Date))), 
                   max = as.character(Sys.Date() - 1),
                   start = as.character(max(min(filter(tidy_dates, Coin == input$slot12_coin1)$Date), min(filter(tidy_dates, Coin == input$slot12_coin2)$Date))), 
                   end = as.character(Sys.Date() - 1))
  })
  

  
}

shinyApp(ui, server)

