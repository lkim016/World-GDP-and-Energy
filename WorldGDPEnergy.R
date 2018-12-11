#### Lori Kim
#### clean energy and it's economic impact on the country

#setwd("C:\Users\lkim016\Desktop")

# install.packages("dplyr")
# install.packages("data.table")
# install.packages("rworldmap")
# install.packages("googleVis")
# install.packages("shinydashboard")
# install.packages("mice")
# install.packages("VIM")
# install.packages("DMwR")

# load libraries
library(readxl)
library(mice)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(maps)
library(ggplot2)
library(googleVis)
library(shinydashboard)
library(shiny)
library(VIM)
library(car)
library(MASS)
library(DMwR)
library(DT)
library(caret)
library(glmnet)
library(reshape2)
library(corrplot)

select = dplyr::select

# cost effectiveness of clean energy
# energy = read.csv("global_power_plant_database.csv")
wdi = read_excel("W03b_wdi.xlsx")

##################### INFO ###################

# response = NY.GDP.PCAP.KD.ZG (GDP per capita growth (annual %))
# response = NY.GDP.PCAP.CD
response = "NY.GDP.PCAP.CD" # gdp per capita (current us$)

#####################

##################### CODE STARTS ###########

# renaming wdi
qual = c("country.name", "country.code", "indicator.name", "indicator.code")
yr = 1960:2017
yr = paste("x", yr, sep = "")
colnames(wdi) = c(qual, yr)

# indicator df
ind.df = wdi %>% filter(country.code %in% "USA") %>%
  select(one_of(c("indicator.name","indicator.code"))) # dataframe just for indicators
ind.df$indicator.name = sapply(ind.df$indicator.name, tolower)

########## FINAL DATASET ##########
  # filter unnecessary countries:
  # Early-demographic dividend, (excluding high income), (IDA & IBRD countries),
  # IDA, Heavily indebted poor countries (HIPC), High income, Late-demographic dividend,
  # Least developed countries: UN classification, Low & middle income, Low income, Lower middle income,
  # OECD members, Other small states, Post-demographic dividend, Pre-demographic dividend, Small states, Upper middle income, World = 18
  # Region country code
rm.reg = c("ARB","CEB","CSS","EAP","EAR","EAS","ECA","ECS","EUU","HIC","HPC","IBD","IBT","IDA","IDX",
           "LAC","LCN","LDC","LIC","LMC","LMY","MEA","MIC","MNA","NAC","OED","OSS","PRE","PSS","PST",
           "SAS","SSA","SSF","SST","TEA","TEC","TLA","TMN","TSA","UMC","WLD", "FCS", "INX", "IDB", "LTE")

  # Remove those regions, non-country data rows
final.df = wdi %>% filter(!country.code %in% rm.reg) %>% select(c(qual,'x2015'))
# filter out indicator codes related to energy and GDP
en.word = "energy | electricity | clean | emission | electrical | power"
en.ind.df = ind.df %>% filter(str_detect(indicator.name, en.word))

final.df = final.df %>% filter(indicator.code %in% c(en.ind.df$indicator.code, response)) %>%
  select(one_of(qual, "x2015"))

# Transport the data frame between years and variables
final.df = dcast(melt(as.data.table(final.df), id.vars = c("country.name", 
                                                        "country.code","indicator.name", "indicator.code")), 
               country.name + country.code + variable ~ indicator.code, value.var = "value")

final.df = final.df %>% select(-variable)

##########  CLEAN AND MANIPULATE DATA ##########
# checking NA for final.df
na = final.df %>% summarize_all(funs(sum(is.na(.))/n()))
na = gather(na, key="feature", value="missing_pct")
goodv = filter(na, missing_pct < 0.25)
final.df = final.df %>% select(one_of(c(goodv$feature))) # filter NA
# note: make sure to keep response variable after cleaning out the NAs

# reorder column in final.df
final.df = final.df[,c(1,2,13,3,4,5,6,7,8,9,10,11,12)]

# look at the features
i = colnames(final.df)
ind1 = ind.df %>% filter(indicator.code %in% i)

### PLOT VISUALIZATION -------------------------------
  # feature name table
plot.df = final.df[,-c(1,2)]
log.plot.df = log(plot.df)
plot.ind.name = ind.df %>% filter(indicator.code %in% colnames(plot.df))
plot.name = colnames(plot.df)


### 1. IMPUTATION w/ mice ###
aft.transpose = c("country.name","country.code")
  # visual analysis of missing data using mice functions
par("mar")
par(mar = c(1,1,1,1))
md.pattern(final.df)
  
ggplot_missing <- function(x){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

  
  # clean the data for imputation
miss.df = final.df %>% select(-one_of(aft.transpose))
miss.df = scale(miss.df) # scale the data to impute it
  
imputed.data <- mice(miss.df, m = 16, maxit = 50, method = 'pmm', seed = 500)
  
comp.df <- mice::complete(imputed.data, 4)

########## DATA ANALYSIS ##########
# response: NY.GDP.PCAP.CD

## analyze the explanatory variables by checking the indicator names
i = colnames(comp.df)
ind2 = wdi %>% filter(country.code %in% "USA") %>% filter(indicator.code %in% i) %>% select(one_of(c("indicator.name","indicator.code")))

### split train and test set
smp_size = floor(0.75*nrow(comp.df))
train.i = sample(seq_len(nrow(comp.df)), size = smp_size)
train = comp.df[train.i, ]
test = comp.df[-train.i, ]

corr = cor(train)

# LASSO: https://www.r-bloggers.com/ridge-regression-and-the-lasso/
fit = lm(NY.GDP.PCAP.CD ~. , data = train)
coef(fit)

x = model.matrix(NY.GDP.PCAP.CD ~ ., data = train)[,-1]
y = train$NY.GDP.PCAP.CD

ytest = y[1:nrow(test)]

z = model.matrix(NY.GDP.PCAP.CD ~ . , data = test)[,-1]
lambda = 10^seq(10, -2, length = 100)

ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, exact = F, type = 'coefficients')

# find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x, y, alpha = 0)

# checking Ridge
bestlam <- cv.out$lambda.min

# make predictions
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[1:nrow(test),])
s.pred <- predict(fit, newdata = test)
# check MSE
mean((s.pred-ytest)^2)
mean((ridge.pred-ytest)^2)

# a look at the coefficients
out = glmnet(x[1:nrow(train),],y[1:nrow(train)],alpha = 0)
predict(ridge.mod, type = "coefficients", s = bestlam)

# lasso
lasso.mod <- glmnet(x[1:nrow(train),], y[1:nrow(train)], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[1:nrow(test),])
mean((lasso.pred-ytest)^2)

predict(lasso.mod, type = 'coefficients', s = bestlam)

fit1 = lm(NY.GDP.PCAP.CD ~ EG.CFT.ACCS.ZS+EG.EGY.PRIM.PP.KD+IC.ELC.TIME +NY.ADJ.DPEM.CD+NY.ADJ.DPEM.GN.ZS, data = train)
summary(fit1)

par(mfrow=c(1,1))
plot(fit1)

# stepwise from MASS
fit = lm(NY.GDP.PCAP.CD ~ ., data = train)
step = stepAIC(fit, trace = FALSE)
step$anova
model = lm(NY.GDP.PCAP.CD ~ EG.CFT.ACCS.ZS + EG.ELC.ACCS.ZS, data = train)
summary(model)

# check what the explanatory variables are
exp.var = c("EG.CFT.ACCS.ZS", "EG.ELC.ACCS.ZS")
exp = ind.df %>% filter(indicator.code %in% exp.var) %>% select(one_of(c("indicator.name", "indicator.code")))

# PREDICTION
test.feat = test %>% select(-response)
pred = predict(model, newdata = test, interval = 'prediction')
pred.con = predict(model, newdata = test, interval = 'confidence')
pred = unscale(pred.con, miss.df)
pred.rname = rownames(pred)
pred.rname = as.numeric(pred.rname)
fin.df.row = final.df[pred.rname,] # filter the y's from final.df using the test row samples
# combine final.df data w/ predictions
test.pred = fin.df.row %>% select(one_of(c("country.name",response)))
test.pred = cbind(test.pred, pred)

head(test.pred, 10)
summary(test.pred)

########## GVIS MAP ##########

# 1. make a dataframe with country name and numbers
map.x = "EG.CFT.ACCS.ZS"
gdp.map = final.df %>% select(one_of(c("country.name", response, map.x)))
# filter out NAs from the response
gdp.map$bool = is.na(subset(gdp.map, select = response))
gdp.map$bool = as.logical(gdp.map$bool)
gdp.map = gdp.map %>% filter(bool == FALSE) %>% select(-bool)
# filter out NAs from the x var
gdp.map$bool = is.na(subset(gdp.map, select = map.x))
gdp.map$bool = as.logical(gdp.map$bool)
gdp.map = gdp.map %>% filter(bool == FALSE) %>% select(-bool)

colnames(gdp.map) = c("country", "gdp.2015", "clean.fuel.and.tech")
#gdp.map$gdp.2015 = gdp.map$gdp.2015
#gdp.map$scale = scale(gdp.map$gdp.2015) # standardization: (x - mean(x) / sd(x)

geo.y = gvisGeoChart(gdp.map, locationvar = "country",
                   colorvar = "gdp.2015",
                   options = list(
                     colorAxis = "{colors: ['burlywood', 'cornsilk', 'gold']}",
                     backgroundColor = "aliceblue",
                     height = 450
                   )
)

geo.x = gvisGeoChart(gdp.map, locationvar = "country",
                   colorvar = "clean.fuel.and.tech",
                   options = list(
                     colorAxis = "{colors: ['burlywood','cornsilk', 'gold']}",
                     backgroundColor = "aliceblue",
                     height = 450
                   )
)

### CLEANING UP DATASETS FOR DISPLAY

########## SHINY ##########
# Shiny App
dashHead = dashboardHeader(title = "UCLA Extension: Data Science")

sideBar = dashboardSidebar(
  sidebarMenu(
    menuItem("2015 GDP Maps", tabName = "map"),
    menuItem("Data Analysis: 1", tabName = "dataVis"),
    menuItem("Data Analysis: 2", tabName = "dataAn"),
    menuItem("Trained Model", tabName = "model")
  )
)

body = dashboardBody(
  tabItems(
      tabItem(tabName = "map", fluidPage( # pg 1
        fluidRow(
          h2("Energy-Related Energy vs. GDP Around the World"),
          h3("By: Lori Kim"),
          h5("Purpose: I decided to focus my project around the World Bank Indicators to put GDP against any indicator that is connected with energy")
          ),
        fluidRow(column(12, align = "center",
                        div(style = "border-bottom: solid #A9A9A9",
                          h3("2015 GDP Map"),
                          htmlOutput("geo.y"),
                          h4("This is a map depicting the GDP around the world in 2015.")
                        ),
                          h3("2015 Access to Clean Fuels and Technologies for Cooking"),
                          htmlOutput("geo.x"),
                          h4("This is a map depicting the access to clean fuels and technologies for cooking by % of population")
                        ) )
        ) ),
      tabItem(tabName = "dataVis", # pg 2
              fluidPage(fluidRow(column(12, align = "center",
                                        div(style = "border-bottom: solid #A9A9A9",
                                            h2("Data Analysis"),
                                            h3("2015 GDP and Energy Related Table"),
                                            tabsetPanel(
                                              tabPanel("Final Dataset", br(), DT::dataTableOutput("final.df")),
                                              tabPanel("Pred Dataset", br(), DT::dataTableOutput("test.pred"))
                                              
                                              ),
                                            h4("This is a compact visualization of the dataset that I started with and the dataset with my prediction results.")
                                            ) )
                                 ),
                        fluidRow(column(12, align = "center",
                                            h3("Data Plots"),
                                            sidebarLayout(sidebarPanel(
                                              conditionalPanel('input.dataset === "plot" || "log plot"',
                                                               selectInput('x', 'x Variable:', choices = plot.name[-1], selected = plot.name[2])
                                                               )
                                              ),
                                              mainPanel(
                                                tabsetPanel(
                                                  id = 'dataset',
                                                  tabPanel("plot", plotOutput('ggplot') ),
                                                  tabPanel("log plot", plotOutput('ggplot.log') )
                                                  ),
                                                br()
                                                
                                                )
                                              )
                                            )
                                        ) )
      ),
      tabItem(tabName = "dataAn",
              fluidPage(column(12, align = "center",
                               div(style = "border-bottom: solid #A9A9A9",
                                   h2("Missing Data"),
                                   plotOutput('ggplot.na'),
                                   h4("This is a graph representing the amount of data that is NA. With this information I decided I needed to impute my data in order to do any kind of model training.  But first, I had to scale (normalize) the variables that I was left with in order to perform imputation.")
                               ),
                               div(style = "border-bottom: solid #A9A9A9",
                               h3("Imputation"),
                               verbatimTextOutput(outputId = "imputed.df"),
                               h4("The mice or the Multivariate Imputation by Chained Equations was used to impute the scaled dataset. The PMM (Predictive Mean Matching) method was used to impute the data.")
                               ),
                               
                                   h3("Correlation Matrix"),
                                   DT::dataTableOutput("corr"),
                               h4("Then I analyzed the variables to look for any multi-collinearity.")
                                   
                               ) )
              ),
    
      tabItem(tabName = "model",
              fluidPage(column(12, align = "center",
                               div(style = "border-bottom: solid #A9A9A9",
                               h2("Model Analysis"),
                               
                               verbatimTextOutput(outputId = "sum.final.df"),
                               h4("Summary of the model using all of the explanatory variables.")
                               ),
                               br(),
                               verbatimTextOutput(outputId = "sum.test.pred"),
                               h4("Summary of the best fit model. This was the model that came out to be the best fit model after splitting my data into a training / testing set, and running it through the LASSO and stepAIC function.")
                               )
                        )
              )
      )
)
      


ui <- dashboardPage(dashHead,sideBar, body)

server = function(input, output) {
  ## map
  output$geo.y <- renderGvis({
    map <- geo.y
  })
  output$geo.x <- renderGvis({
    map <- geo.x
  })
  ## datatable
  output$final.df = DT::renderDataTable({
    final.df}, option = list(scrollX = TRUE))
  
  output$test.pred = DT::renderDataTable({
    test.pred}, option = list(scrollX = TRUE))
  
  output$corr = DT::renderDataTable({
    corr}, option = list(scrollX = TRUE))
  
  ## ggplot
  output$ggplot <- renderPlot({
    ggplot(data = plot.df, aes_string(x = input$x, y = plot.name[1])) +
      geom_point(color = "lightblue") +
      geom_line(color = "darkred") +
      xlab(ind.df %>% filter(indicator.code %in% input$x) %>% select(indicator.name)) +
      ylab(ind.df %>% filter(indicator.code %in% response) %>% select(indicator.name))
  })
  output$ggplot.log <- renderPlot({
    ggplot(data = log.plot.df, aes_string(x = input$x, y = plot.name[1])) +
      geom_point(color = "lightblue") +
      geom_line(color = "darkred") +
      xlab(ind.df %>% filter(indicator.code %in% input$x) %>% select(indicator.name)) +
      ylab(ind.df %>% filter(indicator.code %in% response) %>% select(indicator.name))
  })
  
  output$ggplot.na <- renderPlot({
    ggplot_missing(final.df[,-c(1,2)])
  })
  
  output$imputed.df <- renderPrint({
    print(summary(imputed.data))
  })
  
  ## summary
  output$sum.final.df <- renderPrint({
    fit = lm(NY.GDP.PCAP.CD~ ., data = train)
    print(summary(fit))
  })
  output$sum.test.pred <- renderPrint({
    model = lm(NY.GDP.PCAP.CD ~ EG.CFT.ACCS.ZS + EG.ELC.RNEW.ZS, data = train)
    print(summary(model))
  })
  #output$box <- renderPlot({
  #  boxplot(obesity~year,data=adult.ob, main="Average Obesity in 2011 & 2016", 
   #         xlab="Years", ylab="Average Percent Population of Obesity")
  #})
  #output$adultOb <- renderGvis({
    #map <- J
  #})
}

shinyApp(ui, server)











