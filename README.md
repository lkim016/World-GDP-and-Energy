# World-GDP-and-Energy

## Prerequisites
**Install R and Rstudio IDE**

R: https://cran.r-project.org/

Rstudio: https://www.rstudio.com/products/rstudio/#Desktop

## Getting Started
**Set working directory**

A. Open "WorldGDPEnergy.R" with Rstudio

B. Set working directory:

1. uncomment this code **#setwd("C:\Users\lkim016\Desktop")** so that it looks like this: **setwd("C:\Users\lkim016\Desktop")**

and

2. change **C:\Users\lkim016\Desktop** to the path to the folder where you downloaded the above repository files

**Note:** the dataset is too large to upload into GitHub so need to retrieve the dataset from below.

The world development indicators dataset: https://www.kaggle.com/worldbank/world-development-indicators

## Project Guide
Objective: Observe the world development indicators to analyze the correlation of the different countries' GDP against energy related indicators for the year 2015.

1. Since its founding in 1944, the World Bank has been gathering data (WDI: World Development Indicators) to help it alleviate poverty all cross the world. In this project, you will need to predict the main goal indicator: gdp per capita (current us$); code: NY.GDP.PCAP.CD) for those countries that have available data in 2015.  

2. The task is find five major indicators which could predict poverty of a country 'power' or 'energy' related indicators. By doing so, the World Bank and United Nations will be able to follow those predictors to reduce poverty.

3. Write a R markdown with both explanation and script for your problem-solving project. The data is W03b_wdi.xlsx.
