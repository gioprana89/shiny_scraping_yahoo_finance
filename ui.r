


library(rvest)
library(stringr)
library(magrittr)


library(pdfetch)

library(shiny)

library(ggplot2)

library(DT)

library(pdfetch)
library(rvest)

library(magrittr)



library(shinycssloaders)


library(DT)
library(data.table)



ui <- fluidPage(
  
 
  
  
  tabsetPanel(  #Choose Variable and Result
    
    
    tabPanel("Income Statement", 
             
             
             uiOutput("yahoo_finance_income_statement"),
             
             
             
             br()
             
             
    ),
    
    
    tabPanel("Balance Sheet", 
             
             
             uiOutput("yahoo_finance_balance_sheet"),
             
             
             
             
             br()
             
             
    ),
    
    
    tabPanel("Cash Flow", 
             
             uiOutput("yahoo_finance_cash_flow"),
             
             
             
             
             
             br()
             
             
    ),
    
    
    
    tabPanel("Stock Price", 
             
             
             uiOutput("yahoo_finance_harga_saham_per_tahun"),
             
             
             
             br()
             
             
    )
    
    
    
    
  ), # akhir tabsetpanel
             
             

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
                
                br()
                
) #Akhir dari UI