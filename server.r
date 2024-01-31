



server <- function(input, output) {
  
  

  
  
  ###########Yahoo Finance Income Statement
  
  output$yahoo_finance_income_statement <- renderUI({
    
    
    
    source("module//yahoo_finance_income_statement.R")
    callModule(module = yahoo_finance_income_statement_server, id = "yahoo_finance_income_statement")
    yahoo_finance_income_statement_ui(id = "yahoo_finance_income_statement")
    
    
    
  })
  
  
  
  
  
  ###############Yahoo Balance Sheet
  
  
  
  output$yahoo_finance_balance_sheet <- renderUI({
    
    
    
    source("module//yahoo_finance_balance_sheet.R")
    callModule(module = yahoo_finance_balance_sheet_server, id = "yahoo_finance_balance_sheet")
    yahoo_finance_balance_sheet_ui(id = "yahoo_finance_balance_sheet")
    
    
    
  })
  
  
  
  
  #############Yahoo Cash Flow
  
  
  
  output$yahoo_finance_cash_flow <- renderUI({
    
    
    
    source("module//yahoo_finance_cash_flow.R")
    callModule(module = yahoo_finance_cash_flow_server, id = "yahoo_finance_cash_flow")
    yahoo_finance_cash_flow_ui(id = "yahoo_finance_cash_flow")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############Yahoo Finance Harga Saham
  
  output$yahoo_finance_harga_saham_per_tahun <- renderUI({
    
    
    
    source("module//yahoo_finance_harga_saham_per_tahun.R")
    callModule(module = yahoo_finance_harga_saham_per_tahun_server, id = "yahoo_finance_harga_saham_per_tahun")
    yahoo_finance_harga_saham_per_tahun_ui(id = "yahoo_finance_harga_saham_per_tahun")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server