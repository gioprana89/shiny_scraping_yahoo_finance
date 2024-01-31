


########################################
########UI (User Interface)#############
########################################

yahoo_finance_cash_flow_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
   # HTML('<center><img src="ugi.png" width="800px"></center>'),

   # h3("TRIK-TRIK R-SHINY: checkboxGroupInput",
      # style="color:red; text-align:center"),
    
   # br(),
    #br(),
   # br(),

    
    
               
               h2("Cash Flow", style="
    font-family: 'cursive';
    color: #0000cd;
    font-size:40px;
    font-weight: bold; 
    text-align:center
    
    "),
               
               
               br(),
               
               
               
               
               sidebarLayout(
                 
                 sidebarPanel(width = 2,
                              
                              textAreaInput(ns("get_variabel"), "Companies", value = "AMAN.JK
ASPI.JK
ATAP.JK
BBSS.JK
BCIP.JK
BKDP.JK
CBPE.JK
CSIS.JK
DUTI.JK
EMDE.JK
FMII.JK
GRIA.JK
HOMI.JK
INDO.JK
INPP.JK
KOCI.JK
LPLI.JK
MKPI.JK
MPRO.JK
MTSM.JK
NIRO.JK
OMRE.JK
POLI.JK
POLL.JK
PURI.JK", height = 250, width = 260),
                              
                              
                              
                              br(),
                              
                              
                              
                              textAreaInput(ns("get_var_financial"), "Variable(s)", value = "Cash Flows from Used in Operating Activities Direct\nInvesting Cash Flow\nFinancing Cash Flow\nCapital Expenditure\nFree Cash Flow", height = 150, width = 260),
                              
                              
                              
                              
                              textAreaInput(ns("get_sector"), "Sector", value = "Properties & Real Estate", height = 150, width = 260),
                              
                              
                              
                              
                              textAreaInput(ns("get_papan"), "Board", value = "Pengembangan", height = 150, width = 260)
                              
                              
                              
                              
                              
                 ), #Akhir sidebarPanel
                 
                 
                 
                 
                 mainPanel(width = 10,
                           
                           
                           uiOutput(ns("pemilihan_variabel_checkboxGroupInput")),
                           
                           br(),
                           br(),
                           
                           actionButton(ns("goButton"), "Get Data"),
                           withSpinner(verbatimTextOutput(ns("cetak_variabel"))),
                           
                           br(),
                           
                           
                           
                           actionButton(ns("goButton_excel"), "Print Data to Excel"),
                           
                           withSpinner(textOutput(ns("cetak_excel_ya")))
                           
                           
                           #verbatimTextOutput("tes"),
                           
                           
                 ) #Akhir mainpanel
                 
                 
                 
               ), #Akhir sidebar layout
               
               
               
               
               
               
               
               
    
    
    
    
    
    
    
    
    
    
    
    
    
    br()
    
  ) #Akhir fluidpage
  
  
} #Akhir dari UI

















































########################################
################Server##################
########################################



yahoo_finance_cash_flow_server <- function(input, output, session) {
  

  
  
  
  output$tes <- renderPrint({  
    
    
    #get_variabel <- read.csv(text=input$get_variabel, header = FALSE, sep="", na.strings=c("","NA","."))
    #get_variabel = unlist(get_variabel)
    
    get_variabel = input$get_variabel
    get_variabel = strsplit(get_variabel, "\n")
    get_variabel = unlist(get_variabel)
    
    
    print(get_variabel)
    
    
  })
  
  
  
  
  kirim_nama <- function()
  {
    
    get_variabel = input$get_variabel
    get_variabel = strsplit(get_variabel, "\n")
    get_variabel = unlist(get_variabel)
    
    
    return(get_variabel)
    
  }
  
  
  
  output$pemilihan_variabel_checkboxGroupInput <- renderUI({
    
    
    
    checkboxGroupInput(session$ns("terpilih_checkboxGroupInput"), label="Select Companies:", choices = c( kirim_nama()), selected=c(), inline = TRUE)
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  output$cetak_variabel <- renderPrint({  
    
    
   tes <- input$goButton
    #Sys.sleep(1)
    
    
    
  #  if(tes == 1)
    #{
    #  isolate({
        
        
        
    
    
    #get_var = c("Total Revenue","Net Income Common Stockholders","Basic EPS", "Basic Average Shares")
    
    #sektor = c("Properties & Real Estate")
    
   # papan = c("Pengembangan")
    
    
    #get_var = isolate(input$get_var_financial)
    #get_var = unlist(get_var)
   #get_var = as.vector(get_var)
   
   
   
   get_var = isolate(input$get_var_financial)
   get_var = strsplit(get_var, "\n")
   get_var = unlist(get_var)
   
    
    
    
    sektor = isolate(input$get_sector)
    sektor = unlist(sektor)
    sektor = as.vector(sektor)
    
    
    
    papan = isolate(input$get_papan)
    papan = unlist(papan)
    papan = as.vector(papan)
    
    
    
    
 
    
    
    
    
    
    
    get_variabel = isolate(input$terpilih_checkboxGroupInput)
    get_variabel = unlist(get_variabel)
    get_variabel = as.vector(get_variabel)
    
    #print(get_variabel)
    #print(length(get_variabel))
    #print(get_var)
    
    
    
    #cat(sprintf("\n\n\n"))
    
    
    company <- get_variabel
    
    
    simpan_dframe = 0
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    for(k in 1 :  length(company) )
    {
      
      cat(sprintf(""))
      
      # cat(sprintf("%s\n\n", company[k]))    
      
      
     # alamat <- paste0('https://finance.yahoo.com/quote/',company[k],'/financials?p=',company[k])
      
      alamat <- paste0('https://finance.yahoo.com/quote/',company[k],'/cash-flow?p=',company[k])
      
      
      page <- read_html(alamat)
      nodes <- page %>%html_nodes(".fi-row")
      df = NULL
      
      for(i in nodes){
        r <- list(i %>%html_nodes("[title],[data-test='fin-col']")%>%html_text())
        df <- rbind(df,as.data.frame(matrix(r[[1]], ncol = length(r[[1]]), byrow = TRUE), stringsAsFactors = FALSE))
      }
      
      matches <- str_match_all(page%>%html_node('#Col1-3-Financials-Proxy')%>%html_text(),'\t{1,2}/\t{1,2}/\t{4}')  
      headers <- c('Breakdown','TTM', matches[[1]][,1]) 
      
      
      
      
      if(length(df) < 5)
      {
        # print(length(df))
        # cat(sprintf("%s Tidak Dapat Discraping, Karena Data Tidak Lengkap\n\n", company[k]))
        next
      }
      
      
      # print(length(df))
      # cat(sprintf("%s Dapat Discraping, Karena Data Lengkap\n\n", company[k]))
      
      names(df) <- headers
      
      
      hasil = df
      
      #############
      
      all_var = hasil[,1]
      indeks = all_var %in% get_var
      
      indeks = which(indeks == TRUE )
      
      
      hasil2 = hasil[c(indeks),]
      
      
      
      
      nama = get_var
      
      
      
      hasil2 = hasil2[-c(1)]
      
      
      colnames(hasil2) = c("TTM","2022","2021","2020")
      
      
      putar_data <- as.data.frame(t(as.matrix(hasil2)))
      
      
      
      perusahaan = rep(company[k],  length(  putar_data[,1]   )    )
      
      
      putar_data = data.frame(putar_data, perusahaan)
      
      Waktu = rownames(putar_data)
      
      
      
      
      Sector = rep(sektor,  length(  putar_data[,1]   )    )
      Board = rep(papan,  length(  putar_data[,1]   )    )
      
      
      
      putar_data = data.frame(putar_data, Waktu, sektor, Board)
      
      colnames(putar_data) = c(nama, "Perusahaan", "Waktu", "Sektor", "Papan")
      
      
      
      if(k == 1)
      {
        simpan_dframe = putar_data
      }
      
      if(k > 1)
      {
        simpan_dframe = rbind(simpan_dframe, putar_data)
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #Akhir
      
      if(k == length(company))
      {
        rownames(simpan_dframe) = NULL
        
        #simpan_dframe
        
        
        
        simpan_dframe2 = simpan_dframe
        
        TTM <- simpan_dframe2[,c("Waktu")]
        
        TTM <- as.vector(TTM)
        
        indeks <- TTM %in% c("2020","2021", "2022")
        
        
        indeks = which(indeks == TRUE)
        
        simpan_dframe2 = simpan_dframe2[c(indeks),]
        
        
        
        jumlah_kolom <- length(colnames(simpan_dframe2))
        
        
        a <- jumlah_kolom
        b <- jumlah_kolom - 1
        d <- jumlah_kolom - 2
        e <- jumlah_kolom - 3
        
        colnames(simpan_dframe2)[c(a, b, d, e)] = c("Board","Sector","Year", "Company")
        
        
        rownames(simpan_dframe2) = NULL
        
       
        print(simpan_dframe2)
        
      }
      
      
      
      
      
      
      
      
      
    }#Akhir for
    
      
   # }
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############
  ##############
  
  
  
  output$cetak_excel_ya <- renderText({
    
    
    
    
    
    input$goButton_excel
    #Sys.sleep(1)
    
    
    
    #  if(tes == 1)
    #{
    #  isolate({
    
    
    
    
    
    #get_var = c("Total Revenue","Net Income Common Stockholders","Basic EPS", "Basic Average Shares")
    
    #sektor = c("Properties & Real Estate")
    
    # papan = c("Pengembangan")
    
    
    #get_var = isolate(input$get_var_financial)
    #get_var = unlist(get_var)
    #get_var = as.vector(get_var)
    
    
    
    get_var = isolate(input$get_var_financial)
    get_var = strsplit(get_var, "\n")
    get_var = unlist(get_var)
    
    
    
    
    sektor = isolate(input$get_sector)
    sektor = unlist(sektor)
    sektor = as.vector(sektor)
    
    
    
    papan = isolate(input$get_papan)
    papan = unlist(papan)
    papan = as.vector(papan)
    
    
    
    
    
    
    
    
    
    
    
    get_variabel = isolate(input$terpilih_checkboxGroupInput)
    get_variabel = unlist(get_variabel)
    get_variabel = as.vector(get_variabel)
    
    #print(get_variabel)
    #print(length(get_variabel))
    #print(get_var)
    
    
    
    #cat(sprintf("\n\n\n"))
    
    
    company <- get_variabel
    
    
    simpan_dframe = 0
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    for(k in 1 :  length(company) )
    {
      
      
      # cat(sprintf("%s\n\n", company[k]))    
      
      
     # alamat <- paste0('https://finance.yahoo.com/quote/',company[k],'/financials?p=',company[k])
      
      alamat <- paste0('https://finance.yahoo.com/quote/',company[k],'/cash-flow?p=',company[k])
      
      
      page <- read_html(alamat)
      nodes <- page %>%html_nodes(".fi-row")
      df = NULL
      
      for(i in nodes){
        r <- list(i %>%html_nodes("[title],[data-test='fin-col']")%>%html_text())
        df <- rbind(df,as.data.frame(matrix(r[[1]], ncol = length(r[[1]]), byrow = TRUE), stringsAsFactors = FALSE))
      }
      
      matches <- str_match_all(page%>%html_node('#Col1-3-Financials-Proxy')%>%html_text(),'\t{1,2}/\t{1,2}/\t{4}')  
      headers <- c('Breakdown','TTM', matches[[1]][,1]) 
      
      
      
      
      if(length(df) < 5)
      {
        # print(length(df))
        # cat(sprintf("%s Tidak Dapat Discraping, Karena Data Tidak Lengkap\n\n", company[k]))
        next
      }
      
      
      # print(length(df))
      # cat(sprintf("%s Dapat Discraping, Karena Data Lengkap\n\n", company[k]))
      
      names(df) <- headers
      
      
      hasil = df
      
      #############
      
      all_var = hasil[,1]
      indeks = all_var %in% get_var
      
      indeks = which(indeks == TRUE )
      
      
      hasil2 = hasil[c(indeks),]
      
      
      
      
      nama = get_var
      
      
      
      hasil2 = hasil2[-c(1)]
      
      
      colnames(hasil2) = c("TTM","2022","2021","2020")
      
      
      putar_data <- as.data.frame(t(as.matrix(hasil2)))
      
      
      
      perusahaan = rep(company[k],  length(  putar_data[,1]   )    )
      
      
      putar_data = data.frame(putar_data, perusahaan)
      
      Waktu = rownames(putar_data)
      
      
      
      
      Sector = rep(sektor,  length(  putar_data[,1]   )    )
      Board = rep(papan,  length(  putar_data[,1]   )    )
      
      
      
      putar_data = data.frame(putar_data, Waktu, sektor, Board)
      
      colnames(putar_data) = c(nama, "Perusahaan", "Waktu", "Sektor", "Papan")
      
      
      
      if(k == 1)
      {
        simpan_dframe = putar_data
      }
      
      if(k > 1)
      {
        simpan_dframe = rbind(simpan_dframe, putar_data)
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #Akhir
      
      if(k == length(company))
      {
        rownames(simpan_dframe) = NULL
        
        #simpan_dframe
        
        
        
        simpan_dframe2 = simpan_dframe
        
        TTM <- simpan_dframe2[,c("Waktu")]
        
        TTM <- as.vector(TTM)
        
        indeks <- TTM %in% c("2020","2021", "2022")
        
        
        indeks = which(indeks == TRUE)
        
        simpan_dframe2 = simpan_dframe2[c(indeks),]
        
        
        
        jumlah_kolom <- length(colnames(simpan_dframe2))
        
        
        a <- jumlah_kolom
        b <- jumlah_kolom - 1
        d <- jumlah_kolom - 2
        e <- jumlah_kolom - 3
        
        colnames(simpan_dframe2)[c(a, b, d, e)] = c("Board","Sector","Year", "Company")
        
        
        rownames(simpan_dframe2) = NULL
        
        
        wb <- openxlsx::createWorkbook()
        
        
        hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
                                     border = "Bottom")
        
        
        hs2 <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                                     halign = "center", valign = "center", textDecoration = "bold",
                                     border = "TopBottomLeftRight")
        
        openxlsx::addWorksheet(wb, "Data", gridLines = TRUE)
        
        openxlsx::writeDataTable(wb, "Data", simpan_dframe2, rowNames = FALSE, startRow = 2, startCol = 2, tableStyle = "TableStyleMedium21")
        
        
        
        
        openxlsx::openXL(wb)
        
        
        
        
        
      }
      
      
      
      
      
      
      
      
      
    }   #Akhir for
    
    

    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  


} #akhir dari server











