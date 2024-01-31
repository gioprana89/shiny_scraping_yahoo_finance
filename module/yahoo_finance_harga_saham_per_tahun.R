


########################################
########UI (User Interface)#############
########################################

yahoo_finance_harga_saham_per_tahun_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    
    h2("Stock Price", style="
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
                   
                   
                   
                   #textAreaInput(ns("get_var_financial"), "Year (Only One Year)", value = "Total Revenue\nNet Income Common Stockholders\nBasic EPS\nBasic Average Shares", height = 150, width = 260),
                   #
                   #
                   
                   
                   textAreaInput(ns("get_year"), "Year (Only One Year)", value = "2022", height = 150, width = 260),
                   
                   
                   
                   
                  # textAreaInput(ns("get_papan"), "Sector", value = "Pengembangan", height = 150, width = 260)
                   #
                   
                   
                   
                   
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



yahoo_finance_harga_saham_per_tahun_server <- function(input, output, session) {
  

  
  
  
  
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
    
    
    input$goButton
    Sys.sleep(1.5)
    
    
    #get_var = c("Total Revenue","Net Income Common Stockholders","Basic EPS", "Basic Average Shares")
    
    #sektor = c("Properties & Real Estate")
    
   # papan = c("Pengembangan")
    
    
    #get_var = isolate(input$get_var_financial)
    #get_var = unlist(get_var)
   #get_var = as.vector(get_var)
   
    if(input$goButton > 0)
    {
   
    
    get_year = isolate(input$get_year)
    get_year = strsplit(get_year, "\n")
    get_year = unlist(get_year)
    get_year = as.character(get_year)
    
    
    
    
    
    
    
    get_variabel = isolate(input$terpilih_checkboxGroupInput)
    get_variabel = unlist(get_variabel)
    get_variabel = as.vector(get_variabel)
    
    
    
    
    ambil_tahun <- get_year
    
    waktu1 <- as.Date(paste0(ambil_tahun,"-12-01"))
    waktu2 <- as.Date(paste0(ambil_tahun,"-12-31"))
    
    
    simpan_open <- vector(mode = "numeric")
    simpan_high <- vector(mode = "numeric")
    simpan_low <- vector(mode = "numeric")
    simpan_close <- vector(mode = "numeric")
    simpan_adjust_close <- vector(mode = "numeric")
    simpan_volume <- vector(mode = "numeric")
    simpan_waktu <- vector(mode = "character")
    simpan_nama_perusahaan <- vector(mode = "character")
    
    
    simpan_indeks <- vector(mode = "numeric")
    
    nama_perusahaan <- get_variabel
    
    dframe <- 0
    
    for(i in 1 : length(nama_perusahaan))
    {
      
      
      dataku <- pdfetch_YAHOO(nama_perusahaan[i],
                              from = as.Date(waktu1),
                              to = as.Date(waktu2),
                              interval = "1d")
      
      
   
      
      if(length(dataku) > 0)
      {
      
      dataku <- as.data.frame(dataku)
      
      panjang <- length(dataku[,1])
      
      x <- dataku[c(panjang),]
      
      tanggal <- rownames(x)
      
      
      simpan_open[i] <- x[,1]
      simpan_high[i] <- x[,2]
      simpan_low[i] <- x[,3]
      simpan_close[i] <- x[,4]
      simpan_adjust_close[i] <- x[,5]
      simpan_volume[i] <- x[,6]
      simpan_waktu[i] <- tanggal
      simpan_nama_perusahaan[i] <- nama_perusahaan[i]
      
      
      }
      
      
      
      
    }
    
    
    dframe <- data.frame(simpan_nama_perusahaan, simpan_open, simpan_high, simpan_low, simpan_close, simpan_adjust_close, simpan_volume, simpan_waktu)
    
    
    colnames(dframe) <- c("Company","Open", "High", "Low", "Close", "Adj. Close", "Volume", "Date")
    
    
    
    print(dframe)
    
    
    
    }
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$cetak_excel_ya <- renderText({
    
    
    
    
    
    input$goButton_excel
 
    
    
    #get_var = c("Total Revenue","Net Income Common Stockholders","Basic EPS", "Basic Average Shares")
    
    #sektor = c("Properties & Real Estate")
    
    # papan = c("Pengembangan")
    
    
    #get_var = isolate(input$get_var_financial)
    #get_var = unlist(get_var)
    #get_var = as.vector(get_var)
    
    if(input$goButton_excel > 0)
    {
      
      
      get_year = isolate(input$get_year)
      get_year = strsplit(get_year, "\n")
      get_year = unlist(get_year)
      get_year = as.character(get_year)
      
      
      
      
      
      
      
      get_variabel = isolate(input$terpilih_checkboxGroupInput)
      get_variabel = unlist(get_variabel)
      get_variabel = as.vector(get_variabel)
      
      
      
      
      ambil_tahun <- get_year
      
      waktu1 <- as.Date(paste0(ambil_tahun,"-12-01"))
      waktu2 <- as.Date(paste0(ambil_tahun,"-12-31"))
      
      
      simpan_open <- vector(mode = "numeric")
      simpan_high <- vector(mode = "numeric")
      simpan_low <- vector(mode = "numeric")
      simpan_close <- vector(mode = "numeric")
      simpan_adjust_close <- vector(mode = "numeric")
      simpan_volume <- vector(mode = "numeric")
      simpan_waktu <- vector(mode = "character")
      simpan_nama_perusahaan <- vector(mode = "character")
      
      
      simpan_indeks <- vector(mode = "numeric")
      
      nama_perusahaan <- get_variabel
      
      dframe <- 0
      
      for(i in 1 : length(nama_perusahaan))
      {
        
        
        dataku <- pdfetch_YAHOO(nama_perusahaan[i],
                                from = as.Date(waktu1),
                                to = as.Date(waktu2),
                                interval = "1d")
        
        
        
        
        if(length(dataku) > 0)
        {
          
          dataku <- as.data.frame(dataku)
          
          panjang <- length(dataku[,1])
          
          x <- dataku[c(panjang),]
          
          tanggal <- rownames(x)
          
          
          simpan_open[i] <- x[,1]
          simpan_high[i] <- x[,2]
          simpan_low[i] <- x[,3]
          simpan_close[i] <- x[,4]
          simpan_adjust_close[i] <- x[,5]
          simpan_volume[i] <- x[,6]
          simpan_waktu[i] <- tanggal
          simpan_nama_perusahaan[i] <- nama_perusahaan[i]
          
          
        }
        
        
        
        
      }
      
      
      dframe <- data.frame(simpan_nama_perusahaan, simpan_open, simpan_high, simpan_low, simpan_close, simpan_adjust_close, simpan_volume, simpan_waktu)
      
      
      colnames(dframe) <- c("Company","Open", "High", "Low", "Close", "Adj. Close", "Volume", "Date")
      
      
      
      wb <- openxlsx::createWorkbook()
      
      
      hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
                                   border = "Bottom")
      
      
      hs2 <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                                   halign = "center", valign = "center", textDecoration = "bold",
                                   border = "TopBottomLeftRight")
      
      openxlsx::addWorksheet(wb, "Stock Price", gridLines = TRUE)
      
      openxlsx::writeDataTable(wb, "Stock Price", dframe, rowNames = FALSE, startRow = 2, startCol = 2, tableStyle = "TableStyleMedium21")
      
      
      
      
      openxlsx::openXL(wb)
      
      
      
    }
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


} #akhir dari server











