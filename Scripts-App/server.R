###############################################
#
# project: Compare tariff prices by Monitor
#          from  16/17 to 17/18
#    date: 09/12/16
# details: Server for app
#
###############################################

shinyServer(function(input, output){
  
  #filter hrg list and return a data frame
  hrglist <- reactive({
    hrg.choice <- input$hrgchoice
    h <- full.df %>%
      filter(hrgid.1112 == hrg.choice)
    h <- as.data.frame(h)
    h
  })
  
  #reactive function for pricetype
  price.type <- reactive({
    p <- input$pricetype
    if (p == "Day-case and Elective") return("tariff.dcel")
    if (p == "Day-case only") return("tariff.dc")
    if (p == "Elective only") return("tariff.el")
    if (p == "Non-elective") return("tariff.nel")
  })
  
  #function for tooltip
  hrg_tooltip <- function(x) {
    
    if(is.null(x)) return(NULL)
    if(is.null(x$altID)) return(NULL)
    
    #pick out all hrgs linked to the chosen 11/12 hrg
    hrgs <- full.df[full.df$altID == x$altID,]
    
    #create label
    hrglabel <- paste(hrgs$hrgcode, hrgs$hrg, sep = "-")
    rootlabel <- paste0("Root: ", hrgs$root)
    paste0("<b>", hrglabel, "</b><br>", rootlabel, "<br>")
    
  }
  
  #reactive for ggvis plot
  vis <- reactive({
    
    xvar <- prop("x", ~year)
    yvar <- prop("y", as.symbol(price.type()))
    
    hrglist %>% 
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 75, 
                   size.hover := 200, 
                   fillOpacity := 0.5, 
                   fillOpacity.hover := 0.7,
                   key := ~altID) %>%
      add_tooltip(hrg_tooltip, "hover") %>%
      add_axis("y", title = "Tariff in sterling", title_offset = 50) %>%
      add_axis("x", values = seq(from=2015, to=2018, by = 1)) %>%
      scale_numeric("x", domain = c(2015, 2018),round = TRUE) %>%
      set_options(renderer = "canvas",width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
  
})