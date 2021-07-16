library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
print("Doubletest 22")

pkadata <- read.table(file = "compound_data.txt",
                      sep = ";",
                      header = TRUE,
                      dec = ".")

substrcount <- function(haystack, needle) {
  v = attr(gregexpr(needle, haystack, fixed = T)[[1]], "match.length")
  if (identical(v, -1L)) 0 else length(v)
}

getpkasFromFile <- function(compoundname){
  index <- which(pkadata$Compound_Name == compoundname)
  pkastring <- as.character(pkadata$pka_values[index])
  print(pkastring)
  pkas <- as.numeric(as.vector(unlist(strsplit(pkastring,","))))
  
  cat(sprintf("Function getpkasFromFile run. Compound %s returned pkas %s",
          compoundname,
          pkas))
  
  return(pkas)
}

getpkas <- function(compoundname){
  index <- which(pkadata$Compound_Name == compoundname)
  pkastring <- as.character(pkadata$pka_values[index])
  print(pkastring)
  pkas <- as.numeric(as.vector(unlist(strsplit(pkastring,","))))
  
  sprintf("Function getpkas run. Compound %s returned pkas %s",
          compoundname,
          pkas)
  
  return(pkas)
}

getbases <- function(compoundname) {
  index <- which(pkadata$Compound_Name == compoundname)
  pkastring <- pkadata$pka_types[index]
  n_bases = substrcount(pkastring, "B")
  
  sprintf("Function getbases run. Compound %s returned number of bases %s",
          compoundname,
          n_bases)
  
  return(n_bases)
}

setcustom <- function() {
  updateSelectInput(inputId = "compound", selected = "Custom")
}

server <- function(input, output, session) {
  
  n_pkas <- function() {
    return(input$n_acids + input$n_bases)
  }
  
  getchargerange <- function(compoundname) {
    
    n_acids = input$n_acids %>% as.integer
    n_bases = input$n_bases %>% as.integer
    
    cat(sprintf("Function getchargerange run. n_acids returned as %s and n_bases returned as %s",
            n_acids,
            n_bases)
    )
    
    return(c(-n_acids,n_bases))
  }
  
  observeEvent(input$n_acids, {
    showHidePkaSliders()
  })
  
  observeEvent(input$n_bases, {
    showHidePkaSliders()
  })

  observeEvent(input$pHdefault, {
    updateSliderInput(inputId = "pHrange", value = c(0,14))
  })
  
  showHidePkaSliders <- function(){
    
    n_pkas = input$n_bases + input$n_acids
    
    for(i in 1:6){
      id = paste("pka",i,sep="")
      if(n_pkas >= i){ 
        shinyjs::show(id)
      } else {
        shinyjs::hide(id)
      }
    }
  
  }
  
  observeEvent(input$compound, {

    pkas = getpkas(input$compound)
    n_bases = getbases(input$compound) %>% as.integer()
    n_acids = length(pkas) - n_bases %>% as.integer()
 
    for(i in 1:length(pkas)){
      id = paste("pka",i,sep="")
      updateSliderInput(inputId = id,
                        value = pkas[i])
      print(paste("Updated slider",i,"with value",pkas[i]))
    }

    updateNumericInput(inputId = "n_bases", value = n_bases)
    
    updateNumericInput(inputId = "n_acids", value = n_acids)
    
    if (input$compound == "Custom"){
      shinyjs::enable("n_acids")
      shinyjs::enable("n_bases")
      shinyjs::enable("pka1")
      shinyjs::enable("pka2")
      shinyjs::enable("pka3")
      shinyjs::enable("pka4")
      shinyjs::enable("pka5")
      shinyjs::enable("pka6")
    } else {  
      shinyjs::disable("n_acids")
      shinyjs::disable("n_bases")
      shinyjs::disable("pka1")
      shinyjs::disable("pka2")
      shinyjs::disable("pka3")
      shinyjs::disable("pka4")
      shinyjs::disable("pka5")
      shinyjs::disable("pka6")
      updateSliderInput(inputId = "pHrange",value=c(0,14))
    }

  })

  output$pkaplot <- renderPlot({
    
    pka1 <- input$pka1
    pka2 <- input$pka2
    pka3 <- input$pka3
    pka4 <- input$pka4
    pka5 <- input$pka5
    pka6 <- input$pka6
    
    pHrange <- input$pHrange
    bases <- input$n_bases
    
    coefficients = c(0,0,0,0,0,0)

    for(i in 1:n_pkas()){
      coefficients[i] <- 1
    }
    
    df <- data.frame(pHrange)
    func <- function(x) -coefficients[1] * exp(x - pka1)/(1 + exp(x - pka1)) +
      -coefficients[2] * exp(x - pka2)/(1 + exp(x - pka2)) +
      -coefficients[3] * exp(x - pka3)/(1 + exp(x - pka3)) +
      -coefficients[4] * exp(x - pka4)/(1 + exp(x - pka4)) +
      -coefficients[5] * exp(x - pka5)/(1 + exp(x - pka5)) +
      -coefficients[6] * exp(x - pka6)/(1 + exp(x - pka6)) + bases
    
    colourspectrum <- rev(c("#ff6666", # colour for +3 charge state
                      "#ff9e9e", # colour for +2 charge state
                      "#ffcbcb", # colour for +1 charge state
                      "#ffffff", # colour for 0 charge state
                      "#d0cbff", # colour for -1 charge state
                      "#9b91ff", # colour for -2 charge state
                      "#6d5eff")) # colour for -3 charge state
                    
    chargerange <- getchargerange(input$compound)
    
    charges <- rev(c(chargerange[1]:chargerange[2]))
  
    colours <- colourspectrum[charges + 4]
    
    reds <- c("#fea9a8","#ffffff","#a8d6fe")
    bg <- rasterGrob(colours, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
    
    print(c("ph range: ", pHrange))
    graph <- ggplot(df,aes(pHrange)) +
                  stat_function(fun=func,
                    size = 1.5) +
                  #ylim(chargerange) +
                  xlab("pH") +
                  ylab("Charge state") +
                  theme (plot.background = element_blank(),
                         panel.border = element_blank(),
                         panel.background=element_blank(),
                         text = element_text(size = 20),
                         panel.grid = element_line(color="Grey")) +
                  scale_y_continuous(limits = chargerange, breaks = seq(-3,3,0.25)) +
                  scale_x_continuous(limits = pHrange, breaks = seq(-10,50,2))
    grid.draw(bg)
    print(graph, newpage =FALSE)
    
  })
  
  output$compound_image <- renderImage({
    
    fname = paste("assets/", input$compound, ".png",sep ="")
    filename <- (fname)
    list(src = filename, align = "center",
         width = session$clientData$output_pkaplot_width)
    
  }, deleteFile = FALSE)
  
}
