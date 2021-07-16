pkadata <- read.table(file = "compound_data.txt",
           sep = ";",
           header = TRUE,
           dec = ".")

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # App title ----
  titlePanel("Manpulating pKa values"),
  
  # Sidebar layout with input and output definitions ----s
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(inputId = "compound",
                  label = "Compound",
                  choices = pkadata[1], 
                  selected = 1),
      
      shinyjs::disabled(
      
       numericInput(inputId = "n_acids",
                   label = "Number of acidic pKas",
                   min = 0,
                   max = 3,
                   value = 1),
      
      numericInput(inputId = "n_bases",
                   label = "Number of basic pKas",
                   min = 0,
                   max = 3,
                   value = 0),
     
      sliderInput(inputId = "pka1",
                  label = "pka 1:",
                  min = -10,
                  max = 50,
                  value = 4.76,
                  step = 0.1),

      sliderInput(inputId = "pka2",
                  label = "pka 2:",
                  min = -10,
                  max = 50,
                  value = 50,
                  step = 0.1),

      sliderInput(inputId = "pka3",
                  label = "pka 3:",
                  min = -10,
                  max = 50,
                  value = 50,
                  step = 0.1),
      
      sliderInput(inputId = "pka4",
                  label = "pka 4:",
                  min = -10,
                  max = 50,
                  value = 50,
                  step = 0.1),
      
      sliderInput(inputId = "pka5",
                  label = "pka 5:",
                  min = -10,
                  max = 50,
                  value = 50,
                  step = 0.1),
      
      sliderInput(inputId = "pka6",
                  label = "pka 6:",
                  min = -10,
                  max = 50,
                  value = 50,
                  step = 0.1)
      ),
      
      sliderInput(inputId = "pHrange",
                  label = "pH range:",
                  min = -10,
                  max = 50,
                  value = c(0,14)),
      
      
      actionButton(inputId = "pHdefault",
                   label = "Reset pH range to default (0-14)")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
       plotOutput(outputId = "pkaplot"),
       
       plotOutput(outputId = "compound_image")
      
    )
  )
)