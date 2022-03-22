library(shiny)
library(tidyverse)
library(here)

# Load complete RNA dataset ---------------------------------------------------
rnaData = readRDS(here("data", "rnaData.rds"))

# Vector with unique gene names
genes = sort(unique.default(rnaData$Gene))



# Define UI for application that draws a histogram
ui = fluidPage(

  # Application title
  titlePanel("Explore the AIM dataset"),

  p("This amazing app gives a unique insight in the AIM dataset."),

  # Sidebar with controls
  sidebarLayout(
    sidebarPanel(
      selectizeInput("geneselect", label = "Gene", choices = NULL),
      radioButtons("splitvar", label = "Split on", choices = c("Sex", "Treatment", "Modic"), inline = TRUE),
      checkboxInput("logy", "Use logarithmic y-axis")
    ),

    # Main plot
    mainPanel(
      plotOutput("violinplot"),
      plotOutput("spagplot"),
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output, session) {

  # populate gene selection
  updateSelectizeInput(session, 'geneselect', choices = genes, selected = "ENSG00000223972", server = TRUE)

  plotdat = reactive({
    rnaData %>% filter(Gene == input$geneselect)
  })

  output$violinplot = renderPlot({
    p = violins(plotdat(), input$splitvar) +
      labs(title = input$geneselect,
           x = NULL,
           y = ifelse(input$logy, "Read counts (log)", "Read counts")) +
      scale_fill_manual(values = c("orange", "blue")) +
      theme_classic(base_size = 16)

    if(input$logy)
      p = p + scale_y_log10(oob = scales::squish_infinite)

    suppressWarnings(print(p))
  })

  output$spagplot = renderPlot({
    p = spaghetti(plotdat(), input$splitvar) +
      labs(title = input$geneselect,
           x = NULL,
           y = ifelse(input$logy, "Read counts (log)", "Read counts")) +
      scale_colour_manual(values = c("orange", "blue")) +
      theme_classic(base_size = 16)

    if(input$logy)
      p = p + scale_y_log10(oob = scales::squish_infinite)

    suppressWarnings(print(p))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
