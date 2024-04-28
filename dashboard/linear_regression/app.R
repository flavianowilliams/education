library(tidyverse)
library(shiny)
library(plotly)

ui = fluidPage(
  titlePanel("Statistical analysis of physical problems"),
  sidebarLayout(
    sidebarPanel(helpText("Analysis is an open source platform dedicated
                          to data analysis of physical problems. For more information, visit ",
                 a("wiki documentation",href="https://github.com/flavianowilliams/analysis"),
                 " in the GitHub page."),
    fileInput("file","Upload your dataset",accept = ".csv"),
    checkboxInput("checkbox", label = "Separador ponto e vírgula.", value = FALSE),
    numericInput("num_x", label = "Escolha um valor acima de zero para o dado que gostaria de excluir.", value = 0),
    textInput("num_y", label = "Escolha a variável que gostaria de excluir.", value = NULL),
    br(),
    actionButton("apply", label="Aplicar")),
    mainPanel(
      navbarPage("Analysis",
                 tabPanel("Dataframe",
                          tableOutput("dataframe")
                 ),
                 tabPanel("Statistics",
                          tableOutput("stats")
                 ),
                 tabPanel("Plot",
                            plotlyOutput("plot")
    )
      )
    )
  )
)

# Define server logic ----
server = function(input, output) {
  
  v = reactiveValues(data = NULL)
  
  currentFile = reactive({
    file = input$file
    ext = tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please, upload a csv file!"))
    
    if (input$checkbox) {
      read_csv2(file$datapath)
    } else {
      read.csv(file$datapath)
    }
  })

  output$dataframe = renderTable({currentFile()})

  observeEvent(input$apply, {

    i = input$num_x
    j = input$num_y

    data = currentFile() %>%
      gather(everything(), key = "Experimento", value = "valor", na.rm = TRUE) %>%
      mutate(valor = as.numeric(valor)) %>%
      filter(Experimento != j)

    if (i > 0) {
      data = data[-i, ]
    }
    
    v$data = data

  })
  
  statsreact = eventReactive(input$apply, {
    data = v$data %>%
      group_by(Experimento) %>%
      summarise(media = mean(valor), desvio_padrao = sd(valor))
  })
  
  output$stats = renderTable({statsreact()})
  
    plotreact = eventReactive(input$apply, {

      p = ggplot(data = v$data, aes(x = Experimento, y = valor, fill = Experimento))+
        geom_violin()+
        theme(legend.position = "none")

    ggplotly(p)
    
  })
  
  output$plot = renderPlotly({
      plotreact()
  })

}

shinyApp(ui, server)
