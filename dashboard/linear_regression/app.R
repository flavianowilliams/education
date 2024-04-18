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
    textInput("x", h4("Informe a coluna com os valores de x"), value = "X"),
    textInput("y", h4("Informe a coluna com os valores de y"), value = "Y"),
    checkboxInput("checkbox", label = "Separador ponto e vírgula.", value = FALSE),
    numericInput("num", label = "Escolha um valor acima de zero para o dado que gostaria de excluir.", value = 0),
    br(),
    actionButton("apply", label="Aplicar")),
  #    br(),
#    downloadButton("download", label="Download")),
    mainPanel(
      navbarPage("Analysis",
                 tabPanel("Dataframe",
                          tableOutput("dataframe")
                 ),
                 tabPanel("Statistics",
                          textOutput("stats")
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

    x = input$x
    y = input$y
    i = input$num
    
    data = currentFile() %>%
      mutate(x = get(x)) %>%
      mutate(y = get(y))

    if (i > 0) {
      data = data[-i,]
    }
    
    v$data = data
    v$x = x
    v$y = y

  })
  
  output$stats = renderText({
    
    if (is.null(v$data$y)) return()
      sprintf("Valor médio: %f; Desvio padrão: %f", mean(v$data$y), sd(v$data$y))

    })

    plotreact = eventReactive(input$apply, {

    p = ggplot(data=v$data,aes(x=x,y=y))+
      xlab(v$x)+
      ylab(v$y)+
      geom_point()+
      annotate("segment", x = "A", xend = "J", y = mean(v$data$y), yend = mean(v$data$y), colour = "red")+
      geom_errorbar(ymin = mean(v$data$y)-0.5*sd(v$data$y), ymax = mean(v$data$y)
                    +0.5*sd(v$data$y), colour = "orange")
    
    ggplotly(p)
    
  })
  
  output$plot = renderPlotly({
      if (is.null(v$data$y)) return()
      plotreact()
  })

#  output$download = downloadHandler(
#    filename = function() {
#      paste(input$x, "_", input$y, "-", Sys.Date(), ".pdf", sep = "")
#    },
#    content = function(file) {
#    ggsave(file,plot = plotreact())
#  })

}

shinyApp(ui, server)
