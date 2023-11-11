#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(latex2exp)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Incerteza na medição aplicado em pacotes de onda"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          helpText("Incerteza na posição e número de onda aplicato a um pacote de onda. O pacote de onda foi formado a partir da superposição de 30 ondas progressivas com números de onda variando entre 0 a 2pi. A amplitude dessas ondas também variou seguindo uma distribuição gaussiana de largura sigma."),
          helpText(tags$li(tags$strong("Percebe-se que a largura da onda (linha vermelha) diminui à medida que a largura da curva k aumenta e vice-versa."))),
          helpText(tags$li(tags$strong("Percebe-se que quando sigma << 1 quase todas as ondas desaparecem restando apenas a onda progressiva onde k = pi. Agora, o mesmo efeito ocorre para os valores de x ( que indica a localização da partícula) à medida que a largura de k aumenta relativamente."))),
          sliderInput("sigma",
                        "Sigma:",
                        min = round(0.15, 2),
                        max = round(pi/2, 2),
                        value = round(0.15, 2))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distPlot")
        )
    )
)
