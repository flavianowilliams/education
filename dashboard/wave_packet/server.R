#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlotly({

      # espaço dos momentos
      k0 = pi
      sigma = input$sigma
      n=40
      #
      k = seq(0, 2*pi, 2*pi/n)
      #
      k_func = function(k, k0, sigma) {
        funcao = exp(-0.5*(k-k0)**2/sigma**2)
        return(funcao)
      }
      
      k_df = data.frame(k=k, psi = k_func(k, k0, sigma = sigma))
      
      # espaço das posições
      psi = function(n, x, k0, sigma, omega, t) {
        dk = 2*pi/n
        soma = 0.0
        for (i in 0:n) {
          k = i*dk
          soma = soma+k_func(k, k0, sigma)*cos(k*x-omega*t)
        }
        return(soma)
      }
#      
      x = seq(-20, 20, 0.1)
      t = 0
#      
      df = data.frame(x = x, t = t, psi = psi(n=n, x=x, t=t, omega = pi, sigma=sigma, k0=k0))
      
      fig1 = ggplot(data = k_df, mapping = aes(x = k, y = psi))+
        geom_point()+
        geom_line()+
        labs(caption = "Amplitude versus número de onda")+
        xlab("k")+
        ylab("Amplitude")
#      
      fig2 = ggplot(mapping = aes(x=x, y=psi))+
        geom_line(data = df)+
        labs(caption = "Amplitude versus posição")+
        xlab("k")+
        ylab("Amplitude")

      fig1 = plot_ly(data = k_df, x = ~k, y = ~psi, name = "k", mode = 'lines+markers', marker = list(size = 10, color = "black"), line = list(size = 10, color = "black"))
            
      fig2 = plot_ly(data = df, x = ~x, y = ~psi, name = "x", mode = 'lines', line = list(size = 10, color = "red")) %>%
        layout(xaxis = list(title = "x", range = c(-20, 20)))

      fig = subplot(fig1, fig2)
      
      return(fig)

    })

}
