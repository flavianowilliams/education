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

    output$distPlot <- renderPlot({

      # espaço dos momentos
      k0 = pi
      sigma = input$sigma
      n=10
      #
      k = seq(0, 2*pi, 2*pi/n)
      #
      k_func = function(k, k0, sigma) {
        funcao = exp(-0.5*(k-k0)**2/sigma**2)
        return(funcao)
      }
      
      k_df = data.frame(k=k, sigma=sigma, psi = k_func(k, k0, sigma = sigma))
      
      # espaço das posições
#      psi = function(n, x, k0, sigma, omega, t) {
#        dk = 2*pi/n
#        soma = 0.0
#        for (i in 0:n) {
#          k = i*dk
#          soma = soma+k_func(k, k0, sigma)*cos(k*x-omega*t)
#        }
#        return(soma)
#      }
#      
#      x = seq(0, 20, 0.1)
#      t = 0
#      
#      df = data.frame(x = x, t = 0, frame=1, psi = psi(n=n, x=x, t=t, omega = pi, sigma=sigma, k0=k0))
      
      ggplot(mapping = aes(x = k, y = psi))+
        geom_point(data = filter(k_df, sigma == sigma))
      
    })

}
