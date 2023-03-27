#rm(list = ls())

#library(tidyverse)
#library(plotly)

pot_harm = function(de, beta, x, x0){
  valor = 0.5*de*beta**2*(x-x0)**2
  return(valor)
}

pot_morse = function(de, beta, x, x0){
  valor = de+de*(exp(-2*beta*(x-x0))-2*exp(-beta*(x-x0)))
  return(valor)
}

x_max = function(e, de, beta, x0){
  valor = x0+sqrt(2*e/de)/beta
}

harmonico = function(de, beta, n){
  valor = beta*sqrt(2*de)*(n+0.5)
  return(valor)
}

morse = function(de, beta, n){
  valor = beta*sqrt(2*de)*((n+0.5)-(n+0.5)**2/sqrt(8*de))
  return(valor)
}

smpl=3000
rm = 5
de = 500
beta = 1.75
r0 = 1.5
r=seq(0,rm,2*rm/smpl)
#

df2 = data.frame(r=c(), n=c(), Potencial=c(), Energia=c())

# Potencial harmônico

for (i in 1:10) {
  
  df0 = data.frame(r=r, n=i, Potencial = "Harmônico", Energia = harmonico(de, beta, i))
  
  df0 = df0 %>%
    filter(
      r >= -x_max(harmonico(de, beta, i), de, beta, -r0) & r <= x_max(harmonico(de, beta, i), de, beta, r0)
    )
  
  df2 = rbind(df2,df0)
  
}

# Potencial Morse

for (i in 1:10) {
  
  x_mors = c()
  
  for (j in 1:length(r)) {
    if(pot_morse(de, beta, r[j], r0) < morse(de, beta, i) & r[j] < r0){
      x_mors[1] = r[j]
      break
      }
  }

  for (j in 1:length(r)) {
    if(pot_morse(de, beta, r[j], r0) > morse(de, beta, i) & r[j] > r0){
      x_mors[2] = r[j]
      break
    }
  }

  df0 = data.frame(r=r, n=i, Potencial = "Morse", Energia = morse(de, beta, i)) %>%
    filter(
    r >= x_mors[1] & r <= x_mors[2]
    )
  
  df2 = rbind(df2,df0)
  
}

# Curva

for (i in 1:10) {
  df2 = rbind(df2,
              data.frame(r=r, n=i, Potencial="Harm", Energia = pot_harm(de, beta, r, r0)),
              data.frame(r=r, n=i, Potencial="Mors", Energia = pot_morse(de, beta, r, r0))
  )
}

p = ggplot(df2, aes(x=r, y=Energia, frame=n))+
  geom_line(data=filter(df2, Potencial == "Harmônico"), color="blue", linetype = "dotted")+
  geom_line(data=filter(df2, Potencial == "Morse"), color="red", linetype = "dotted")+
  geom_line(data=filter(df2, Potencial == "Harm"), color="blue")+
  geom_line(data=filter(df2, Potencial == "Mors"), color="red")+
  ylim(0,700)

ggplotly(p)
