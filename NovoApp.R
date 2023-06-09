library(shiny)
library(ggplot2)
library(OpenStreetMap)
library(tidyverse)
library(readxl)
library(sp)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Testes de Hipóteses"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu0", "Selecione mu0",
                  min = 0, max = 30, value = 15
      ),
      
      sliderInput("alfa", "Selecione alfa",
                  min = 0.01, max = 0.10, value = 0.05
      ),
      
      radioButtons("tipo", "Tipo do teste",
                   c("Bilateral" = "bi",
                     "Unilateral a Esquerda" = "esq",
                     "Unilateral a Direita" = "dir")),
      numericInput("sig2",
                   "Selecione a Variância",
                   value = 0)
      )
      ,
    
    # Show a plot of the generated distribution
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa", plotOutput('sa_map2_plt')),
                  tabPanel("Testes de hipóteses",
                           tableOutput('table'),
                           plotOutput('hist')
                  ),
                  tabPanel("Intervalo de Confiança", 
                           sliderInput(
                    "intervalo", "Selecione o Nível de Confiança: ",
                    min = 90, max = 99.9, value = 95, post = '%'
                  ),tableOutput('table2')),
                  tabPanel("Regressão", plotOutput('reg'),
                           tableOutput('table3'),
                           tableOutput('table4'),
                           tableOutput('table5'))
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  excel.data = read_xlsx("C:\\Users\\rodri\\OneDrive\\Documentos\\Estatística Exploratória 1\\2 VA\\dados_de_caminhada_corrida.xlsx")
  
  excel.data = excel.data %>% separate(Coordenadas,c("lat","long"),",") %>%
    separate(Velocidade,c("veloc","khm")," ") %>%
    mutate(long = as.numeric(long), lat = as.numeric(lat), veloc = as.numeric(veloc))
  
  long = excel.data %>%
    select(long)
  lat = excel.data %>%
    select(lat)
  
  bb = matrix(c(-34.945, -34.955,
                -8.018, -8.0135), 2,2, byrow=T)
  rownames(bb) = c('long', 'lat')
  colnames(bb) = c('min', 'max')
  
  crs = CRS("+proj=utm +zone=25 +south +datum=WGS84")
  
  
  df = data.frame(veloc = excel.data %>% select(veloc),
                  long = excel.data %>% select(long),
                  lat = excel.data %>% select(lat))
  
  
  lonr = bb[1,2]; latu = bb[2,2]
  lonl = bb[1,1]; latd = bb[2,1]
  
  sa_map = openmap(c(latu+0.001, lonl-0.001),
                   c(latd-0.001, lonr+0.001),
                   type = "osm", mergeTiles = TRUE, minNumTiles = 9L)
  
  sa_map2 = openproj(sa_map)
  
  
  sa_map2_plt = OpenStreetMap::autoplot.OpenStreetMap(sa_map2) +
    geom_point(data = df,
               aes(x = long, y = lat), # slightly shift the points
               colour = "red", size =  2.5) +
    xlab("Longitude") + ylab("Latitude")
  
  output$sa_map2_plt <- renderPlot(sa_map2_plt)
  
  ################################################################################
  
  df2 = excel.data %>% separate(Hora, c("dia", "hora"), " ") %>% 
    separate(hora, c("horas","minutos","segundos"), ":")
  
  
  df2_new = df2 %>% filter(minutos == 40 & segundos >= 53 | minutos > 40) %>%
    filter(minutos == 45 & segundos <= 12 | minutos < 45)
  
  
  sig2 = reactive({
    as.numeric(input$sig2)
  })

  
  x = df2_new %>% select(veloc)
  
  
  n = reactive(length(x$veloc))
  
  xbarra = reactive(mean(x$veloc))
  
  sig_xbar = reactive(sqrt(sig2())/sqrt(n()))
  
  mu0 = reactive({
    as.integer(input$mu0)
  })
  
  alfa = reactive(as.numeric(input$alfa))
  
  tipo = reactive(input$tipo)
  teste = renderText(tipo())
  
  p = reactive({
    if(teste() == "bi"){
      p = 1 - alfa() + alfa()/2
    }else if(teste() == "esq"){
      p = alfa()
    }else{
      p = 1-alfa()
    }
  })
  
  
  ztab = reactive(
    as.numeric(qnorm(p()))
  )
  
  
  zcalc = reactive(
    as.numeric((xbarra()-mu0())/sig_xbar())
  )
  
  
  output$table <- renderTable(
    if(teste() == "bi" & zcalc() < ztab() & zcalc() > -ztab() |
       teste() == "esq" & zcalc() > ztab() |
       teste() == "dir" & zcalc() < ztab()
    ){
      data.frame(Resultado = paste0('Aceitamos H0 ao nível de ', (alfa()*100), '%'))
    }else{
      data.frame(Resultado = paste0('Rejeitamos H0 ao nível de ', (alfa()*100), '%'))
    }
  )
  
  output$hist = renderPlot({
    hist(x$veloc, main='', freq = FALSE)
    abline(v = mu0(), col= 'red')
    abline(v = xbarra(), col= 'blue')
  })
  
  #########################################################################
  
  df3 = excel.data %>% separate(Hora, c("dia", "hora"), " ") %>% 
    separate(hora, c("horas","minutos","segundos"), ":")
  
  
  df3_new = df2 %>% filter(minutos == 45 & segundos >= 18 | minutos > 45) %>%
    filter(minutos == 49 & segundos <= 23 | minutos < 49)
  
  x3 = df3_new %>% select(veloc)
  
  x3bar = reactive(mean(x3$veloc))
  
  intervalo = reactive(as.numeric(input$intervalo))
  
  n3 = reactive(length(x3$veloc))
  
  alfa3 = reactive((100-intervalo())/100)
  
  z = reactive(qnorm(1 - (alfa3()/2)))
  
  s2 = reactive(var(x3$veloc))
  
  sxbar = reactive(sqrt(s2())/sqrt(n3()))
  
  
  ic_neg = reactive(x3bar() - (z()*sxbar()))
  ic_pos = reactive(x3bar() + (z()*sxbar()))
  
  output$table2 <- renderTable(
      data.frame(Resultado = paste0('IC(ao nível de ', intervalo(), '%) = [ ',
                                    ic_neg(), ' ; ', ic_pos(),' ]'))
    
  )
  
  
  #############################################################################
  
  n4 = length(cars$speed)
  
  x4 = cars$dist
  y4 = cars$speed
  num = n4*sum(x4*y4) - sum(x4)*sum(y4)
  den1 = sqrt(n4*sum(x4^2) - sum(x4)^2)
  den2 = sqrt(n4*sum(y4^2) - sum(y4)^2)
  
  r = num/(den1*den2)
  r2 = r^2
  
  reta = lm(y4~x4)
  
  a = round(reta$coefficients[1], 2)
  b = round(reta$coefficients[2], 2)
 
  output$reg <- renderPlot(plot(cars$dist,cars$speed,pch = 16,
                                cex = 1.3, col = "blue",xlab="Distância",
                                ylab="Velocidade", xlim = c(0,120),
                                ylim = c(0 , 30)) + abline(a = a, b = b))
  
  output$table3 <- renderTable(
    data.frame("R" = paste0(r))
  )
  
  output$table4 <- renderTable(
    data.frame("R2" = paste0(r2))
  )
  
  output$table5 <- renderTable(
    data.frame("Equação da Reta" = paste0('y = ', b, ' + ', a , 'x'))
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
