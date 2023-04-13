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
      radioButtons("dados", "Escolher dados",
                   c("Dados 1" = "d1",
                     "Dados 2" = "d2",
                     "Dados 3" = "d3")
      ),
      sliderInput("mu0", "Selecione mu0",
                  min = 0, max = 30, value = 15
      ),
      
      sliderInput("alfa", "Selecione alfa",
                  min = 0.01, max = 0.10, value = 0.05
      ),
      
      radioButtons("tipo", "Tipo do teste",
                   c("Bilateral" = "bi",
                     "Unilateral a Esquerda" = "esq",
                     "Unilateral a Direita" = "dir"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa", plotOutput('sa_map2_plt')),
                  tabPanel("Testes de hipóteses",
                           tableOutput('table'),
                           plotOutput('hist')
                  ),
                  tabPanel("Intervalo de Confiança", "Nada por enquanto!"),
                  tabPanel("Regressão", plotOutput('reg'))
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  dados = reactive(input$dados)
  escolha_dados = renderText(dados())
  
  x = reactive({
    if(escolha_dados() == "d1"){
      c(2,5,13, 27, 21, 10, 9, 15)
    }else if(escolha_dados() == "d2"){
      c(15,2,13, 15, 12, 10, 5, 4)
    }else{
      c(1, 2, 13, 27, 7, 16, 5, 20)
    }
  })
  
  
  n = reactive(length(x()))
  xbarra = reactive(mean(x()))
  sig = reactive(sd(x()))
  sig_xbar = reactive(sig()/sqrt(n()))
  
  mu0 = reactive({
    as.integer(input$mu0)
  })
  
  
  alfa = reactive(as.numeric(input$alfa))
  
  tipo = reactive(input$tipo)
  teste = renderText(tipo())
  
  p = reactive({
    if(teste() == "bi"){
      1 - alfa() + alfa()/2
    }else if(teste() == "esq"){
      alfa()
    }else{
      1-alfa()
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
       teste() == "esq" & zcalc() < ztab() |
       teste() == "dir" & zcalc() > ztab()
    ){
      data.frame(Resultado = paste0('Aceitamos H0 ao nível de sig. = ', alfa()))
    }else{
      data.frame(Resultado = paste0('Rejeitamos H0 ao nível de sig. = ', alfa()))
    }
  )
  
  
  
  output$hist = renderPlot({
    hist(x(), main='', freq = FALSE)
    abline(v = mu0(), col= 'red')
    abline(v = xbarra(), col= 'blue')
  })
  
  output$reg = renderPlot({
    #gráfico de regressão com dados 'cars' aqui!
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
