#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

library(plotly)
source('leitor.R')


dados = ler_gastos()

qtdPorEstado = dados %>%
  select(txNomeParlamentar, sgUF) %>%
  distinct() %>% 
  group_by(sgUF)%>%
  summarise(depPorEstado = n())


gastosPorEstado = dados %>%
  select(vlrLiquido, sgUF) %>% 
  group_by(sgUF)%>%
  summarise(gastoEstado = sum(vlrLiquido))

x = merge(qtdPorEstado, gastosPorEstado)

nordeste = c("MA", "PI", "CE","RN", "SE", "BA", "PB", "PE", "AL")
norte = c("AM", "RR", "AP","PA", "TO", "RO", "AC")
centroeste = c("DF", "MT", "MS", "GO")
sudeste = c("SP", "RJ", "ES","MG")
sul = c("PR", "RS", "SC")

dataframe = mutate(x, regiao = "NA")
dataframe$sgUF = as.character(dataframe$sgUF)

for (row in 1:nrow(dataframe)) {
  if(dataframe$sgUF[row] %in% nordeste){
    dataframe$regiao[row] = "Nordeste"
  }
  else if(dataframe$sgUF[row] %in% norte){
    dataframe$regiao[row] = "Norte"
  }
  else if(dataframe$sgUF[row] %in% centroeste){
    dataframe$regiao[row] = "Centro-Oeste"
  }  
  else if(dataframe$sgUF[row] %in% sudeste){
    dataframe$regiao[row] = "Sudeste"
  }
  else if(dataframe$sgUF[row] %in% sul){
    dataframe$regiao[row] = "Sul"
  }
  
}
dataframe = na.omit(dataframe)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$trendPlot <- renderPlotly({
    
    # a simple histogram of movie ratings
    p <- plot_ly(dataframe, x = depPorEstado, y = gastoEstado, text = paste("Estado: ", sgUF), name = "nome",
                 mode = "markers", group = regiao)
    # style the xaxis
    layout(
      title = "Gráfico da relação: Quantidade de deputados x Gastos",
      xaxis = list(title = "Quantidade de deputados"),
      yaxis = list(title = "Gasto em milhões de reais"),
      margin = list(l = 65)
    )
  })
})