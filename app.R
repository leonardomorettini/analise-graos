library(shiny)
library(ggplot2)
library(geobr)
library(sf)
library(dplyr)

#Data transformation
analise_graos <- read.csv ("analise_graos1.csv")

df_01 <- transform(analise_graos, kg_hectare = as.integer(analise_graos$kg_hectare))

df_02<-  transform(df_01, Ano = as.Date(as.character(df_01$Ano), "%Y"))

#Final data df_02

ui<-fluidPage(
  titlePanel("Analise Agricola no Brasil"),
  
  sidebarPanel(
    p("Fonte: IBGE - Producao Agricola Municipal")),
  
  selectInput( "Grao", 
               label = "Escolha o Grao",
               choices = unique(df_02$Produto_lavouras)),
  selectInput( "Estado", 
               label = "Escolha o Estado",
               choices = unique(df_02$UF)),
  
  # Copy the line below to make a slider range 
  #  sliderInput("slider2", label = h3("Slider Range"), min = 2011, 
  #             max = 2021, value = c(2011, 2018)),
  
  mainPanel(
    plotOutput(outputId = "dados")
  )
)

server<-function(input, output, session){
  
  #Create FIlter
  output$dados <- renderPlot({    
    data_a<- df_02
    
    if (input$Grao != "All") {
      data_a <- data_a[data_a$Produto_lavoura == input$Grao,]
    }
    
    if (input$Estado != "All") {
      data_a <- data_a[data_a$UF == input$Estado,]
    }
    
    ggplot(data_a, aes(x = data_a$Ano, y = data_a$kg_hectare)) + 
      
      geom_col(stat="identity",color="black",fill="blue") +
      labs(title = element_text("Kg produzidos por hectare")) +
    ylab("Quantidade") +
      xlab("Ano") +
      theme_classic()
      
  }
  )
}

shinyApp(ui=ui, server=server)
