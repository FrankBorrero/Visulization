#Interactive Visualization Tool Assignment 
#Visualization 

# Define UI ----

#setwd("C:/Users/leang/OneDrive/Maestría/I Semestre/Visualization/Final assigment/Int_Visualization")
bd <- read.csv(file = "data/nba_2017_br.csv", header = TRUE, sep = ",")

ui <- fluidPage (
  titlePanel("NBA Stats - Regular Season 2017"),
  sidebarLayout(
    sidebarPanel(
      img(src = "nba.png", height = 50, width = 100),
      helpText("General NBA Stats for each player per game of the 2017 regular season"),
      selectInput("select", h4("Choose a NBA team to display"), 
                  choices = bd$Tm, selected = 1),
      sliderInput("slider", p(h4("Games played per player in regular season"),
                            h5("Range of interest:")),
                  min(bd$G), max (bd$G), value = c(min(bd$G),max(bd$G)))
    ),
    mainPanel(
      plotOutput("plot"))
  )
)

# Define server logic ----

server <- function(input, output) {
  
  Graph <- bd[,c(2,5,6,10)]
  
  output$plot <- renderPlot({
    ggplot(Graph, aes(x=Player, y=FGA)) +
      geom_segment(aes(x=Player, xend=Player, y=0, yend=FGA), color="black")+
      geom_point(color="ligthblack", size=4, alpha=0.6)+
      theme_light() +
      coord_flip()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
