library(tidyverse)
library(deSolve)
library(shiny)

direct_sys <- function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    
    dX  <- -b1*X*Y1 - b2*X*Y2
    dY1 <- b1*X*Y1 - g*Y1 + d*b1*Y1*Z1 + a*b1*Y1*Z2
    dZ1 <- g*Y1 - d*b1*Y1*Z1 - a*b2*Y2*Z1
    if (t < t_i){
      dY2 <- 0.0
      dZ2 <- 0.0
    }else{
      dY2 <- b2*X*Y2 - g*Y2 + d*b2*Y2*Z2 + a*b2*Y2*Z1
      dZ2 <- g*Y2 - d*b2*Y2*Z2 - a*b1*Y1*Z2  
    }
    
    return(list(c(dX,dY1,dY2,dZ1,dZ2)))
    
  })
  
}

server <- function(input, output) {
  output$direct <- renderPlot({
    state <- c(X=input$X,Y1=input$Y1,Y2=input$Y2,Z1=0,Z2=0)
    parameters <- c(b1=1.0,b2=input$b2,a=input$a,g=0.3,d=0.5,t_i=input$t_i)
    times <- seq(0,100,by=0.1)
    direct_sol <- ode(y=state,times=times,func=direct_sys,parms=parameters)
    direct_sol_df <- direct_sol %>% 
      data.frame() %>% as_tibble() %>% 
      mutate(support_site_1=Y1+Z1,support_site_2=Y2+Z2)
    
    direct_sol_df %>% 
      pivot_longer(c(support_site_1,support_site_2),
                   names_to = "site",values_to="proportion") %>%
      ggplot(aes(x=time,y=proportion,color=site)) + 
      geom_line(lwd=1) + ylim(c(0,1)) + theme(text = element_text(size = 18))
  })
}

ui <- fluidPage(
  headerPanel("Direct Switching"),
  sidebarLayout(
    sidebarPanel(
      h3("Initial values"),
      numericInput("X", label = "X",
                   min = 0.0, max = 1,  value = 0.7, step = 0.01, width=100),
      numericInput("Y1", label = "Y1",
                   min = 0.0, max = 1,  value = 0.15, step = 0.01, width=100),
      numericInput("Y2", label = "Y2",
                   min = 0.0, max = 1,  value = 0.15, step = 0.01, width=100),
      
      h3("Parameters"),
      numericInput("b2", label = "b2",
                   min = 0.0, max = 3,  value = 1.2, step = 0.05, width=100),
      numericInput("a", label = "a",
                   min = 0.0, max = 1,  value = 0.2, step = 0.01, width=100),
      numericInput("t_i", label = "t_i",
                   min = 0.0, max = 10,  value = 5.0, step = 0.1, width=100)
    ),
    mainPanel(
      h3("Simulation results"),
      plotOutput("direct")
    )
  )
)

shinyApp(ui = ui, server = server)
