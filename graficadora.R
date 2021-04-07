ui<-fluidPage(
    titlePanel("Graph a probability distribution :)", windowTitle = "Nice"),
    fluidRow(
      column(2,
        selectInput("dist","Choose the probability distribution: ", choices =
                  list(Continuas = list("Normal", "Beta","Chi-cuadrado"),
                       Discretas = list("Binomial", "Poisson")))
      ),
      column(2,
             conditionalPanel(
               condition = "input.dist == 'Normal'",
               numericInput("media","Mean: ", value = 0),
               numericInput("desv", "Standard deviation: ", value = 1,min=0)
             ),
             conditionalPanel(
               condition = "input.dist == 'Chi-cuadrado'",
               numericInput("df", "Degrees of freedom: ", value = 1, min = 1, step = 1)
             ),
             conditionalPanel(
               condition = "input.dist == 'Beta'",
               numericInput("alfa","Alpha: ", value = 1,min=1),
               numericInput("beta", "Beta: ", value = 1,min=1)
             ),
             conditionalPanel(
               condition = "input.dist == 'Poisson'",
               numericInput("lambda","Lambda: ", value = 1),
             ),
             conditionalPanel(
               condition = "input.dist == 'Binomial'",
               numericInput("n","n: ", value = 10,step=1),
               numericInput("p", "p: ", value = 0.5)
             ),
      ),
      column(2,
             checkboxInput(inputId="otradist",label="I wanna graph two distributions", value = F)),
      column(2,
             conditionalPanel(
               condition = "input.otradist == true",
               selectInput("dist2","Choose the distribution: ", choices =
                           list("None :)", Continuas = list("Normal", "Beta", "Chi-cuadrado"),
                                Discretas = list("Binomial", "Poisson")))
             )
      
       ),
      column(2,
              conditionalPanel(
                condition = "input.dist2 == 'Normal' && input.otradist == true",
                numericInput("media2","Media: ", value = 0),
                numericInput("desv2", "Desviación típica: ", value = 1)
              ),
              conditionalPanel(
                condition = "input.dist2 == 'Beta' && input.otradist == true",
                numericInput("alfa2","Alfa: ", value = 1),
                numericInput("beta2", "Beta: ", value = 1)
              ),
              conditionalPanel(
                condition = "input.dist2 == 'Poisson' && input.otradist == true",
                numericInput("lambda2","Lambda: ", value = 1),
              ),
              conditionalPanel(
                condition = "input.dist2 == 'Chi-cuadrado' && input.otradist == true",
                numericInput("df2", "Grados de libertad: ", value = 1, min = 1, step = 1)
              ),
              conditionalPanel(
                condition = "input.dist2 == 'Binomial' && input.otradist == true",
                numericInput("n2","n: ", value = 10),
                numericInput("p2", "p: ", value = 0.5)
              ),
       )
             
    ),
    fluidRow(
      column(6,
             plotOutput("plot")
      ),
      
      column(6,
             conditionalPanel(
               condition = "input.otradist== true && dist2!=='Ninguna :)'",
               plotOutput("plot2")
             )
      )
    )
)
server<-function(input, output, session){
    dist<-reactive(input$dist)
    media<-reactive(input$media)
    sd<-reactive(input$desv)
    lambda<-reactive(input$lambda)
    n<-reactive(input$n)
    p<-reactive(input$p)
    alfa<-reactive(input$alfa)
    beta<-reactive(input$beta)
    df<-reactive(input$df)
    output$plot<-renderPlot({
      
      inf<-switch(dist(),"Normal"=media()-3*sd(),"Beta"=0,"Poisson"=0,"Binomial"=0,"Chi-cuadrado"=0)
      sup<-switch(dist(),"Normal"=media()+3*sd(),"Beta"=1,"Poisson"=2*lambda(),"Binomial"=n(),"Chi-cuadrado"=df()*2)
      puntos<-switch(dist(),"Normal"=seq(from=inf,to=sup,length.out=1000),"Beta"=seq(from=inf,to=sup,length.out=1000),
                     "Poisson"=inf:sup,"Binomial"=inf:sup, "Chi-cuadrado"=seq(from=inf,to=sup,length.out = 1000))
      Density<-switch(dist(),"Normal"=dnorm(puntos,media(),sd()),
                      "Beta"=dbeta(puntos,alfa(),beta()),
                      "Poisson"=dpois(puntos,lambda()),
                      "Binomial"=dbinom(puntos,n(),p()),
                      "Chi-cuadrado"=dchisq(puntos,df()))
      media<-switch(dist(),"Normal"=media(),"Beta"=alfa()/(alfa()+beta()),"Poisson"=lambda(),"Binomial" = n()*p(),"Chi-cuadrado" = df2())
      plot(puntos,Density,type="l", col = "cyan3",lwd=2.5, xlab="Values",main=dist())
      abline(v = media, col="cyan2",lwd = 2, lty = 2)})
    dist2<-reactive(input$dist2)
    media2<-reactive(input$media2)
    sd2<-reactive(input$desv2)
    lambda2<-reactive(input$lambda2)
    n2<-reactive(input$n2)
    p2<-reactive(input$p2)
    alfa2<-reactive(input$alfa2)
    beta2<-reactive(input$beta2)
    df2<-reactive(input$df2)
    output$plot2<-renderPlot({
      
      inf2<-switch(dist2(),"Normal"=media2()-3*sd2(),"Beta"=0,"Poisson"=0,"Binomial"=0,"Chi-cuadrado"=0)
      sup2<-switch(dist2(),"Normal"=media2()+3*sd2(),"Beta"=1,"Poisson"=2*lambda2(),"Binomial"=n2(),"Chi-cuadrado"=df2()*2)
      puntos2<-switch(dist2(),"Normal"=seq(from=inf2,to=sup2,length.out=1000),"Beta"=seq(from=inf2,to=sup2,length.out=1000),
                     "Poisson"=inf2:sup2,"Binomial"=inf2:sup2, "Chi-cuadrado"=seq(from=inf2,to=sup2,length.out = 1000))
      Density2<-switch(dist2(),"Normal"=dnorm(puntos2,media2(),sd2()),
                       "Beta"=dbeta(puntos2,alfa2(),beta2()),
                       "Poisson"=dpois(puntos2,lambda2()),
                       "Binomial"=dbinom(puntos2,n2(),p2()),
                       "Chi-cuadrado"=dchisq(puntos2,df2()))
      media2<-switch(dist2(),"Normal"=media2(),"Beta"=alfa2()/(alfa2()+beta2()),"Poisson"=lambda2(),"Binomial" = n2()*p2(),"Chi-cuadrado" = df2())
      plot(puntos2,Density2,type="l", col = "indianred2",lwd=2.5, xlab="Values",main=dist2())
      abline(v = media2, col="mediumaquamarine",lwd = 2, lty = 2)})
      
}

shinyApp(ui, server)
