library(shiny)
library(shinydashboard)
#outputOptions(output, "paramBernoulli", suspendWhenHidden = FALSE)


ui <- dashboardPage(

  dashboardHeader(title = "Geostatistic App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bonjour hi !", tabName = "Summary")

    )
  ),
  dashboardBody(
    tabItems(

      # Onglet Bonjour hi !
      tabItem(tabName = "Summary",
              h2("Summary"),
              htmlOutput("paramBernoulli"),
              fluidRow(
                box(conditionalPanel(
                  condition = TRUE,
                  #textOutput("paramBernoulli")
                  sliderInput("dzdz", "dzdz", 2, 3, 4)
                )
      ))
      )
      )
  )
)

server <- function(input, output) {

  output$paramBernoulli <- renderText({
    paste("<b>Quick reminder : </b><br><br>
          <b>Bernoulli distribution : </b><br>
                                    <i>Expected value</i> : p <br>
                                    <i>Variance</i> : p(1-p) <br>
                                    <b>Poisson distribution : </b><br>
                                    <i>Expected value</i> : &lambda; <br>
                                    <i>Variance</i> : &lambda; <br>
                                    <b>Student distribution : </b><br>
                                    <i>Expected value</i> : <br>
                                     undefined <i>if k &le; 1</i> , <br>
                                     0 <i>if k &gt; 1 </i><br>
                                     <i>Variance</i> :<br>
                                     undefined <i>if k &le; 1, </i><br>
                                     +&infin; <i>if 1 &lt; k &le; 2, </i><br>
                                     <sup>k</sup>&frasl;<sub>k-2</sub> <i>if k &gt; 2</i>
  ")
})
}

shinyApp(ui, server)
