library(shinydashboard)
library(ggplot2)
library(randomfields)
source('Randomly_generated_fields.R')


ui <- dashboardPage(
  dashboardHeader(title = "Random Fields"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bonjour hi !", tabName = "Summary"),
      menuItem("Randomly generated fields", tabName = "Couple_distribution_matrice"),
      menuItem("Moving average - Summary ", tabName = "plot_summary"),
      menuItem("Moving average - Actual covariance", tabName = "MA_AC"),
      menuItem("Expected covariance", tabName = "Expected_covariance")

    )
  ),
  dashboardBody(
    tabItems(

      # Onglet Bonjour hi !
      tabItem(tabName = "Summary",
              h2("Welcome !"),
              htmlOutput("presentation")

      ),

      # Onglet Distributions
      tabItem(tabName = "plot_summary",
              fluidRow(

                box(
                  title = "Distribution",
                  selectInput_distributions(inputId = "distribution_s"),

                  #CONDITIONAL PANELS
                  conditionalPanel_normal_distribution(
                    condition = "input.distribution_s == 'Normal distribution'",
                    meanId = "mean_s", varianceId = "variance_s", setxId = ""
                  ),
                  conditionalPanel_poisson_distribution(
                    condition = "input.distribution_s == 'Poisson distribution'",
                    lambdaId = "lambda_s", setxId = ""
                  ),
                  conditionalPanel_bernoulli_distribution(
                    condition = "input.distribution_s == 'Bernoulli distribution'",
                    pId = "p_s", setxId = ""
                  ),
                  conditionalPanel_student_distribution(
                    condition = "input.distribution_s == 'Student distribution'",
                    kId = "k_s", setxId = ""
                  ),

                  numericInput("lengthX_s",
                               label = "Choose the length of X and Y axis",
                               value = 10),

                  # numericInput("lengthY_s",
                  #              label = "Choose the length of Y axis",
                  #              min = 1, max = 200, value = 10),
                  numericInput("r_s",
                               label = "Choose the radius",
                               min = 1, max = 30, value = 1
                  ),
                  checkboxInput("button_MA_S", "Common color scale"),

                  width = 4

                ),

                box(
                  h3("Summary"),
                  plotOutput("plot_summary"),
                  width = 8
                )


              )
      ),

      # Onglet randomly generated fields
      tabItem(tabName = "Couple_distribution_matrice",
              fluidRow(

                box(
                  h3("Theoritical density and a realization on a matrix"),
                  plotOutput("plot1_densite", height = 250),
                  plotOutput("plot2_matrice", height = 250)
                ),

                box(
                  title = "Distribution",
                  selectInput_distributions(inputId = "distribution"),

                  #CONDITIONAL PANELS
                  conditionalPanel_normal_distribution(
                    condition = "input.distribution == 'Normal distribution'",
                    meanId = "mean", varianceId = "variance", setxId = "xminmax"),
                  conditionalPanel_poisson_distribution(
                    condition = "input.distribution == 'Poisson distribution'",
                    lambdaId = "lambda", setxId = "xminmaxP"
                  ),
                  conditionalPanel_bernoulli_distribution(
                    condition = "input.distribution == 'Bernoulli distribution'",
                    pId = "p", setxId = "xminmaxB"
                  ),
                  conditionalPanel_student_distribution(
                    condition = "input.distribution == 'Student distribution'",
                    kId = "k", setxId = "xminmaxS"
                  ),
                  htmlOutput("param")

                )
              )


              #)
      ),

      # Onglet Moving average actual corelation
      tabItem(tabName = "MA_AC",
              fluidRow(

                box(h3("Moving average on a matrix \n \n"), plotOutput("matrice", height = 500)), #plotOutput("mean_of_matrix", height = 250)),

                box(
                  title = "Distribution ",
                  selectInput("distribution2",
                              label = "Choose a random distribution",
                              choices = distribution_choices,
                              selected = "Normal distribution"),

                  #CONDITIONAL PANELS
                  conditionalPanel_normal_distribution(
                    condition = "input.distribution2 == 'Normal distribution'",
                    meanId = "mean2", varianceId = "variance2", setxId = ""),
                  conditionalPanel_poisson_distribution(
                    condition = "input.distribution2 == 'Poisson distribution'",
                    lambdaId = "lambda2", setxId = ""
                  ),
                  conditionalPanel_bernoulli_distribution(
                    condition = "input.distribution2 == 'Bernoulli distribution'",
                    pId = "p2", setxId = ""
                  ),
                  conditionalPanel_student_distribution(
                    condition = "input.distribution2 == 'Student distribution'",
                    kId = "k2", setxId = ""
                  ),

                  ###################################################################

                  numericInput("lengthX",
                               label = "Choose the length of X and Y axis",
                               value = 10),


                  sliderInput("rayon",
                              label = "Choose the radius r of the window used for the moving average:",
                              min = 0, max = 100, value = 1),
                  checkboxInput("button_MA_AC", "Common color scale"),
                  htmlOutput("espace")



                ),

                # box(
                #   checkboxGroupInput(inputId = "les_directions_a",
                #                      label = "Choose the direction",
                #                      choices = c("(0, 1)", "(1, 1)", "(1, 2)", "(1, 3)", "(1, 4)", "(2, 3)", "(3, 4)"),
                #                      selected = "(0, 1)"),
                #   width = 3
                # ),

                box(checkboxGroupInput(inputId = "les_directions_a",
                                       label = "Choose the direction",
                                       choices = c("(0, 1)", "(1, 1)", "(1, 2)", "(1, 3)", "(1, 4)", "(2, 3)", "(3, 4)"),
                                       selected = "(0, 1)"),
                    numericInput("maxcZ", "Choose maximum value of x axis for Z grid", value = 10, min = 1, step = 0.5),
                    numericInput("maxcY", "Choose maximum value of x axis for Y grid", value = 10, min = 1, step = 0.5),
                    width = 3),
                   box( plotOutput("Cov_Z"),
                    plotOutput("Cov_Y"),
                    width = 9)
              )

      ),
      # Third tab content
      tabItem(tabName = "Expected_covariance",
              fluidRow(
                box(
                  numericInput("variance_c", "Choose the variance of the initial grid Z", value = 1, min = 1, max = 20, step = 0.5),
                  # checkboxGroupInput(inputId = "rayons_c",
                  #                    label = "Choose the radius",
                  #                    choices = c("0", "1", "2", "3", "4", "5", "6", "7"," 8"),
                  #                    selected = "1"),
                  textInput("rayons_c", "Choose the radius (several are possible)", value = "1"),
                  checkboxGroupInput(inputId = "directions_c",
                                     label = "Choose the direction",
                                     choices = c("(0, 1)", "(1, 1)", "(1, 2)", "(1, 3)", "(1, 4)", "(2, 3)", "(3, 4)"),
                                     selected = "(0, 1)"),



                  width = 3
                ),
                box(h2("Expected covariance of Y grid"), plotOutput("covariance_e"),
                    width = 8, height = 8)

              )

      )

    )
  )
)


