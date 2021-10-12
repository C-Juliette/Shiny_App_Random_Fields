library(shinydashboard)

server <- function(input, output) {

  ################################### PANEL MOVING AVERAGE - Actual covariance########################
  #########

  MNormale <- reactive(matrix(rnorm((input$lengthX)**2, input$mean2, sqrt(input$variance2)), nrow = input$lengthX))
  MoyN <- reactive(moving_average(MNormale(), r = input$rayon))

  MPoisson <- reactive(matrix(rpois((input$lengthX)**2, input$lambda2), nrow = input$lengthX))
  MoyP <- reactive(moving_average(MPoisson(), r = input$rayon))
  MBernoulli <- reactive(matrix(rbinom((input$lengthX)**2, 1, input$p2), nrow = input$lengthX))
  MoyB <- reactive(moving_average(MBernoulli(), r = input$rayon))
  MStudent <- reactive(matrix(rt((input$lengthX)**2, input$k2), nrow = input$lengthX))
  MoyS <- reactive(moving_average(MStudent(), r = input$rayon))

  output$matrice <- renderPlot({
    if (input$distribution2 == "Normal distribution"){
      if(!input$button_MA_AC){
        paletteinf <- min(MoyN())
        palettesup <- max(MoyN())
      }
      else if(input$button_MA_AC){
        paletteinf <- min(MNormale())
        palettesup <- max(MNormale())
      }
      p1 <- plot_matrix(MNormale(), titre = "Matrice")
      p2 <- plot_matrix(MoyN(), titre = "Matrice", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MNormale())[1]), ylim = c(dim(MNormale())[2], 0))
      cowplot::plot_grid(p1,p2, ncol = 1, nrow = 2)}
    #plot_matrix(matrix(c(1,2,3,4), nrow = 2) ) }
    else if (input$distribution2 == "Poisson distribution"){
      if(!input$button_MA_AC){
        paletteinf <- min(MoyP())
        palettesup <- max(MoyP())
      }
      else if(input$button_MA_AC){
        paletteinf <- min(MPoisson())
        palettesup <- max(MPoisson())
      }
      p1 <- plot_matrix(MPoisson(), titre = "Matrice")
      p2 <- plot_matrix(MoyP(), titre = "Matrice", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MPoisson())[1]), ylim = c(dim(MPoisson())[2], 0))
      cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
    else if (input$distribution2 == "Bernoulli distribution"){
      if(!input$button_MA_AC){
        paletteinf <- min(MoyB())
        palettesup <- max(MoyB())
      }
      else if(input$button_MA_AC){
        paletteinf <- min(MBernoulli())
        palettesup <- max(MBernoulli())
      }
      p1 <- plot_matrix(MBernoulli(), titre = "Matrice")
      p2 <- plot_matrix(MoyB(), titre = "Matrice", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MBernoulli())[1]), ylim = c(dim(MBernoulli())[2], 0))
      cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
    else if (input$distribution2 == "Student distribution"){
      if(!input$button_MA_AC){
        paletteinf <- min(MoyS())
        palettesup <- max(MoyS())
      }
      else if(input$button_MA_AC){
        paletteinf <- min(MStudent())
        palettesup <- max(MStudent())
      }
      p1 <- plot_matrix(MStudent(), titre = "Matrice")
      p2 <- plot_matrix(MoyS(), titre = "Matrice", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MStudent())[1]), ylim = c(dim(MStudent())[2], 0))
      cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
  })


  output$Cov_Z <- renderPlot({
  if (input$distribution2 == "Normal distribution"){
    covZ <- plot_actual_cov(MNormale(), 0, liste(input$les_directions_a), max = input$maxcZ)
    covY <- plot_actual_cov(MNormale(), input$rayon, liste(input$les_directions_a), max = input$maxcY)
    cowplot::plot_grid(covZ, covY, ncol=2, nrow=1)
  }
 else if (input$distribution2 == "Poisson distribution"){
   covZ <- plot_actual_cov(MPoisson(), 0, liste(input$les_directions_a), max = input$maxcZ)
   covY <- plot_actual_cov(MPoisson(), input$rayon, liste(input$les_directions_a), max = input$maxcY)
   cowplot::plot_grid(covZ, covY, ncol=2, nrow=1)
 }
 else if (input$distribution2 == "Bernoulli distribution"){
   covZ <- plot_actual_cov(MBernoulli(), 0, liste(input$les_directions_a), max = input$maxcZ)
   covY <- plot_actual_cov(MBernoulli(), input$rayon, liste(input$les_directions_a), max = input$maxcY)
   cowplot::plot_grid(covZ, covY, ncol=2, nrow=1)
 }

    else if (input$distribution2 == "Student distribution"){
      covZ <- plot_actual_cov(MStudent(), 0, liste(input$les_directions_a), max = input$maxcZ)
      covY <- plot_actual_cov(MStudent(), input$rayon, liste(input$les_directions_a), max = input$maxcY)
      cowplot::plot_grid(covZ, covY, ncol=2, nrow=1)
    }

    })


  # output$Cov_Y<- renderPlot({
  #   if (input$distribution2 == "Normal distribution"){
  #   plot_actual_cov(MoyN(), as.numeric(input$les_rayons_a), liste(input$les_directions_a))
  #   }
  # })


  output$espace <- renderText(
    if (input$distribution2 == "Normal distribution"){
      {paste("<br><br><br>")}
    }

    else{paste("<br><br><br><br><br><br><br>")})








  ########################### PANEL MOVING AVERAGE - SUMMARY ##############################

  output$plot_summary <- renderPlot({
    if (input$distribution_s == "Normal distribution"){
      affichage_general(matrix(rnorm(input$lengthX_s**2, input$mean_s, sqrt(input$variance_s)), nrow = input$lengthX_s), input$r_s, mmechelle = input$button_MA_S)}
    else if (input$distribution_s == "Poisson distribution"){
      affichage_general(matrix(rpois(input$lengthX_s**2, input$lambda_s), nrow = input$lengthX_s), input$r_s, mmechelle = input$button_MA_AC)}
    else if (input$distribution_s == "Bernoulli distribution"){
      affichage_general(matrix(rbinom(input$lengthX_s**2, 1, input$p_s), nrow = input$lengthX_s), input$r_s, mmechelle = input$button_MA_AC)}
    else if (input$distribution_s == "Student distribution"){
      affichage_general(matrix(rt(input$lengthX_s**2, input$k_s), nrow = input$lengthX_s), input$r_s, mmechelle = input$button_MA_AC)}

  })





###################  PANEL EXPECTED COVARIANCE ####################
  output$covariance_e <- renderPlot({
    plot_expected_cov(sqrt(input$variance_c), charactervector_as_vector(input$rayons_c), liste(input$directions_c))
  })



  # if (input$distribution2 == "Normal distribution"){
  # output$Cov_Z <- renderPlot({
  #   plot_actual_cov(MNormale, as.numeric(input$rayons_c), liste(input$directions_c))
  # })
  #
  # output$CovY<- renderPlot({
  #   plot_actual_cov(MNormale, as.numeric(input$rayons_c), liste(input$directions_c))
  # })
  #
  # }




  # output$paramPoisson <- renderText({"Expected value : lambda \n Variance : lambda"})
  # output$paramStudent <- renderText({"Expected value : \n
  #                                   undefined if k <= 1 \n
  #                                   0 if k > 1 \n
  #                                   Variance :\n
  #                                   undefined if k <= 1 \n
  #                                   +infinity if 1 < k <= 2 \n
  #                                   k/(k-2) if k > 2"})


  ################ PANEL RANDOMLY GENERATED FIELDS (distributions et matrices affichées) ##############
  output$plot1_densite <- renderPlot({

    if (input$distribution == "Normal distribution"){
      #x = seq(input$mean - 4*input$variance, input$mean + 4*input$variance, 0.01)
      x = seq(input$xminmax[1] , input$xminmax[2], length = 100)
      d1= c(dnorm(x, input$mean, sqrt(input$variance)))
      df = data.frame(x, d1)
      p <- ggplot(df, aes(x)) +
        geom_area(aes(y = d1), color = "orange",lwd = 0.9, fill = "orange", alpha=0.2) +
        ylab("Normal theoritical distribution")
      p
    }
    else if (input$distribution == "Poisson distribution"){
      #x = seq(input$mean - 4*input$variance, input$mean + 4*input$variance, 0.01)
      x = seq(input$xminmaxP[1] , input$xminmaxP[2])
      d1= c(dpois(x, input$lambda))
      df = data.frame(x, d1)
      p <- ggplot(df, aes(x)) +
        geom_point(aes(y = d1), color = "magenta3",lwd = 0.9) +
        geom_segment(data = df, aes(x = x, y = d1, xend = x, yend = 0), color = "magenta3") +
        ylab("Poisson theoritical distribution")

      p
    }

    else if (input$distribution == "Bernoulli distribution"){
      #x = seq(input$mean - 4*input$variance, input$mean + 4*input$variance, 0.01)
      x = seq(input$xminmaxB[1] , input$xminmaxB[2])
      d1= c(dbinom(x, 1, input$p))
      df = data.frame(x, d1)
      p <- ggplot(df, aes(x)) +
        geom_point(aes(y = d1), color = "green",lwd = 0.9) +
        geom_segment(data = df, aes(x = x, y = d1, xend = x, yend = 0), color = "green") +
        ylab("Bernoulli theoritical distribution")
      p
    }
    else if (input$distribution == "Student distribution"){
      x = seq(input$xminmaxS[1] , input$xminmaxS[2], length = 100)
      d1= c(dt(x, input$k))
      df = data.frame(x, d1)
      p <- ggplot(df, aes(x)) +
        geom_area(aes(y = d1), color = "red",lwd = 0.9, fill = "red", alpha=0.2) +
        ylab("Student theoritical distribution")
      p
    }
  }
  )

  output$plot2_matrice <- renderPlot({
    n = 20
    if (input$distribution == "Normal distribution"){
      plot_matrix( matrix(rnorm(n*n, input$mean, sqrt(input$variance)), nrow = n), titre = "Matrice")}
    else if (input$distribution == "Poisson distribution"){
      plot_matrix( matrix(rpois(n*n, input$lambda), nrow = n), titre = "Matrice")}
    else if (input$distribution == "Bernoulli distribution"){
      plot_matrix( matrix(rbinom(n*n, 1, input$p), nrow = n), titre = "Matrice")}
    else if (input$distribution == "Student distribution"){
      plot_matrix(matrix(rt(n*n, input$k), nrow = n), titre = "Matrice")}
  })


  output$param <- renderText({paste("<b>Quick reminder : </b><br><br>
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
  ")})



  ###############################################################

  output$presentation <- renderText(
    paste("This app shows random generated fieds as matrix. It also shows the distributions and their parametres.<br><br>
          What is more, you can use a moving average function and vizualise the new field generated (this is then a structured field !)<br>
          The moving average is calculated on a squared moving window.
          <br><br>
          In the app, the parameter called <i>radius of the window</i> gives the number of points under, above, at the right and at the left of the central point of the window. For
          example, r = 10 means that the window is 21x21.<br>
          The actual correlation (the empirical one) and the expected correlation (the theoritical one) are showned in the two last panels.<br><br>
          Hope you will enjoy !
          "))




}
