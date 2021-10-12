library(shinydashboard)
library(patchwork)
library(plotly)
server <- function(input, output) {

  ################################### PANEL EMPIRICAL COVARIANCE########################
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
      p1 <- plot_matrix(MNormale(), titre = "Z grid")
      p2 <- plot_matrix(MoyN(), titre = "Y grid", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MNormale())[1]), ylim = c(dim(MNormale())[2], 0))
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
      p1 <- plot_matrix(MPoisson(), titre = "Z grid")
      p2 <- plot_matrix(MoyP(), titre = "Y grid", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MPoisson())[1]), ylim = c(dim(MPoisson())[2], 0))
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
      p1 <- plot_matrix(MBernoulli(), titre = "Z grid")
      p2 <- plot_matrix(MoyB(), titre = "Y grid", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MBernoulli())[1]), ylim = c(dim(MBernoulli())[2], 0))
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
      p1 <- plot_matrix(MStudent(), titre = "Z grid")
      p2 <- plot_matrix(MoyS(), titre = "Y grid", r = input$rayon, paletteinf = paletteinf, palettesup = palettesup) + coord_cartesian(xlim = c(0, dim(MStudent())[1]), ylim = c(dim(MStudent())[2], 0))
      cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
  })


  output$Cov_Z <- renderPlot({
  if (input$distribution2 == "Normal distribution"){
    covZ <- plot_actual_cov(MNormale(), 0, liste(input$les_directions_a), max = input$maxcZ, relier = FALSE, ylabs = "Empirical covariance - Z grid")
    covY <- plot_actual_cov(MNormale(), input$rayon, liste(input$les_directions_a), max = input$maxcY, ylabs = "Empirical covariance - Y grid")
    covZ / covY
  }
 else if (input$distribution2 == "Poisson distribution"){
   covZ <- plot_actual_cov(MPoisson(), 0, liste(input$les_directions_a), max = input$maxcZ, relier = FALSE, ylabs = "Empirical covariance - Z grid")
   covY <- plot_actual_cov(MPoisson(), input$rayon, liste(input$les_directions_a), max = input$maxcY, ylabs = "Empirical covariance - Y grid")
   covZ / covY
 }
 else if (input$distribution2 == "Bernoulli distribution"){
   covZ <- plot_actual_cov(MBernoulli(), 0, liste(input$les_directions_a), max = input$maxcZ, relier = FALSE, ylabs = "Empirical covariance - Z grid")
   covY <- plot_actual_cov(MBernoulli(), input$rayon, liste(input$les_directions_a), max = input$maxcY, ylabs = "Empirical covariance - Y grid")
   covZ / covY
 }

    else if (input$distribution2 == "Student distribution"){
      covZ <- plot_actual_cov(MStudent(), 0, liste(input$les_directions_a), max = input$maxcZ, relier = FALSE, ylabs = "Empirical covariance - Z grid")
      covY <- plot_actual_cov(MStudent(), input$rayon, liste(input$les_directions_a), max = input$maxcY, ylabs = "Empirical covariance - Y grid")
      covZ / covY
    }

    })


  # output$Cov_Y<- renderPlot({
  #   if (input$distribution2 == "Normal distribution"){
  #   plot_actual_cov(MoyN(), as.numeric(input$les_rayons_a), liste(input$les_directions_a))
  #   }
  # })


  #output$espace <- renderText(
  #  if (input$distribution2 == "Normal distribution"){
  #    {paste("<br><br><br><br>")}
  #  }
#
  #  else{paste("<br><br><br><br><br><br><br><br>")})




  ########################### PANEL MOVING AVERAGE - SUMMARY ##############################

  Znormale_s <- reactive(matrix(rnorm(input$lengthX_s**2, input$mean_s, sqrt(input$variance_s)), nrow = input$lengthX_s))
  r <- reactive(input$r_s)
  Ynormale_s <- reactive(moving_average(Znormale_s() ,r()))

  Zpoisson_s <- reactive(matrix(rpois(input$lengthX_s**2, input$lambda_s), nrow = input$lengthX_s))
  Ypoisson_s <- reactive(moving_average(Zpoisson_s() ,r()))

  Zber_s <- reactive(matrix(rbinom(input$lengthX_s**2, 1, input$p_s), nrow = input$lengthX_s))
  Yber_s <- reactive(moving_average(Zber_s() ,r()))

  Zst_s <- reactive(matrix(rt(input$lengthX_s**2, input$k_s), nrow = input$lengthX_s))
  Yst_s <- reactive(moving_average(Zst_s() ,r()))

  output$plot_summary <- renderPlot({
    if (input$distribution_s == "Normal distribution"){
     affichagegeneral2(Znormale_s(), Ynormale_s(), r(), samescale = input$button_MA_S)
    }
    else if (input$distribution_s == "Poisson distribution"){
      affichagegeneral2(Zpoisson_s(), Ypoisson_s(), r(), samescale = input$button_MA_S)
    }
    else if (input$distribution_s == "Bernoulli distribution"){
      affichagegeneral2(Zber_s(), Yber_s(), r(), samescale = input$button_MA_S)
    }
    else if (input$distribution_s == "Student distribution"){
      affichagegeneral2(Zst_s(), Yst_s(), r(), samescale = input$button_MA_S)
    }

  })





###################  PANEL EXPECTED COVARIANCE ####################
  output$covariance_e <- renderPlotly({
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
      plot_matrix( matrix(rnorm(n*n, input$mean, sqrt(input$variance)), nrow = n), titre = "Z grid")}
    else if (input$distribution == "Poisson distribution"){
      plot_matrix( matrix(rpois(n*n, input$lambda), nrow = n), titre = "Z grid")}
    else if (input$distribution == "Bernoulli distribution"){
      plot_matrix( matrix(rbinom(n*n, 1, input$p), nrow = n), titre = "Z grid")}
    else if (input$distribution == "Student distribution"){
      plot_matrix(matrix(rt(n*n, input$k), nrow = n), titre = "Z grid")}
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



  ####################### Présentation ########################################

  output$presentation <- renderText(
    paste("This app shows random fields, generated from statistical distributions. Users can choose the statistical distribution and its parameters. On a grid, a pixel represents a realization of one of the variables. <br><br>
          The app uses a moving average function on the random fields and enables the visualization of the new fields generated (they are then structured fields !)<br>
          The moving average is calculated with a squared moving window. There is a border truncating effect.
          <br><br>
          In the app, the <i>radius of the moving average's window</i>, called <i>r </i>, gives the number of points below, above, on the right and on the left of the window's central point. For
          example, <i>r = 10</i> means that the moving average's window is <i>21x21</i>.<br>
          The empirical covariance and the theoritical covariance according to the distance between variables are shown in the two last tabs.
          <br><br>
          I hope you will enjoy to use this application !
          <br><br><br>
          <i>An example :<br>
          Z grid : 10x10 grid generated from a normal distribution<br>
          r = 1<br>
          Window of the moving average : n = (2r + 1)<SUP>2</SUP> = 3x3 points<br>
          Y grid : 8x8 grid<br>
          </i>
          ")
    )

  output$illustration <- renderImage({
    return(list(src = "illustration3.png",contentType = "image/png", alt = "illustration"))
  }, deleteFile = FALSE)



  output$presentation2 <- renderText(
    paste("<i>
          Author of the app : Juliette Chiapello,<br>
          during an intership supervised by Aymeric Stamm.<br>
          (Laboratoire de Mathématiques Jean Leray, Nantes)<br>
          The app is the continuation of a project made with Céline Herbreteau and Léonie Veuille and supervised by Nicolas Bez.<br>
          (M1 Ingénierie Statistique, Université de Nantes)<br>
          2021<br><br>
          juliette.chiapello@etu.univ-nantes.fr
          </i>")
  )

  ########### Randomly generated field ###################

  output$randomlygeneratedfields <- renderText(
    paste("In this tab, you can choose a statistical distribution and its parameters.
            The first plot shows the theoritical density graph and the second plot shows a grid filled
            with values generated from this distribution. The empirical variance is displayed above the graph.
")
  )

  ########### Moving average summary ###################



  output$movingaveragesummary <- renderText(
    paste("In this tab, you can choose a statistical distribution and its parameters again.<br>
            The first plot shows a grid filled with values generated from this distribution. <br>
            The second plot shows a grid generated from the first one, after the application of a moving average function.
            You can choose <i>r</i>, the radius of the moving average's window. You can also choose whether grids' color scales are the same or not.<br>
            The third plot shows the repartition of values with histograms. <br>
            The fourth plot shows the repartition of values with boxplots.

")
  )

  output$movingaveragesummary1 <- renderText(
    paste("Precision : the theoritical variance <i>&sigma;<SUB>Y</SUB><SUP>2</SUP></i>
            of Y grid and the theoritical variance <i>&sigma;<SUB>Z</SUB><SUP>2</SUP></i> of Z grid are
            linked by the following formula :
")
  )

  output$movingaveragesummary2 <- renderUI({
    withMathJax(helpText('\\begin{gather*}\\sigma_Y^2 = \\frac{\\sigma_Z^2}{n}\\end{gather*}'))
  })

  output$movingaveragesummary3 <- renderText(
    paste("where <i>n</i> is the number of points in the window of the moving average. <br>
            Ex : <i>r = 1</i> implies <i>n = (2r + 1)<SUP>2</SUP> = 3*3 = 9</i>.
")
  )

  ########### Moving average empirical covariance ###################

  output$actualcovariance01 <- renderText(
    paste("This tab focuses on the visualization of spatial structures on the Y grid. The first plot shows a Z grid.
          The second plot shows the corresponding Y grid (after the moving average process on Z grid).
          One can see that typical dimensions of structures on the Y grid are linked to <i>r</i> (the radius of the moving window).
          The empirical covariance between the Y grid variables is shown below.

")
  )

  output$actualcovariance02 <- renderText(
    paste("The first graph below shows the empirical covariance
  graph for the Z grid above (<i>r = 0</i>) and the second graph below
  shows the empirical covariance graph for the Y grid above
  (you can choose the radius <i>r</i> of the moving average with the panel above).

")
  )

    output$actualcovariance1 <- renderText(
    paste("<b>Calculation of the empirical covariance.</b><br>
          To calculate the empirical covariance according to the distance between variables, the concerned grid is duplicated.
          One can imagine two superimposed identical grids : a front grid and a back grid. The front grid is then shifted by a given distance.
          Thus, all the couples of superimposed back/front points can be used in the following formula, which gives the empirical covariance :

")
  )

    output$actualcovariance2 <- renderUI({
      withMathJax(helpText('\\begin{gather*}\\boxed{Cov(distance)\\; = \\;\\overline{Y_{front}Y_{back}} - \\overline{Y_{front}} \\; \\overline{Y_{back}}}\\end{gather*}'))
    })

    output$actualcovariance3 <- renderText(
      paste("This calculation is made for all possible distances. <br><br>

            The calculation is less and less precise as the distance between variables increases :
            variables used to do the calculation are variables in the common area between front and back grids. This area is smaller
            and smaller as the shift (the distance between variables) increases. That explains the increasing
            statistical fluctuations on the second graph as the distance increases.

")
    )




 ########## Expected covariance #########


    output$presentationcovariance01 <- renderText(
      paste("The theoritical covariance formula between two variables Y<SUB>ij</SUB> and Y<SUB>i'j'</SUB> of a Y grid is the following :

")
    )

    output$presentationcovariance02 <- renderUI({
      withMathJax(helpText('\\begin{gather*}\\boxed{Cov(Y_{ij}, Y_{i\'j\'})\\; = \\;\\frac{\\sigma_Z^2}{n}\\; \\frac{n_{commonpoints}}{n}\\; =\\;  \\sigma_Y^2 \\; \\frac{n_{commonpoints}}{n}}\\end{gather*}'))
    })

    output$presentationcovariance03 <- renderText(
      paste("where <i>&sigma;<SUB>Z</SUB></i>  is the standard error of Z grid,<br>
          <i>&sigma;<SUB>Y</SUB></i>  the standard error of Y grid,<br>
          <i>n</i> the number of points in the moving average window : <i>n = (2r + 1)<SUP>2</SUP></i>,<br>
          and <i>n<SUB>commonpoints</SUB></i> the number of common points between the two windows that generated the variables <i>Y<SUB>ij</SUB></i> and <i>Y<SUB>i'j'</SUB></i><br>
          (<i>i</i> and <i>j</i> represent the variables position on the grid).

")
    )

    output$presentationcovariance <- renderText(
      paste("<b>Some examples</b><br>
         <b>Example 1 :</b>  <br>
         Let's take : <i>&sigma;<SUB>Z</SUB><SUP>2</SUP></i> = 1 and r = 1 in (0,1) direction.<br>
         That implies : <i>n</i> = 3*3 = 9 and <i>&sigma;<SUB>Y</SUB></i> = 1/9 = 0.111<br>
         <i>If the distance d = 0, cov = 0.111 * 9/9 = <b>0.111</b> (9 common points between windows)<br>
         If d = 1, cov = 0.111 * 6/9 = <b>0.074</b> (6 common points between windows)<br>
         If d = 2, cov = 0.111 * 3/9 = <b>0.037</b> (3 common points between windows)<br>
         If d = 3 or more, cov = 0.111 * 0/9 = <b>0</b> (0 common points between windows)</i><br>
         <br>

         <b>Example 2 :</b>  <br>
         Let's take : <i>&sigma;<SUB>Z</SUB><SUP>2</SUP></i> = 1 and r = 1 in (1,1) direction.<br>
         That implies : <i>n</i> = 3*3 = 9 and <i>&sigma;<SUB>Y</SUB><SUP>2</SUP></i> = 1/9 = 0.111<br>
         <i>If d = 0, cov = 0.111 * 9/9 = <b>0.111</b> (9 common points between windows)<br>
         If d = &radic;2, cov = 0.111 * 4/9 = <b>0.049</b> (4 common points between windows)<br>
         If d = 2&radic;2, cov = 0.111 * 1/9 = <b>0.012</b> (1 common points between windows)<br>
         If d = 3&radic;2 or more, cov = 0.111 * 0/9 = <b>0</b> (0 common points between windows)</i><br>
         <br>

         <b>Example 3 :</b>  <br>
         Let's take : <i>&sigma;<SUB>Z</SUB><SUP>2</SUP></i> = 16 and r = 2 in (0,1) direction.<br>
         That implies : <i>n</i> = 5*5 = 25 and <i>&sigma;<SUB>Y</SUB><SUP>2</SUP></i> = 16/25 = 0.64<br>
         <i>If d = 0, cov = 0.64 * 25/25 = <b>0.64</b> (25 common points between windows)<br>
         If d = 1, cov = 0.64 * 20/25 = <b>0.512</b> (20 common points between windows)<br>
         If d = 2, cov = 0.64 * 15/25 = <b>0.384</b> (15 common points between windows)<br>
         If d = 3, cov = 0.64 * 10/25 = <b>0.256</b> (10 common points between windows)<br>
         If d = 4, cov = 0.64 * 5/25 = <b>0.128</b> (5 common points between windows)<br>
         If d = 5 or more, cov = 0.64 * 0/25 = <b>0</b> (0 common points between windows)</i><br>
         <br>

         <br><br>
         <b>Some theoritical precisions</b><br>
         The theorical covariance between two pixels separated by a given distance can be calculated if the following hypotheses are proved :
         <br><br>
         <i>Required hypotheses</i> <br>
         - <b>First order stationarity :</b> stationarity of the expected value.
         That's to say, all <i>Y<SUB>ij</SUB></i> variables of Y grid (where <i>i</i> and <i>j</i> represent the variables position)
         have the same expected value. The word 'stationarity' refers to a spatial stationarity.")
    )

    output$presentationcovariance2 <- renderUI({
      withMathJax(helpText('\\begin{gather*}\\mathbb{E}(Y_{ij}) = m \\quad \\forall (i, j)\\end{gather*}'))
    })

    output$presentationcovariance3 <- renderText(
      paste("where <i>m</i> is a constant (it is actually the expected value of the distribution used for Z grid).
        <br><br>
         - <b>Second  order stationarity :</b> the covariance between two variables only depends on the distance between them (and that also implies the stationarity of variance).
        <br><br>
        <i> If  : <br>
        - Z grids are randomly generated according to a distribution (it implies that Z<SUB>ij</SUB> variables are independent),<br>
        - the expected value and the standard deviation of this distribution are defined, <br>
        the moving average process on Z grids leads to Y grids validating the two hypotheses.
        <br><br>
")
    )


    affichagegeneral2 <- function(Z, Y, r, samescale){
      paletteinf <- ""
      palettesup <- ""
      paletteinfY <- ""
      palettesupY <- ""
      #affichage_general(matrix(rnorm(input$lengthX_s**2, input$mean_s, sqrt(input$variance_s)), nrow = input$lengthX_s), input$r_s, mmechelle = input$button_MA_S)}
      p1 <- plot_matrix(Z, titre = "Z grid", paletteinf = paletteinf, palettesup = palettesup) + ggplot2::coord_cartesian(xlim = c(0, dim(Z)[1]), ylim = c(0, dim(Z)[2]))
      if(samescale){
        paletteinfY <- min(Z)
        palettesupY <- max(Z)}
      p2 <- plot_matrix(Y,  r, titre = "Y grid", paletteinf = paletteinfY, palettesup = palettesupY)+ ggplot2::coord_cartesian(xlim = c(0, dim(Z)[1]), ylim = c(0, dim(Z)[2]))
      p3 <- les_histogrammes(Z, Y, titre = "Histograms")
      p4 <- les_boxplots(Z, Y)

      #title <- cowplot::ggdraw() + cowplot::draw_label(titre, fontface='bold')

      p <- cowplot::plot_grid(p1,p2,p3,p4, ncol=2, nrow=2) +
        scale_fill_viridis_c(option = "B", direction = -1)
      p
    }


}



