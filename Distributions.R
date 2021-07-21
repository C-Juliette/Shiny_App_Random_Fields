###### Variables


distribution_choices <- c("Normal distribution",
                          "Poisson distribution",
                          "Bernoulli distribution",
                          "Student distribution")

checkboxGroupInput_distributions <- function(inputId){
return(checkboxGroupInput(inputId = inputId,
                   label = "Choose your distribution(s)",
                   choices = distribution_choices,
                   selected = "Normal distribution"))
}


plot_densite <- function(xmin, xmax, ymin, ymax, mean, sd, lambda, p, k, Normal = TRUE, Poisson = TRUE, Bernoulli = TRUE, Student = TRUE){
  x <- seq(xmin, xmax, 0.01)
  n <- dnorm(x = x, mean = mean, sd = sd)
  p <- dpois(x = abs(x), lambda = lambda)
  b <- dbinom(x = x, size = 1, prob = 0.5)
  s <- dt(x = x, df = k)
  df <- data.frame(x, n, p, b, s)
  distribution_plot <- ggplot(df)+
    xlim(xmin, xmax)+
    ylim(ymin, ymax)

  if (Normal == TRUE){
  distribution_plot <- distribution_plot +
     geom_line(aes(x = x, y = n, fill = n), color = "red")#+
    # geom_area(aes(x = x, y = n), alpha = 0.5, color = "red")
  }

  return(distribution_plot)
}
