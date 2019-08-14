# How do sample behave from a population?

# If this script brings error, first connect your computer to the internet and rerun it again

# Importing the R libraries

if (!require("install.load")) {
  install.packages("install.load")
}

install.load::install_load(c("ggplot2", "tibble", "magrittr"))


# Generating population data in which we shall take the sample from

dat <- tibble(x = 20:290, y = dnorm(20:290, mean = 150, sd = 40))

dat %>% head(10)


# Plot a normal probability distribution

ggplot(dat, aes(x = x, y = y)) + geom_line() + theme_bw() + 
  labs(y = "Probability density", title = "Normal distribution", subtitle = "X ~ norm(mean = 150, sd = 40)") + scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(breaks = seq(20, 290, 50)) + 
  geom_vline(xintercept = 150, color = "red", show.legend = TRUE) + 
  annotate("text", x = 150, y = 0.0075, label = "mean = 150")



# This function generate random sampling, plot its distribution and test for its normality

sampling <- function(n) {
  n = as.integer(readline(cat('How many sample size do you want to generate?\nn=')))
  if (is.na(n) == TRUE) {
    cat('Pease enter a positive number greater than 5 \n and rerun the sampling() function again')
  }else if (n < 5) {
    cat('Pease enter a positive number greater than 5 \n and rerun the sampling() function again')
  }else {
    sample_df = tibble(sample = rnorm(n, mean = 150, sd = 40))
    print(sample_df)
    cat('The sample size is:', n, '\n')
    cat('The mean of the sample is:', mean(sample_df$sample),'\n')
    cat('The standard deviation of the sample is:', sd(sample_df$sample), '\n')
    print(sample_df %>% ggplot(aes(x = sample, y = ..density..))+ 
             geom_histogram(fill = 'blue', color= 'black', 
                            breaks = pretty(range(sample_df$sample), 
                                            n = nclass.Sturges(sample_df$sample), min.n = 1)) + 
            geom_density() + theme_bw()+
             labs(y = 'Probability density', 
                  x = 'Sample', title = paste0('Distribution of ', n, ' samples'), 
                  subtitle = 'from a normal distribution with mean = 150, sd = 40'))
    print(shapiro.test(sample_df$sample))
    if (shapiro.test(sample_df$sample)$p.value > 0.05) cat("The sample is normally distributed") else cat("The sample is not normally distributed")
  }
  
}


sampling()


# Please respond to the console




# keep on running sampling() function below and vary the value of sample size n to see how sample behave from the population.

sampling()

# Please respond to the console

