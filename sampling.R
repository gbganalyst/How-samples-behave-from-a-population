# How do sample behave from a population?

# If this script brings error, first connect your computer to the internet and rerun it again

# Importing the r libraries

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
  geom_vline(xintercept = 150, color = "red", show.legend = T) + 
  annotate("text", x = 150, y = 0.0075, label = "mean = 150")



# This function generate random sampling and plot its distribution 

sampling <- function(n = 155){
  if (n < 0 ){
    print('Pease enter a positive number')
  }else {
    sample = tibble(sample = rnorm(n, mean = 150, sd = 40))
    print(sample)
    cat('The sample size is:', n, '\n')
    cat('The mean of the sample is:', mean(sample$sample),'\n')
    cat('The standard deviation of the sample is:', sd(sample$sample))
    return(sample %>% ggplot(aes(x = sample, y = ..density..))+ 
     geom_histogram(fill = 'blue', color= 'black', 
    breaks = pretty(range(sample$sample), n = nclass.Sturges(sample$sample),min.n = 1)) + geom_density() + theme_bw()+
        labs(y = 'Probability density', x = 'Sample', title = paste0('Distribution of ', n, ' samples'), subtitle = 'from a normal distribution with mean = 150, sd = 40'))
  }
  
  }
  
# Take a sample size (n) to see how sample behave from the population

sampling(n = 30)

# keep on changing sample size (n) to see how sample behave from the population

sampling(n = 52)


sampling(n= 180)

sampling(n= 300)


sampling(n= 500)

sampling(250)

sampling(52000)
