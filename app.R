#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(ggplot2)
library(dplyr)
library(png)
library(shiny)
library(shinyWidgets)
library(Rlab)
library(extraDistr)
library(stats)




skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "green"

header <- dashboardHeader(
  title = "Distribution in Statistic"
)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
    menuItem("Introduction", tabName = "intro", icon = icon("home")),
    menuItem("Distribution", tabName = "dist", icon = icon("th")),
    menuItem("Plots", tabName = "plots" ,icon = icon("chart-line"),
             menuSubItem("Bernoulli’s Distribution", tabName = "Bernoulli"),
             menuSubItem("Binomial Distribution", tabName = "Binomial"),
             menuSubItem("Normal (Gaussian) Distribution", tabName = "Normal"),
             menuSubItem("Poisson Distribution", tabName = "Poisson"),
             menuSubItem("Exponential Distribution", tabName = "Exponential"),
             menuSubItem("Multinomial Distribution", tabName = "Multinomial"),
             menuSubItem("Beta Distribution", tabName = "Beta"),
             menuSubItem("Beta-binomial distribution", tabName = "Beta-binomial"),
             menuSubItem("T-Distributions", tabName = "T-"),
             menuSubItem("Uniform distribution", tabName = "Uniform")),
    menuItem("Source code for app", icon = icon("file-code-o"),
             href = "https://github.com/Abbasjalili/Shiny-Project/blob/d799e1183352a66345e0a293c594bee7c5aaa19a/app.R"),
    menuItem("Reference", icon = icon("list"),
             menuSubItem("Reference 1", 
                         href = "https://www.analyticssteps.com/blogs/10-types-statistical-data-distribution-models"),
             menuSubItem("Reference 2", 
                         href = "https://www.rdocumentation.org/")
             
    )
             
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("intro",
            # Solid backgrounds
            fluidPage(
              box(
                title = h1("Introduction"),
                width = 12,
                background = "olive",
                HTML(paste(h4("This is a project for Stat 651 class in Fall-2021 at CSU East Bay. I created this shiny app to 
                              introduce ten different types of statistical data distribution models. Data distribution is 
                              a function that determines the values of a variable and quantifies relative frequency, and it 
                              transforms raw data into graphical methods to give valuable information. Therefore, it becomes 
                              substantial to understand the kind of distribution that a population has that assists in applying 
                              proper statistical techniques/methods. In this project, I present ten types of models for data distributions. listed below:"), "<br>",
                              "1- Bernoulli’s Distribution", "<br>","2- Binomial Distribution", "<br>",
                              "3- Normal (Gaussian) Distribution", "<br>","4- Poisson Distribution", "<br>",
                              "5- Exponential Distribution", "<br>","6- Multinomial Distribution", "<br>",
                              "7- Beta Distribution", "<br>","8- Beta-binomial distribution", "<br>",
                              "9- T- Distributions", "<br>","10- Uniform distribution", "<br>",
                              "I am going to introduce each distribution with their graphs.")))
              )
            ),
    
    tabItem("dist",
            # Solid backgrounds
            fluidPage(
                     tabBox(
                         height = 300,
                         width = 12,
                         tabPanel("Bernoulli",
                            withMathJax(),
                            HTML(paste(h5("This is one of the simplest distributions that can be used as an initial point to derive more complex distributions. 
                                           Bernoulli’s distribution has possibly two outcomes (success or failure) and a single trial."), "<br>",
                                        h5("For example, tossing a coin, the success probability of an outcome to be heads is p, then the probability of 
                                            having tail as outcome is (1-p). Bernoulli’s distribution is the special case of binomial distribution with a single trial."), "<br>",
                                        h5("The density function can be given as"), "<br>",
                                        h5("$$f(x) = {p^x}  (1-p)^{(1-x)}   \\ where \\ x \\ € \\ (0,1)$$"), "<br>",
                                        h5("It can also be written as;"), "<br>",
                                        h5("$$P(x) = \\bigg\\{_{p \\ \\ \\ \\ \\ \\ , \\ x = 1}^{1-p \\ , \\ x = 0}$$"), "<br>",
                                        h5("The distribution has following characteristics;"), "<br>",
                                        h5("* The number of trials, to be performed, need to be predefined for a single experiment."), "<br>",
                                        h5("* Each trial has only two possible outcomes-success or failure."), "<br>",
                                        h5("* The probability of success of each event/experiment must be the same."), "<br>",
                                        h5("* Each event must be independent of each other.")))
                            ),
                            tabPanel("Binomial",
                                     withMathJax(),
                                     HTML(paste(h5("The binomial distribution is applied in binary outcomes events where the probability of success is equal to the probability of 
                                                   failure in all the successive trials. Its example includes tossing a biased/unbiased coin for a repeated number of times."), "<br>",
                                                h5("As input, the distribution considers two parameters, and is thus called as bi-parametric distribution. The two parameters are;"), "<br>",
                                                h5("* The number of times an event occurs, n, and"),"<br>",
                                                h5("* Assigned probability, p, to one of the two classes"),"<br>",
                                                h5("For n number of trials, and success probability, p, the probability of successful event (x) 
                                                   within n trials can be determined by the following formula"), "<br>",
                                                h5("$$P(X \\ = \\ x)=\\frac{n!}{x! \\ (n-x)!} \\ {p^x} \\ (1-p)^{(n-x)}$$"), "<br>",
                                                h5("The binomial distribution holds the following properties;"), "<br>",
                                                h5("* For multiple trials provided, each trial is independent to each other, i.e, the result of one trial cannot influence other trials."), "<br>",
                                                h5("* Each of the trials can have two possible outcomes, either success or failure, with probabilities p, and (1-p)."), "<br>",
                                                h5("* A total number of n identical trials can be conducted, and the probability of success and failure is the same for all trials.")))
                                     
                            ),
                            tabPanel("Normal",
                                     withMathJax(),
                                     HTML(paste(h5("Being a continuous distribution, the normal distribution is most commonly used in data science. A very common process of our day to day 
                                                   life belongs to this distribution- income distribution, average employees report, average weight of a population, etc."), "<br>",
                                                h5("The formula for normal distribution;"), "<br>",
                                                h5("$$P(x) \\ = \\frac{1}{\\sigma \\sqrt{2\\pi}} \\ e^{\\frac{-1}{2}(\\frac{x-\\mu}{\\sigma})^2}$$"),"<br>",
                                                h5("Where μ = Mean value,"),"<br>",
                                                h5("σ = Standard probability distribution of probability,"), "<br>",
                                                h5("x = random variable"), "<br>",
                                                h5("According to the formula,  the distribution is said to be normal if mean (μ) = 0 and standard deviation (σ) = 1"), "<br>",
                                                h5("Normal distribution has the following properties;"), "<br>",
                                                h5("* Mean, mode and median coincide with each other."), "<br>",
                                                h5("* The distribution has a bell-shaped distribution curve."), "<br>",
                                                h5("* The distribution curve is symmetrical to the centre."), "<br>",
                                                h5("* The area under the curve is equal to 1.")))
                                     
                            ) ,
                            tabPanel("Poisson",
                                     withMathJax(),
                                     HTML(paste(h5("Being a part of discrete probability distribution, poisson distribution outlines the probability for a given number of events that 
                                                   take place in a fixed time period or space, or particularized intervals such as distance, area, volume."), "<br>",
                                                h5("For example, conducting risk analysis by the insurance/banking industry, anticipating the number of car accidents in a particular 
                                                   time interval and in a specific area."), "<br>",
                                                h5("Poisson distribution considers following assumptions;"),"<br>",
                                                h5("* The success probability for a short span is equal to success probability for a long period of time."),"<br>",
                                                h5("* The success probability in a duration equals to zero as the duration becomes smaller."), "<br>",
                                                h5("* A successful event can’t impact the result of another successful event"), "<br>",
                                                h5("A poisson distribution can be modeled using the formula below, "), "<br>",
                                                h5("$$P(X)\\ = \\frac{\\lambda^x \\ e^{-\\lambda}}{X!}$$"), "<br>",
                                                h5("Where 𝝺 represents the possible number of events take place in a fixed period of time, and X is the number of events in that time 
                                                   period."), "<br>",
                                                h5("Poisson distribution has the following characteristics;"), "<br>",
                                                h5("* The events are independent of each other, i.e, if an event occurs, it doesn’t affect the probability of another event occurring."), "<br>",
                                                h5("* An event could occur any number of times in a defined period of time."), "<br>",
                                                h5("* Any two events can’t be occurring at the same time."), "<br>",
                                                h5("* The average rate of events to take place is constant.")))
                                     
                            ) ,
                            tabPanel("Exponential",
                                     withMathJax(),
                                     HTML(paste(h5("Like the poisson distribution, exponential distribution has the time element; it gives the probability of a time duration before 
                                                   an event takes place."), "<br>",
                                                h5("Exponential distribution is used for survival analysis, for example, life of an air conditioner, expected life of a machine,and 
                                                   length of time between metro arrivals."), "<br>",
                                                h5("A variable X is said to possess an exponential distribution when "),"<br>",
                                                h5("$$f(x;\\lambda) = \\bigg\\{_{0 \\ \\ \\ \\ \\ \\ , \\ \\ x < 0}^{\\lambda \\ e^{-\\lambda x} \\ , \\ x \\geq 0}$$"),"<br>",
                                                h5("Where  λ stands for rate and always has value greater than zero."), "<br>",
                                                h5("The exponential distribution has following characteristics;"), "<br>",
                                                h5("* As shown in the graph, the higher the rate, the faster the curve drops, and lower the rate, flatter the curve."), "<br>",
                                                h5("* In survival analysis, λ is termed as a failure rate of a machine at any time t with the assumption that the machine will 
                                                   survive upto t time.")))
                                     
                            ) ,
                            tabPanel("Multinomial",
                                     withMathJax(),
                                     HTML(paste(h5("The multinomial distribution is used to measure the outcomes of experiments that have two or more variables. It is the special 
                                                   type of binomial distribution when there are two possible outcomes such as true/false or success/failure."), "<br>",
                                                h5("The distribution is commonly used in biological, geological and financial applications. "), "<br>",
                                                h5("A very popular Mendel experiment where two strains of peas (one green and wrinkled seeds and other is yellow and smooth seeds)
                                                   are hybridized that produced four different strains of seeds-green and wrinkled, green and round, yellow and round, and yellow 
                                                   and wrinkled. This resulted in multinomial distribution and led to the discovery of the basic principles of genetics."),"<br>",
                                                h5("The density function for multinomial distribution is"),"<br>",
                                                h5("$$P= \\frac{n!}{(n_1!)(n_2!)...(n_x!)} P_1n1P_2n2...P_xnx$$"), "<br>",
                                                h5("Where n= number of experiments."), "<br>",
                                                h5("Px= probability of occurrence of an experiment."), "<br>",
                                                h5("The following are properties of multinomial distribution;"), "<br>",
                                                h5("* An experiment can have a repeated number of trials, for example, rolling of  a dice multiple times."), "<br>",
                                                h5("* Each trial is independent of each other."), "<br>",
                                                h5("* The success probability of each outcome must be the same (constant) for all trials of an experiment.")))
                                     
                            ) ,
                            tabPanel("Beta",
                                     withMathJax(),
                                     HTML(paste(h5("Beta distribution comes under continuous probability distributions having the interval [0,1] with two shape parameters that can 
                                                   be expressed by alpha (ɑ) and beta(ꞵ). These two parameters are the exponent of a random variable and control the shape of the 
                                                   distribution."), "<br>",
                                                h5("The distribution shows the family of probabilities and is a suitable model to depict random behaviour of percentages or proportions. 
                                                   It is used for the data models that hold uncertainties of the success probabilities in a random experiment. "), "<br>",
                                                h5("The probability density function for the beta distribution is"),"<br>",
                                                h5("$$\\frac{x^{\\alpha -1}(1-x)^{\\beta-1}}{B(\\alpha , \\beta)}$$"),"<br>",
                                                h5("Where 𝝱 is second shape parameter and B( ɑ, ꞵ) is normalizing constant that makes sure area under the curve is one."), "<br>",
                                                h5("The general formulation of beta distribution is also known as the beta distribution of first kind and beta distribution of 
                                                   second kind is another name of beta prime distribution."), "<br>",
                                                h5("Beta distribution has many applications in statistical description of allele frequencies in genetic population, time allocation 
                                                   in project management, sunshine data, proportions of minerals in rocks, etc.")))
                                     
                            ),
                            tabPanel("Beta-binomial",
                                     withMathJax(),
                                     HTML(paste(h5("A data distribution is said to be beta-binomial if the "), "<br>",
                                                h5("* Probability of success, p, is greater than zero."), "<br>",
                                                h5("* And, shape of beat binomial parameter, α > 0, as well as β > 0"),"<br>",
                                                h5("Being the simplest form of Bayesian mode, beta-binomial distribution has 
                                                   extensive applications in intelligence testing, epidemiology, and marketing.  "),"<br>",
                                                h5("The parametric shape can be defined in the form of  the probability of success such that"), "<br>",
                                                h5("* A distribution tends to a binomial distribution for the greater value of α and β."), "<br>",
                                                h5("* The value of discrete uniform distribution is equivalent to the distribution between 0 to n, if both the 
                                                   values α = β = 1."), "<br>",
                                                h5("* For n = 1, the beta-binomial distribution is approximately the same as Bernoulli distribution."), "<br>",
                                                h5("Talking about the key difference amid a beta-distribution and binomial distribution, the success probability, 
                                                p, is always fixed for a set of trials whereas it is not fixed for beta-binomial distribution and changes trail to trail.")))
                                     
                            ),
                            tabPanel("T-Distributions",
                                     withMathJax(),
                                     HTML(paste(h5("In statistics, t-distribution is the most important distribution, also known as student’s t-distribution. It is employed to 
                                                   estimate population parameters when the sample size is small, and the standard deviation is unknown."), "<br>",
                                                h5("It is widely used for hypothesis testing and built confidence intervals for mean values."), "<br>",
                                                h5("T-distribution has the following properties;"),"<br>",
                                                h5("* Similar to normal distribution, the t-distribution has bell-shaped curve distribution and is symmetric when mean is zero."),"<br>",
                                                h5("* The shape of distribution doesn’t alter with degrees of freedom, and has the range – ∞ to ∞."), "<br>",
                                                h5("* The variance is always more than one."), "<br>",
                                                h5("* As the sample size, n, increases, t-distribution acts as normal distribution where the considered sample size is greater than 30.")))
                                     
                            ),
                            tabPanel("Uniform",
                                     withMathJax(),
                                     HTML(paste(h5("Uniform distribution can either be discrete or continuous where each event is equally likely to occur. It has a constant probability 
                                                   constructing a rectangular distribution.  "), "<br>",
                                                h5("In this type of distribution, an unlimited number of outcomes will be possible and all the events have the same probability, similar 
                                                   to Bernoulli’s distribution. For example, while rolling a dice, the outcomes are 1 to 6 that have equal probabilities of ⅙ and represent 
                                                   a uniform distribution. "), "<br>",
                                                h5("A variable X is said to have uniform distribution if the probability density function is "),"<br>",
                                                h5("$$f(x) = \\frac{1}{b-a} \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ for \\ -\\infty < a \\leq x \\leq b \\leq \\infty$$"),"<br>",
                                                h5("The uniform distribution has the following properties;"), "<br>",
                                                h5("* The probability density function combines to unity."), "<br>",
                                                h5("* Every input function has an equal weightage.")))
                                     
                            )  
                           )
                    )
            
            
            ),
    tabItem("Bernoulli",
            fluidPage(
              box(
                title = h3("Bernoulli R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Density, distribution function, quantile function and random generation for the Bernoulli 
                              distribution with parameter prob."), "<br>",
                              ("R has four in-built functions to generate binomial distribution. They are described below."), "<br>",
                              ("dbern(x, prob, log = FALSE)"), "<br>",
                              ("pbern(q, prob, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                              ("qbern(p, prob, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                              ("rbern(n, prob)"), "<br>",
                              h5("Following is the description of the parameters used −"), "<br>",
                              ("x, q: is a vector of numbers."), "<br>",
                              ("p: is a vector of probabilities."), "<br>",
                              ("n: is number of observations."), "<br>",
                              ("prob: is the probability of success of each trial."), "<br>",
                              ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                              ("lower.tail: logical; if TRUE (default)")))
                )
              ),
            fluidPage(
              box(
                title = "Histogram",
                status = "primary",
                plotOutput("plot1_1", height = 300),
                height = 400),
              box(
                title = "Quantile Function Graph",
                status = "primary",
                plotOutput("plot1_2", height = 300),
                height = 400)
            ),
            
            # Boxes with solid headers
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count1_1", "Count", min = 1, max = 500, value = 120),
                numericInput("prob1_1", "Prob", value = 0.5 , min = 0, max = 1, step = 0.1),
                collapsible = TRUE, collapsed = TRUE)
            )
    ),
    tabItem("Binomial",
            fluidPage(
              box(
                title = h3("Binomial R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("The binomial distribution model deals with finding the probability of success of an event which has only 
                              two possible outcomes in a series of experiments. For example, tossing of a coin always gives a head or a 
                              tail. The probability of finding exactly 3 heads in tossing a coin repeatedly for 10 times is estimated 
                              during the binomial distribution."), "<br>",
                           ("R has four in-built functions to generate binomial distribution. They are described below."), "<br>",
                           ("dbinom(x, size, prob)"), "<br>",
                           ("pbinom(x, size, prob)"), "<br>",
                           ("qbinom(p, size, prob)"), "<br>",
                           ("rbinom(n, size, prob)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x: is a vector of numbers."), "<br>",
                           ("p: is a vector of probabilities."), "<br>",
                           ("n: is number of observations."), "<br>",
                           ("size: is the number of trials."), "<br>",
                           ("prob: is the probability of success of each trial.")))
              )
            ),
            fluidPage(
              
              box(
                title = "Histogram",
                status = "primary",
                plotOutput("plot2_1", height = 300),
                height = 400)
            ),
            fluidRow(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count_2", "Size", min = 1, max = 500, value = 20),
                numericInput("success", "Success", value = 20 , min = 0, max = 500, step = 1),
                numericInput("prob_2", "Prob", value = 0.5 , min = 0.1, max = 1, step = 0.1),
                collapsible = TRUE, collapsed = TRUE))
              ),
    tabItem("Normal",
            fluidPage(
              box(
                title = h3("Normal R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Generally, it is observed that the collection of random data from independent sources is distributed 
                              normally. We get a bell shape curve on plotting a graph with the value of the variable on the horizontal 
                              axis and the count of the values in the vertical axis. The centre of the curve represents the mean of the dataset."), "<br>",
                           ("R has It has four inbuilt functions. They are described below:"), "<br>",
                           ("dnorm(x, mean = 0, sd = 1, log = FALSE)"), "<br>",
                           ("pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("rnorm(n, mean = 0, sd = 1)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x, q: is a vector of numbers."), "<br>",
                           ("p: is a vector of probabilities."), "<br>",
                           ("n: is number of observations."), "<br>",
                           ("mean: vector of means."), "<br>",
                           ("sd: vector of standard deviations."), "<br>",
                           ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                           ("lower.tail: logical; if TRUE (default)")))
              )
            ),
            fluidPage(
      box(
        title = "Histogram",
        status = "primary",
        plotOutput("plot3_1", height = 300),
        height = 400),
      box(
        title = "Line Graph",
        status = "primary",
        plotOutput("plot3_2", height =300),
        height = 400)
    ),
    
    # Boxes with solid headers
    fluidPage(
      box(
        title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
        sliderInput("count_3", "Count", min = 1, max = 500, value = 120),
        numericInput("mean", "Mean", value = 1 , min = 0, max = 100, step = 0.1),
        numericInput("sd", "SD", value = 1 , min = 0, max = 100, step = 0.1),
        collapsible = TRUE, collapsed = TRUE)
      )
     ),
    tabItem("Poisson",
            fluidPage(
              box(
                title = h3("Poisson R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Density, distribution function, quantile function and random generation for the Poisson distribution with parameter lambda."), "<br>",
                           ("R has It has four inbuilt functions. They are described below:"), "<br>",
                           ("dpois(x, lambda, log = FALSE)"), "<br>",
                           ("ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("qpois(p, lambda, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("rpois(n, lambda)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x, q: is a vector of numbers."), "<br>",
                           ("p: is a vector of probabilities."), "<br>",
                           ("n: is number of observations."), "<br>",
                           ("lambda: vector of (non-negative) means."), "<br>",
                           ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                           ("lower.tail: logical; if TRUE (default)")))
              )
            ),
            fluidPage(
              box(
                title = "PDF plot",
                status = "primary",
                plotOutput("plot4_1", height = 300),
                height = 400),
              box(
                title = "CDF plot",
                status = "primary",
                plotOutput("plot4_2", height = 300),
                height = 400),
              box(
                title = "Quntile function plot",
                status = "primary",
                plotOutput("plot4_3", height = 300),
                height = 400),
              box(
                title = "Histogram (Random sample size)",
                status = "primary",
                plotOutput("plot4_4", height = 300),
                height = 400)
            ),
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                numericInput("count4_1", "Success", value = 20 , min = 0, max = 500, step = 1),
                numericInput("count4_2", "Lambda", value = 5 , min = 0, max = 20, step = 1),
                numericInput("count4_3", "Sample Size", value = 100 , min = 1, max = 1000000, step = 1),
                collapsible = TRUE, collapsed = TRUE))
    ),
    tabItem("Exponential",
            fluidPage(
              box(
                title = h3("Exponential R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Density, distribution function, quantile function and random generation for the exponential distribution with rate rate 
                              (i.e., mean 1/rate)."), "<br>",
                           ("R has It has four inbuilt functions. They are described below:"), "<br>",
                           ("dexp(x, rate = 1, log = FALSE)"), "<br>",
                           ("pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("rexp(n, rate = 1)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x, q: is a vector of numbers."), "<br>",
                           ("p: is a vector of probabilities."), "<br>",
                           ("n: is number of observations."), "<br>",
                           ("rate: vector of rates."), "<br>",
                           ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                           ("lower.tail: logical; if TRUE (default)")))
              )
            ),
            fluidPage(
              box(
                title = "PDF Plot",
                status = "primary",
                plotOutput("plot5_1", height = 300),
                height = 400),
              box(
                title = "CDF Plot",
                status = "primary",
                plotOutput("plot5_2", height =300),
                height = 400)
            ),
            
            # Boxes with solid headers
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count5_1", "Count", min = 4, max = 10, value = 10),
                numericInput("count5_2", "rate1", value = 0.5 , min = 0, max = 100, step = 0.1),
                numericInput("count5_3", "rate2", value = 1 , min = 0, max = 100, step = 0.1),
                numericInput("count5_4", "rate3", value = 1.5 , min = 1, max = 100, step = 0.1),
                collapsible = TRUE, collapsed = TRUE))
    ),
    tabItem("Multinomial",
            fluidPage(
              box(
                title = h3("Multinomial R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Generate multinomially distributed random number vectors and compute multinomial probabilities."), "<br>",
                           ("R has It has four inbuilt functions. They are described below:"), "<br>",
                           ("rmultinom(n, size, prob)"), "<br>",
                           ("dmultinom(x, size = NULL, prob, log = FALSE)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x : vector of length K of integers in 0:size."), "<br>",
                           ("size: integer, say N, specifying the total number of objects that are put into K boxes in the typical 
                            multinomial experiment. For dmultinom, it defaults to sum(x)."), "<br>",
                           ("n: is number of observations."), "<br>",
                           ("prob: numeric non-negative vector of length K, specifying the probability for the K classes; 
                            is internally normalized to sum 1. Infinite and missing values are not allowed."), "<br>",
                           ("log : logical; if TRUE, log probabilities are computed.")))
              )
            ),
            fluidPage(
              box(
                title = "Multinomial BarPlot",
                status = "primary",
                plotOutput("plot6_1", height = 300),
                height = 400, width = 400)
            ),
            
            # Boxes with solid headers
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count6_1", "Number of Samples", min = 1, max = 10, value = 5),
                sliderInput("count6_2", "Number of Experiments", min = 1, max = 1000, value = 10),
                numericInput("prob6_1", "Prob1", value = 0.25 , min = 0, max = 1, step = 0.1),
                numericInput("prob6_2", "Prob1", value = 0.25 , min = 0, max = 1, step = 0.1),
                numericInput("prob6_3", "Prob1", value = 0.25 , min = 0, max = 1, step = 0.1),
                numericInput("prob6_4", "Prob1", value = 0.25 , min = 0, max = 1, step = 0.1),
                collapsible = TRUE, collapsed = TRUE
              ))
    ),
    tabItem("Beta",
            fluidPage(
              box(
                title = h3("Beta R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Density, distribution function, quantile function and random generation for the Beta 
                              distribution with parameters shape1 and shape2 (and optional non-centrality parameter ncp)."), "<br>",
                           ("R has It has four inbuilt functions. They are described below:"), "<br>",
                           ("dbeta(x, shape1, shape2, ncp = 0, log = FALSE)"), "<br>",
                           ("pbeta(q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("qbeta(p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("rbeta(n, shape1, shape2, ncp = 0)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x, q: is a vector of numbers."), "<br>",
                           ("p: is a vector of probabilities."), "<br>",
                           ("n: is number of observations."), "<br>",
                           ("shape1, shape2	: non-negative parameters of the Beta distribution."), "<br>",
                           ("ncp	: non-centrality parameter."), "<br>",
                           ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                           ("lower.tail: logical; if TRUE (default)")))
              )
            ),
            fluidPage(
              box(
                title = "Beta PDF Plot",
                status = "primary",
                plotOutput("plot7_1", height = 300),
                height = 400)
            ),
            
            # Boxes with solid headers
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                numericInput("count7_2", "Alpha", value = 5 , min = 0.1, max = 1000, step = 0.1),
                numericInput("count7_3", "Beta", value = 5 , min = 0.1, max = 1000, step = 0.1),
                collapsible = TRUE, collapsed = TRUE
              ))
    ),
    tabItem("Beta-binomial",
            fluidPage(
              box(
                title = h3("Beta-binomial R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Density, distribution function, quantile function, and random generation for the beta-binomial 
                              distribution. A variable with a beta-binomial distribution is distributed as binomial distribution 
                              with parameters N and p, where the probability p of success iteself has a beta distribution with 
                              parameters u and v."), "<br>",
                           ("R has It has four inbuilt functions. They are described below:"), "<br>",
                           ("dbbinom(x, size, alpha = 1, beta = 1, log = FALSE)"), "<br>",
                           ("pbbinom(q, size, alpha = 1, beta = 1, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("rbbinom(n, size, alpha = 1, beta = 1)"), "<br>",
                         h5("Following is the description of the parameters used −"), "<br>",
                           ("x, q: is a vector of numbers."), "<br>",
                           ("size: number of trials (zero or more)."), "<br>",
                           ("n: number of observations. If length(n) > 1, the length is taken to be the number required"), "<br>",
                           ("alpha, beta	: non-negative parameters of the beta distribution."), "<br>",
                           ("ncp	: non-centrality parameter."), "<br>",
                           ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                           ("lower.tail: logical; if TRUE (default)")))
              )
            ),
            fluidPage(
              box(
                title = "Hostogram Simulation (Random sample size)",
                status = "primary",
                plotOutput("plot8_1", height = 300),
                height = 400),
              box(
                title = "Randomly drawn Histogram Plot",
                status = "primary",
                plotOutput("plot8_2", height = 300),
                height = 400),
              box(
                title = "CDF Plot",
                status = "primary",
                plotOutput("plot8_3", height = 300),
                height = 400)
            ),
            
            # Boxes with solid headers
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count8_1", "n", min = 1, max = 1000000, value = 1000),
                numericInput("count8_2", "Sample size", value = 1000 , min = 0, max = 10000, step = 10),
                numericInput("count8_3", "Alpha", value = 5 , min = 0.1, max = 1000, step = 0.1),
                numericInput("count8_4", "Beta", value = 5 , min = 0.1, max = 1000, step = 0.1),
                collapsible = TRUE, collapsed = TRUE))
    ),
    tabItem("T-",
            fluidPage(
              box(
                title = h3("T-Distributions R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("Density, distribution function, quantile function and random generation for 
                              the t distribution with df degrees of freedom (and optional non-centrality parameter ncp)."), "<br>",
                           ("R has It has four inbuilt functions. They are described below:"), "<br>",
                           ("dt(x, df, ncp, log = FALSE)"), "<br>",
                           ("pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("rt(n, df, ncp)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x, q: is a vector of numbers."), "<br>",
                           ("size: number of trials (zero or more)."), "<br>",
                           ("n: number of observations. If length(n) > 1, the length is taken to be the number required"), "<br>",
                           ("df	: degrees of freedom (> 0, maybe non-integer). df = Inf is allowed."), "<br>",
                           ("ncp	: non-centrality parameter delta; currently except for rt(), only for abs(ncp) <= 37.62. If omitted, 
                            use the central t distribution."), "<br>",
                           ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                           ("lower.tail: logical; if TRUE (default)")))
              )
            ),
            fluidPage(
              box(
                title = "T-Distributions PDF Simulation (with different degrees of freedom)",
                status = "primary",
                plotOutput("plot9_1", height = 300),
                height = 400),
              box(
                title = "T-Distributions CDF Simulation",
                status = "primary",
                plotOutput("plot9_2", height = 300),
                height = 400),
              box(
                title = "T-Distributions quantile function Simulation",
                status = "primary",
                plotOutput("plot9_3", height = 300),
                height = 400),
              box(
                title = "T-Distributions Generating random numbers Simulation",
                status = "primary",
                plotOutput("plot9_4", height = 300),
                height = 400)
              ),
           # Boxes with solid headers
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count9_1", "Range of the Distribution", min = 1, max = 20, value = 10),
                numericInput("count9_2", "1st curve df", value = 2 , min = 0, max = 100, step = 1),
                numericInput("count9_3", "2nd curve df", value = 15 , min = 0, max = 100, step = 1),
                numericInput("count9_4", "3rdt curve df", value = 35 , min = 0, max = 100, step = 1),
                numericInput("count9_5", "Sample Size", value = 100 , min = 1, max = 1000000, step = 1),
                collapsible = TRUE, collapsed = TRUE))
    ),
    tabItem("Uniform",
            fluidPage(
              box(
                title = h3("Uniform Distributions R function document"),
                width = 12,
                background = "olive",
                HTML(paste(h5("These functions provide information about the uniform distribution on the interval from min to max.
                              dunif gives the density, punif gives the distribution function qunif gives the quantile function and 
                              runif generates random deviates."), "<br>",
                           ("dunif(x, min = 0, max = 1, log = FALSE)"), "<br>",
                           ("punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)"), "<br>",
                           ("runif(n, min = 0, max = 1)"), "<br>",
                           h5("Following is the description of the parameters used −"), "<br>",
                           ("x, q: is a vector of numbers."), "<br>",
                           ("size: number of trials (zero or more)."), "<br>",
                           ("n: number of observations. If length(n) > 1, the length is taken to be the number required"), "<br>",
                           ("min, max	: lower and upper limits of the distribution. Must be finite."), "<br>",
                           ("log, log.p: logical; if TRUE, probabilities p are given as log(p)."), "<br>",
                           ("lower.tail: logical; if TRUE (default)")))
              )
            ),
            fluidPage(
              box(
                title = "Uniform Simulation (with different degrees of freedom)",
                status = "primary",
                plotOutput("plot10_1", height = 300),
                height = 400),
              box(
                title = "Uniform Simulation (with different degrees of freedom)",
                status = "primary",
                plotOutput("plot10_2", height = 300),
                height = 400),
              box(
                title = "Uniform Simulation (with different degrees of freedom)",
                status = "primary",
                plotOutput("plot10_3", height = 300),
                height = 400),
              box(
                title = "Uniform Simulation (with different degrees of freedom)",
                status = "primary",
                plotOutput("plot10_4", height = 300),
                height = 400),
            ),
            
            # Boxes with solid headers
            fluidPage(
              box(
                title = "Main control", width = 4, solidHeader = TRUE, status = "primary",
                sliderInput("count10_1", "Range of the Distribution", min = 1, max = 1000, value = 100),
                numericInput("count10_2", "a", value = 5 , min = 0, max = 75, step = 1),
                numericInput("count10_3", "b", value = 50 , min = 0, max = 75, step = 1),
                numericInput("count10_4", "Sample Size", value = 100 , min = 1, max = 1000000, step = 1),
                collapsible = TRUE, collapsed = TRUE))
    )
  )
)



ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output, session) {
  
  set.seed(122)
  
  output$plot1_1 <- renderPlot({
    if (is.null(input$count1_1))
      return()
    x <- rbern(input$count1_1, prob = input$prob1_1)
   hist(x, col = "gray", 
         main = paste("Generating Random Numbers(n = ", input$count1_1, ")"))
  })
  
  output$plot1_2 <- renderPlot({
    x <- seq(0 , 1 , by = 0.1)
    y <- qbern(x, prob = input$prob1_1, lower.tail = TRUE, log.p = FALSE)
    plot(y, type = "o", main = "Quantile Function")
  })
  
  output$plot2_1 <- renderPlot({
    success <- 0:input$success
    y <- dbinom(success, size = input$count_2, prob = input$prob_2)
    plot(success, y, type = "h",
         main = paste("Binomial Distribution (n =",input$count_2,"p = " ,input$prob_2,")"),
         ylab='Probability',
         xlab ='# Successes',
         lwd=5)
  })
  
  output$plot3_1 <- renderPlot({
    if (is.null(input$count_3))
      return()
    
       x  <- rnorm(input$count_3)
    hist(x, col = "gray", main = paste("Normal Distribution (n =",input$count_3,")"))
  })
  
  output$plot3_2 <- renderPlot({
    
    set.seed(91929)
    
    x <- seq(-10 , 10 , by = 0.1)
    y <- dnorm(x, mean = input$mean, sd = input$sd)
    plot(x, y, type = "o")
  })
  
  output$plot4_1 <- renderPlot({
    set.seed(91929)
    
    x <- seq(0, input$count4_1, by = 1) 
    
    y <- dpois(x, lambda = input$count4_2)  
    
    plot(x , y, type = "l", main= "Poisson Distribution PDF plot")  
  })
  
  output$plot4_2 <- renderPlot({
    set.seed(91929)
    
    x <- seq(0, input$count4_1, by = 1) 
    
    y <- ppois(x, lambda = input$count4_2)  
    
    plot(y, type = "o" , main= "Poisson Distribution CDF plot")  
  })
  
  output$plot4_3 <- renderPlot({

    
    set.seed(91929)
    
    x <- seq(0, 1, by = 0.01)       
    y <- qpois(x, lambda = input$count4_2)
    
    plot(y, type = "o", main= "Poisson Distribution Quntile function plot")  
  })
  
  output$plot4_4 <- renderPlot({
    set.seed(91929)  
    
    N <- input$count4_3  
    
    y <- rpois(N, lambda = input$count4_2)
    
    hist(y,                                       
         breaks = 10,
         main = "",
         xlim = c(0, 50))
  })
  
  output$plot5_1 <- renderPlot({
    
    curve(dexp(x, input$count5_2), from=0, to=input$count5_1, col='blue', 
          main = paste("PDF Plot for Exponential (Rate = " ,input$count59_2, input$count5_3,input$count5_4, ")"), ylab = "y", lty=1)
    curve(dexp(x, input$count5_3), from=0, to=input$count5_1, col='red', add=TRUE, lty=1)
    curve(dexp(x, input$count5_4), from=0, to=input$count5_1, col='green', add=TRUE, lty=1)
    })
  
  output$plot5_2 <- renderPlot({
    
    curve(pexp(x, input$count5_2), from=0, to=input$count5_1, col='blue', 
          main = paste("CDF Plot for Exponential (Rate = " ,input$count59_2, input$count5_3,input$count5_4, ")"), ylab = "y", lty=1)
    curve(pexp(x, input$count5_3), from=0, to=input$count5_1, col='red', add=TRUE, lty=1)
    curve(pexp(x, input$count5_4), from=0, to=input$count5_1, col='green', add=TRUE, lty=1)
    
  })
  
  output$plot6_1 <- renderPlot({
    experiments <- rmultinom(n = input$count6_2, size = input$count6_1, prob=c(input$prob6_1, input$prob6_2, input$prob6_3, input$prob6_4))
    
    df=data.frame(experiments)/input$count6_1
    
    par(mfrow=c(2,5))
    
    for(i in 1:input$count6_1) {
      barplot(df[,i],ylim=c(0,1))
    }
  })
  
  output$plot7_1 <- renderPlot({
    
    set.seed(91929)
    
    x <- seq(0,1, length=1000)
    
    plot(x, dbeta(x, input$count7_2, input$count7_3), ylab='Density',
         type ='l', col='purple', 
         main = paste('Beta Distribution (ɑ = ', input$count7_2, "ꞵ =", input$count7_3,")"))
  })
  
  output$plot8_1 <- renderPlot({
    
    x <- rbbinom(input$count8_1, input$count8_2, input$count8_3, input$count8_4)
    
    xx <- 0:input$count8_2
    
    hist(x, 100, freq = FALSE)
    lines(xx-0.5, dbbinom(xx, input$count8_2, input$count8_3, input$count8_4), col = "red")
  })
  output$plot8_2 <- renderPlot({
    
    x <- rbbinom(input$count8_1, input$count8_2, input$count8_3, input$count8_4)
 
    hist(pbbinom(x, input$count8_2, input$count8_3, input$count8_4), 
         main = paste("Beta-binomial (n =",input$count8_1, ",Sample size =",input$count8_2,",ɑ = ", input$count8_3, ",ꞵ =", input$count8_4,")"), 
         xlab ='x',)
  })
  output$plot8_3 <- renderPlot({
    
    x <- rbbinom(input$count8_1, input$count8_2, input$count8_3, input$count8_4)
    
    set.seed(91929)
    
    xx <- seq(0, 1000, by = 0.1)
    
    plot(ecdf(x), 
         main = paste("Beta-binomial CDF plot (n =",input$count8_1, ",Sample size =",input$count8_2,",ɑ = ", input$count8_3, ",ꞵ =", input$count8_4,")"))
    lines(xx, pbbinom(xx, input$count8_2, input$count8_3, input$count8_4), col = "red", lwd = 2)
  })
  
  output$plot9_1<- renderPlot({
    
    curve(dt(x, df=input$count9_2), from=-input$count9_1, to=input$count9_1, col='blue', 
          main = paste("PDF Plot for T-Distribution (df = " ,input$count9_2, input$count9_3,input$count9_4, ")"), ylab = "y")
    curve(dt(x, df=input$count9_3), from=-input$count9_1, to=input$count9_1, col='red', add=TRUE)
    curve(dt(x, df=input$count9_4), from=-input$count9_1, to=input$count9_1, col='green', add=TRUE)
    
    
  })
  
  output$plot9_2<- renderPlot({
    
    x <- seq(- input$count9_1, input$count9_1, by = 0.01)
    
    
    y <- pt(x, df = input$count9_2)
    
    
    plot(y, type = "l", main = "T-distribution CDF plot", las=1)
  })
  
  output$plot9_3<- renderPlot({
    
    x <- seq(0.1, by = 0.01)
    
    
    y <- qt(x, df = input$count9_2)
    
    
    plot(y, main = "T-distribution Quantile function plot", las = 1)
  })
  
  output$plot9_4<- renderPlot({
    
    set.seed(91929)
    
  
    N <- input$count9_5
    
    
    y <- rt(N, df = input$count9_2)
    
    
    hist(y, breaks = 100, main = paste("Randomly drawn t density(df = ",input$count9_2, "n =",input$count9_5,")"))
  })
  
  output$plot10_1<- renderPlot({
    
    set.seed(91929)
    
    x <- seq(0, input$count10_1, by = 1) 
    
    y <- dunif(x, min = input$count10_2, max = input$count10_3)  
    
    plot(y, type = "l")  
    
  })
  output$plot10_2<- renderPlot({
    
    set.seed(91929)
    
    x <- seq(0, input$count10_1, by = 1)
    
    y <- punif(x, min = input$count10_2, max = input$count10_3)
    
    plot(y, type = "o") 
    
  })
  output$plot10_3<- renderPlot({
    
    set.seed(91929)
    
    x <- seq(0, 1, by = 0.01)       
    y <- qunif(x, min = input$count10_2, max = input$count10_3)
    
    plot(y, type = "o")  
    
  })
  output$plot10_4<- renderPlot({
    
    set.seed(91929)  
    
    N <- input$count10_4  
    
    y <- runif(N, min = input$count10_2, max = input$count10_3)
    
    hist(y,                                       
         breaks = 10,
         main = "",
         xlim = c(0, 80))
    
  })
  
}

shinyApp(ui, server)