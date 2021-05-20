## Casino solution
# name: Warakorn Na.
# date: 18 May 2021 

# win change 30% -> win 150 USD, cost 50 usd
# play 10 times


casino_royal <- function() {
    # initialize prize vector
    prize <- vector(length = 10, mode = "numeric")
    
    # play games 10 times
    for(i in 1:10) {
        result <- sample(c("win", "loss"), size = 1, prob = c(0.3, 0.7))
        if(result == "win") {
            prize[i] <- 150
        } else {
            prize[i] <- 0
        }
    }
    ## return prize
    return(sum(prize))
}


## replicate intro to simulation

mean(replicate(n = 100000, expr = casino_royal()) > 500)



# Thai population
# Mean Height = 168 cm, SD Height = 7 cm
# Questio: How many Thai (%) that height < 158 cm?

height <- rnorm(n = 100000, mean = 168, sd = 7)

hist(height)

simulated_height_lt158 <- replicate(n = 10000, 
                                    mean(rnorm(n = 1000, mean = 168, sd = 7) < 158))

mean(simulated_height_lt158)









