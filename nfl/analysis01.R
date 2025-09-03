
# Style Guide:
# For functions with many arguments, use this style:
# hist(
#     sims, breaks = (-0.5):(17.5), freq = FALSE, 
#     xlab = "Number of Wins", ylab = "Probability",
#     main = "Simulated Distribution of Wins for Houston Texans in 2025"
# )
# Nothing on the first line after the opening parenthesis
# Each argument on its own line, indented by 4 spaces
# For Short calls, putting everything on one line is fine

library(aod) 
library(lme4)
library(tidyverse) 

# read in the data 
data <- read.csv("nfl/raw_data/standings.csv") 
head(data)

# remove * and + from data$Tm in one line using gsub
data$Tm <- gsub("[\\*\\+]", "", data$Tm)
# write a comment explaining the regex above
# The regex "[\\*\\+]" matches either a '*' or a '+' character
# say more about how [] works
# The square brackets [] denote a character class, meaning it will match any one of the characters
# say more about the double backslash
# The double backslash \\ is used to escape the special characters * and +,

# redefine team names
data$Tm[data$Tm == "Oakland Raiders"] <- "Las Vegas Raiders"
data$Tm[data$Tm == "St. Louis Rams"] <- "Los Angeles Rams"
data$Tm[data$Tm == "San Diego Chargers"] <- "Los Angeles Chargers"
data$Tm[data$Tm == "Washington Redskins"] <- "Washington Commanders"
data$Tm[data$Tm == "Washington Football Team"] <- "Washington Commanders"

# calculate prior wins and losses
data$prior_wins <- NA
data$prior_losses <- NA
data$prior_SoS <- NA

for( j in 1:nrow(data) ) {
    ii <- which( data$Tm == data$Tm[j] & data$year == data$year[j] - 1 )
    if( length(ii) == 1 ){
        data$prior_wins[j] <- data$W[ii]
        data$prior_losses[j] <- data$L[ii]
        data$prior_SoS[j] <- data$SoS[ii]
    }
}

# calculate prior percent wins
data$prior_pct <- data$prior_wins / (data$prior_wins + data$prior_losses)

n <- nrow(data)
plot(
    data$prior_wins, data$W,
    xlab = "Prior Wins (with jitter)", ylab = "Current Wins (with jitter)"
)

n <- nrow(data)
plot(
    data$prior_wins + 0.1*rnorm(n), data$W  + 0.1*rnorm(n),
    xlab = "Prior Wins (with jitter)", ylab = "Current Wins (with jitter)"
)

# try fitting a binomial glm for wins and losses based on prior percent wins
m1 <- glm( cbind(W, L) ~ prior_pct, data = data, family = binomial(link = "logit") )
summary(m1)

# try a transformation
data$logit_prior_pct <- log( data$prior_pct / (1 - data$prior_pct) )
# handle the case where prior_pct is 0
data$logit_prior_pct[ data$prior_pct == 0 ] <- log( (0.5/17) / (16.5/17) )
m2 <- glm( cbind(W, L) ~ logit_prior_pct, data = data, family = binomial(link = "logit") )
summary(m2)

# make a prediction dataset for 2025
pred_data <- data[ data$year == 2024, c("Tm", "W", "L") ]
pred_data$prior_pct <- pred_data$W / (pred_data$W + pred_data$L)
pred_data$logit_prior_pct <- log( pred_data$prior_pct / (1 - pred_data$prior_pct) )

# get predictions using pred_data and model m1
pred_df <- predict( m2, newdata = pred_data, type = "response", se.fit = TRUE )
pred_data$pred_prob <- pred_df$fit
pred_data$pred_se <- pred_df$se.fit

# pick a team (Houston Texans) and get probabilities for 0 to 17 wins
p <- pred_data$pred_prob[ pred_data$Tm == "Houston Texans" ]
probs <- dbinom( 0:17, size = 17, prob = p )

# plot the probabilities using barplot
barplot(
    probs, names.arg = 0:17, xlab = "Number of Wins", ylab = "Probability",
    main = "Predicted Distribution of Wins for Houston Texans in 2025"
)

# you can also do this by simulating from the model 10000 times
# please follow the style guide above
n_sims <- 10000
sims <- rbinom( n_sims, size = 17, prob = p )
hist(
    sims, breaks = (-0.5):(17.5), freq = FALSE, 
    xlab = "Number of Wins", ylab = "Probability",
    main = "Simulated Distribution of Wins for Houston Texans in 2025"
)

# You can also simulate the distribution of wins for all teams in 2025
pred_probs <- predict( m1, newdata = pred_data, type = "response" )
n_sims <- 10000
sims <- matrix( NA, nrow = n_sims, ncol = 32)
for( j in 1:32 ) {
    sims[, j] <- rbinom( n_sims, size = 17, prob = pred_probs[j] ) 
}

# make a histogram of the simulated results for all teams
# plot it on a plot below the histogram of the actual number of wins
par(mfrow = c(2,1))
hist( data$W[ data$year >= 2021 ], breaks = (-0.5):(17.5), freq = FALSE,
      xlab = "Number of Wins", ylab = "Probability",
      main = "Actual Distribution of Wins for All Teams in 2021-2024"
)
hist(
    as.vector(sims), breaks = (-0.5):(17.5), freq = FALSE, 
    xlab = "Number of Wins", ylab = "Probability",
    main = "Simulated Distribution of Wins for All Teams in 2025"
)

# instead of a histogram, plot cumulative distribution functions of actual and simulated
sim_pcts <- as.vector(sims) / 17
par(mfrow = c(1,1))
plot( ecdf(data$W[ data$year >= 2021 ] / 17), 
     verticals = TRUE, do.points = FALSE, col = "red", lwd = 2,
     xlab = "Wins", ylab = "Cumulative Probability",
     main = "2021-2024 Actual vs Simulated Wins"
)
lines( ecdf(sim_pcts), verticals = TRUE , do.points = FALSE, col = "blue", lwd = 2 )

# Fit a beta binomial model
library( "aod" )
data2 <- data[ !is.na(data$prior_pct), ]
data2$id <- 1:nrow(data2)
m3 <- betabin( cbind(W, L) ~ logit_prior_pct, random  = ~ 1, data = data2 )
summary(m3)


# try a genearlized linear mixed model
library( "lme4" )
m4 <- glmer( cbind(W, L) ~ logit_prior_pct + (1|id), data = data2, family = binomial(link = "logit") )
summary(m4)
 
season2015 = read_csv("nfl/raw_data/season2015.csv") 
season2016 = read_csv("nfl/raw_data/season2016.csv") 
season2017 = read_csv("nfl/raw_data/season2017.csv") 
season2018 = read_csv("nfl/raw_data/season2018.csv") 
season2019 = read_csv("nfl/raw_data/season2019.csv") 
season2020 = read_csv("nfl/raw_data/season2020.csv")
season2021 = read_csv('nfl/raw_data/season2021.csv') 
season2022 = read_csv("nfl/raw_data/season2022.csv") 
season2023 = read_csv("nfl/raw_data/season2023.csv") 
season2024 = read_csv("nfl/raw_data/season2024.csv") 
season2025 = read_csv("nfl/raw_data/season2025.csv")

seasons15_to_24 = bind_rows(
    season2015, season2016, season2017, season2018, season2019, season2020, 
    season2021, season2022, season2023, season2024
)

seasons15_to_24 = seasons15_to_24 %>% 
    mutate(
        home_team_win = case_when(
            ...6 == '@' ~ 0, 
            .default = 1 
        ), 
        home_team_win = as.integer(home_team_win)
    )

saveRDS(seasons15_to_24, "nfl/raw_data/season15_to_24.rds") 
