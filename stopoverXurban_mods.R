source("setup.R")

# set data directory
data.dir<-"/Users/mikkojimenez/Desktop/urbanstopover_git/data"
# read in dfs for counties + parks
counties_df <- read.csv(file.path(data.dir, "counties_df.csv"))

# mini data prep steps
## make X2013_c ordinal
counties_df$X2013_c <- factor(counties_df$X2013_c, levels = 6:1, ordered = TRUE)
## create flyway subsets
counties_Atlantic_df <- counties_df %>% filter(flyway == "Atlantic Flyway")
counties_Mississippi_df <- counties_df %>% filter(flyway == "Mississippi Flyway")
counties_Central_df <- counties_df %>% filter(flyway == "Central Flyway")
counties_Pacific_df <- counties_df %>% filter(flyway == "Pacific Flyway")

# ORDINAL MODELS
## continental us model
model_na_ord_sp <- brm(
  mean_stopover_sp ~ X2013_c, 
  data = counties_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)

model_na_ord_fa <- brm(
  mean_stopover_fa ~ X2013_c, 
  data = counties_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)

## atlantic model
model_atl_ord_sp <- brm(
  mean_stopover_sp ~ X2013_c, 
  data = counties_Atlantic_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)

model_atl_ord_fa <- brm(
  mean_stopover_fa ~ X2013_c, 
  data = counties_Atlantic_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)
## mississippi model
model_miss_ord_sp <- brm(
  mean_stopover_sp ~ X2013_c, 
  data = counties_Mississippi_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)

model_miss_ord_fa <- brm(
  mean_stopover_fa ~ X2013_c, 
  data = counties_Mississippi_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)
## central model
model_cent_ord_sp <- brm(
  mean_stopover_sp ~ X2013_c, 
  data = counties_Central_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)

model_cent_ord_fa <- brm(
  mean_stopover_fa ~ X2013_c, 
  data = counties_Central_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)
## pacific model
model_pac_ord_sp <- brm(
  mean_stopover_sp ~ X2013_c, 
  data = counties_Pacific_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)

model_pac_ord_fa <- brm(
  mean_stopover_fa ~ X2013_c, 
  data = counties_Pacific_df,
  family = Gamma(link = "log"),  
  prior = c(
    prior(normal(0, 10), class = "b"),  # Prior for coefficients
    prior(normal(0, 10), class = "Intercept")  # Prior for intercept
  ),
  iter = 2000,  # Number of iterations
  warmup = 1000,  # Number of warmup iterations
  chains = 4  # Number of chains
)

# summarize model estimates
modelofinterest <- model_na_ord_fa
summary(modelofinterest)
conditional_effects(modelofinterest)
