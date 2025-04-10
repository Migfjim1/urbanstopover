source("setup.R")

# set data directory
data.dir<-"/Users/mikkojimenez/Desktop/urbanstopover_git/data"
# read in dfs for counties + parks
counties_df <- read.csv(file.path(data.dir, "counties_df.csv"))

# create dfs for full model, subset model, and subsub model (>5 parks per city)
## read in dfs for counties + parks, scale p
metro_park_shp <- read.csv(file.path(data.dir, "metro_park_df.csv"))
## create subset data (removing parks smaller than pixel size)
metro_park_sub_shp <- metro_park_shp %>%
  filter(Park_Size_ > 247.105)
## create subsubset data (>5 parks per city that fit criteria)
summary_df <- metro_park_sub_shp %>%
  group_by(Park_Urban) %>% # group by unique values in Park_Urban
  summarise(count = n())  # count the number of rows in each group
summary_df <- summary_df %>%
  filter(count > 4)
metro_park_subsub_shp <- metro_park_sub_shp %>%
  filter(Park_Urban %in% summary_df$Park_Urban)

# full model (w all parks)
model_full <- brm(
  formula = mean_stopover_sp ~ prop.high + (1 + prop.high | Park_Urban),
  data = metro_park_shp,
  family = Gamma(link = "log"),
  prior = c(set_prior("normal(0, 2)", class = "b"),   # Prior for fixed effects
            set_prior("exponential(1)", class = "sd")), # Prior for random effects
  iter = 8000,       # Number of iterations
  warmup = 4000,     # Number of warmup iterations
  chains = 4,        # Number of chains
  cores = 8,         # Number of cores to use
  control = list(
    adapt_delta = 0.99, 
    max_treedepth = 15
  )
)

summary(model_full)
conditional_effects(model_full)

# subset model (w parks > than pixel size)
model_sub <- brm(
  formula = mean_stopover_sp ~ prop.high + (1 + prop.high | Park_Urban),
  data = metro_park_sub_shp,
  family = Gamma(link = "log"),
  prior = c(set_prior("normal(0, 2)", class = "b"),   # Prior for fixed effects
            set_prior("exponential(1)", class = "sd")), # Prior for random effects
  iter = 8000,       # Number of iterations
  warmup = 4000,     # Number of warmup iterations
  chains = 4,        # Number of chains
  cores = 8,         # Number of cores to use
  control = list(
    adapt_delta = 0.99, 
    max_treedepth = 15
  )
)

summary(model_sub)
conditional_effects(model_sub)

# subsubset model (w >5 parks)
model_subsub <- brm(
  formula = mean_stopover_sp ~ prop.high + (1 + prop.high | Park_Urban),
  data = metro_park_subsub_shp,
  family = Gamma(link = "log"),
  prior = c(set_prior("normal(0, 2)", class = "b"),   # Prior for fixed effects
            set_prior("exponential(1)", class = "sd")), # Prior for random effects
  iter = 8000,       # Number of iterations
  warmup = 4000,     # Number of warmup iterations
  chains = 4,        # Number of chains
  cores = 8,         # Number of cores to use
  control = list(
    adapt_delta = 0.99, 
    max_treedepth = 15
  )
)

summary(model_subsub)
conditional_effects(model_subsub)