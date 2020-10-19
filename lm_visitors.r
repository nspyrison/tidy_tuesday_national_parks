## Read
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop   <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price   <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

library(tidyverse)

## Want to explain the variation visitors
str(park_visits)
skimr::skim(park_visits)
#View(park_visits)
summary(park_visits$visitors)
hist(park_visits$visitors[park_visits$visitors < 1000000])

### removed NA and "Total", and windorize 2% from each tail
rows_na <- park_visits$visitors %>% is.na()
rows_total <- park_visits$year == "Total"
sub <- park_visits[!rows_na & !rows_total,]
wind2pct <- quantile(sub$visitors, probs = c(.02, .98))
sub <- sub[sub$visitors > wind2pct[1] & sub$visitors < wind2pct[2], ]
sub$year <- as.integer(sub$year)

### Denormalize, remove new NAs
dn_sub <- left_join(sub, state_pop, by = c("state", "year")) %>% left_join(gas_price, "year")
skimr::skim(dn_sub)
rows_na2 <- dn_sub$pop %>% is.na()
rows_na3 <- dn_sub$gas_constant %>% is.na()
dn_sub <- dn_sub[!rows_na2 & !rows_na3,]
dn_sub$state <- as.factor(dn_sub$state)
dn_sub$state_int <- as.integer(dn_sub$state)

### Linear model:
mod <- lm(visitors ~ year + pop + state + gas_constant, data = dn_sub)

## Shapes and color
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1, 2, 3, 6, 8)]
shap <- c(0:6,8,11,14)
state_cols  <- rep(pal, each = 10)
state_shaps <- rep(shap, 5)
cols  <- state_cols[dn_sub$state_int]
shaps <- state_shaps[dn_sub$state_int]

### Plot with shape-color, R^2 and equ
gg <- ggplot(mod) + 
  geom_point(aes(x=.fitted, y=.resid), alpha = 1,
             color = cols, shape = shaps) +
  theme_minimal() +
  geom_text(x = 0, y = 3800000, label =
              paste0("adj R^2: ", format(summary(mod)$adj.r.squared, digits = 4))
  ) +
  ggtitle("US National Parks \n
          Linear model: Visitors ~ year, pouplation, state, gas prices")

tictoc::tic("print large ggplot2")
print(gg)
tictoc::toc()
beepr::beep()

ggsave(filename = "US_park_lm-visitors.png")
