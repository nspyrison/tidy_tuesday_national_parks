## Read
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

library(tidyverse)

## Want to explain the variation visitors
str(park_visits)
skimr::skim(park_visits)
View(park_visits)
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

### Linear model:
lm(visitors ~ year + pop + state + gas_constant, data =  dn_sub)

