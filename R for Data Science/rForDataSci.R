# load tidyverse
library(tidyverse)

# load csv 'as tibble'
gapminder <- read_csv("R for Data Science\\gapminder_clean.csv")

# filter for year 1962
gapminder_1962 <- gapminder %>%
    filter(Year == 1962) # select year

# plot of co2 emmisions vs gdp per capita, for 1962
plot_1962 <- ggplot(gapminder_1962, aes(x = `CO2 emissions (metric tons per capita)`, # nolint
    # use backticks to get around spaces in column names
    y = gdpPercap)) +
    geom_point() +
    labs(title = "CO2 emissions vs. GDP per capita")

print(plot_1962)

# correlation and p-value
cor.test(gapminder_1962$`CO2 emissions (metric tons per capita)`,
    gapminder_1962$gdpPercap,
    use = "complete.obs",
    method = "pearson"
    )

# 'no hardcoded results'
# correlation is 0.9260817
# p-value is 2.2e-16

# In what year was the correlation between CO2 emissions
# and GDPpercap the strongest
cor_year <- gapminder %>%
    group_by(Year) %>%
    summarize(cor = cor(`CO2 emissions (metric tons per capita)`,
    `gdpPercap`, use = "complete.obs")) %>%
    slice(which.max(cor))

#print(cor_year)

# filtered for strongest correlation year
# using Plotly
# create scatter plot
# co2 emissions vs gdppercap
# point size determined by pop
# color is determined by continent
library(plotly)

corr_year_plot <- gapminder %>%
    filter(Year == cor_year$Year) %>%
    ggplot(aes(x = `CO2 emissions (metric tons per capita)`,
    y = gdpPercap)) +
    geom_point(aes(color = continent, size = pop)) +
    labs(title = "CO2 Emissions vs GDP Per Cap, 1967")

corr_year_plot <- ggplotly(corr_year_plot)

cont_v_energy <- gapminder %>%
    group_by(continent) %>%
    summarize(sum_energy = sum(`Energy use (kg of oil equivalent per capita)`, na.rm = T)) %>% # nolint
    ggplot(aes(x = continent, y = sum_energy)) +
    geom_col(aes(color = continent)) +
    labs(title = "Continent Vs Energy Use")

cont_v_energy <- ggplotly(cont_v_energy)