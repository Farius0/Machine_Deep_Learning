library("div")
library("conflicted")
library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_57_87_07 <- dplyr::filter(gapminder, year %in% c(1957,1987, 2007))
head(gapminder_57_87_07)
esp_vie <- group_by(gapminder_57_87_07, year)

esp_vie_summary <- summarize(esp_vie,
  Moy_EspVie = mean(lifeExp),
  Med_EspVie = median(lifeExp),
  Sd_EspVie = sd(lifeExp) * sqrt((length(lifeExp) / (length(lifeExp) - 1))),
  CV_Espvie = Sd_EspVie / Moy_EspVie,
  Q25_EspVie = quantile(lifeExp, 0.25),
  Q75_EspVie = quantile(lifeExp, 0.75))
print(esp_vie_summary)

ggplot(gapminder_57_87_07, aes(x = year, y = lifeExp, color = factor(year))) +
  geom_boxplot() +
  labs(x = "Année", y = "Espérance de vie", 
  title = "Évolution de l'espérance de vie (1957-1987-2007)") + scale_color_discrete(name = "Année")
gdp <- group_by(gapminder_57_87_07, year)

gdp_percap_summary <- summarize(gdp,
  Moy_gdp = mean(gdpPercap),
  Med_gdp = median(gdpPercap),
  Sd_gdp = sd(gdpPercap) * sqrt((length(gdpPercap) / (length(gdpPercap) - 1))),
  CV_gdp = Sd_gdp / Moy_gdp,
  Q25_gdp = quantile(gdpPercap, 0.25),
  Q75_gdp = quantile(gdpPercap, 0.75))
print(gdp_percap_summary)

ggplot(gapminder_57_87_07, aes(x = year, y = gdpPercap, color = factor(year))) +
  geom_violin() +
  labs(x = "Année", y = "Pib par habitant",
  title = "Évolution du PIB par habitant (1957-1987-2007)") + scale_color_discrete(name = "Année")
correlation <- cor(gapminder_57_87_07$lifeExp, gapminder_57_87_07$gdpPercap)
correlation

ggplot(gapminder_57_87_07, aes(x = lifeExp, fill = continent)) + geom_density() + 
labs(x = " Espérance de vie",y = " Densité ", 
title = "Relation entre l'espérance de vie et les continents") + facet_wrap(~continent)

ggplot(esp_vie_summary, aes(x = year,  y = Moy_EspVie, fill = factor(year))) + 
geom_bar(stat = "identity") +
geom_text(aes(label = round(Moy_EspVie, 2)), vjust = -0.5) +
labs(x = "Année",y = " Espérance de vie",
title = "Moyenne de l'Espérance de vie par année")

ggplot(gdp_percap_summary, aes(x = year,  y = Moy_gdp, fill = factor(year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Moy_gdp, 2)), vjust = -0.5) +
  labs(
    x = "Année",
    y = " PIB par habitant",
    title = "Moyenne du PIB par habitant par année")
ggplot(gdp_percap_summary, aes(x = factor(year))) +
  geom_bar(aes(y = Moy_gdp, fill = "Moyenne"), stat = "identity", position = position_dodge(width = 0.75), width = 0.5) +
  geom_bar(aes(y = Med_gdp, fill = "Médiane"), stat = "identity", position = position_dodge(width = 0.75), width = 0.5) +
  geom_text(aes(y = Moy_gdp, label = round(Moy_gdp, 2)), vjust = -0.5, position = position_dodge(width = 0.75)) +
  geom_text(aes(y = Med_gdp, label = round(Med_gdp, 2)), vjust = -0.5, position = position_dodge(width = 0.75)) +
  labs(
    x = "Année",
    y = "PIB par habitant",
    title = "Moyenne et Médiane du PIB par habitant par année"
  )

ggplot(gapminder_57_87_07, aes(x = lifeExp, y = gdpPercap,color = factor(year))) + 
geom_point() + labs(x = "Espérance de vie", y = "PIB par habitant",
title = "Relation entre PIB par habitant et l'espérance de vie (Filtre Année)") + 
scale_color_discrete(name = "Année")


ggplot(gapminder_57_87_07, aes(x = lifeExp, y = gdpPercap,color = factor(continent))) +
geom_boxplot() + labs(x = "Espérance de vie", y = "PIB par habitant",
title = "Relation entre PIB par habitant et l'espérance de vie (Filtre continent)") + 
scale_color_discrete(name = "Continent")
