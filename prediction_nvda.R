
# Charger les packages
library(quantmod)
library(forecast)
library(ggplot2)

# Obtenir les données boursières de Nvidia (NVDA)
getSymbols("NVDA", src = "yahoo", from = Sys.Date() - 10, to = Sys.Date())
nvda_data <- NVDA[, "NVDA.Adjusted"]

# Créer un modèle de prévision pour prédire le prix de l'action demain
model <- auto.arima(nvda_data)
forecasted <- forecast(model, h = 1)

# Combiner les données historiques et la prévision
nvda_combined <- data.frame(
  Date = index(nvda_data),
  Price = as.numeric(nvda_data),
  Type = "Actual"
)
nvda_forecast <- data.frame(
  Date = index(nvda_data)[length(nvda_data)] + 1,
  Price = as.numeric(forecasted$mean),
  Type = "Forecast"
)
nvda_combined <- rbind(nvda_combined, nvda_forecast)

# Créer un graphique avec ggplot2
ggplot(nvda_combined, aes(x = Date, y = Price, color = Type)) +
  geom_line(data = subset(nvda_combined, Type == "Actual"), size = 1) +
  geom_line(data = subset(nvda_combined, Type == "Forecast"), linetype = "dashed", size = 1) +
  labs(title = "Prix de l'action Nvidia avec prévision pour demain",
       x = "Date",
       y = "Prix de l'action ($)",
       color = "Légende") +
  theme_minimal()
