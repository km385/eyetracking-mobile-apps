# Przegląd danych - struktura, kilka pierwszych wierszy
glimpse(dane_aoi)
head(dane_aoi)
str(dane_aoi)

# Wstępna wizualizacja TTFF w zależności od aplikacji
ggplot(dane_aoi, aes(x = aplikacja, y = ttff)) +
  geom_boxplot() +
  labs(title = "TTFF dla różnych aplikacji")

# Test Shapiro-Wilka dla normalności rozkładu TTFF w każdej aplikacji
# Wnioski:
# - TTFF w DUOLINGO i MONDLY nie mają rozkładu normalnego (p < 0.05)
# - BUSUU i MEMRISE mają rozkład zbliżony do normalnego (p > 0.05)

by(dane_aoi$ttff, dane_aoi$aplikacja, shapiro.test)

# Próba analizy wariancji (ANOVA), mimo naruszenia założeń
anova_model <- aov(ttff ~ aplikacja, data = dane_aoi)
summary(anova_model)

# Post-hoc do ANOVA (niewiarygodny z uwagi na brak normalności reszt)
TukeyHSD(anova_model)

library(dplyr)
library(rstatix)

# ANOVA z pakietu rstatix
# Również nie wykazała istotnych różnic między aplikacjami (p > 0.05)

dane_aoi %>%
  anova_test(ttff ~ aplikacja) %>%
  get_anova_table()

# Test Kruskala-Wallisa jako nieparametryczny odpowiednik ANOVA
# Wniosek:
# - Brak statystycznie istotnych różnic między aplikacjami pod względem TTFF (p = 0.179)

kruskal.test(ttff ~ aplikacja, data = dane_aoi)

# Test normalności reszt modelu ANOVA
# Wniosek:
# - Reszty modelu nie mają rozkładu normalnego (p = 0.0016)
# - ANOVA w tym przypadku nie jest wiarygodna – należy oprzeć się na teście Kruskala

model <- aov(ttff ~ aplikacja, data = dane_aoi)
shapiro.test(residuals(model))

# Estetyczna wizualizacja TTFF z rozrzutem punktów
# Widać, że DUOLINGO ma nieco wyższe TTFF, ale różnice nie są istotne statystycznie

ggplot(dane_aoi, aes(x = aplikacja, y = ttff, fill = aplikacja)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "TTFF (Time to First Fixation) dla różnych aplikacji",
    y = "TTFF (ms)", x = "Aplikacja"
  ) +
  theme(legend.position = "none")

# Test post-hoc do Kruskala (Dunn's test)
# Wniosek:
# - Największa różnica obserwowana między BUSUU a DUOLINGO (p = 0.027), 
#   ale po poprawce Bonferroniego staje się nieistotna (p = 0.164)
# - Brak istotnych różnic w parach aplikacji (p.adj > 0.05 dla wszystkich)

library(FSA)
dunnTest(ttff ~ aplikacja, data = dane_aoi, method = "bonferroni")