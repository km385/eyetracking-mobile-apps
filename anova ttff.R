# Przegląd danych - struktura, kilka pierwszych wierszy
glimpse(dane_aoi)
head(dane_aoi)
str(dane_aoi)

library(tidyverse)
### !!!!!!!!! WYNIKI W KOMENTARZACH DLA DANYCH Z AOI 2 I 3. GDY ZROBIONO TYLKO DLA AOI
### LICZBY SA INNE ALE WYNIKI I WNIOSKI NADAL TAKIE SAME



aggr_aoi <- read_csv("data-clean/combined_aoi.csv", skip = 7)

# Krok 1: Przenieś pierwszy wiersz jako kolumny
colnames(aggr_aoi) <- aggr_aoi[1, ]

# Krok 2: Usuń pierwszy wiersz (nagłówek w danych)
aggr_aoi <- aggr_aoi[-1, ]


# Wczytanie i przygotowanie danych
dane_aoi <- aggr_aoi %>%
  filter(Type == "Static AOI", Label == "AOI 1") %>%
  select(
    app_task = `Parent Label`,
    zadanie = `Parent Label`,
    czas_fiksacji = `Dwell time (fixation, ms)`,
    liczba_rewizyt = `Revisit count (fixation dwells)`,
    ttff = `TTFF (AOI)`
  ) %>%
  separate(
    col = app_task,
    into = c("aplikacja", "nr_zadania"),
    sep = "(?<=[A-Za-z])(?=[0-9])",
    remove = FALSE
  ) %>%
  mutate(
    aplikacja = as.factor(toupper(aplikacja)),
    zadanie = as.factor(nr_zadania),
    across(c(czas_fiksacji, liczba_rewizyt, ttff), as.numeric)
  )


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


dane_aoi %>% 
  summarize(avg = mean(ttff))




## wizualizacja rozkladow i normalnosci
library(ggpubr)
ggplot(dane_aoi, aes(x = ttff, fill = aplikacja)) +
  geom_histogram(aes(y = ..density..), bins = 15, color = "black", alpha = 0.6, position = "identity") +
  geom_density(alpha = 0.3) +
  facet_wrap(~aplikacja, scales = "free") +
  theme_minimal() +
  labs(
    title = "Rozkład TTFF dla każdej aplikacji",
    x = "TTFF (ms)",
    y = "Gęstość"
  )


ggqqplot(dane_aoi, x = "ttff", facet.by = "aplikacja", color = "aplikacja") +
  labs(
    title = "Q-Q ploty TTFF dla aplikacji",
    x = "Teoretyczne kwantyle",
    y = "Obserwowane kwantyle"
  )
