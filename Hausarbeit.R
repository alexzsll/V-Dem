library(vdemdata)
library(tidyverse)
vdem_df <- vdemdata::vdem
library(dplyr)
library(ggplot2)

baltikum_df_90 <- vdem_df %>%
  filter(country_name %in% c("Estonia", "Latvia", "Lithuania"), year >= 1990) %>%
  group_by(year) %>%
  summarise(
    country_name = "Baltikum (Durchschnitt)",
    v2x_libdem = mean(v2x_libdem, na.rm = TRUE),
    .groups = "drop"
  )

vergleich_einzel_90 <- vdem_df %>%
  filter(country_name %in% c("Hungary", "Georgia"), year >= 1990) %>%
  select(year, country_name, v2x_libdem) %>%
  mutate(country_name = case_when(
    country_name == "Georgia" ~ "Georgien",
    country_name == "Hungary" ~ "Ungarn",
    TRUE ~ country_name
  ))

vergleich_df_90 <- bind_rows(vergleich_einzel_90, baltikum_df_90)

ggplot(vergleich_df_90, aes(x = year, y = v2x_libdem, color = country_name, group = country_name)) +
  theme_bw(base_size = 11) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  
  annotate("text", x = 1991.4, y = 0.00, label = "1991: UdSSR-Zerfall", angle = 90, size = 3, color = "grey35", hjust = 0) +
  annotate("text", x = 2004.4, y = 0.00, label = "2004: EU-Osterweiterung", angle = 90, size = 3, color = "grey35", hjust = 0) +
  annotate("text", x = 2010.4, y = 0.00, label = "2010: Fidesz-Wahlsieg", angle = 90, size = 3, color = "grey35", hjust = 0) +
  
  geom_line(linewidth = 1.1, linetype = "solid") +
  geom_point(size = 1.6) +
  
  scale_color_manual(values = c(
    "Baltikum (Durchschnitt)" = "#0072B2", # Blau
    "Ungarn"                  = "#D55E00", # Vermillion (Orange-Rot)
    "Georgien"                = "#E69F00"  # Gelb-Gold
  )) +
  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  
  labs(
    title = "Abbildung 1: Regimetransformationen im postsozialistischen Raum (1990–2025)",
    subtitle = "Vergleich des Liberal Democracy Index (LDI) zwischen Baltikum, Ungarn und Georgien",
    x = "Jahr",
    y = "Liberal Democracy Index (LDI)",
    color = "Untersuchte Fälle:",
    caption = "Datenquelle: V-Dem Dataset v16"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )


hungary_df <- vdem_df %>%
  filter(country_name == "Hungary", year >= 1990) %>%
  select(year, v2mecenefm, v2csreprss) %>%
  pivot_longer(
    cols = c(v2mecenefm, v2csreprss),
    names_to = "Indikator",
    values_to = "Wert"
  ) %>%
  mutate(Indikator = case_when(
    Indikator == "v2mecenefm" ~ "Freiheit von Medienzensur",
    Indikator == "v2csreprss"  ~ "Freiheit der Zivilgesellschaft",
    TRUE ~ Indikator
  ))

ggplot(hungary_df, aes(x = year, y = Wert, color = Indikator, group = Indikator)) +
  theme_bw(base_size = 11) +
  
  geom_vline(xintercept = 2010, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  annotate("text", x = 2010.4, y = 2.7, label = "2010: Fidesz-Wahlsieg", angle = 90, size = 3, color = "grey35", hjust = 0) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  
  scale_color_manual(values = c(
    "Freiheit der Zivilgesellschaft" = "#0072B2",
    "Freiheit von Medienzensur"     = "#D55E00"
  )) +
  
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  
  labs(
    title = "Abbildung 2: Sequenzen des Demokratieabbaus in Ungarn (1990–2025)",
    subtitle = "Schleichender Verfall von Medienfreiheit und Zivilgesellschaftsrechten",
    x = "Jahr",
    y = "V-Dem Indikatorwert (0–4)",
    color = "Untersuchte Indikatoren:",
    caption = "Datenquelle: V-Dem Dataset v16 | Skala 0–4: Höhere Werte bedeuten mehr Freiheit / weniger Repression"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )


georgia_df <- vdem_df %>%
  filter(country_name == "Georgia", year >= 1990) %>%
  select(year, v2mecenefm, v2csreprss) %>%
  pivot_longer(
    cols = c(v2mecenefm, v2csreprss),
    names_to = "Indikator",
    values_to = "Wert"
  ) %>%
  mutate(Indikator = case_when(
    Indikator == "v2mecenefm" ~ "Freiheit von Medienzensur",
    Indikator == "v2csreprss"  ~ "Freiheit der Zivilgesellschaft",
    TRUE ~ Indikator
  ))

ggplot(georgia_df, aes(x = year, y = Wert, color = Indikator, group = Indikator)) +
  theme_bw(base_size = 11) +
  
  geom_vline(xintercept = 2003, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  annotate("text", x = 2003.4, y = 2.7, label = "2003: Rosenrevolution", angle = 90, size = 3, color = "grey35", hjust = 0) +
  annotate("text", x = 2024.4, y = 2.7, label = "2024: Agenten-Gesetz", angle = 90, size = 3, color = "grey35", hjust = 0) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  
  scale_color_manual(values = c(
    "Freiheit der Zivilgesellschaft" = "#0072B2",
    "Freiheit von Medienzensur"     = "#D55E00"
  )) +
  
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  
  labs(
    title = "Abbildung 3: Sequenzen des Demokratieabbaus in Georgien (1990–2025)",
    subtitle = "Entwicklung der Medienfreiheit und Zivilgesellschaftsrepression",
    x = "Jahr",
    y = "V-Dem Indikatorwert (0–4)",
    color = "Untersuchte Indikatoren:",
    caption = "Datenquelle: V-Dem Dataset v16 | Skala 0–4: Höhere Werte bedeuten mehr Freiheit / weniger Repression"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )




ggplot(vergleich_df_90, aes(x = year, y = v2x_libdem, color = country_name, group = country_name)) +
  theme_bw(base_size = 11) +
  
  geom_vline(xintercept = 1991, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  
  annotate("text", x = 1991.4, y = 0.00, label = "1991: UdSSR-Zerfall", angle = 90, size = 3, color = "grey35", hjust = 0) +
  annotate("text", x = 2004.4, y = 0.00, label = "2004: EU-Osterweiterung", angle = 90, size = 3, color = "grey35", hjust = 0) +
  annotate("text", x = 2010.4, y = 0.00, label = "2010: Fidesz-Wahlsieg", angle = 90, size = 3, color = "grey35", hjust = 0) +
  
  geom_line(linewidth = 1.1, linetype = "solid") +
  geom_point(size = 1.6) +
  
  scale_color_manual(values = c(
    "Baltikum (Durchschnitt)" = "#0072B2", # Blau
    "Ungarn"                  = "#D55E00", # Vermillion (Orange-Rot)
    "Georgien"                = "#E69F00"  # Gelb-Gold
  )) +
  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  
  labs(
    x = "Jahr",
    y = "Liberal Democracy Index (LDI)",
    color = "Untersuchte Fälle:"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )




hungary_df <- vdem_df %>%
  filter(country_name == "Hungary", year >= 1990) %>%
  select(year, v2mecenefm, v2csreprss) %>%
  pivot_longer(
    cols = c(v2mecenefm, v2csreprss),
    names_to = "Indikator",
    values_to = "Wert"
  ) %>%
  mutate(Indikator = case_when(
    Indikator == "v2mecenefm" ~ "Freiheit von Medienzensur",
    Indikator == "v2csreprss"  ~ "Freiheit der Zivilgesellschaft",
    TRUE ~ Indikator
  ))

ggplot(hungary_df, aes(x = year, y = Wert, color = Indikator, group = Indikator)) +
  theme_bw(base_size = 11) +
  
  geom_vline(xintercept = 2010, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  annotate("text", x = 2010.4, y = 2.7, label = "2010: Fidesz-Wahlsieg", angle = 90, size = 3, color = "grey35", hjust = 0) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  
  scale_color_manual(values = c(
    "Freiheit der Zivilgesellschaft" = "#0072B2",
    "Freiheit von Medienzensur"     = "#D55E00"
  )) +
  
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  
  labs(
    x = "Jahr",
    y = "V-Dem Indikatorwert (0–4)",
    color = "Untersuchte Indikatoren:"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )



georgia_df <- vdem_df %>%
  filter(country_name == "Georgia", year >= 1990) %>%
  select(year, v2mecenefm, v2csreprss) %>%
  pivot_longer(
    cols = c(v2mecenefm, v2csreprss),
    names_to = "Indikator",
    values_to = "Wert"
  ) %>%
  mutate(Indikator = case_when(
    Indikator == "v2mecenefm" ~ "Freiheit von Medienzensur",
    Indikator == "v2csreprss"  ~ "Freiheit der Zivilgesellschaft",
    TRUE ~ Indikator
  ))

ggplot(georgia_df, aes(x = year, y = Wert, color = Indikator, group = Indikator)) +
  theme_bw(base_size = 11) +
  
  geom_vline(xintercept = 2003, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  geom_vline(xintercept = 2024, linetype = "dashed", color = "grey65", linewidth = 0.5) +
  annotate("text", x = 2003.4, y = 2.7, label = "2003: Rosenrevolution", angle = 90, size = 3, color = "grey35", hjust = 0) +
  annotate("text", x = 2024.4, y = 2.7, label = "2024: Agenten-Gesetz", angle = 90, size = 3, color = "grey35", hjust = 0) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  
  scale_color_manual(values = c(
    "Freiheit der Zivilgesellschaft" = "#0072B2",
    "Freiheit von Medienzensur"     = "#D55E00"
  )) +
  
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  
  labs(
    x = "Jahr",
    y = "V-Dem Indikatorwert (0–4)",
    color = "Untersuchte Indikatoren:"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )
