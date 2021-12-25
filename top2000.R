library(tidyverse)
library(patchwork)

# top2000 theme
top2000_theme <- function() {
  theme_classic() +
    theme(strip.background = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# read in data
top2000 <- read.delim("top2000_2021.txt") %>%
  as_tibble() %>%
  mutate(across(.cols = 5:27, .fns = function(x) {gsub(x = x, pattern = "X", 
                                                       replacement = NA)}),
         across(.cols = 5:27, .fns = function(x) {as.integer(x)}),
         artiest_titel = paste(artiest, titel, sep = " - "),
         decennium = case_when(jaar < 2000 ~ paste0("19", substr(x = jaar, start = 3, stop = 3), "0s"),
                               jaar >= 2000 ~ paste0("20", substr(x = jaar, start = 3, stop = 3), "0s"))) %>%
  select(-HP)

top2000_tidy <- top2000 %>%
  gather(-artiest, -titel, -jaar, -decennium, -artiest_titel,
         key = "editie", value = "positie") %>%
  mutate(editie = case_when(editie == "X99" ~ 1999,
                            TRUE ~ as.numeric(gsub(x = editie, pattern = "X", replacement = "20"))))

# number of songs per decade
decade_count <- top2000_tidy %>%
  na.omit() %>%
  group_by(editie, decennium) %>%
  dplyr::count() %>%
  filter(n >= 5) %>%
  ungroup()

# aantal liedjes per decennium
p1 <- decade_count %>%
  ggplot(aes(x = editie, y = n, group = decennium, col = decennium)) +
  geom_vline(xintercept = 2008, linetype = 2, col = "darkgrey") +
  geom_line(size = 1, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = decade_count %>%
                             filter(editie == max(editie)), xlim = c(2021, 2021),
                           aes(label = decennium), hjust = -0.1, show.legend = FALSE) +
  expand_limits(x = c(1999, 2022)) +
  scale_x_continuous(breaks = seq(1999, 2021)) +
  labs(x = "Editie", y = "Aantal nummers", colour = "Decennium",
       title = "Opkomst en 'ondergang' van verschillende decennia",
       subtitle = "Alleen datapunten van decennia met 5 nummers of meer worden getoond") +
  top2000_theme()
p1

# artists with the most songs per edition
top_artiest_plot <- top2000_tidy %>%
  na.omit() %>%
  group_by(editie, artiest) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(editie) %>%
  top_n(n = 10, wt = n) %>%
  ungroup()

artiest_levels <- top_artiest_plot %>% 
  filter(editie == max(editie),
         artiest != "Adele") %>%
  arrange(desc(n))

p2 <- top_artiest_plot %>%
  filter(artiest %in% artiest_levels$artiest) %>%
  mutate(artiest = factor(artiest, levels = artiest_levels$artiest)) %>%
  ggplot(aes(x = editie, y = n, group = artiest, col = artiest)) +
  geom_line(size = 1) +
  # ggrepel::geom_text_repel(data = top_artiest_plot %>%
  #                            filter(editie == max(editie), artiest != "Adele"), xlim = c(2021, 2021),
  #                          aes(label = artiest), hjust = -0.001, show.legend = FALSE) +
  labs(x = "Editie", y = "Aantal nummers", col = "Artiest",
       title = "Aantal nummers van de top 10 artiesten in de lijst",
       subtitle = "Artiesten met de meeste nummers in 2021 worden getoond") +
  expand_limits(x = c(1999, 2021)) +
  scale_x_continuous(breaks = seq(1999, 2021)) +
  top2000_theme() +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 3))
p2

# diversity in years across editions
p3 <- top2000_tidy %>%
  na.omit() %>%
  filter(editie %in% c(1999, 2005, 2010, 2015, 2021)) %>%
  ggplot(aes(x = jaar, group = editie, colour = factor(editie))) +
  geom_density(size = 1) +
  labs(x = "Jaar van uitgave", y = "Dichtheid", col = "Editie",
       title = "Diversiteit in tijdperken van nummers over vijf verschillende edities") +
  scale_x_continuous(breaks = seq(1920, 2030, 5)) +
  coord_cartesian(x = c(1950, 2021)) +
  top2000_theme() +
  theme(legend.position = "bottom")
p3

library(patchwork)
p <- wrap_plots(list(p1, p2, p3), ncol = 1)

# create clean matrix only including positional data across time
top2000_mat <- top2000 %>%
  select(-artiest, -titel, -jaar, -decennium) %>%
  as.data.frame() %>%
  tibble::column_to_rownames("artiest_titel") %>%
  mutate(across(.cols = 1:23, .fns = function(x) {ifelse(is.na(x), 2001, x)})) %>%
  as.matrix()

cluster_info <- hclust(dist(top2000_mat))
artiest_levels <- rownames(top2000_mat)[cluster_info$order]

p4 <- top2000_tidy %>%
  mutate(artiest_titel = factor(artiest_titel, levels = artiest_levels)) %>%
  ggplot(aes(x = editie, y = artiest_titel, fill = positie)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "white", direction = -1) +
  labs(x = "Editie", y = "Nummer", fill = "Positie",
       title = "Posities van alle nummers over de jaren heen",
       subtitle = "Nummers zonder een notering in een jaar worden getoond in wit") +
  scale_x_continuous(breaks = seq(1999, 2021, 1)) +
  top2000_theme() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
p4

ggsave(p, file = "plot.png", width = 6, height = 15)
ggsave(p1, file = "plot1.png", width = 6, height = 5)
ggsave(p2, file = "plot2.png", width = 6, height = 5)
ggsave(p3, file = "plot3.png", width = 6, height = 5)
ggsave(p4, file = "plot4.png", width = 6, height = 5)

top2000_tidy %>%
  filter(jaar >= 1950) %>%
  ggplot(aes(x = jaar, y = positie, group = editie, col = factor(editie))) +
  #geom_point(shape = 1, col = "lightgrey") +
  geom_hex() +
  geom_smooth(method = "lm", se = FALSE) +
  ggpubr::stat_cor(cor.coef.name = "rho") +
  scale_y_reverse(breaks = c(2000, 1500, 1000, 500, 1)) +
  #facet_wrap(~editie) +
  top2000_theme()

cor_data <- top2000_tidy %>%
  na.omit() %>%
  split(.$editie) %>%
  map(function(x) {lm(data = x, formula = positie ~ jaar, method = "spearman")}) %>%
  map(broom::glance) %>%
  bind_rows(.id = "editie")

cor_data %>%
  mutate(signif = ifelse(p.value < 0.05, "p < 0.05", "N.S.")) %>%
  ggplot(aes(x = editie, y = adj.r.squared, fill = signif)) +
  geom_bar(stat = "identity", col = NA) +
  labs(x = "Editie", y = "Rho", fill = "Significantie") +
  top2000_theme() +
  coord_flip()


# top artist
oldest <- top2000_tidy %>%
  na.omit() %>%
  mutate(age = editie - jaar) %>%
  group_by(editie) %>%
  top_n(wt = age, n = 5) %>%
  top_n(wt = positie, n = 3) %>%
  ungroup() %>%
  mutate(group = "oldest")

youngest <- top2000_tidy %>%
  na.omit() %>%
  mutate(age = editie - jaar) %>%
  group_by(editie) %>%
  top_n(wt = age, n = -3) %>%
  top_n(wt = positie, n = -3) %>%
  ungroup() %>%
  mutate(group = "newest")

oldest_youngest <- bind_rows(oldest, youngest) %>%
  group_by(editie, group) %>%
  mutate(rank = rank(positie)) %>%
  ungroup() %>%
  mutate(rank = ifelse(group == "oldest", rank + 3, rank),
         colour_text = ifelse(positie >= 1250, "wit", "zwart")) %>%
  arrange(editie, rank)

oldest_youngest %>%
  ggplot(aes(y = editie, x = rank, fill = positie)) +
  geom_tile(col = "black") +
  geom_text(aes(label = artiest_titel, colour = colour_text), show.legend = FALSE) +
  scale_y_continuous(breaks = seq(1999, 2021)) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~group, nrow = 1, scale = "free_x") +
  labs(x = "", y = "") +
  top2000_theme() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

