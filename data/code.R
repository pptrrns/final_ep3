#-----
# library
library(ggplot2)
library(dplyr)
library(janitor)
library(unikn)
library(tidyverse)
library(ggforce)
library(hrbrthemes)

# data
party.position <-  read.csv("/Users/jtorrensh/Documents/Github/final_ep3/data/party-position.csv") %>%
  select(party, party, 
         party_id, 
         eu_foreign, # position of the party leadership in YEAR on EU foreign and security policy.
         spendvtax, # improving public services vs. reducing taxes.
         redistribution, # redistribution of wealth from the rich to the poor.
         immigrate_policy, # position on immigration policy.
         multiculturalism, # position on integration of immigrants and asylum seekers (multicul- turalism vs. assimilation).
         nationalism, # position towards cosmopolitanism vs. nationalism.
         anti_islam_rhetoric) # salience of anti-Islam rhetoric for the party leadership.
  
data <- party.position %>% filter(party_id == "1601" | party_id == "1602" | party_id == "1603" | party_id == "1604" |
           party_id == "1605" | party_id == "1606" | party_id == "1607" | party_id == "1608" |
           party_id == "1609" | party_id == "1610" | party_id == "1611" | party_id == "1612") %>%
  mutate(eje = (c(.5))) %>%
  mutate(name = case_when(party_id == "1602" ~ "SSDP", 
                          party_id == "1603" ~ "CP",
                          party_id == "1604" ~ "LIB",
                          party_id == "1605" ~ "MP",
                          party_id == "1606" ~ "CDP",
                          party_id == "1607" ~ "G",
                          party_id == "1610" ~ "SD",
                          party_id == "1601" ~ "LP")) %>%
  select(-c(party_id, party)) %>%
  filter(name == "SSDP" | name == "CDP" | name == "SD" | name == "LP" | name == "LIB") %>%
  relocate(name, .before = eu_foreign)

data[data == "SSDP"] <- "A"
data[data == "CDP"] <- "B"
data[data == "SD"] <- "C"
data[data == "LP"] <- "D"
data[data == "LIB"] <- "E"

source("/Users/jtorrensh/Documents/Github/final_ep3//data/tema.R")

#-----
# data -- electoral returns
n <-  c(107, 87, 73, 66, 16)
p <-  c("A", "B", "C", "D", "E")

# choose(n = 5, k = 5) -- 1
win5parties <- as.data.frame(t(combn(n, 5)))

# choose(n = 4, k = 4) -- 5
win4parties <- as.data.frame(t(combn(n, 4)))

# choose(n = 3, k = 3) -- 10
win3parties <- as.data.frame(t(combn(n, 3)))

# choose(n = 2, k = 2) -- 10
win2parties <- as.data.frame(t(combn(n, 2)))

# choose(n = 1, k = 1) -- 5
win1parties <- as.data.frame(t(combn(n, 1)))

winparties <-  bind_rows(win1parties, win2parties, win3parties, win4parties, win5parties)

winparties[is.na(winparties)] <- 0

winparties <-  winparties %>%
  clean_names() %>%
  rename(p1 = v1, p2 = v2, p3 = v3, p4 = v2, p5 = v5) %>%
  mutate(sum = rowSums(across(where(is.numeric)))) %>%
  filter(sum >= 175) 

winparties[winparties == 107] <- "A"
winparties[winparties == 87] <- "B"
winparties[winparties == 73] <- "C"
winparties[winparties == 66] <- "D"
winparties[winparties == 16] <- "E"
winparties[winparties == 0] <- NA

# write.csv(winparties, "/Users/jtorrensh/Documents/Github/final_ep3/data/winparties.csv", row.names = FALSE)

#-----
# plot dimensiones políticas
## migración
data %>%
  ggplot(aes(x = immigrate_policy,
             y = eje,
             color = name,
             size = 4)) +
  geom_point(alpha = 1) +
  ylim(0, 1) +
  xlim(-1, 11) +
  tema.postura +
  labs(caption = "Elaboración propia // Data: 2019 Chapel Hill Expert Survey (CHES)", 
       x = "Política de apertura a la migración v. política restrictiva frente a la migración", 
       y = "") +
  geom_text(aes(x = immigrate_policy, y = eje, label = name), color = "#1B2128", size = 3,
            position = position_dodge(1), vjust = -3, check_overlap = TRUE) +
  annotate("text", x = 0, y = 0.15, label = "Favors a liberal policy", size = 4) +
  annotate("text", x = 10, y = 0.15, label = "Favors a restrictive policy", size = 4)

ggsave("/Users/jtorrensh/Documents/Github/final_ep3/winsetCalc/migracion.png", width = 16, height = 10, dpi = 600)

## anti_islam
data %>%
  ggplot(aes(x = anti_islam_rhetoric,
             y = eje,
             color = name,
             size = 4)) +
  geom_point(alpha = 1) +
  ylim(0, 1) +
  xlim(-1, 11) +
  tema.postura +
  labs(caption = "Elaboración propia // Data: 2019 Chapel Hill Expert Survey (CHES)", 
       x = "Retórica anti-islámica en el partido", 
       y = "") +
  geom_text(aes(x = anti_islam_rhetoric, y = eje, label = name), color = "#1B2128", size = 3,
            position = position_dodge(1), vjust = -3, check_overlap = TRUE) +
  annotate("text", x = 0, y = 0.15, label = "Not important at all", size = 4) +
  annotate("text", x = 10, y = 0.15, label = "Extremely important", size = 4)

ggsave("/Users/jtorrensh/Documents/Github/final_ep3/winsetCalc/anti_islam.png", width = 16, height = 10, dpi = 600)

## redistibution
data %>%
  ggplot(aes(x = redistribution,
             y = eje,
             color = name,
             size = 4)) +
  geom_point(alpha = 1) +
  ylim(0, 1) +
  xlim(-1, 11) +
  tema.postura +
  labs(caption = "Elaboración propia // Data: 2019 Chapel Hill Expert Survey (CHES)", 
       x = "Redistribución de la riqueza de 'ricos hacia pobres' ", 
       y = "") +
  geom_text(aes(x = redistribution, y = eje, label = name), color = "#1B2128", size = 3,
            position = position_dodge(1), vjust = -3, check_overlap = TRUE) +
  annotate("text", x = 0, y = 0.15, label = "Strongly favors redistribution", size = 4) +
  annotate("text", x = 10, y = 0.15, label = "Strongly opposes redistribution", size = 4)

ggsave("/Users/jtorrensh/Documents/Github/final_ep3/winsetCalc/redistribution.png", width = 16, height = 10, dpi = 600)

## eu_foreign
data %>%
  ggplot(aes(x = eu_foreign,
             y = eje,
             color = name,
             size = 4)) +
  geom_point(alpha = 1) +
  ylim(0, 1) +
  xlim(-1, 11) +
  tema.postura +
  labs(caption = "Elaboración propia // Data: 2019 Chapel Hill Expert Survey (CHES)", 
       x = "Posición del partido en política europea", 
       y = "") +
  geom_text(aes(x = eu_foreign, y = eje, label = name), color = "#1B2128", size = 3,
            position = position_dodge(1), vjust = -3, check_overlap = TRUE) +
  annotate("text", x = 0, y = 0.15, label = "Strongly opposes", size = 4) +
  annotate("text", x = 10, y = 0.15, label = "Strongly favors", size = 4)


ggsave("/Users/jtorrensh/Documents/Github/final_ep3/winsetCalc/eu_foreign.png", width = 16, height = 10, dpi = 600)

## nationalism
data %>%
  ggplot(aes(x = nationalism,
             y = eje,
             color = name,
             size = 4)) +
  geom_point(alpha = 1) +
  ylim(0, 1) +
  xlim(-1, 11) +
  tema.postura +
  labs(caption = "Elaboración propia // Data: 2019 Chapel Hill Expert Survey (CHES)", 
       x = "Posición del partido hacia el cosmopolitismo frente al nacionalismo", 
       y = "") +
  geom_text(aes(x = nationalism, y = eje, label = name), color = "#1B2128", size = 3,
            position = position_dodge(1), vjust = -3, check_overlap = TRUE) +
  annotate("text", x = 1, y = 0.15, label = "Strongly promotes cosmopolitan conceptions of society", size = 4) +
  annotate("text", x = 9, y = 0.15, label = "Strongly promotes nationalist conceptions of society", size = 4)

ggsave("/Users/jtorrensh/Documents/Github/final_ep3/winsetCalc/nationalism.png", width = 16, height = 10, dpi = 600)

#-----
## plot -- análisis espacial de las dimensiones políticas
# linea.h <- c(0.5000, 2.8125, 6.7500, 6.2500, 7.1875, 6.3125, 3.0000, 4.6875) #
# linea.v <- c(1.529412, 6.294117, 1.941176, 4.500000, 8.000000, 7.823529, 1.312500, 9.764706) #

# data %>%
  # ggplot(aes(x = immigrate_policy,
    #         y = redistribution,
    #         color = name,
    #         size = 4)) +
  # geom_hline(yintercept = linea.h, linetype = "dotted", size =0.3) +
  # geom_vline(xintercept = linea.v, linetype = "dotted", size =0.3) +
  # geom_point(alpha = 1) +
  # tema +
  # labs(title = "",
       # subtitle = "",
       # caption = "Elaboración propia." , 
       # x = "Ministerio de Justicia", 
       # y = "Ministerio de Finanzas") +
  # coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10)) +
  # geom_text(aes(x = immigrate_policy, y = redistribution, 
               # label = name), color = "#1B2128", size = 3,
            # position = position_dodge(1), vjust = -2, check_overlap = TRUE)