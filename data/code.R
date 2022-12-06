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
party.position <-  read.csv("/Users/jtorrensh/Desktop/data/party-position.csv") %>%
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
  mutate(eje = (c(2))) %>%
  mutate(name = case_when(party_id == "1602" ~ "SDP", 
                          party_id == "1603" ~ "CP",
                          party_id == "1604" ~ "LIB",
                          party_id == "1605" ~ "MP",
                          party_id == "1606" ~ "CDP",
                          party_id == "1607" ~ "G",
                          party_id == "1610" ~ "SD",
                          party_id == "1601" ~ "LP")) %>%
  filter(party_id == "G") %>%
  

source("/Users/jtorrensh/Desktop/data/tema.R")

#-----
## plot -- análisis espacial de las dimensiones políticas
linea.h <- c(0.5000, 2.8125, 6.7500, 6.2500, 7.1875, 6.3125, 3.0000, 4.6875) #
linea.v <- c(1.529412, 6.294117, 1.941176, 4.500000, 8.000000, 7.823529, 1.312500, 9.764706) #

data %>%
  ggplot(aes(x = immigrate_policy,
             y = redistribution,
             color = name,
             size = 4)) +
  geom_hline(yintercept = linea.h, linetype = "dotted", size =0.3) +
  geom_vline(xintercept = linea.v, linetype = "dotted", size =0.3) +
  geom_point(alpha = 1) +
  tema +
  labs(title = "",
       subtitle = "",
       caption = "Elaboración propia." , 
       x = "Ministerio de Justicia", 
       y = "Ministerio de Finanzas") +
  coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10)) +
  geom_text(aes(x = immigrate_policy, y = redistribution, 
                label = name), color = "#1B2128", size = 3,
            position = position_dodge(1), vjust = -2, check_overlap = TRUE) +
  # geom_circle(aes(x0 = 4, y0 = 2, r = 3, color = "green"), inherit.aes = FALSE)

#
##
###
####
###
##
#

#-----
# plot dimensiones políticas
## migración
data %>%
  ggplot(aes(x = immigrate_policy,
             y = eje,
             color = name,
             size = 4)) +
  geom_point(alpha = 1) +
  ylim(0, 4) +
  xlim(0, 11) +
  tema.postura +
  labs(caption = "Elaboración propia // Data: 2019 Chapel Hill Expert Survey (CHES)", 
       x = "Migración", 
       y = "") +
  geom_text(aes(x = immigrate_policy, y = eje, label = name), color = "#1B2128", size = 3,
            position = position_dodge(1), vjust = -3, check_overlap = TRUE) +
  annotate("text", x = 1, y = 0.45, label = "Favors a liberal policy", size = 3) +
  annotate("text", x = 10, y = 0.45, label = "Favors a restrictive policy", size = 3) 

#
##
###
####
###
##
#

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

data <-  bind_rows(win1parties, win2parties, win3parties, win4parties, win5parties)

data[is.na(data)] <- 0

data <-  data %>%
  clean_names() %>%
  rename(p1 = v1, p2 = v2, p3 = v3, p4 = v2, p5 = v5) %>%
  mutate(sum = rowSums(across(where(is.numeric)))) %>%
  filter(sum >= 175) 

data[data == 107] <- "A"
data[data == 87] <- "B"
data[data == 73] <- "C"
data[data == 66] <- "D"
data[data == 16] <- "E"
data[data == 0] <- NA

# n <- c(107, 73, 68, 24, 24.1, 19, 18, 16)
# p <- c("SDP", "SD", "MP", "LP", "CP", "CDP", "G", "LIB")

# data[data == 107] <- "SDP"
# data[data == 73] <- "SD"
# data[data == 68] <- "MP"
# data[data == 24] <- "LP"
# data[data == 24.1] <- "CP"
# data[data == 19] <- "CDP"
# data[data == 18] <- "G"
# data[data == 16] <- "LIB"
# data[data == 0] <- ""

# data <- data %>% mutate(sum = floor(data$sum))

# write.csv(data, "/Users/jtorrensh/Desktop/combinations.csv", row.names = FALSE)