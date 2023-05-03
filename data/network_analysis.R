rm(list = ls())
setwd("C:/Users/thiya/OneDrive/Uchicago/thesis/data")

library(dplyr)
library(tidyverse)
library(igraph)
library(kableExtra)
library(intergraph)
library(stargazer)
library(ergm)
library(texreg)
library(stringr)


windowsFonts("Arial" = windowsFont("Arial"))

# Import csv containing information on podcasts
polpod_hosts_df <- read.csv("podcast_hosts.csv")
polpod_bias_df <- read.csv("podcast_bias.csv")

# Data preprocessing

# Combine dataframe
nodes_df <- left_join(polpod_hosts_df, polpod_bias_df, by = "podcasts")

# Franchises
franchise_df <- nodes_df %>%
  group_by(parent) %>% 
  summarise(count = n())
franchises <- franchise_df$parent[franchise_df$count > 1]

nodes_df <- nodes_df %>% 
  mutate(franchise = ifelse(parent %in% franchises, parent, 
                            ifelse(parent != "Independent", "Single Company", "Independent")),
         bias_ratio = numbiased / (numbiased + numunbiased),
         bias_ratio = ifelse(is.nan(bias_ratio), mean(bias_ratio, na.rm = TRUE), bias_ratio),
         bias_ratio = ifelse(bias_ratio > 0, bias_ratio, mean(bias_ratio)),
         id_code = ifelse(main_ideology == "reactionary", 1, 
                          ifelse(main_ideology == "conservative", 2, 
                                 ifelse(main_ideology == "moderate", 3, 
                                        ifelse(main_ideology == "liberal", 4, 
                                               ifelse(main_ideology == "radical", 5, 6))))))


# Import csv containing collaborations between podcasts
polpod_collabs <- read.csv("podcast_collabs.csv")

# Get counts of repeated collaborations and rename columns
collab_df <- polpod_collabs %>% 
  rename("from" = "From",
         "to" = "To") %>% 
  group_by(from, to) %>% 
  summarise(weight = n())

# Subset vertex graph for visualization
main_exclude <- unique(c(polpod_collabs$From, polpod_collabs$To, extra_exclude))
extra_exclude <- c("POLITICO Energy", "Washington Today") 

nodes_subset_df <- nodes_df %>% 
  filter(podcasts %in% main_exclude[! main_exclude %in% extra_exclude])
collab_subset_df <- collab_df %>% 
  filter(from %in% main_exclude[! main_exclude %in% extra_exclude],
         to %in% main_exclude[! main_exclude %in% extra_exclude])

# create social network graph
g <- graph_from_data_frame(collab_subset_df,
                           directed = TRUE,
                           vertices = nodes_subset_df$podcasts) # Directed edges

# Set node attributes
g <- g %>% 
  set_vertex_attr("ideology",
                  index = V(g),
                  value = nodes_subset_df$id_code) %>% 
  set_vertex_attr("franchise",
                  index = V(g),
                  value = nodes_subset_df$franchise) %>% 
  set_vertex_attr("bias_count",
                  index = V(g),
                  value = nodes_subset_df$numbiased) %>%
  set_vertex_attr("bias_ratio",
                  index = V(g),
                  value = nodes_subset_df$bias_ratio) 
  
  
# Set node color based on ideology
colors <- c("darkred", "tomato", "gray80", "skyblue", "darkblue", "gray80")
V(g)$color <- colors[V(g)$ideology]  

# Scale node size based on the number of collaborations
V(g)$size <- log(igraph::degree(g, mode = "all"))

# Set edge attributes
# Set arrow size
E(g)$arrow.size <- 0.01
# Scale edge width according to weight
E(g)$width <- log10(E(g)$weight)

# Assign degree to each node
deg <- igraph::degree(g, mode = "all")

# Visualize graph
l <- layout_with_kk(g)
windows(height = 10, width = 12)
par(mar=c(0,0,0,0)+1)
plot(g,
     vertex.label = ifelse(deg[V(g)$name] > 100, V(g)$name, NA), # remove labels if low degree
     vertex.label.family = "Arial",
     vertex.label.color = "black",
     vertex.label.cex = 0.5,
     edge.color = "gray90",
     main = "Collaborations Among The 250 Most Popular Political Podcasts",
     margin = 0)

legend(
  "bottomright",
  legend = c("Reactionary", "Conservative", "Moderate", "Liberal", "Radical"),
  pt.bg  = c("darkred", "tomato", "gray50", "skyblue", "darkblue"),
  pt.cex = c(2, 2, 2),
  pch    = 21,
  cex    = 0.8,
  bty    = "o",
  title  = "Ideological Affiliation"
)

legend(
  "bottomleft",
  legend = c("< 10", "10 - 20", "20 - 40", "40 - 60", "> 60"),
  pt.bg  = "gray50",
  pt.cex = c(0.4, 0.8, 1.2, 1.6, 1.8),
  pch    = 21,
  cex    = 0.8,
  bty    = "o",
  title  = "Number of Collaborations"
)

nodes_df %>% 
  group_by(main_ideology) %>% 
  summarise(count = n())


# network simulations


# recreate full graph
# create social network graph
g_full <- graph_from_data_frame(collab_df,
                           directed = TRUE,
                           vertices = nodes_df$podcasts) # Directed edges

# franchise
nodes_df <- nodes_df %>% 
  mutate(franchise_code = ifelse(franchise == "Independent", 3,
                                 ifelse(franchise == "Single Company", 2, 1)))

# Set node attributes
g_full <- g_full %>% 
  set_vertex_attr("ideology",
                  index = V(g_full),
                  value = nodes_df$id_code) %>% 
  set_vertex_attr("franchise",
                  index = V(g_full),
                  value = nodes_df$franchise_code) %>% 
  set_vertex_attr("bias_count",
                  index = V(g_full),
                  value = nodes_df$numbiased) %>%
  set_vertex_attr("bias_ratio",
                  index = V(g_full),
                  value = nodes_df$bias_ratio) 


# ergm graphs


# build model
network <- intergraph::asNetwork(g_full)

mod <- ergm(network ~ edges +
              nodematch("ideology", diff = TRUE) +
              nodematch("franchise", diff = TRUE) +
              nodecov("bias_count") +
              nodecov("bias_ratio") +
              mutual,
            control = control.ergm(MCMLE.maxit= 40))
summary(mod)
plot(gof(mod))

library(texreg)

htmlreg(list(mod),
        file="ergm_model.html",
        custom.coef.names = c("Number of Edges",
                              "Reactionary", 
                              "Conservative",
                              "Moderate",
                              "Liberal",
                              "Radical",
                              "NULL",
                              "Indepdendent",
                              "Single Franchise",
                              "Major Conglomerate",
                              "Number of Biased Statements",
                              "Ratio of Biased to Unbiased Statements",
                              "Reciprocal"))

# Degree distribution plots

# Create stacked dataframe
ideologies <- c("Reactionary", "Conservative", "Moderate", "Liberal", "Radical", "NULL")
colors <- c("tomato", "skyblue", "gray50", "black", "darkblue", "darkred")

for (i in 1:6){
  indeg_ideology <- igraph::degree(g_full,
                                   v = V(g_full)$ideology == i,
                                   mode = "in")
  indeg_df <- as.data.frame(table(indeg_ideology)) %>% 
    mutate(ideology = ideologies[i])
  if (i == 1){
    master_indeg_df <- indeg_df
  } else {
    master_indeg_df <- rbind(master_indeg_df, indeg_df)    
  }
}

master_indeg_df$indeg_ideology <- factor(master_indeg_df$indeg_ideology, levels = c("0", "1", "2", "3", "4", "5", "6", "7", 
                                                                    "8", "9" , "10", "11", "12", "13", "14",
                                                                    "15", "16", "17", "19", "24", "26", "27", "28"))
windows(height = 12, width = 15)
ggplot(master_indeg_df,                                      
       aes(x = indeg_ideology,
           y = Freq,
           fill = ideology)) +
  geom_bar(stat = "identity",
           position = "stack") +
  xlab("Number of Guests Hosted") +
  ylab("Number of Podcasts") +
  labs(title = "In Degree Distribution by Ideology") +
  scale_fill_manual(name = "Ideology",
                    values = colors) +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5))

mean(as.integer(master_df$indeg_ideology))

for (i in 1:6){
  indeg_ideology <- igraph::degree(g_full,
                                   v = V(g_full)$ideology == i,
                                   mode = "out")
  indeg_df <- as.data.frame(table(indeg_ideology)) %>% 
    mutate(ideology = ideologies[i])
  if (i == 1){
    master_outdeg_df <- indeg_df
  } else {
    master_outdeg_df <- rbind(master_outdeg_df, indeg_df)    
  }
}



master_outdeg_df$indeg_ideology <- factor(master_outdeg_df$indeg_ideology, levels = c("0", "1", "2", "3", "4", "5", "6", "7", 
                                                                        "8", "9" , "10", "11", "12", "13","14","16", "17", 
                                                                        "18", "21", "22", "25", "27", "31", "45", "63", "67"))

windows(height = 10, width = 12)
ggplot(master_outdeg_df,                                      
       aes(x = indeg_ideology,
           y = Freq,
           fill = ideology)) +
  geom_bar(stat = "identity",
           position = "stack") +
  xlab("Number of Guest Appearances") +
  ylab("Number of Podcasts") +
  labs(title = "Out Degree Distribution by Ideology") +
  scale_fill_manual(name = "Ideology",
                    values = colors) +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5))

mean(as.integer(master_df$indeg_ideology))
median(as.integer(master_df$indeg_ideology))

# Betweenness centrality

btwn <- igraph::betweenness(
  g_full,
  v = V(g_full),
  directed = TRUE,
  weights = E(g_full)$weight,
  nobigint = TRUE,
  normalized = TRUE,
  cutoff = -1
)

btwn_df <- as.data.frame(btwn) %>% 
  rownames_to_column() %>% 
  rename(podcasts = rowname) %>% 
  left_join(nodes_df, by = "podcasts") %>% 
  mutate(ideology = as.factor(main_ideology)) %>% 
  filter(btwn > 0.0001)

windows(height = 10, width = 10)

ideologies <- c("Conservative", "Liberal", "NULL", "Radical", "Moderate", "Reactionary")
colors <- c("tomato", "skyblue", "gray50", "darkblue", "darkred", "black")

ggplot(btwn_df,
       aes(x = reorder(podcasts, -btwn),
           y = btwn,
           fill = main_ideology)) +
  geom_bar(stat = "identity")  +
  xlab("Podcast Names") +
  ylab("Normalized Betweenness Centrality") +
  labs(title = "Podcast Betweenness Centrality Distribution by Ideology") +
  scale_fill_manual(name = "Ideology",
                    values = colors,
                    labels = c("Conservative", "Liberal", "Moderate", "Radical", "Reactionary", "NULL")) +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


# Collaboration distribution

ideology_df <- nodes_df %>% 
  select(podcasts, main_ideology)

collab_df <- collab_df %>% 
  mutate(from = as.character(from)) %>% 
  mutate(to = as.character(to))

collab_ideology_df <- collab_df %>% 
  merge(x = collab_df,
        y = ideology_df,
        by.x = "to",
        by.y = "podcasts") %>% 
  rename(ideology_host = main_ideology)

collab_ideology_df <- collab_ideology_df %>% 
  merge(x = collab_ideology_df,
        y = ideology_df,
        by.x = "from",
        by.y = "podcasts") %>% 
  rename(ideology_guest = main_ideology)

collab_ideology_df <- collab_ideology_df %>% 
  filter(! ideology_host %in% "NULL",
         ! ideology_guest %in% "NULL") 

collab_ideology_df$crosspart <- str_c(collab_ideology_df$ideology_guest, " - ", collab_ideology_df$ideology_host)

collab_ideology_sum_df <- collab_ideology_df %>% 
  group_by(crosspart) %>% 
  summarise(count = n(),
            weight = sum(weight))

#write.csv(collab_ideology_sum_df, "crosspart2.csv")

collab_ideology_sum_df <- read.csv("crosspart2.csv")

collab_ideology_sum_df <- collab_ideology_sum_df %>% 
  group_by(crosspart) %>% 
  summarise(count = sum(count),
            weight = sum(weight),
            homophilic = mean(homophilic),
            cross = mean(cross))

windows(height = 10, width = 12)
pair_bar_uniq <- ggplot(collab_ideology_sum_df,
                        aes(x = reorder(crosspart, -count), y = count, fill = cross)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Unique Cross Partisan Collaborations",
       subtitle = "Color Scaled According to Magnitude of Ideological Difference") +
  xlab("Type of Collaboration") + 
  ylab("Number of Unique Collaborations") +
  scale_fill_gradient(name = "Cross Partisan Rating \n (4 = High, 0 = Low)",
                      low = "darkred", high = "grey")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
pair_bar_uniq

pair_bar <- ggplot(collab_ideology_sum_df,
                        aes(x = reorder(crosspart, -weight), y = weight, fill = cross)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of All Cross Partisan Collaborations",
       subtitle = "Color Scaled According to Magnitude of Ideological Difference") +
  xlab("Type of Collaboration") + 
  ylab("Total Number of Collaborations") +
  scale_fill_gradient(name = "Cross Partisan Rating \n (4 = High, 0 = Low)",
                      low = "darkred", high = "grey")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme_bw(base_size = 18) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18))
pair_bar

# Power Law

for (ideology in c("Reactionary", "Conservative", "Moderate", "Liberal", "Radical")){
  df = as.data.frame(power.law.fit(master_indeg_df$Freq[master_indeg_df$ideology == ideology]))
  if (ideology == "Reactionary"){
    master_plaw_df = df
  } else {
    master_plaw_df = rbind(master_plaw_df, df)
  }
}

par(mar=c(0,0,0,0)+1)
master_plaw_df %>% 
  rename(Fit = continuous) %>% 
  mutate(Fit = c("Reactionary", "Conservative", "Moderate", "Liberal", "Radical")) %>% 
  kbl(caption = "Power Law Fit on In Degree Distribution") %>%
  kable_classic(full_width = F, html_font = "Garamond") %>%
  save_kable(file = "C:/Users/thiya/OneDrive/Uchicago/thesis/plots/indegree_plaw.png",
             zoom = 1.5)


for (ideology in c("Reactionary", "Conservative", "Moderate", "Liberal", "Radical")){
  df = as.data.frame(power.law.fit(master_outdeg_df$Freq[master_outdeg_df$ideology == ideology]))
  if (ideology == "Reactionary"){
    master_plaw_df = df
  } else {
    master_plaw_df = rbind(master_plaw_df, df)
  }
}

par(mar=c(0,0,0,0)+1)
master_plaw_df %>% 
  rename(Fit = continuous) %>% 
  mutate(Fit = c("Reactionary", "Conservative", "Moderate", "Liberal", "Radical")) %>% 
  kbl(caption = "Power Law Fit on Out Degree Distribution") %>%
  kable_classic(full_width = F, html_font = "Garamond") %>%
  save_kable(file = "C:/Users/thiya/OneDrive/Uchicago/thesis/plots/outdegree_plaw.png",
             zoom = 1.5)


as.data.frame(power.law.fit(btwn_df$btwn)) %>% 
  rename(Fit = continuous) %>% 
  mutate(Fit = c("Betweenness")) %>% 
  kbl(caption = "Power Law Fit on Betweenness Centrality Distribution") %>%
  kable_classic(full_width = F, html_font = "Garamond") %>%
  save_kable(file = "C:/Users/thiya/OneDrive/Uchicago/thesis/plots/plaw_btwn.png",
             zoom = 1.5)

as.data.frame(power.law.fit(collab_ideology_sum_df$count)) %>% 
  rename(Fit = continuous) %>% 
  mutate(Fit = c("Betweenness")) %>% 
  kbl(caption = "Power Law Fit on Unique Cross-Partisan Collaborations") %>%
  kable_classic(full_width = F, html_font = "Garamond") %>%
  save_kable(file = "C:/Users/thiya/OneDrive/Uchicago/thesis/plots/plaw_corsspart_uniq.png",
             zoom = 1.5)

as.data.frame(power.law.fit(collab_ideology_sum_df$weight)) %>% 
  rename(Fit = continuous) %>% 
  mutate(Fit = c("Betweenness")) %>% 
  kbl(caption = "Power Law Fit on All Cross-Partisan Collaborations") %>%
  kable_classic(full_width = F, html_font = "Garamond") %>%
  save_kable(file = "C:/Users/thiya/OneDrive/Uchicago/thesis/plots/plaw_crosspart_total.png",
             zoom = 1.5)

# ERGM models

# load in host attributes
polpod_hosttype_df <- read.csv("podcast_hostattribs.csv") %>% 
  rename(podcasts = Podcast,
         alternative_media_personality = other,
         political_personality = politician,
         professional_personality = political_professional,
         academic_personality = academic) %>% 
  mutate(legacy_media_personality = tv_personality + print_personality + radio_personality) %>% 
  select(2, 4, 8:12) %>% 
  mutate(sum = rowSums(across(where(is.numeric)))) %>% 
  mutate(across(2:7, ~ . / sum)) %>% 
  mutate_at(2:7, ~replace(., is.nan(.), 0)) %>% 
  select(1:7)



collab_ergm_df <- collab_ideology_df %>% 
  select(from, to, weight)

nodes_ergm_df <- nodes_df %>% 
  left_join(polpod_hosttype_df, by = "podcasts") %>% 
  filter(! main_ideology %in% "NULL") %>% 
  mutate(bias_std = (numbiased - mean(numbiased)) / sd(numbiased) ) %>% 
  mutate(franchise_code = ifelse(franchise == "Independent", 3,
                                 ifelse(franchise == "Single Company", 2, 1))) %>% 
  mutate_at(11:16, ~ifelse(. > 0, 1, 0))


# create social network graph
g_ergm <- graph_from_data_frame(collab_ergm_df,
                                directed = TRUE,
                                vertices = nodes_ergm_df$podcasts) # Directed edges

# Set node attributes
g_ergm <- g_ergm %>% 
  set_vertex_attr("ideology",
                  index = V(g_ergm),
                  value = nodes_ergm_df$id_code) %>% 
  set_vertex_attr("franchise",
                  index = V(g_ergm),
                  value = nodes_ergm_df$franchise_code) %>% 
  set_vertex_attr("bias_count",
                  index = V(g_ergm),
                  value = nodes_ergm_df$bias_std) %>%
  set_vertex_attr("bias_ratio",
                  index = V(g_ergm),
                  value = nodes_ergm_df$bias_ratio)  %>% 
  set_vertex_attr("political_personality",
                  index = V(g_ergm),
                  value = nodes_ergm_df$political_personality)  %>%  
  set_vertex_attr("academic_personality",
                  index = V(g_ergm),
                  value = nodes_ergm_df$academic_personality)  %>%
  set_vertex_attr("professional_personality",
                  index = V(g_ergm),
                  value = nodes_ergm_df$professional_personality)  %>%  
  set_vertex_attr("alt_media_personality",
                  index = V(g_ergm),
                  value = nodes_ergm_df$alternative_media_personality)  %>%  
  set_vertex_attr("religious_personality",
                  index = V(g_ergm),
                  value = nodes_ergm_df$religious_personality)  %>% 
  set_vertex_attr("legacy_media_personality",
                  index = V(g_ergm),
                  value = nodes_ergm_df$legacy_media_personality)  
# compute ergm


# Model 1

network <- intergraph::asNetwork(g_ergm)

mod <- ergm(network ~ edges +
              nodematch("ideology", diff = T) +
              nodeofactor("ideology") +
              nodeifactor("ideology") +
              nodecov("bias_ratio") +
              nodecov("political_personality") +
              nodecov("legacy_media_personality") +
              nodecov("alt_media_personality") +
              nodecov("religious_personality") +
              nodecov("academic_personality") +
              nodematch("franchise", diff = T) +
              odegree(0:5) +
              mutual,
            control = control.ergm(MCMLE.maxit= 40))

summary(mod)

texreg(list(mod),
       file = "ergm_homophily_fit.html",
        custom.coef.names = c("Number of Edges",
                              "Reactionary",
                              "Conservative",
                              "Moderate",
                              "Liberal",
                              "Radical",
                              "Difference in In Degree - Conservative",
                              "Difference in In Degree - Moderate",
                              "Difference in In Degree - Liberal",
                              "Difference in In Degree - Radical",
                              "Difference in Out Degree - Conservative",
                              "Difference in Out Degree - Moderate",
                              "Difference in Out Degree - Liberal",
                              "Difference in Out Degree - Radical",
                              "Ratio of Biased to Unbiased Statements",
                              "Host is a political figure",
                              "Host is a legacy media figure",
                              "Host is a alternative media figure",
                              "Host is a relgious leader",
                              "Host is an academic",
                              "Independent Podcast",
                              "Podcast owned by a Single Company",
                              "Podcast part of a Distribution Network",
                              "0 Guest Appearances",
                              "1 Guest Appearances",
                              "2 Guest Appearances",
                              "3 Guest Appearances",
                              "4 Guest Appearances",
                              "5 Guest Appearances",
                              "Presence of Mutual Connection"),
        bold = 0.5)


mod_2 <- ergm(network ~ edges +
              nodemix("ideology", base = NULL) +
              nodecov("bias_ratio") +
              mutual,
            control = control.ergm(MCMLE.maxit= 40))

summary(mod_2)

plot(gof(mod))

htmlreg(list(mod_2),
       file = "ergm_crosspart_fit.doc",
       custom.coef.names = c("Number of Edges",
                             "Conservative -> Reactionary",
                             "Moderate -> Reactionary",
                             "Liberal -> Reactionary",
                             "Radical -> Reactionary",
                             "Reactionary -> Conservative",
                             "Conservative -> Conservative",
                             "Moderate -> Conservative",
                             "Liberal -> Conservative",
                             "Radical -> Conservative",
                             "Reactionary -> Moderate",
                             "Conservative -> Moderate",
                             "Moderate -> Moderate",
                             "Liberal -> Moderate",
                             "Radical -> Moderate",
                             "Reactionary -> Liberal",
                             "Conservative -> Liberal",
                             "Moderate -> Liberal",
                             "Liberal -> Liberal",
                             "Radical -> Liberal",
                             "Reactionary -> Radical",
                             "Conservative -> Radical",
                             "Moderate -> Radical",
                             "Liberal -> Radical",
                             "Radical -> Radical",   
                             "Ratio of Biased to Unbiased Statements",
                             "Presence of Reciprocal Collaboration"),
       bold = 0.05)


