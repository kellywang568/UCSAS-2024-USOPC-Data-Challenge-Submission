# set the working directory

library(tools)
library(dplyr)
library(lubridate)

source("main.r")
source("clean_data.r")



# # Initial pool from men and women for top 12 countries
mens <- score_variables %>%
  filter(Gender == 'm', Country %in% top_countries_m) %>%
  group_by(Country, Apparatus) %>%
  arrange(desc(pred)) %>%
  slice_head(n = 7)

womens <- score_variables %>%
  filter(Gender == 'w', Country %in% top_countries_w) %>%
  group_by(Country, Apparatus) %>%
  arrange(desc(pred)) %>%
  slice_head(n = 7)

contenders <- function(data, gender, country){
  if (gender == 'w'){
    people <- data %>%
      filter(Country == country, Name %in% womens$Name) %>%
      pivot_wider(names_from = Apparatus, values_from = pred) %>%
      replace(is.na(.), 0)
    return(people)
  }
  if (gender == 'm'){
    people <- data %>%
      filter(Country == country, Name %in% mens$Name) %>%
      pivot_wider(names_from = Apparatus, values_from = pred) %>%
      replace(is.na(.), 0)
    return(people)
  }
}
# # Finding contenders for combos
# #top12 women

# # women gymnasts in top12 countries: example
contenders_w_CHN <- contenders(data = score_variables, gender = 'w', country = "DOM")

# # men gymnasts in top12 countries: example
contenders_w_CHN <- contenders(data = score_variables, gender = 'm', country = "CHN")

# # five gymnasts that are participants for the 12 selected countries
top5_w_score <- lapply(top_countries_w$Country, top5_score, gender = 'w')
top5_w_score
top5_m_score <- lapply(top_countries_m$Country, top5_score, gender = 'm')
top5_m_score

top5_w_prob <- lapply(top_countries_w$Country, top5_prob, gender = 'w')
top5_w_prob
top5_m_prob <- lapply(top_countries_m$Country, top5_prob, gender = 'm')
top5_m_prob

# # Finding the 36 individuals from countries that did not qualify
vaults = c("VT", "VT1", "VT2")
individual_w <- top_individual(data = score_variables, gender = 'w',
                      country_list = top_countries_w, bucket = 4,
                      vaults = vaults)

individual_m <- top_individual(data = score_variables, gender = 'm',
                               country_list = top_countries_m, bucket = 6,
                               vaults = vaults)

individual_w %>% select(Name, Apparatus, Gender, Country, pred)
individual_m %>% select(Name, Apparatus, Gender, Country, pred)


# # All unique names check (should be 36)
length(unique(individual_w$Name))
length(unique(individual_m$Name))

# # None from top 12 countries check (should be 0)
sum(individual_w$Country %in% top_countries_w$Country)
sum(individual_m$Country %in% top_countries_m$Country)


# # Selection process of the 5-person team.
head(score_variables)
head(score_aa_variables)
candidates_w <- candidate_selection(score_variables, score_aa_variables, "w", 2, vaults)
candidates_w

candidates_m <- candidate_selection(score_variables, score_aa_variables, "m", 2, vaults)
candidates_m


# ########## DATA VISUALIZATION ##########

# ## Top Counties Competitors:

# # Men’s qual: china, Japan, Great Britain
# # Women: US, Great Britain, Canada

### VISUALIZATIONS FOR PREDICTED SCORES ###

######## Women


# RECALL top5_w_score = predicted scores of 12 countries competing 

# Combine data frames and rename the "pred" column to "prob"
combined_data_score_w <- bind_rows(top5_w_score)


# Identify the rows with the highest "prob" for each "Country"
top_w_pred_12Countries <- combined_data_score_w %>%
  group_by(Country) %>%
  top_n(1, pred)

# Create a ggplot to display the highest player predicted scores
ggplot(top_w_pred_12Countries, aes(x = reorder(Name, -pred), y = pred, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Female Competitors with highest predicted score") +
  ylab("Predicted Scores") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")


### Identifying average scores in each country 

# Calculate the average scores (pred) for each country
avg_w_pred_12Countries <- combined_data_score_w %>%
  group_by(Country) %>%
  summarise(AvgPred = mean(pred))

# Create a ggplot to display the average country predicted scores
ggplot(avg_w_pred_12Countries, aes(x = reorder(Country, -AvgPred), y = AvgPred, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Country") +
  ylab("Average Scores of top 5 Female players") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")


# ## US women (original team of 5)
USA_candidates_w_plot <- candidates_w %>% group_by(Name) %>%
  summarise(pred = mean(pred)) %>% 
  top_n(10, wt = pred)

ggplot(USA_candidates_w_plot, aes(x = reorder(Name, -pred), 
                                  y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='skyblue',fill='steelblue') +
  xlab("Female contestants") + ylab("Predicted Score for USA women's team")


## Recall:


# ## Prediction dataframe for women all-around
USA_gymnast_predicts_w_aa <- gymnast_predicts_w_aa
USA_gymnast_predicts_w_aa_plot <- USA_gymnast_predicts_w_aa %>% group_by(Name) %>%
  select("Name", "pred") %>% 
  mutate(pred = as.numeric(pred))

ggplot(USA_gymnast_predicts_w_aa_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='blue',fill='skyblue') +
  xlab("Female contestants") + 
  ylab("Predicted Scores for USA women's all arounds")


#### Comments:
## As we can see in both gymnast_probs_w_aa and gymnast_predicts_w_aa, Jade Carey
## is included in the top selections as the AA gymnast. Additionally, she is also 
## included within the team of 5 for the USA olympics team as well. 



## Recall:
# # Men’s qual: china, Japan, Great Britain
# # Women: US, Great Britain, Canada


###### Other countries:

ENG_top5_women <- top5(score_variables, "ENG", "w")
CAN_top5_women <- top5(score_variables, "CAN", "w")

ENG_candidates_w_plot <- ENG_top5_women %>% group_by(Name) %>%
  summarise(pred = mean(Score))
ggplot(ENG_candidates_w_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='darkgreen',fill='chartreuse3') +
  xlab("Female contestants") + ylab("ENG Women Total Score Average")

CAN_candidates_w_plot <-  CAN_top5_women %>% group_by(Name) %>%
  summarise(pred = mean(Score))  %>% 
  top_n(5, wt = pred)

ggplot(CAN_candidates_w_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='burlywood',fill='red4') +
  xlab("Female contestants") + ylab("CAN Women Total Score Average")

# # Combine all together
USA_candidates_w_plot$country <- "USA"
#to ensure all have same names
colnames(USA_candidates_w_plot)[2] <- "pred"
ENG_candidates_w_plot$country <- "ENG"
CAN_candidates_w_plot$country <- "CAN"

all_women_simulation_score <- rbind(USA_candidates_w_plot, 
                                    ENG_candidates_w_plot, CAN_candidates_w_plot)

country_colors <- 
  c("CAN" = "red4", 
    "USA" = "steelblue", "ENG" = "chartreuse3")

ggplot(all_women_simulation_score, aes(x = reorder(Name, -pred), y = pred, fill = country)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors) +  # Set the custom color palette
  xlab("Female contestants") +
  ylab("Predicted scores of Gymnastic Women's team")

############################################## cont frm there


# ## Comments
# # It's very clear from the graph that the US has a higher variance in many
# # of the players but they overall score higher than the other countries.
# # In order from strongest to weakest of the top countries, we have: USA, ENG,
# # and CAN. I'm wondering if Zoe Miller may be an outlier since having over 60%
# # chance of winning and being higher than everyone else is very unlikely.

######## Men

# RECALL top5_m_score = predicted scores of 12 countries competing 

# Combine data frames and rename the "pred" column to "prob"
combined_data_score_m <- bind_rows(top5_m_score)


# Identify the rows with the highest "prob" for each "Country"
top_m_pred_12Countries <- combined_data_score_m %>%
  group_by(Country) %>%
  top_n(1, pred)

# Create a ggplot to display the highest player predicted scores
ggplot(top_m_pred_12Countries, aes(x = reorder(Name, -pred), y = pred, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Male Competitors with highest predicted score") +
  ylab("Predicted Scores") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")


### Identifying average scores in each country 

# Calculate the average scores (pred) for each country
avg_m_pred_12Countries <- combined_data_score_m %>%
  group_by(Country) %>%
  summarise(AvgPred = mean(pred))

# Create a ggplot to display the average country predicted scores
ggplot(avg_m_pred_12Countries, aes(x = reorder(Country, -AvgPred), y = AvgPred, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Country") +
  ylab("Average Scores of top 5 male players") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")







# ## US men



USA_candidates_m_plot <- candidates_m %>% group_by(Name) %>%
  summarise(pred = mean(pred)) %>% 
  top_n(5, wt = pred)

ggplot(USA_candidates_m_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='skyblue',fill='steelblue') +
  xlab("Male contestants") + ylab("Predicted Score for USA men's team")



## Recall:
# ## Probability dataframe for men all-around
gymnast_probs_m_aa
USA_gymnast_probs_m_aa_plot <- gymnast_probs_m_aa %>% group_by(Name) %>%
  select("Name", "pred") %>% rename("prob" = "pred") %>%
  mutate(prob = as.numeric(prob))

ggplot(USA_gymnast_probs_m_aa_plot, aes(reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='blue',fill='skyblue') +
  xlab("Male contestants") + 
  ylab("Probabilities for USA men's all arounds to win medals")

# ## Prediction dataframe for men all-around

USA_gymnast_probs_m_aa <- gymnast_predicts_m_aa
USA_gymnast_predicts_m_aa_plot <- gymnast_predicts_m_aa %>% group_by(Name) %>%
  select("Name", "pred") %>% 
  mutate(pred = as.numeric(pred))

ggplot(USA_gymnast_predicts_m_aa_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='blue',fill='skyblue') +
  xlab("Male contestants") + 
  ylab("Predicted Scores for USA men's all arounds")


###### Other countries:


CHN_top5_men <- top5(score_variables, "CHN", "m")
JPN_top5_men <- top5(score_variables, "JPN", "m")
ENG_top5_men <- top5(score_variables, "ENG", "m")

# # Plotting the probabilities of top players in competitor country
ENG_candidates_m_plot <- ENG_top5_men %>% group_by(Name) %>%
  summarise(pred = mean(Score)) %>% 
  top_n(5, wt = pred)

ggplot(ENG_candidates_m_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='darkgreen',fill='chartreuse3') +
  xlab("Male contestants") + ylab("ENG Men's Total Score Average")

JPN_candidates_m_plot <- JPN_top5_men %>% group_by(Name) %>%
  summarise(pred = mean(Score)) %>% 
  top_n(5, wt = pred)

ggplot(JPN_candidates_m_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='darkred',fill='darkred') +
  xlab("Male contestants") + ylab("JPN Men's Total Score Average")

CHN_candidates_m_plot <- CHN_top5_men %>% group_by(Name) %>%
  summarise(pred = mean(Score)) %>% 
  top_n(5, wt = pred)

ggplot(CHN_candidates_m_plot, aes(x = reorder(Name, -pred), y = pred)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='red2',fill='red2') +
  xlab("Male contestants") + ylab("CHN Men's Total Score Average")

# # Combine all together
USA_candidates_m_plot$country <- "USA"
ENG_candidates_m_plot$country <- "ENG"
JPN_candidates_m_plot$country <- "JPN"
CHN_candidates_m_plot$country <- "CHN"

all_men_simulation <- rbind(USA_candidates_m_plot, ENG_candidates_m_plot,
                            JPN_candidates_m_plot, CHN_candidates_m_plot)

country_colors <- 
  c("JPN" = "darkred", "CHN" = "red", 
    "USA" = "steelblue", "ENG" = "chartreuse3")

ggplot(all_men_simulation, aes(x = reorder(Name, -pred), y = pred, fill = country)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors) +  # Set the custom color palette
  xlab("Male contestants") +
  ylab("Predicted scores of Gymnastic Men's team")


# ## Comments
# # Interestingly enough, in this graph, the US does not trump all countries-
# # although we are working with more countries and players in this simulation
# # set. We matched the top 5 players from the other countries alongside a
# # potential team of US male players. In this case, China trumps above all with
# # Jingyuan Zou having a probability of above 75% in winning a medal. Unlike the
# # women's team, China's team is less variable and has a smoother descent in the
# # bars. The US is also one of the weakest teams in comparison with the top
# # countries here.



### VISUALIZATIONS FOR PREDICTED PROBABILITIES FOR MEDALING ###

# Recall 
# # Men’s qual: china, Japan, Great Britain
# # Women: US, Great Britain, Canada

######## Women
# top5_w_prob = probabilities of 12 countries competing 


### Identifying top player probabilities in each country 
combined_data <- lapply(top5_w_prob, function(df) {
  df <- df %>% rename(prob = pred)
  return(df)
}) %>% bind_rows()

# Identify the rows with the highest "prob" for each "Country"
top_w_prob_12Countries <- combined_data %>%
  group_by(Country) %>%
  top_n(1, prob)

# Display results
ggplot(top_w_prob_12Countries, aes(x = reorder(Name, -prob), y = prob, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Female Competitors with highest probability ") +
  ylab("Probability of winning medal") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")

### Identifying average probabilities in each country 

# Calculate the average probability (prob) for each country
avg_w_prob_12Countries <- combined_data %>%
  group_by(Country) %>%
  summarise(AvgProb = mean(prob))

# Create a ggplot to display the average probabilities
ggplot(avg_w_prob_12Countries, aes(x = reorder(Country, -AvgProb), y = AvgProb, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Country") +
  ylab("Average probability of Female medaling of top 5 players") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")


## Recall:
# ## Probability dataframe for women all-around
gymnast_probs_w_aa
USA_gymnast_probs_w_aa_plot <- gymnast_probs_w_aa %>% group_by(Name) %>%
  select("Name", "pred") %>% rename("prob" = "pred") %>% #rename pred to prob
  mutate(prob = as.numeric(prob))

ggplot(USA_gymnast_probs_w_aa_plot, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='blue',fill='skyblue') +
  xlab("Female contestants") + 
  ylab("Probabilities for USA women's all arounds to win medals")



#### Competing countries that already qualified (WOMEN) - prob of medaling
top5_w_USA_prob = top5_prob("USA", "w")
top5_w_CAN_prob = top5_prob("CAN", "w")
top5_w_ENG_prob = top5_prob("ENG", "w")

USA_candidates_w_plot_prob <- top5_w_USA_prob %>% group_by(Name) %>%
  summarise(prob = mean(pred)) %>% 
  top_n(5, wt = prob)

ggplot(USA_candidates_w_plot_prob, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='blue',fill='steelblue') +
  xlab("Female contestants") + ylab("USA Top 5 Probability of Medaling")




CAN_candidates_w_plot_prob <- top5_w_CAN_prob %>% group_by(Name) %>%
  summarise(prob = mean(pred)) %>% 
  top_n(5, wt = prob)

ggplot(CAN_candidates_w_plot_prob, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='burlywood',fill='red4') +
  xlab("Female contestants") + ylab("CAN Top 5 Probability of Medaling")


ENG_candidates_w_plot_prob <- top5_w_ENG_prob %>% group_by(Name) %>%
  summarise(prob = mean(pred)) %>% 
  top_n(5, wt = prob)

ggplot(ENG_candidates_w_plot_prob, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='darkgreen',fill='chartreuse3') +
  xlab("Female contestants") + ylab("ENG Top 5 Probability of Medaling")


# # Combine all together
USA_candidates_w_plot_prob$country <- "USA"
ENG_candidates_w_plot_prob$country <- "ENG"
CAN_candidates_w_plot_prob$country <- "CAN"

all_women_simulation_prob <- rbind(USA_candidates_w_plot_prob, 
                                   ENG_candidates_w_plot_prob,
                                   CAN_candidates_w_plot_prob)

country_colors <- 
  c("CAN" = "red4",  "USA" = "steelblue", "ENG" = "chartreuse3")

ggplot(all_women_simulation_prob, aes(x = reorder(Name, -prob), y = prob, fill = country)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors) +  # Set the custom color palette
  xlab("Female contestants") +
  ylab("Probability of medaling")


#### Competing countries that already qualified (MEN) - prob of medaling

#top5_m_prob = probabilities of 12 countries competing 


### Identifying top player probabilities in each country 
combined_data <- lapply(top5_m_prob, function(df) {
  df <- df %>% rename(prob = pred)
  return(df)
}) %>% bind_rows()

# Identify the rows with the highest "prob" for each "Country"
top_m_prob_12Countries <- combined_data %>%
  group_by(Country) %>%
  top_n(1, prob)



# Display results
ggplot(top_m_prob_12Countries, aes(x = reorder(Name, -prob), y = prob, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Male Competitors with highest probability ") +
  ylab("Probability of winning medal") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")

### Identifying average probabilities in each country 

# Calculate the average probability (prob) for each country
avg_m_prob_12Countries <- combined_data %>%
  group_by(Country) %>%
  summarise(AvgProb = mean(prob))

# Create a ggplot to display the average probabilities
ggplot(avg_m_prob_12Countries, aes(x = reorder(Country, -AvgProb), y = AvgProb, fill = Country)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Country") +
  ylab("Average probability of Male medaling of top 5 players") +
  labs(fill = "Country") +
  scale_fill_discrete(name = "Country")



##### Simulation Graphics

top5_m_USA_prob = top5_prob("USA", "m")
top5_m_CHN_prob = top5_prob("CHN", "m")
top5_m_JPN_prob = top5_prob("JPN", "m")
top5_m_ENG_prob = top5_prob("ENG", "m")

USA_candidates_m_plot_prob <- top5_m_USA_prob %>% group_by(Name) %>%
  summarise(prob = mean(pred)) %>% 
  top_n(5, wt = prob)

ggplot(USA_candidates_m_plot_prob, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='blue',fill='steelblue') +
  xlab("Male contestants") + ylab("USA Top 5 Probability of Medaling")



CHN_candidates_m_plot_prob <- top5_m_CHN_prob %>% group_by(Name) %>%
  summarise(prob = mean(pred)) %>% 
  top_n(5, wt = prob)

ggplot(CHN_candidates_m_plot_prob, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='red2',fill='red2') +
  xlab("Male contestants") + ylab("CHN Top 5 Probability of Medalin")


JPN_candidates_m_plot_prob <- top5_m_JPN_prob %>% group_by(Name) %>%
  summarise(prob = mean(pred)) %>% 
  top_n(5, wt = prob)

ggplot(JPN_candidates_m_plot_prob, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='darkred',fill='darkred') +
  xlab("Male contestants") + ylab("JPN Men's Top 5 Probability of Medaling")

ENG_candidates_m_plot_prob <- top5_m_ENG_prob %>% group_by(Name) %>%
  summarise(prob = mean(pred)) %>% 
  top_n(5, wt = prob)

ggplot(ENG_candidates_m_plot_prob, aes(x = reorder(Name, -prob), y = prob)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_bar(stat = "identity" , color='darkgreen',fill='chartreuse3') +
  xlab("Male contestants") + ylab("ENG Men's Top 5 Probability of Medaling")


# # Combine all together
USA_candidates_m_plot_prob$country <- "USA"
ENG_candidates_m_plot_prob$country <- "ENG"
JPN_candidates_m_plot_prob$country <- "JPN"
CHN_candidates_m_plot_prob$country <- "CHN"

all_men_simulation_prob <- rbind(USA_candidates_m_plot_prob, ENG_candidates_m_plot_prob,
                                 JPN_candidates_m_plot_prob, CHN_candidates_m_plot_prob)

country_colors <- 
  c("JPN" = "darkred", "CHN" = "red", 
    "USA" = "steelblue", "ENG" = "chartreuse3")

ggplot(all_men_simulation_prob, aes(x = reorder(Name, -prob), y = prob, fill = country)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = country_colors) +  # Set the custom color palette
  xlab("Male contestants") +
  ylab("Probability of medaling")



