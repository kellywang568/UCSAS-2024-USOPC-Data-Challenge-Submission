
library(tools)
library(dplyr)
library(lubridate)
library(shiny)

############# DATA CLEANING #################
source("clean_data.r")
# Note: if you want to do the same thing for olympic dataset, try this function
# data <- clean_olympic_data()

data <- clean_championship_data()
head(data)
# Note: we still have 26 entries that not have Country

## Basic Visualization
# Scatter plot of D_Score vs E_Score with points colored by Gender
ggplot(data, aes(x = D_Score, y = E_Score, color = Gender)) +
  geom_point() +
  labs(title = "Scatter Plot of D_Score vs E_Score",
       x = "D_Score",
       y = "E_Score",
       color = "Gender")

# Calculate the correlation coefficient
correlation_coefficient <- cor(data$D_Score, data$E_Score)
cat("Correlation Coefficient:", correlation_coefficient, "\n")


# Bar plot of average scores for each Apparatus
ggplot(data, aes(x = Apparatus, y = Score, fill = Gender)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Scores for Each Apparatus by Gender",
       x = "Apparatus",
       y = "Average Score",
       fill = "Gender")


############# MODEL BUILDING (LOGISTIC) #######
source("craft_variables.r")
source("fit_model.r")
prob_variables <- craft_prob_variables(data)
# Note: only part of the gymnasts have ever taken part in the AAfinal competition
# so this table does not contain every gymnast.
prob_aa_variables <- craft_prob_aa_variables(data)
# fit model
logit_model <- fit_full_logistic_model(prob_variables)
logit_aa_model <- fit_full_logistic_model(prob_aa_variables)

############# MODEL BUILDING (LINEAR) ##########
source("craft_variables.r")
source("fit_model.r")
score_variables <- craft_score_variables(data, "final")
score_aa_variables <- craft_score_variables(data, "AAfinal")
# Note: we have Round: aa, AAfinal, AAqual, final, qual, and TeamFinal

# fit model
linear_model <- fit_full_linear_model(score_variables)
linear_aa_model <- fit_full_linear_model(score_aa_variables)

############# PREDICTION #################
olympic_difficulty <- mean(score_variables$difficulty)

prob_variables <- prob_variables %>% mutate(difficulty = olympic_difficulty)
prob_aa_variables <- prob_aa_variables %>% mutate(difficulty = olympic_difficulty)
prob_variables$pred <- predict(logit_model, newdata = prob_variables, type='response')
prob_aa_variables$pred <- predict(logit_aa_model, newdata = prob_aa_variables, type='response')
gymnast_prob_predicts_aa <- prob_aa_variables %>% select(Name, Apparatus, Gender, Country, pred) %>% unique()
gymnast_prob_predicts <- prob_variables %>% select(Name, Apparatus, Gender, Country, pred) %>% unique()
head(gymnast_prob_predicts, 20)
head(gymnast_prob_predicts_aa, 10)

score_variables <- score_variables %>% mutate(difficulty = olympic_difficulty)
score_aa_variables <- score_aa_variables %>% mutate(difficulty = olympic_difficulty)
score_variables$pred <- predict(linear_model, newdata = score_variables, type='response')
score_aa_variables$pred <- predict(linear_aa_model, newdata = score_aa_variables, type='response')
gymnast_score_predicts <- score_variables %>% select(Name, Apparatus, Gender, Country, pred) %>% unique()
gymnast_score_predicts_aa <- score_aa_variables %>% select(Name, Apparatus, Gender, Country, pred) %>% unique()
head(gymnast_score_predicts)
head(gymnast_score_predicts_aa, 10)

############## FINALIZE PROB & PRED DF ###########
vaults <- c("VT", "VT1", "VT2")
gymnast_predicts_w <- gymnast_score_predicts %>%
  filter(Country == "USA" & Gender == "w") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name, Apparatus) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  unique()
gymnast_probs_w <- gymnast_prob_predicts %>%
  filter(Country == "USA" & Gender == "w") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name, Apparatus) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  unique()
gymnast_predicts_w_aa <- gymnast_score_predicts_aa %>%
  filter(Country == "USA" & Gender == "w") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  select(Name, Gender, Country, pred) %>%
  mutate(Apparatus = "AA") %>%
  unique()
gymnast_probs_w_aa <- gymnast_prob_predicts_aa %>%
  filter(Country == "USA" & Gender == "w") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  select(Name, Gender, Country, pred) %>%
  mutate(Apparatus = "AA") %>%
  unique()
gymnast_probs_m <- gymnast_prob_predicts %>%
  filter(Country == "USA" & Gender == "m") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name, Apparatus) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  unique()
gymnast_preditcs_m <- gymnast_score_predicts %>%
  filter(Country == "USA" & Gender == "m") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name, Apparatus) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  unique()
gymnast_predicts_m_aa <- gymnast_score_predicts_aa %>%
  filter(Country == "USA" & Gender == "m") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  select(Name, Gender, Country, pred) %>%
  mutate(Apparatus = "AA") %>%
  unique()
gymnast_probs_m_aa <- gymnast_prob_predicts_aa %>%
  filter(Country == "USA" & Gender == "m") %>%
  mutate(Apparatus = ifelse(Apparatus %in% vaults, "VT", Apparatus)) %>%
  group_by(Name) %>%
  mutate(pred = sprintf("%0.8f",mean(pred))) %>%
  ungroup() %>%
  select(Name, Gender, Country, pred) %>%
  mutate(Apparatus = "AA") %>%
  unique()

# Probability dataframe for women
gymnast_probs_w
# Prediction dataframe for women
gymnast_predicts_w
# Probability dataframe for women all-around
gymnast_probs_w_aa
# Prediction dataframe for women all-around
gymnast_predicts_w_aa
# Probability dataframe for men
gymnast_probs_m
# Prediction dataframe for men
gymnast_preditcs_m
# Probability dataframe for men all-around
gymnast_predicts_m_aa
# Prediction dataframe for men all-around
gymnast_probs_m_aa


############## FOR LOOP -> COMBINATION ###########
run_combination <- function(df_prediction, df_prediction_aa) {
  # Create a list where each element contains all the gymnasts for a specific apparatus
  apparatus_list <- list()
  for (apparatus in unique(df_prediction$Apparatus)) {
    gymnasts <- unique(df_prediction %>% filter(Apparatus == apparatus)) %>% arrange(desc(as.numeric(pred)))
    apparatus_list[[apparatus]] <- gymnasts
  }
  apparatus_list[["AA"]] <- df_prediction_aa

  # Create a list that represents the index of the gymnast for each apparatus
  index_list <- lapply(apparatus_list, function(df) {
    return(1 : pmin(nrow(df), 5))
  })

  # Use expand.grid to get all combinations
  combinations <- expand.grid(index_list)
  all_combinations <- do.call(rbind, apply(combinations, 1, function(row_indices) {
    # Extract rows based on the current combination of indices
    rows <- mapply(function(df, idx) df[idx, ], apparatus_list, row_indices, SIMPLIFY = FALSE)

    # Combine rows into one dataframe and return
    combined_row <- do.call(cbind, rows)
    return(combined_row)
  }))

  return(all_combinations)
}

table(gymnast_predicts_w$Apparatus) # BB FX UB VT
table(gymnast_preditcs_m$Apparatus) # FX HB PB PH SR VT

result_pred_w <- run_combination(gymnast_predicts_w, gymnast_predicts_w_aa)
result_prob_w <- run_combination(gymnast_probs_w, gymnast_probs_w_aa)
result_pred_w <- result_pred_w %>%
  select(BB.Name, BB.pred, FX.Name, FX.pred, UB.Name, UB.pred, VT.Name, VT.pred, AA.Name, AA.pred) %>%
  mutate(BB = as.numeric(BB.pred)) %>%
  mutate(FX = as.numeric(FX.pred)) %>%
  mutate(UB = as.numeric(UB.pred)) %>%
  mutate(VT = as.numeric(VT.pred)) %>%
  mutate(AA = as.numeric(AA.pred)) %>%
  select(BB.Name, BB, FX.Name, FX, UB.Name, UB, VT.Name, VT, AA.Name, AA) %>%
  mutate(Total = BB + FX + UB + VT + AA) %>%
  arrange(desc(Total))
result_prob_w <- result_prob_w %>%
  select(BB.Name, BB.pred, FX.Name, FX.pred, UB.Name, UB.pred, VT.Name, VT.pred, AA.Name, AA.pred) %>%
  mutate(BB = as.numeric(BB.pred)) %>%
  mutate(FX = as.numeric(FX.pred)) %>%
  mutate(UB = as.numeric(UB.pred)) %>%
  mutate(VT = as.numeric(VT.pred)) %>%
  mutate(AA = as.numeric(AA.pred)) %>%
  select(BB.Name, BB, FX.Name, FX, UB.Name, UB, VT.Name, VT, AA.Name, AA) %>%
  mutate(Total = BB + FX + UB + VT + AA) %>%
  arrange(desc(Total))

head(result_pred_w)
head(result_prob_w)

result_pred_m <- run_combination(gymnast_preditcs_m, gymnast_predicts_m_aa)
result_prob_m <- run_combination(gymnast_probs_m, gymnast_probs_m_aa)
result_pred_m <- result_pred_m %>%
  select(FX.Name, FX.pred, HB.Name, HB.pred, PB.Name, PB.pred, PH.Name, PH.pred, SR.pred, SR.Name, VT.Name, VT.pred, AA.Name, AA.pred) %>%
  mutate(FX = as.numeric(FX.pred)) %>%
  mutate(HB = as.numeric(HB.pred)) %>%
  mutate(PB = as.numeric(PB.pred)) %>%
  mutate(PH = as.numeric(PH.pred)) %>%
  mutate(SR = as.numeric(SR.pred)) %>%
  mutate(VT = as.numeric(VT.pred)) %>%
  mutate(AA = as.numeric(AA.pred)) %>%
  select(FX.Name, FX, HB.Name, HB, PB.Name, PB, PH.Name, PH, SR.Name, SR, VT.Name, VT, AA.Name, AA) %>%
  mutate(Total = FX + HB + PB + PH + SR + VT) %>%
  arrange(desc(Total))
result_prob_m <- result_prob_m %>%
  select(FX.Name, FX.pred, HB.Name, HB.pred, PB.Name, PB.pred, PH.Name, PH.pred, SR.pred, SR.Name, VT.Name, VT.pred, AA.Name, AA.pred) %>%
  mutate(FX = as.numeric(FX.pred)) %>%
  mutate(HB = as.numeric(HB.pred)) %>%
  mutate(PB = as.numeric(PB.pred)) %>%
  mutate(PH = as.numeric(PH.pred)) %>%
  mutate(SR = as.numeric(SR.pred)) %>%
  mutate(VT = as.numeric(VT.pred)) %>%
  mutate(AA = as.numeric(AA.pred)) %>%
  select(FX.Name, FX, HB.Name, HB, PB.Name, PB, PH.Name, PH, SR.Name, SR, VT.Name, VT, AA.Name, AA) %>%
  mutate(Total = FX + HB + PB + PH + SR + VT) %>%
  arrange(desc(Total))

head(result_pred_m)
head(result_prob_m)

############### SIMULATION FOR OTHER COUNTRIES #######
# Finding top 12 countries and the five athletes for each team
source("select_competitor.r")

top_countries_w <- topn_countries(data = score_variables,
                                  gender = 'w',
                                  topn = 12)
top_countries_w
country_codes_w <- as.list(top_countries_w$Country)
top_countries_m <- topn_countries(data = score_variables,
                                  gender = 'm',
                                  topn = 12)
top_countries_m
country_codes_m <- as.list(top_countries_m$Country)

# Find top 5 gymnasts in each country
top5_m <- lapply(top_countries_m$Country, top5, data = gymnast_score_predicts, gender ="m")

country_tables_m <- setNames(replicate(length(country_codes_m), NULL, simplify = FALSE), country_codes_m)
for (i in 1 : length(country_codes_m)) {
    country_tables_m[country_codes_m[[i]]] <- top5_m[[i]]
}
country_tables_m

top5_w <- lapply(top_countries_w$Country, top5, data = gymnast_score_predicts, gender ="w")

country_tables_w <- setNames(replicate(length(country_codes_w), NULL, simplify = FALSE), country_codes_w)
for (i in 1 : length(country_codes_w)) {
    country_tables_w[country_codes_w[[i]]] <- top5_w[[i]]
}
country_tables_w



ui <- fluidPage(
  # Application title
  titlePanel("Olympics Teams Performance Analysis"),

  # Sidebar with dropdown menus
  sidebarLayout(
    sidebarPanel(
      # Dropdown for choosing Male or Female team
      selectInput("team", "Select Team", choices = c("Male", "Female")),

      # Dropdown for choosing Score Prediction or Winning Probability
      selectInput("metric", "Select Metric", choices = c("Score Prediction", "Winning Probability")),

      # UI Output for the reactive checkbox group
      uiOutput("countryCheckboxes"),
    ),

    # Main panel
    mainPanel(
        tableOutput("resultsTable"),
        tableOutput("compareTable")
    )
  )
)

server <- function(input, output, session) {
  # Reactive UI for the checkboxes based on the team selection
  output$countryCheckboxes <- renderUI({
    if (input$team == "Male") {
      checkboxGroupInput("compare_countries", "Compare with Countries", choices = country_codes_m)
    } else if (input$team == "Female") {
      checkboxGroupInput("compare_countries", "Compare with Countries", choices = country_codes_w)
    } else {
      return(NULL) # Return nothing if no team is selected
    }
  })


  selected_countries <- reactive({
    if (input$team == "Male") {
      country_tables_m[names(country_tables_m) %in% input$compare_countries]
      } else {
        country_tables_w[names(country_tables_w) %in% input$compare_countries]
      }
  })

  output$compareTable <- renderTable({
    df <- t(sapply(selected_countries(), unlist))
    colnames(df) <- head(input$compare_countries, ncol(df))
    format(df, justify="left")
  })


  # Render the result tables based on the selections
  output$resultsTable <- renderTable({
    if (input$team == "Female" && input$metric == "Score Prediction") {
      return(head(result_pred_w))
    } else if (input$team == "Male" && input$metric == "Score Prediction") {
      return(head(result_pred_m))
    } else if (input$team == "Female" && input$metric == "Winning Probability"){
      return(head(result_prob_w))
    } else {
      return(head(result_prob_m))
    }
  })

  # Create a reactive expression for displaying the selected team and metric
  output$selection_text <- renderText({
    paste("Selected Team:", input$team, "| Selected Metric:", input$metric, "| Compared Countries:", input$compare_countries) # paste(input$compare_countries, collapse = ", "))
  })
}

shinyApp(ui, server)
