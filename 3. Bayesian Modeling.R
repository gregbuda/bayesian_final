
setwd("C:/Users/gregb/OneDrive/Asztali gép/UPC_master/BAY/Final Project")
library(readr)
library(rstan)
library(ggplot2)
#install.packages('tidytext')
library(tidytext)
library(tidyverse)
library(dplyr)
library(data.table)
library(tidyr)
#install.packages('udpipe')
library(udpipe)
#install.packages("tm")
library(tm)
library(stringr)
library(topicmodels)
library(slam)
library(Matrix)
library(reshape2)
library(patchwork)
library(tokenizers)

options(mc.cores = parallel::detectCores())


######################## DATA PREPARATION  ############################

#Load data
df <- read_csv("Webscrape/cleaned_df.csv")
doc_term <- read_csv("Webscrape/document_term_matrix.csv")

#Add an ID column
df$ID <- seq.int(nrow(df))
doc_term$ID <- seq.int(nrow(doc_term))



#Delete lines where price is not correct (checked website) 
dim(df)[1]
df <- df[df$price<=13800000,]
df <- df[df$price>10000,]
dim(df)[1]

#Make 1 room and 1 bathroom the baseline
min(df$no_rooms)
min(df$bathroom)
df$no_rooms <- df$no_rooms -1
df$bathroom <- df$bathroom -1

#3 lines have erroneous surface measure
df <- df[df$surface >=10,]

#Final data size
dim(df)[1]


#############################   DATA EXPLORATION ####################

summary(df)

####Check price distribution
#plot(density(df$price))
p1 <- ggplot(data=df)+
  geom_density(aes(x=price),alpha=.25)

  #Model for log price
df$price <- log(df$price)
p2 <- ggplot(data=df)+
  geom_density(aes(x=price),alpha=.25)+ labs(x = "log price")

p1 + theme_bw() + p2 + theme_bw()

#Normality test
library(nortest)
ad.test(df$price)$p.value

   #Will be modeled as normal WITHIN each district
df_subset <- subset(df, barrio %in% c("Pedralbes", "Guinardó", "Barri de les Corts"))
ggplot(data=df_subset)+
  geom_density(aes(x=price, fill=barrio),alpha=.25)+
  theme_bw()+ 
  scale_fill_discrete(name = "Neighborhood")+ 
  labs(x = "log price")

#Plot the average prices per neighbourhood
avg_prices <- df %>% 
  group_by(barrio) %>% 
  summarize(avg_price = mean(price))

ggplot(data=avg_prices)+
  geom_density(aes(x=avg_price),alpha=.25)+
  theme_bw()


#### Convert neighbourhoods to index
df$barrio <- as.factor(df$barrio)
levels(df$barrio)
df$district_index <- as.numeric(df$barrio)

#Correlation between 3 variables
cor(df[,c(3,4,5)])


dim(df)
################################ MODELING ##############

######  Data list
data_list <- list(
  N = dim(df)[1],
  price = df$price,
  no_rooms = df$no_rooms,
  surface = df$surface,
  bathroom = df$bathroom,
  num_districts = length(levels(df$barrio)),
  district_index = df$district_index,
  ascensor = df$Ascensor,
  calef = df$Calefacción,
  aire = df$`Aire acondicionado`,
  park = df$`Plaza parking`,
  piscina = df$`Piscina comunitaria`,
  mueble =df$Amueblado
)

#####################   MODEL 1b: fixed, 3 variables ################
fit_1 <- stan("stan_1.stan", iter = 2000, chains = 4,
               data = data_list, seed = 1)

#Diagnostics
print(fit_1)
traceplot(fit_1)

plot(fit_1)

#Posterior
posterior <- as.matrix(fit_1)
plot(density(posterior[, "beta_1"]))
plot(density(posterior[, "beta_2"]))
plot(density(posterior[, "beta_3"]))

beta_0 <- rstan::extract(fit_1,"beta_0")[[1]] 
beta_1 <- rstan::extract(fit_1,"beta_1")[[1]]  
beta_2 <- rstan::extract(fit_1,"beta_2")[[1]]
beta_3 <- rstan::extract(fit_1,"beta_3")[[1]]
sigma <- rstan::extract(fit_1,"sigma")[[1]]
  
# Combine the arrays into a list
parameters <- list(beta_0, beta_1, beta_2, beta_3, sigma)
calculate_stats <- function(arr) {
  mean_val <- mean(arr)
  p25_val <- quantile(arr, 0.025)
  p975_val <- quantile(arr, 0.975)
  return(c(mean_val, p25_val, p975_val))
}
lapply(parameters, calculate_stats)

#Posterior predictive
new_data<-c(1,80,1)
post_pred_1 <- sapply(1:length(beta_0), function(i) {
  rnorm(1, beta_0[i] + beta_1[i]*new_data[1] + beta_2[i]*new_data[2] + beta_3[i]*new_data[3], sigma)
})
plot(density(post_pred_1))

#Calculate LOO
loo1 <- loo(fit_1)
print(loo1)



#####################   MODEL 1a: FIXED, 1 variable #############################
#FIXED EFFECT MODEL - 1 variable
fit_2 <- stan("stan_2.stan", iter = 2000, chains = 4,
                data = data_list, seed = 1)

#Diagnostics
print(fit_2)
traceplot(fit_2)

#Calculate an example
exp(12.0364 + 0.0059 * 40) * 1.0059

#Posterior
posterior <- as.matrix(fit_2)
plot(density(posterior[, "beta_1"]))

beta_0 <- rstan::extract(fit_2,"beta_0")[[1]] 
beta_1 <- rstan::extract(fit_2,"beta_1")[[1]]  
sigma <- rstan::extract(fit_2,"sigma")[[1]]


# Combine the arrays into a list
parameters <- list(beta_0, beta_1, sigma)
calculate_stats <- function(arr) {
  mean_val <- mean(arr)
  p25_val <- quantile(arr, 0.025)
  p975_val <- quantile(arr, 0.975)
  return(c(mean_val, p25_val, p975_val))
}
lapply(parameters, calculate_stats)

#Posterior predictive
new_data<-c(1,80,1)
post_pred_2 <- sapply(1:length(beta_0), function(i) {
  rnorm(1, beta_0[i] + beta_1[i]*new_data[2], sigma)
})

#Expected
mean(post_pred_2)
mean(post_pred_1)

#Compare posterior predictives
ggplot() +
  geom_density(aes(x = exp(post_pred_2), fill = "Model 1a"), alpha = 0.4) +
  geom_density(aes(x = exp(post_pred_1), fill = "Model 1b"), alpha = 0.4) +
  scale_fill_manual(name = "Model", values = c("Model 1b" = "blue", "Model 1a" = "red"))+
  xlab("Price")+
  theme_bw()

#Compare with data
data_filter <- df %>%
  dplyr::filter(between(surface, 75, 85) & no_rooms==1 & bathroom==1) %>% 
  select(price)

#Overlap
ggplot() +
  geom_density(aes(x = exp(post_pred_2), fill = "Model 1a"), alpha = 0.4) +
  geom_density(aes(x = exp(post_pred_1), fill = "Model 1b"), alpha = 0.4) +
  geom_density(aes(x = exp(data_filter$price), fill = "Data"), alpha = 0.1, linetype = "dashed", size = 0.6) +
  scale_fill_manual(name = "Model", values = c("Model 1b" = "blue", "Model 1a" = "red", "Data" = "green")) +
  xlab("Price") +
  theme_bw()

mean(data_filter$price)
length(data_filter$price)

#LOO
loo2 <- loo(fit_2)
print(loo2)





########################   MODEL 2a, #RANDOM INTERCEPT, 1 variable   ######################
fit_3 <- stan("stan_3.stan", iter = 2000, chains = 4,
                data = data_list, seed = 1)

print(fit_3)
traceplot(fit_3)
acf(posterior[, "beta_1"])

#Posterior
posterior <- as.matrix(fit_3)
colnames(posterior)

#Stats
parameters <- c('beta_0', 'beta_1', 'tau')
calculate_stats <- function(param) {
  mean_val <- mean(posterior[, param])
  p25_val <- quantile(posterior[, param], 0.025)
  p975_val <- quantile(posterior[, param], 0.975)
  return(c(mean_val, p25_val, p975_val))
}
lapply(parameters, calculate_stats)



#####PREDICTIVE
#District 74 = Pedralbes
dist_74_sigma <- posterior[, "sigma_district[74]"]
dist_74_b <- posterior[, "b[74]"]

#District 112 = Trinitat Vella
dist_112_sigma <- posterior[, "sigma_district[112]"]
dist_112_b <- posterior[, "b[112]"]

#General betas
betas <- posterior[,grep("^beta", colnames(posterior))]

#Predictive posteriori - pedralbes
new_data<-c(80)
post_pred_pderal <- sapply(1:length(dist_74_b), function(i) {
  rnorm(1, betas[i,1] + dist_74_b[i] + betas[i,2]*new_data[1], dist_74_sigma[i])
})

#Predictive posteriori - trinitat
new_data<-c(80)
post_pred_trinitat <- sapply(1:length(dist_112_b), function(i) {
  rnorm(1, betas[i,1] + dist_112_b[i] + betas[i,2]*new_data[1], dist_112_sigma[i])
})

#Compare with data
data_pedral <- df %>%
  filter(barrio =='Pedralbes') %>% 
  select(price)

data_trini <- df %>%
  filter(barrio =='Trinitat Vella') %>% 
  select(price)

ggplot() +
  geom_density(aes(post_pred_pderal, color = 'Pedralbes - Posterior Predictive'), linetype = 'solid', size = 1, alpha = 0.25) +
  geom_density(aes(post_pred_trinitat, color = 'Trinitat Vella - Posterior Predictive'), linetype = 'solid', size = 1, alpha = 0.25) +
  geom_density(aes(data_pedral$price, color = 'Pedralbes - Data'), linetype = 'dashed', size = 1, alpha = 0.25) +
  geom_density(aes(data_trini$price, color = 'Trinitat Vella - Data'), linetype = 'dashed', size = 1, alpha = 0.2) +
  scale_color_manual(
    name = ' ',
    values = c(
      'Pedralbes - Posterior Predictive' = 'blue',
      'Trinitat Vella - Posterior Predictive' = 'red',
      'Pedralbes - Data' = 'blue',
      'Trinitat Vella - Data' = 'red'
    ),
    guide = guide_legend(override.aes = list(linetype = c('dashed', 'solid', 'dashed', 'solid')))
  ) +
  theme_bw() +
  xlab('log price')



mean(post_pred_trinitat)
mean(post_pred_pderal)



######## CHECK THE RIGHT TAIL WITH SIMULATIONS
#Model 1a
beta_0 <- rstan::extract(fit_2,"beta_0")[[1]] 
beta_1 <- rstan::extract(fit_2,"beta_1")[[1]]  
sigma <- rstan::extract(fit_2,"sigma")[[1]]

predictions <- c()
for (i in 1:100){
pred <- df %>%
  rowwise() %>%
  mutate(pred = rnorm(1, mean(beta_0) + mean(beta_1) * surface, mean(sigma))) %>%
  ungroup()
  predictions <- c(predictions, pred$pred)
}

#Model 2a
posterior <- as.matrix(fit_3)

predictions_2 <- c()
for (i in 1:100){
  pred <- df %>%
    rowwise() %>%
    mutate(pred = {
      district_idx <- district_index
      b_col <- paste0('b[', district_idx, ']')
      sigma_col <- paste0('sigma_district[', district_idx, ']')
      b_values <- posterior[, b_col]
      sigma_values <- posterior[, sigma_col]
      mean_val <- mean(posterior[, 'beta_0']) + mean(b_values) + mean(posterior[, 'beta_1']) * surface
      rnorm(1, mean_val, mean(sigma_values))
    }) %>%
    ungroup()
  predictions_2 <- c(predictions_2, pred$pred)
}


ggplot() +
  geom_density(aes(x = predictions, fill = "Model 1a"), alpha = 0.3) +
  geom_density(aes(x = predictions_2, fill = "Model 2a"), alpha = 0.3) +
  geom_density(data = df, aes(x = price, fill = "Data"), alpha = 0, size = 1) +
  scale_fill_manual(name = "Model", values = c("Model 1a" = "blue", "Model 2a" = "red","Data" = "white"))+
  xlab("Price")+
  theme_bw()+
  coord_cartesian(xlim = c(10.5, 17))+
  labs('log price')+
  guides(fill = guide_legend(override.aes = list(size = c(1,0.5,0.5))))


ggplot() +
  geom_density(aes(x = predictions, fill = "Model 1a"), alpha = 0.3) +
  geom_density(aes(x = predictions_2, fill = "Model 2a"), alpha = 0.3) +
  geom_density(data = df, aes(x = price, fill = "Data"), alpha = 0, size = 1) +
  scale_fill_manual(name = "Model", values = c("Model 1a" = "blue", "Model 2a" = "red", "Data" = "white")) +
  xlab("Price") +
  theme_bw() +
  coord_cartesian(xlim = c(10.5, 17)) +
  labs(x = "log price") +
  guides(fill = guide_legend(override.aes = list(linetype = c(1, 0, 0), size = c(0.5, 1, 1))))




##########################MODEL 2b:  #ALL VARIABLES + RANDOM INTERCEPT##########################
fit_4 <- stan("stan_4.stan", iter = 2000, chains = 4,
                data = data_list, seed = 1)

print(fit_4)
traceplot(fit_4)

#Look at effects
plot(fit_4)

#Posterior
posterior <- as.matrix(fit_4)
colnames(posterior) 

#Stats
parameters <- c('beta_0', 'beta_1','beta_2','beta_3','beta_4','beta_5','beta_6',
                'beta_7', 'tau')
calculate_stats <- function(param) {
  mean_val <- mean(posterior[, param])
  p25_val <- quantile(posterior[, param], 0.025)
  p975_val <- quantile(posterior[, param], 0.975)
  return(c(mean_val, p25_val, p975_val))
}
lapply(parameters, calculate_stats)


#District 74 = Pedralbes
dist_74_sigma <- posterior[, "sigma_district[74]"]
dist_74_b <- posterior[, "b[74]"]

#### SHOW EFFECT OF AMENITIES
#General betas
betas <- posterior[,grep("^beta", colnames(posterior))]
betas
#Predictive posteriori
#a house of 2 room, 2 bathroom and 80m2, Pedralbes
#with elevator, heating, air conditioning, parking, common pool and furnished
#vs
#without these amenities
new_data<-c(100,1,1,1,1,1,1)
post_pred_all_in <- sapply(1:length(dist_74_b), function(i) {
  rnorm(1, betas[i,1] + dist_74_b[i] + betas[i,2]*new_data[1] + betas[i,3]*new_data[2] + betas[i,4]*new_data[3]+
          + betas[i,5]*new_data[4] + betas[i,6]*new_data[5] + betas[i,7]*new_data[6]+
          + betas[i,8]*new_data[7], dist_74_sigma[i])
})

new_data<-c(100,0,0,0,0,0,0)
post_pred_basic <- sapply(1:length(dist_74_b), function(i) {
  rnorm(1, betas[i,1] + dist_74_b[i] + betas[i,2]*new_data[1] + betas[i,3]*new_data[2] + betas[i,4]*new_data[3]+
          + betas[i,5]*new_data[4] + betas[i,6]*new_data[5] + betas[i,7]*new_data[6]+
          + betas[i,8]*new_data[7], dist_74_sigma[i])
})



ggplot() +
  geom_density(aes(exp(post_pred_all_in), fill = "House with all 6 facilities"), alpha = 0.25) +
  geom_density(aes(exp(post_pred_basic), fill = "House without any of the 6 facilities"), alpha = 0.25) +
  scale_fill_manual(name = " ",values = c("House with all 6 facilities" = "blue", "House without any of the 6 facilities" = "red")) +
  xlab("price")+
  coord_cartesian(xlim = c(0, 3000000))+
  theme_bw()+
  scale_x_continuous(labels=scales::comma)
  
mean(post_pred_all_in)
mean(post_pred_basic)







#########################  TEXT: PREPARATION OF DISTANCE MATRIX #######################

#Filter out the lines that were outliers
doc_term<-doc_term[doc_term$ID %in% df$ID,]
doc_term <- doc_term[, -which(names(doc_term) == "ID")]

#Cut the columns that refer to the location
clean_text <- function(text) {
  text <- str_replace_all(text, "[^[:alpha:][:space:]]", "")
  text <- str_to_lower(text) %>% str_replace_all("[áäàâ]", "a") %>% str_replace_all("[éëèê]", "e") %>% str_replace_all("[íïìî]", "i") %>% str_replace_all("[óöòô]", "o") %>% str_replace_all("[úüùû]", "u")
  words <- str_extract_all(text, "\\w+")
  return(words)
}
cleaned_words <- lapply(list(names(table(df$barrio))), clean_text)
all_barrio_words <- unlist(cleaned_words)
#Add barcelona and others
#all_barrio_words <- c(all_barrio_words,c("barcelona"))
all_barrio_words <- c(all_barrio_words,c("barcelona","gramenet","coloma","vidalet","finestrelles","llobregat","badalona"))
dim(doc_term)
doc_term<-doc_term[, !names(doc_term) %in% all_barrio_words]
dim(doc_term)

#Take off those lines where no useful token is available
any(rowSums(doc_term) == 0)
    # Get the row indices with a row sum of 0
zero_sum_rows <- which(rowSums(doc_term) == 0)
zero_sum_rows
doc_term <- doc_term[-zero_sum_rows,]
#Takes off 4 observations

#Calculate the average price for 0/1 response for each vocabulary words
result <- data.frame()
for (col in names(doc_term)) {
  temp_df <- data.frame(outcome = doc_term[[col]], price = df[-zero_sum_rows,'price'])
  col_result <- temp_df %>% 
    group_by(outcome) %>%
    summarise(avg_price = mean(price))
    result <- bind_rows(result, col_result %>% mutate(column_name = col))
}
result <- result %>% pivot_wider(names_from = "outcome", values_from = "avg_price")
names(result) <- c("word", "outcome_0_avg_price", "outcome_1_avg_price")
result$diff <- result$outcome_0_avg_price - result$outcome_1_avg_price

#Incresas price if there is
result  %>% 
  arrange(diff) %>%
  slice(1:10)

#Decreases price if there is NOT
result  %>% 
  arrange(desc(diff)) %>%
  slice(1:10)

#Create the right format of document-term-matrix
doc_term$doc_id <- rownames(doc_term)
doc_term_melted <- melt(doc_term, id.vars = "doc_id", variable.name = "word", value.name = "count")
doc_term_melted$count <- as.integer(doc_term_melted$count)
head(doc_term_melted)
dtm <- doc_term_melted %>%
  cast_dtm(doc_id, word, count)

#Checkpoint
save(dtm, file = "dtm.Rdata")
load("dtm.Rdata")







########################LDA
#Fit LDA
lda_model <- LDA(dtm, k = 4, control = list(seed = 1234))

# Extract the top terms for each topic
terms(lda_model,20)

#topic 1: reformado, aluminio, gasto, equipado, servicio, precio, agencia, tranquilo
#topic 2: completo, aire, acondicionado, situado, autobus, colegio, comercio
#topic 3: finca, suelo, luminoso, planta, distribuido, terraza, patio
#topic 4: barrio, exterior, amplio, precio, vivir

#See the probabilities for these terms
term_probabilities <- terms(lda_model, 20, probabilities = TRUE)

# Get the posterior probabilities of topics for each document
doc_topic_probs <- posterior(lda_model, dtm)

# View the distribution over topics for the first document
doc_topic_probs$topics

#Assign clusters
df_complete <- df[-zero_sum_rows,]
topics <- doc_topic_probs$topics
topics <- data.frame(topics)
names(topics) <- c('clus1','clus2','clus3','clus4')
highest_col <- names(topics)[max.col(topics, ties.method = "first")]
topics$highest_col <- highest_col

df_complete <- cbind(df_complete,topics)

#Cluster stats
df_complete %>% 
  group_by(highest_col) %>% 
  summarise(mean_price = mean(price),
            n = n())


#New text classification
text1 <- "Un piso o una casa en la periferia de la ciudad, posiblemente con un pequeño jardín y plaza de parking, amplio y cerca de una guardería para los niños"
text2 <- "Un piso renovado y con aire acondicionado cerca de una parada de autobús o metro con restaurantes y comercios cerca, a precio razonable"
new_data <- as.data.frame(matrix(c(1, 2, text1, text2), ncol = 2, byrow = FALSE))

#Preprocess text into tokens
library(udpipe) # la cargamos
modelo_sp <- udpipe::udpipe_download_model('spanish') # descarga el modelo y guarda la referencia  
modelo_sp <- udpipe_load_model(file = modelo_sp$file_model) # cargamos el modelo en memoria
oraciones_anotadas <- udpipe_annotate( 
  object = modelo_sp, # el modelo de idioma
  x = new_data$V2, # el texto a anotar, 
  doc_id = new_data$V1, # el id de cada oracion (el resultado tendrá 1 palabra x fila)
  trace = 100
) %>% as.data.frame(.) # convertimos el resultado en data frame

words_1 <- oraciones_anotadas %>% 
  filter(doc_id==1) %>% 
  select(lemma)
words_2 <- oraciones_anotadas %>% 
  filter(doc_id==2) %>% 
  select(lemma)

#Make new document term matrix
doc_term_new <- data.frame(matrix(ncol = ncol(doc_term), nrow = 0))
colnames(doc_term_new) <- colnames(doc_term)
doc_term_new[1, ] <- ifelse(colnames(doc_term_new) %in% words_1$lemma, 1, 0)
doc_term_new[2, ] <- ifelse(colnames(doc_term_new) %in% words_2$lemma, 1, 0)
doc_term_new$doc_id <- rownames(doc_term_new)
doc_term_melted <- melt(doc_term_new, id.vars = "doc_id", variable.name = "word", value.name = "count")
doc_term_melted$count <- as.integer(doc_term_melted$count)
new_dtm <- doc_term_melted %>%
  cast_dtm(doc_id, word, count)

#Predict posterior cluster
test.topics <- posterior(lda_model,new_dtm)
(test.topics <- apply(test.topics$topics, 1, which.max))

#Use the dummies
make_dummy_vars <- function(data, column_name) {
  unique_values <- unique(data[[column_name]])
  dummy_data <- data.frame(matrix(0, nrow = nrow(data), ncol = length(unique_values)))
  colnames(dummy_data) <- unique_values
  for (i in 1:length(unique_values)) {
    dummy_data[, unique_values[i]] <- as.integer(data[[column_name]] == unique_values[i])
  }
  return(cbind(data, dummy_data))
}

df_complete <- make_dummy_vars(df_complete,'highest_col')
df_complete <- df_complete[,-c(15:19)]

######  Data list
data_list <- list(
  N = dim(df_complete)[1],
  price = df_complete$price,
  surface = df_complete$surface,
  num_districts = length(levels(df_complete$barrio)),
  district_index = df_complete$district_index,
  cluster_1 = df_complete$clus1,
  cluster_2 = df_complete$clus2,
  cluster_3 = df_complete$clus3
)

rm(dtm)
rm(doc_term)
rm(lda_model)
rm(df)
rm(doc_topic_probs)
rm(topics)
rm(temp_df)


##########################     MODEL 5 ##########################
#ALL VARIABLES + RANDOM INTERCEPT
fit_5 <- stan("stan_5.stan", iter = 1000, chains = 4,
              data = data_list, seed = 1)

print(fit_5)

#Posterior
posterior <- as.matrix(fit_5)
colnames(posterior) 

#Stats
parameters <- c('beta_0', 'beta_1','beta_2','beta_3','beta_4', 'tau')
calculate_stats <- function(param) {
  mean_val <- mean(posterior[, param])
  p25_val <- quantile(posterior[, param], 0.025)
  p975_val <- quantile(posterior[, param], 0.975)
  return(c(mean_val, p25_val, p975_val))
}
lapply(parameters, calculate_stats)


