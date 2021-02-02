# Load required packages
library(dplyr)
library(tidyr)
library(broom)
library(purrr)
library(caret)
library(PerformanceAnalytics)
library(GGally)
library(maps)
library(mapproj)
library(C50)

# Load some data
pokemon <- read.csv('https://raw.githubusercontent.com/malepati/adhocwork/master/dataset/Pokemon.csv')
pokemon <- pokemon[,6:11]
data(churn)
churnTrain$churn <- factor(churnTrain$churn, levels = c("no", "yes"))

# Set seed
set.seed(21)

# Create train indexes
myFolds <- createFolds(churnTrain$churn, k = 5)

# Make a custom trainControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # Super important!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

# Make Ranger model
mranger <- train(churn ~ .,
                 data = churnTrain,
                 method = "ranger",
                 tuneLength = 5,
                 trControl = myControl,
                 metric = 'ROC',
                 preProcess = c('nzv','medianImpute', 'center', 'scale')
)

plot(mranger)

# Make glmnet model
mglmnet <- train(churn ~ .,
                 data = churnTrain,
                 method = "glmnet",
                 trControl = myControl,
                 tuneGrid = expand.grid(alpha = 0:1,
                                        lambda = 0:10/10),
                 metric = 'ROC'
)

plot(mglmnet)
plot(mglmnet$finalModel)

# Make a list
model_list <- list(
  glmnet = mglmnet,
  ranger = mranger
)

# Collect resamples from the CV folds
resamps <- resamples(model_list)
summary(resamps)

# box-and-wisker plot
bwplot(resamps, metric = "ROC")
# dot plot
dotplot(resamps, metric = "ROC")
# density plot
densityplot(resamps, metric = "ROC")
# scatter plot
xyplot(resamps, metric = "ROC")

# UNSUPERVISED
# Scale the data
pokemon.scaled <- scale(pokemon)
# Create hierarchical and k-means clustering models
hclust.pokemon <- hclust(dist(pokemon.scaled),
                         method = "complete")
km.pokemon <- kmeans(pokemon.scaled,
                     centers = 3,
                     nstart = 20,
                     iter.max = 50)

# Compare results of the models
cut.pokemon <- cutree(hclust.pokemon, k = 3)
table(km.pokemon$cluster, cut.pokemon)

# PCA
pr.iris <- prcomp(x = iris[-5], scale = F, center = T)
summary(pr.iris)

# biplot
biplot(pr.iris)

# Getting proportion of variance for a scree plot
pr.var <- pr.iris$sdev^2
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve,
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Model selection
# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(pokemon,
                   centers = i,
                   nstart = 20,
                   iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15,
     wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

# SPLOM - Scatter PLOt Matrix
pairs(iris[-5])
chart.Correlation(iris[-5])
ggpairs(mtcars)

par(mfrow=c(1,2))

#Choropleths
usa <- map_data("usa")
ggplot(usa, aes(long, lat, group = group)) +
  geom_polygon() +
  coord_map()

states <- map_data("state")
ggplot(states, aes(long, lat, fill = region, group = group)) +
  geom_polygon(color = "white") +
  coord_map()


movies <- read.csv('https://raw.githubusercontent.com/malepati/adhocwork/master/dataset/movies.csv')
ratings <- read.csv('https://raw.githubusercontent.com/malepati/adhocwork/master/dataset/ratings.csv')
ratings <- ratings[,-4]

library(tm)
library(qdap)
genres <- as.data.frame(gsub(pattern='[|]',replacement=' ',x=movies[,3]), stringsAsFactors = F)
genres <- as.data.frame(gsub(pattern='[(]no genres listed[)]',replacement='',x=genres[,1]), fixed = T)
genre_matrix2 <- genres %>% 
                  DataframeSource() %>% 
                  VCorpus() %>%
                  DocumentTermMatrix() %>%
                  as.matrix()

binaryratings2 <- ratings %>% 
                    mutate(rating = ifelse(rating > 3, 1, -1)) %>% 
                    spread(movieId, rating, fill = 0)
binaryratings2 <- binaryratings2[,-1] %>% 
                    as.matrix()

#Remove rows that are not rated from movies dataset
movieIds <- unique(movies$movieId)
ratingmovieIds <- unique(ratings$movieId)
movies2 <- movies[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(movies2) <- NULL

#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movieIds %in% ratingmovieIds) == FALSE),]
rownames(genre_matrix3) <- NULL

result <- binaryratings2 %*% genre_matrix3
result <- ifelse(result < 0, 0, 1)

result2 <- result[1,] #First user's profile
sim_mat <- rbind.data.frame(result2, genre_matrix3)
sim_mat <- data.frame(lapply(sim_mat,function(x){as.integer(x)})) #convert data to type integer

#Calculate Jaccard distance between user profile and all movies
library(proxy)
sim_results <- dist(sim_mat, method = "Jaccard")
sim_results <- as.data.frame(as.matrix(sim_results[1:8552]))
rows <- which(sim_results == min(sim_results))

#Recommended movies
movies[rows,]

library(recommenderlab)
data(MovieLense)
methods(class = class(MovieLense))
object.size(as(MovieLense, "matrix")) / object.size(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)
average_ratings_per_user <- rowMeans(ratings_movies)
ratings_movies_norm <- normalize(ratings_movies)
qplot(average_ratings_per_user) + geom_histogram(binwidth = 0.1)
image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies,
                          colCounts(ratings_movies_norm) > min_users],
      main = "Heatmap of the top users and movies")
ratings_movies_watched <- binarize(ratings_movies, minRating = 1)
image(ratings_movies_watched[rowCounts(ratings_movies) > min_movies,
                             colCounts(ratings_movies) > min_users],
      main = "Heatmap of the top users and movies")
#### split
# which_train <- sample(x = c(TRUE, FALSE),
#                       size = nrow(ratings_movies),
#                       replace = TRUE,
#                       prob = c(0.8, 0.2))
# recc_data_train <- ratings_movies[which_train, ]
# recc_data_test <- ratings_movies[!which_train, ]
#### cv with k-fold
which_set <- sample(x = 1:5,
                    size = nrow(ratings_movies),
                    replace = TRUE)
for(i_model in 1:5) {
  which_train <- which_set == i_model
  recc_data_train <- ratings_movies[which_train, ]
  recc_data_test <- ratings_movies[!which_train, ]
}
#### Bootstrapping
eval_sets <- evaluationScheme(data = ratings_movies, 
                              method = 'cross-validation', #split/bootstrap/cross-validation
                              train = 0.8, 
                              given = 15, 
                              goodRating = 3, 
                              k = 4) #1/10/10
recc_data_train <- getData(eval_sets, 'train') 
recc_data_test <- getData(eval_sets, 'known') 
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# recommendation algorithms
names(recommender_models)
slotNames(MovieLense)
dim(MovieLense@data)
# item-item recommendation model
recommender_models$IBCF_realRatingMatrix$parameters
recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF")
model_details <- getModel(recc_model)
model_details$description
# similarity_items <- similarity(MovieLense[,1:4], method = "cosine", which = "items")
# similarity_items_m <- as.matrix(similarity_items)
class(model_details$sim)
n_items_top <- 20
image(model_details$sim[1:n_items_top,
                        1:n_items_top],
      main = "Heatmap of the first rows and columns")
n_recommended <- 6
recc_predicted <- predict(object = recc_model,
                          newdata = recc_data_test,
                          n = n_recommended,
                          type = 'ratings')
movies_user_1 <- recc_predicted@itemLabels[recc_predicted@items[[1]]]
recc_matrix <- sapply(recc_predicted@items, function(x){colnames(ratings_movies)[x]})

# user-user collaborative filtering
recommender_models$UBCF_realRatingMatrix$parameters
recc_model <- Recommender(data = recc_data_train,
                          method = "UBCF",
                          parameter = NULL)
model_details <- getModel(recc_model)
model_details$data
# similarity_users <- similarity(MovieLense[1:4,], method = "cosine", which = "users")
# similarity_users_m <- as.matrix(similarity_users)
n_recommended <- 6
recc_predicted <- predict(object = recc_model,
                          newdata = recc_data_test,
                          n = n_recommended)
# movies_user_1 <- recc_predicted@itemLabels[recc_predicted@items[[1]]]
recc_matrix <- sapply(recc_predicted@items, function(x){colnames(ratings_movies)[x]})

eval_accuracy <- calcPredictionAccuracy(x = recc_predicted, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = TRUE) #F wholemodel should
results <- evaluate(x = eval_sets, 
                    method = 'IBCF', 
                    n = seq(10, 100, 10))
plot(results, annotate = TRUE, main = "ROC curve")
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")
models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  random = list(name = "RANDOM", param=NULL)
)
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = c(1, 5, seq(10, 100, 10)))
class(list_results)
class(list_results[[1]])
sapply(list_results, class) == "evaluationResults"
avg_matrices <- lapply(list_results, avg)
head(avg_matrices$IBCF_cos[, 5:8])
plot(list_results, annotate = 1, legend = "topleft", main = 'ROC curve')
plot(list_results, "prec/rec", annotate = 1, legend = "bottomright", main = 'Precision-recall')

vector_k <- c(5, 10, 20, 30, 40)
models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF", param = list(method = "cosine", k = k))
})
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = c(1, 5, seq(10, 100, 10)))

#### NLP
library(NLP)
library(rJava)
library(openNLP)
