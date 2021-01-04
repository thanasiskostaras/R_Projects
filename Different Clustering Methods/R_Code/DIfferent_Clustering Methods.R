#Inserting the libraries

library(tidyr)
library(factoextra)
library(NbClust)
library(ggplot2)
library(gridExtra)
library(ISLR)
library(cluster)
library(reshape2)
library(dplyr)
library(poLCA)
library(plyr)


# Firstly the data-set was imported

data <- read.csv("patient.csv")    #initially there were 377 rows and 26 columns 

# The first column contains only the number of rows so it is useless. we remove it 

data$X <- NULL


# Searching about the number -9 in the data-set which corresponds to  missing values
sum(data==-9)  # 393 values are -9


# we convert -9 to NA values as to deal with them 
data[data == -9] <- NA
sum(is.na(data))  #  393 NA values

# count the NA in each column

sapply(data, function(x) sum(is.na(x)))

# na.omit command to delete the rows with missing values 



my_data<- na.omit(data)  # the new data-set have 292 rows and 25 columns
sum(is.na(my_data))      # we check if all the missing rows were fixed


# writing the new data-set
data_22 <- my_data[1:22]

# count the 1 in each column
count.1 <- ldply(data_22, function(c) sum(c=="1"))
count.1

# count the 2 in each column
count.2 <- ldply(data_22, function(c) sum(c=="2"))
count.2

# count the 3 in each column
count.3 <- ldply(data_22, function(c) sum(c=="3"))
count.3

# count the 4 in each column
count.4 <- ldply(data_22, function(c) sum(c=="4"))
count.4



# these one not included in the report, it was just for exploration for correlation

library(psych)
pairs.panels(data_22,gap=0,bg=c("red","blue","yellow","green"),
             pch=21)



# first we deal with the variables as they are continuous
# PCA implemented to have a better intuition on the data 

data_pca = scale(my_data[1:22])
# Get K=4 princple components
pca <- prcomp(data_pca, scale=TRUE, rank=4)
pca

# Importance of first k=4 (out of 22) components:
summary(pca)


pc.var <- pca$sdev^2
pc.var

#Explained variance by 22 components
pc.var.per<- round(pc.var/sum(pc.var)*100/100,2)
pc.var.per

# Variance Explained plot 
plot(pc.var.per, type="o", ylab="PVE", xlab="Principal Component", col = "blue")

# barplot
barplot(pc.var.per,main ="Principal Components Plot", xlab = "Principal Components", ylab = "Percentage Variation",ylim = c(0,1)) 


plot(pc.var.per , xlab=" Principal Components ", ylab=" Proportion of
Variance Explained ", ylim=c(0,1) ,type="b")

# Cumulative Proportion of Explained Variance
plot(cumsum (pc.var.per), xlab=" Principal Components ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type="b")




# Making the biplot about PCA

biplot(pca, scale = 0, cex=0.6, main="Biplot")




#-------------------------------------- K-Means Clustering-------------------------------------


# set seed to ensure that we get the same result each time we run the same process.
set.seed(123)



# making the k-means algorithm for 2,3,4 and 5 clusters
# parameters used :
# centers : the number of cluster centers
# iter.max : he maximum number of iterations allowed
# nstart : if centers is a number, how many random sets should be chosen
# Reference : https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kmeans

k2 <- kmeans(data_22, centers = 2, iter.max = 300, nstart = 100)
k3 <- kmeans(data_22, centers = 3, iter.max = 300, nstart = 100)
k4 <- kmeans(data_22, centers = 4, iter.max = 300, nstart = 100)
k5 <- kmeans(data_22, centers = 5, iter.max = 300, nstart = 100)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data_22) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data_22) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data_22) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data_22) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


# summary of 4 clusters
k4



# different plots about k-means clusters to make this procedure more interpretable

k.plot <-as.data.frame(k4$centers)


library(tidyr)
library(ggplot2)
DF <- data.frame(group = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
                 Work = c(k.plot$Work),
                 Hobby = c(k.plot$Hobby),
                 Breath= c(k.plot$Breath),
                 Pain = c(k.plot$Pain),
                 Rest = c(k.plot$Rest),
                 Sleep = c(k.plot$Sleep),
                 Appetite= c(k.plot$Appetite),
                 Naussea = c(k.plot$Nausea),
                 Vomit = c(k.plot$Vomit),
                 Constipated = c(k.plot$Constipated),
                 Diarrhoea= c(k.plot$Diarrhoea),
                 Tired = c(k.plot$Tired),
                 Interfere = c(k.plot$Interfere),
                 Concentrate = c(k.plot$Concentrate),
                 Tense= c(k.plot$Tense),
                 Worry = c(k.plot$Worry),
                 Irritate = c(k.plot$Irritate),
                 Depressed = c(k.plot$Depressed),
                 Memory= c(k.plot$Memory),
                 Family = c(k.plot$Family),
                 Social = c(k.plot$Social),
                 Financial = c(k.plot$Financial))

DFtall <- DF %>% gather(key = measures, value = Value, Work:Financial)
DFtall

ggplot(DFtall, aes(measures, Value, fill = group)) + geom_col(position = "dodge") 



ggplot(DFtall, aes(x=measures, y=Value, group=group, color=group)) 
+geom_line() +geom_point() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
+ggtitle("Plot about clusters mean values for K-Means Clustering") 
+ ylab("Mean value") + xlab("Quality variables") 
+ annotate("text", x=11, y=3.7, label= "Sick Patients") 
+ annotate("text", x=11, y=2, label= "Average Health Patients")
+ annotate("text", x=11, y=1, label= "Healthier Patients")


k.m.count.plot <- data.frame(
  name=c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"),
  val=k4$size
)


k.m.count.plot %>%
  arrange(val) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(name, levels=name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=val)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="red") +
  coord_flip() +
  theme_bw() +
  ylab("Size of Clusters") + xlab("Number of Cluster") 
+ annotate("text", x=1.3, y=30, label= "Less Healthy Patients (Population = 41)",color="tomato2")
+ annotate("text", x=2.3, y=30, label= "Low to Mid Healthy Patients (Population = 60)",color="turquoise3")
+ annotate("text", x=3.3, y=30, label= "Healthiest Patients (Population = 95)",color="purple")
+ annotate("text", x=4.3, y=30, label= "Mid to High Healthy Patients (Population = 96)",color="springgreen4")






# Applying Elbow method for determining the optimal clusters

# parameters estimation : the method to be used for estimating the optimal number of clusters
# "wss" (for total within sum of square) 

set.seed(123)

fviz_nbclust(data_22, kmeans, method = "wss")



set.seed(123)




# To perform PAM clustering with euclidean distance (more like k-means)
# To perform classic K-Medoids with PAM change metric=“manhattan”.
k= 4






# -----------------------------implementing the PAM method-----------------------------

# 1st Way Euclidean Distance
# stand = False as we did not want to standarise the features

clust_pam_eu4 <- pam(data_22, k, metric = "euclidean", stand = FALSE)
pameu4 <- fviz_cluster(clust_pam_eu4, data=X) + ggtitle("Euclidean,k = 4")


clust_pam_eu4

# finding the observations in each group

sum(clust_pam_eu4$clustering ==1) #----> 51 in the first group
sum(clust_pam_eu4$clustering ==2) #----> 105 in the second group
sum(clust_pam_eu4$clustering ==3) #----> 85 in the third group
sum(clust_pam_eu4$clustering ==4) #----> 51 in the fourth group



# Manhattan Distance

clust_pam_manh4 <- pam(data_22, k, metric = "manhattan", stand = FALSE)
pamman4 <- fviz_cluster(clust_pam_manh4, data=X) + ggtitle("Manhattan,k = 4")


# Euclidean Distance k =4

clust_pam_eu2 <- pam(data_22, k=2, metric = "euclidean", stand = FALSE)
pameu2 <- fviz_cluster(clust_pam_eu2, data=X) + ggtitle("Euclidean,k = 2")

# Euclidean Distance k = 3

clust_pam_eu3 <- pam(data_22, k=3, metric = "euclidean", stand = FALSE)
pameu3 <- fviz_cluster(clust_pam_eu3, data=X) + ggtitle("Euclidean,k = 3")


library(gridExtra)
grid.arrange(pameu2, pameu3, pameu4, pamman4, nrow = 2)


# plots for the better understanding of the PAM method results

clust_pam_eu4$clusinfo[,1]


pam.count.plot <- data.frame(
  name=c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"),
  val=clust_pam_eu4$clusinfo[,1]
)


pam.count.plot %>%
  arrange(val) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(name, levels=name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=val)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="red") +
  coord_flip() +
  theme_bw() +
  ylab("Size of Clusters") + xlab("Number of Cluster")
+ annotate("text", x=1.3, y=30, label= "Less Healthy Patients (Population = 51)",color="tomato2")
+ annotate("text", x=4.3, y=30, label= "Low to Mid Healthy Patients (Population = 105)",color="springgreen4")
+ annotate("text", x=2.3, y=30, label= "Healthiest Patients (Population = 51)",color="purple")
+ annotate("text", x=3.3, y=30, label= "Mid to High Healthy Patients (Population = 85)",color="turquoise3")


set.seed(123)

# calculating the silhouette method
# method = "silhouette" for calculating the average silhouette width
fviz_nbclust(data_22, pam, method="silhouette",)+theme_classic()



# ----------------------clara technique was implemented just for exploration---------------------------


# the results was similar with the PAM method so we did not include it
# implementing a special version of PAM, which is the CLARA method
# samples :  the number of samples to be drawn from the dataset
# pamLike : logical indicating if the “swap” phase should implement the same algorithm as pam()

set.seed(123)

clust_clara <- clara(data_22, k, metric = "manhattan", stand = FALSE,
                     samples = 100, pamLike = TRUE)
# To get the medoids run:
mu <- clust_clara$medoids
mu
# to get the objective (optimised) run
clust_clara$objective

# plot for clara
fviz_cluster(clust_clara, data=X)




# -------------------------------------------implementing GMM------------------------------------------




library(mclust) 
set.seed(123)

gmm_fit <- Mclust(data_22)



# # To plot the classification (asignment) of data to cluster run 
plot(gmm_fit, what = "classification") 




set.seed(123)

# For a more extensive summary: 
summary(gmm_fit, parameters = TRUE)



# trying to estimate the best model


BIC <- mclustBIC(data_22)
summary(BIC)

# plot the possible models
plot(BIC)


gmm_fit <- Mclust(data_22, x = BIC)

summary(gmm_fit)


# looking at the means of each group
gmm_fit$parameters$mean

# to get the covariance 
gmm_fit$parameters$variance$sigma

# we used the covariance structure to estimate how the variables correlate within clusters


# this plot contains all the plots about BIC, density, classification, uncertainty
plot(gmm_fit)



# making a plot about GMM means
mix.plot <- cbind(gmm_fit$parameters$mean)
a <- gmm_fit$parameters$mean[,1]
b <- gmm_fit$parameters$mean[,2]
mix.plot <- rbind(a,b)
mix.plot <- as.data.frame(mix.plot)

DF_gmm <- data.frame(group = c("Cluster 1", "Cluster 2"),
                     Work = c(mix.plot$Work),
                     Hobby = c(mix.plot$Hobby),
                     Breath= c(mix.plot$Breath),
                     Pain = c(mix.plot$Pain),
                     Rest = c(mix.plot$Rest),
                     Sleep = c(mix.plot$Sleep),
                     Appetite= c(mix.plot$Appetite),
                     Naussea = c(mix.plot$Nausea),
                     Vomit = c(mix.plot$Vomit),
                     Constipated = c(mix.plot$Constipated),
                     Diarrhoea= c(mix.plot$Diarrhoea),
                     Tired = c(mix.plot$Tired),
                     Interfere = c(mix.plot$Interfere),
                     Concentrate = c(mix.plot$Concentrate),
                     Tense= c(mix.plot$Tense),
                     Worry = c(mix.plot$Worry),
                     Irritate = c(mix.plot$Irritate),
                     Depressed = c(mix.plot$Depressed),
                     Memory= c(mix.plot$Memory),
                     Family = c(mix.plot$Family),
                     Social = c(mix.plot$Social),
                     Financial = c(mix.plot$Financial))

DFtall_gmm <- DF_gmm %>% gather(key = measures, value = Value, Work:Financial)
DFtall_gmm

ggplot(DFtall_gmm, aes(measures, Value, fill = group)) + geom_col(position = "dodge") 



ggplot(DFtall_gmm, aes(x=measures, y=Value, group=group, color=group)) 
+ geom_line() +geom_point() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
+ggtitle("Plot about clusters mean values for GMM Clustering") + ylab("Mean value") 
+ xlab("Quality variables") + annotate("text", x=11, y=3.0, label= "Sick Patients") 
+ annotate("text", x=11, y=2, label= "Average Health Patients")
+ annotate("text", x=11, y=1, label= "Healthier Patients")





# -------------------------------------poLCA implementagition--------------------------------------------------


# we should delete also the NA s for the relationship 

my_data<- na.omit(data)
# now we have 292 observations and 25 variables

my_data$Work <- as.factor(my_data$Work)
my_data$Hobby <- as.factor(my_data$Hobby)
my_data$Breath <- as.factor(my_data$Breath)
my_data$Pain <- as.factor(my_data$Pain)
my_data$Rest <- as.factor(my_data$Rest)
my_data$Sleep <- as.factor(my_data$Sleep)
my_data$Appetite <- as.factor(my_data$Appetite)
my_data$Nausea <- as.factor(my_data$Nausea)
my_data$Vomit <- as.factor(my_data$Vomit)
my_data$Constipated <- as.factor(my_data$Constipated)
my_data$Diarrhoea <- as.factor(my_data$Diarrhoea)
my_data$Tired <- as.factor(my_data$Tired)
my_data$Interfere <- as.factor(my_data$Interfere)
my_data$Concentrate <- as.factor(my_data$Concentrate)
my_data$Worry <- as.factor(my_data$Worry)
my_data$Tense <- as.factor(my_data$Tense)
my_data$Irritate<- as.factor(my_data$Irritate)
my_data$Depressed <- as.factor(my_data$Depressed)
my_data$Memory<- as.factor(my_data$Memory)
my_data$Family <- as.factor(my_data$Family)
my_data$Social<- as.factor(my_data$Social)
my_data$Financial <- as.factor(my_data$Financial)
my_data$Sex<- as.factor(my_data$Sex)
my_data$Relationship<- as.factor(my_data$Relationship)


set.seed(123)


# firstly only for the 22 variables
# nclass : the number of latent classes to assume in the model
# maxiter : the maximum number of iterations through which the estimation algorithm will cycle.
# nrep : the number of times to estimate the model, using different values of probs.start.
# The default is one. Setting nrep>1 automates the search for the global---rather than just 
# a local---maximum of the log-likelihood function

# Reference : https://www.rdocumentation.org/packages/poLCA/versions/1.4.1/topics/poLCA


poLCA_fit2<- poLCA(cbind(Work,Hobby,Breath,Pain,Rest,Sleep,Appetite,Nausea,Vomit,
              Constipated,Diarrhoea,Tired,Interfere,Concentrate,Worry,Tense,Irritate,Depressed,
              Memory,Family,Social,Financial) ~ 1, maxiter=50000, nclass=2, nrep=20, data=my_data)


plot(poLCA_fit2)



# metrics about the model

metrics <- as.data.frame(cbind(2,
                               poLCA_fit2$llik, 
                               poLCA_fit2$bic, 
                               poLCA_fit2$aic))
colnames(metrics) <- cbind('K','Log-Likelihood','BIC','AIC')
metrics




# prediction about the proportion of the classes

round(prop.table(table(poLCA_fit2$predclass)),4)*100

# prediction about the member of the each class

table(poLCA_fit2$predclass)



# Number of classes = 3
set.seed(123)


poLCA_fit3<- poLCA(cbind(Work,Hobby,Breath,Pain,Rest,Sleep,Appetite,Nausea,Vomit,Constipated,
            Diarrhoea,Tired,Interfere,Concentrate,Worry,Tense,Irritate,Depressed,Memory,Family,
            Social,Financial) ~ 1, maxiter=50000, nclass=3, nrep=20, data=my_data)

plot(poLCA_fit3)



metrics3 <- as.data.frame(cbind(2,
                                poLCA_fit3$llik, 
                                poLCA_fit3$bic, 
                                poLCA_fit3$aic))
colnames(metrics3) <- cbind('K','Log-Likelihood','BIC','AIC')
metrics3



# prediction about the proportion of the classes

round(prop.table(table(poLCA_fit3$predclass)),4)*100

# prediction about the member of the each class

table(poLCA_fit3$predclass)



set.seed(123)


K = 4   #number of different classes we would like to test 
# Store the results of the analysis in a matrix
metrics <- matrix(,K,4)
for (k in 1:K){
  poLCA_fit_auto <-  poLCA(cbind(Work,Hobby,Breath,Pain,Rest,Sleep,Appetite,Nausea,Vomit,
                                 Constipated,Diarrhoea,Tired,Interfere,Concentrate,Worry,Tense,
                                 Irritate,Depressed,Memory,Family,Social,Financial) ~ 1, maxiter=50000, 
                           nclass=k, nrep=20, data=my_data)
  metrics[k,] <- c(k, poLCA_fit_auto$llik,poLCA_fit_auto$bic,poLCA_fit_auto$aic)
}



metrics <- as.data.frame(metrics) 
colnames(metrics) <- cbind('K','Log-Likelihood','BIC','AIC') 
print(metrics)



# the correct number of classes is three




# including the covariates


set.seed(123)


poLCA_fit<- poLCA(cbind(Work,Hobby,Breath,Pain,Rest,Sleep,Appetite,Nausea,Vomit,Constipated,Diarrhoea,
                        Tired,Interfere,Concentrate,Worry,Tense,Irritate,Depressed,Memory,Family,Social,
                        Financial) ~ Sex + Age +Relationship, maxiter=50000, nclass=3, nrep=20, data=my_data)


poLCA_fit$bic
poLCA_fit$predclass
plot(poLCA_fit)


metrics <- as.data.frame(cbind(2,
                               poLCA_fit$llik, 
                               poLCA_fit$bic, 
                               poLCA_fit$aic))
colnames(metrics) <- cbind('K','Log-Likelihood','BIC','AIC')
metrics

# prediction about the proportion of the classes

round(prop.table(table(poLCA_fit$predclass)),4)*100

# prediction about the member of the each class

table(poLCA_fit$predclass)






