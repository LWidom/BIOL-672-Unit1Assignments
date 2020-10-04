# Louis Widom
# lpw8274@rit.edu
# Designed in Windows 10
# Last updated 03 October 2020

# List of required packages:
#   ggplot2
#   MASS
#   plyr
#   reshape2
#   grid
#   mixtools
# Associated data files (should be located in the same folder as this script):
#   winequality-white.txt
#   EV_cell_migration.txt

# ======================================================================================

# Begin Script
library('ggplot2')
library('MASS')
library('plyr')
library('reshape2')
library('grid')
library('mixtools')
# Set the working directory to be the same location as this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Generates a dataset containing 5000 random numbers in a Poisson distribution,
# with means at 10, 23, and 47
random_data <- rpois(5000,c(10,23,47));

# Calculate the mean and standard deviation
random_data_mean <- mean(random_data);
random_data_sd <- sd(random_data);

# Prints the sample mean and standard deviation
phrase1 <- paste('The sample mean is',toString(random_data_mean))
phrase2 <- paste('and the sample standard deviation is',toString(random_data_sd))
cat(phrase1,phrase2,'\n')

# Create a file to save the sample mean and standard deviation of the random dataset
# located in the same folder as this script
sink(file = paste(getwd(),'/desc.txt',sep=''))
cat(phrase1,phrase2)
sink()

# Fit a normal distribution to the data
fit <- fitdistr(random_data,"normal")
para <- fit$estimate

# Prepare to save plot as a PDF
pdf("histo.pdf")
# Plots a histogram of the data
hist(random_data,col='red',border='black',prob=TRUE,xlab='Value',main='Random Data')
# Plot density line of the data over the histogram
lines(density(random_data),lwd=2,col='blue')
# Plot the normal fit
curve(dnorm(x,mean=para[1],sd=para[2]),lwd=2,col='green',add=T)
# Add a legend to the plot
legend(40,0.03,legend=c('density line','normal fit'),col=c('blue','green'),lty=1)
# Close the PDF
dev.off()

# =====================================================================================

# Read in "Wine Quality Dataset" available from Machine Learning Mastery
# (available: https://machinelearningmastery.com/standard-machine-learning-datasets/)
wine_data=read.table(paste(getwd(),'/winequality-white.txt',sep=''),header=TRUE)
# Only use a subset of the columns
residual_sugar <- wine_data$residual_sugar
pH <- wine_data$pH
alcohol <- wine_data$alcohol
quality <- wine_data$quality
# Change the "quality" from continuous to categorical data
quality = cut(quality,seq(2,9,1),
              labels=c("very low","low","somewhat low","medium",
                       "somewhat high", "high","very high"))
# Set up data frame
wine.data<-data.frame(residual_sugar,pH,alcohol,quality)

# Perform one-way ANOVA on wine variables, with different quality
# ratings (scale of 1 to 10) treated as categories
residual_sugar.anova <- oneway.test(residual_sugar~quality)
pH.anova <- oneway.test(pH~quality)
alcohol.anova <- oneway.test(alcohol~quality)

# Perform pairwise t tests with Bonferroni and Benjamini-Hochberg correction
residual_sugar.t1 <- pairwise.t.test(residual_sugar,quality,p.adj="bonf")
residual_sugar.t2 <- pairwise.t.test(residual_sugar,quality,p.adj="BH")
pH.t1 <- pairwise.t.test(pH,quality,p.adj="bonf")
pH.t2 <- pairwise.t.test(pH,quality,p.adj="BH")
alcohol.t1 <- pairwise.t.test(alcohol,quality,p.adj="bonf")
alcohol.t2 <- pairwise.t.test(alcohol,quality,p.adj="BH")

# Run Kruskal Wallis tests on this data
residual_sugar.kw <- kruskal.test(residual_sugar~quality)
pH.kw <- kruskal.test(pH~quality)
alcohol.kw <- kruskal.test(alcohol~quality)

# Run correlation tests (Pearson and Spearman) between residual sugar and alcohol 
wine.pearson <- cor.test(x=residual_sugar,y=alcohol,method="pearson")
wine.spear <- cor.test(x=residual_sugar,y=alcohol,method="spearman")

# Run linear regression to examine residual_sugar as a function of alcohol
wine.linear <- lm(residual_sugar~alcohol, data=wine.data)

# Run one sample KS tests on the data
residual_sugar.ks <- ks.test(residual_sugar,"pnorm")
pH.ks <- ks.test(pH,"pnorm")
alcohol.ks <- ks.test(alcohol,"pnorm")

# Create a file to save the the various test results and save it in the same
# folder as this script
sink(file = paste(getwd(),'/wine_results.txt',sep=''))
print(residual_sugar.anova)
print(residual_sugar.kw)
print(residual_sugar.t1)
print(residual_sugar.t2)
print(residual_sugar.ks)
print(pH.anova)
print(pH.kw)
print(pH.t1)
print(pH.t2)
print(pH.ks)
print(alcohol.anova)
print(alcohol.kw)
print(alcohol.t1)
print(alcohol.t2)
print(alcohol.ks)
cat('The one-way ANOVA p-values (p < alpha=0.5) indicate that at least one of the',"\n",
    '"quality" groups likely differs from the rest in terms of the residual_sugar',"\n",
    'pH, and alcohol content. This is confirmed by the pairwise t tests; while not',"\n",
    'all of these p-values are significant, there are at least some below alpha for',"\n",
    'each type of value. The low p-values for the KS tests indicate that none of the',
    "\n",'data is normally distributed. Low p-values from the nonparametric Kruskal-',"\n",
    'Wallis tests were consistent with the results from ANOVA, despite the fact that',"\n",
    'the data is not normally distributed. This indicates that there truly are',"\n",
    'differences between some of the group means. Examining "wine_error_bar.pdf"',"\n",
    'reveals a few interesting trends: in general, the mean residual_sugar is reduced',"\n",
    'in higher quality wines, whereas the mean pH and alcohol content are higher. The',"\n",
    'standard error tends to be larger for wines of the lowest and highest quality,',"\n",
    'indicating that these extreme categories have higher variability.',"\n")
print(wine.pearson)
print(wine.spear)
print(summary(wine.linear))
cat('The p-values for the correlation tests are less than alpha=0.05, indicating',
    "\n",'that a correlation between residual_sugar and alcohol is likely.',"\n",
    'The low p-value for the F-statistic of the linear model indicates that the',"\n",
    'data fits the model better than a model without an independent variable.',"\n",
    'However, the low R-squared value indicates that this is still not a very good',
    "\n",'fit. Regression (not correlation) is appropriate when trying to determine causation.')
sink()

# Melt the data
wine.melt = melt(wine.data,id.vars="quality")
# Calculate mean and standard error from the variables
wine.summary <- ddply(wine.melt,c("quality","variable"),summarise,mean=mean(value),
                      sem=sd(value)/sqrt(length(value)))
# Set up lower and upper bound for error bars
wine.summary <- transform(wine.summary,lower=mean-sem,upper=mean+sem)

# Set up error bar chart
wine_bar <- ggplot(wine.summary,aes(fill=quality,x=variable,y=mean)) + 
  geom_col(position="dodge") + geom_errorbar(aes(ymax=upper,ymin=lower),
                                             position="dodge",
                                             data=wine.summary)
# Save plot as PDF
pdf("wine_error_bar.pdf")
print(wine_bar)
dev.off()

# Make scatterplot comparing residual_sugar and alcohol
wine_scatter <- ggplot(wine.data,aes(x=alcohol,y=residual_sugar,shape=quality,color=quality))+
  geom_point() + scale_shape_manual(values=c(15,16,17,18,25,4,8)) + geom_abline(wine.linear,mapping=aes(slope=coef(wine.linear)["alcohol"], intercept=coef(wine.linear)["(Intercept)"]))

# Save plot as PDF
pdf("wine_scatter_plot.pdf")
print(wine_scatter)
dev.off()

# =====================================================================================

# Read in cell migration data from Dr. Gaborski's lab
migration_data <- read.table(paste(getwd(),'/EV_cell_migration.txt',sep=''),header=TRUE)
condition <- migration_data$Condition
average_speed <- migration_data$avg_s
displacement <- migration_data$displacement
persistence_index <- migration_data$persistence_index
pathlength <- migration_data$pathlength

# Set up data frame
migration.data <- data.frame(condition,average_speed,displacement,persistence_index,pathlength)

# Run MANOVA
Y <- cbind(average_speed,displacement,persistence_index,pathlength)
migration.manova <- manova(Y ~ condition)

# Run multiple regression to see if average_speed is predicted by the other variables
migration.multiple_regression <- lm(average_speed ~ displacement + persistence_index +
                                      pathlength, data=migration.data)

# Run multiple regression again, but only within the Complete_media category
migration.multiple_regression_complete <- lm(average_speed ~ displacement + persistence_index +
                                      pathlength, data=subset(migration.data,condition=='Complete_media'))

# Create a composite variable from average_speed and pathlength
average_time = (pathlength/average_speed)
# Run ANCOVA to see how displacement predicts average_speed while controlling for the average_time
migration.ancova = aov(average_speed ~ displacement*average_time, data=migration.data)

# Omit conditions from dataframe so that it is compatible with PCA
migration.data_numerical <- data.frame(average_speed,displacement,persistence_index,pathlength)
# Run Principle Components Analysis
migration.pca <- prcomp(migration.data_numerical, scale=TRUE)
loading_scoresPC1 <- migration.pca$rotation[,1]
loading_scoresPC2 <- migration.pca$rotation[,2]
loading_scoresPC3 <- migration.pca$rotation[,3]
loading_scoresPC4 <- migration.pca$rotation[,4]
# Create a Scree Plot
migration.pca.var <- migration.pca$sdev^2
migration.pca.var.per <- round(migration.pca.var/sum(migration.pca.var)*100, 1)
pdf("migration_scree_plot.pdf")
barplot(migration.pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
dev.off()

# Perform factor analysis
migration.fa <- factanal(migration.data_numerical,1,rotation="varimax")

# Create scatterplot of average_speed and displacement
migration_scatter <- ggplot(migration.data,aes(x=average_speed,y=displacement,shape=condition,color=condition))+
  geom_point() + scale_shape_manual(values=c(15,16,17,18,25))
# Run K means clustering
migration.kmeans <- kmeans(migration.data_numerical,centers=2)
# Add K means to the numerical dataframe
migration.data_numerical$cluster <- as.character(migration.kmeans$cluster)
# Plot the clusters determined by K means
migration_kmeans_scatter <- ggplot(migration.data_numerical,aes(x=average_speed,y=displacement,color=cluster))+
  geom_point()
# Generate pdf with the two scatterplots
pdf("migration_kmeans.pdf")
pushViewport(viewport(layout = grid.layout(2, 1)))
print(migration_scatter, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(migration_kmeans_scatter, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()

# Fit normal, lognormal, exponential, and Gaussian Mixture Model to average_time
fitNORM <- fitdistr(average_time, densfun="normal")
fitLNORM <- fitdistr(average_time, densfun="log-normal")
fitEXP <- fitdistr(average_time, densfun="exponential")
fitGMM <- normalmixEM(average_time)
fitGMM_loglik <- fitGMM$loglik
# Evaluate fits with Bayesian Information Criterion
BIC_GMM <- -2*fitGMM_loglik+4*log(150)
BICfit <- BIC(fitNORM,fitLNORM,fitEXP)
# Plot histograms to demonstrate the fit of each model
model_plot1 <-ggplot(migration.data, aes(x=average_time)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dnorm, color="red", args=list(mean = fitNORM$estimate[1], sd = fitNORM$estimate[2])) 
model_plot2 <-ggplot(migration.data, aes(x=average_time)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dlnorm, color="red", args=list(meanlog = fitLNORM$estimate[1], sdlog = fitLNORM$estimate[2])) 
model_plot3 <-ggplot(migration.data, aes(x=average_time)) + geom_histogram(aes(y=..density..)) + geom_density() + stat_function(fun=dexp, color="red", args=list(rate = fitEXP$estimate[1])) 
model_plot4 <-ggplot(migration.data, aes(x=average_time)) + geom_histogram(aes(y=2*(..density..))) + geom_density(aes(y=2*(..density..))) + stat_function(fun=dnorm, color="red", args=list(mean = fitGMM$mu[1], sd = fitGMM$sigma[1])) + stat_function(fun=dnorm, color="red", args=list(mean = fitGMM$mu[2], sd = fitGMM$sigma[2])) 
pdf("migration_model_fit_histograms.pdf")
pushViewport(viewport(layout = grid.layout(2, 2)))
print(model_plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(model_plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(model_plot3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(model_plot4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
dev.off()

# Create a file to save the the various test results and interpretation and save it in the same
# folder as this script
sink(file = paste(getwd(),'/migration_results.txt',sep=''))
print("MANOVA results:")
print(summary(migration.manova,test="Pillai"))
cat('The low Pr value (< alpha=0.05) indicates that the differences between category \n',
    'means are significant. \n')
print("Multiple regression results:")
print(summary(migration.multiple_regression))
cat('Displacement appears to be a good predictor for average_speed, based on its low \n',
    'Pr value. The other independent variables have Pr values over alpha=0.05, so they \n',
    'are less likely to be good predictors of average_speed. \n')
print("Multiple regression results within complete_media category:")
print(summary(migration.multiple_regression_complete))
cat('Interestingly, the best predictor for average_speed changes when we only examine \n',
    'data within the complete_media category. Displacement no longer has a significant \n',
    'Pr value, but pathlength does. This indicates that pathlength is a better predictor \n',
    'of average_speed within this category. \n')
print("ANCOVA results:")
print(summary(migration.ancova))
cat('As seen in the earlier MANOVA test, ANCOVA also indicates that displacement is a \n',
    'good predictor of average_speed. This is still the case when we control for the \n',
    'composite variable average_time since the Pr value remains below the significance \n',
    'level of alpha=0.05. \n')
print("Loading scores from Principle Components Analysis")
print(loading_scoresPC1)
print(loading_scoresPC2)
print(loading_scoresPC3)
print(loading_scoresPC4)
cat('Based on the Scree plot (visible in \"migration_scree_plot.pdf"), about 70% of the \n',
    'variance is explained by PC1 and around 20% is explained by PC2. PCs 3 and 4 explain \n',
    'about 10% of the variance together. None of the PCs have all positive loadings. \n',
    'Interestingly, all of the loadings for PC1 are negative. This indicates \n',
    'that the variables have an inverse relationship with the component. Furthermore, \n',
    'the loadings are fairly similar for all of the variables (between -0.6 and -0.4), \n',
    'so their impact on PC1 is relatively equal. With that said, it can be argued that \n',
    'displacement has the largest impact since the absolute value of its loading score \n',
    'is the largest out of the group. Pathlength has the largest impact on PC2, \n',
    'average_speed has the largest impact on PC3, and displacement has the largest impact \n',
    'on PC4. \n')
print("Factor Analysis")
print(migration.fa,digits=2,cutoff=.3,sort=TRUE)
cat('There were not enough variables to analyze more than one factor. The low p-value \n',
    '(< alpha=0.05) indicates that this model does not fit the data perfectly. 65% of \n',
    'the variance is explained by the single factor. The uniqueness for each variable \n',
    'indicates the amount of variance that is not explained by the factor. It is worth \n',
    'noting that all of the variance in displacement is explained by the factor. This \n',
    'is reflected by displacement\'s high loading (1.0). There are not enough factors \n',
    'to determine if there are latent underlying factors in the data. More input variables \n',
    'may be required. \n')
print("BIC for normal, log-normal, and exponential model fits")
print(BICfit)
print ("BIC for GMM")
print(BIC_GMM)
cat('Of the models tested, the normal distribution appears to describe the average_time','\n',
    'composite variable the best. This can be determined by looking at the BIC values', '\n',
    'which are smallest for the normal distribution. Since GMM does not have the best fit,','\n',
    'no latent factors are indicated.')
sink()