# Louis Widom
# lpw8274@rit.edu
# Designed in Windows 10
# Last updated 10 September 2020

# List of required packages:
#   ggplot2
#   MASS
#   plyr
#   reshape2
# Associated data files (should be located in the same folder as this script):
#   winequality-white.txt

# Begin Script
library('ggplot2')
library('MASS')
library('plyr')
library('reshape2')
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
cat(phrase1,phrase2)

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