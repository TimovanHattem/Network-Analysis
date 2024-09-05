###### Borderline Personality Disorder Checklist Project ######
# By Emanuela Zhecheva, Timo van Hattem and Maartje Alting



## Load and shape the data ##
# Load data from Richetin et al (2017)
BPDC_Project <- read.table("/Users/maartjealting/Documents/BPDC_Project.csv", sep=",", header=TRUE) # fill in own directory

# Delete non-existing subject numbers
BPDC_Project <- BPDC_Project[,-1]


## Mean imputation for missing data (NA) ##

# Find where the data is missing (for all columns except 'gender', 'age' and 'clinical')
BPDC_Project_ind_NA  <- is.na(BPDC_Project[,c(1:50)])

# Find the location of the NA's
Location_NA <- which(BPDC_Project_ind_NA == TRUE, arr.ind=TRUE)

# Found 19 NA's in 15 columns: 6,7,11,12,16,19,26,28,29,34,37,38,41,42,47
# Do the mean imputation for those columns
for (val in Location_NA[,2]) {
  BPDC_Project[is.na(BPDC_Project[,val]),val] <- round(mean(BPDC_Project[,val], na.rm = TRUE))
}

## Collapsing the 47 questions to 9 nodes ##

# Average the questions to the 9 nodes 
# The questions that belong to each node are given in the column names: e.g., BPDC_imp_1 means that it belongs to IMPulsivity

# (I have written de column numbers of the questions that belong to each node, e.g., question 1 has column number 4)
# Efforts to avoid abandonment (ABA): 16, 21, 24, 30, 31, 46, 48 (7)
# Unstable relationships (REL): 35, 43, 45 (3)
# Identity disturbance (IDE): 10, 13, 17, 18, 23, 33, 37, 49 (8)
# Impulsivity (IMP): 4, 11, 15, 20, 25, 27, 32, 38, 44 (9)
# Para(Suicidal behaviour (SUI): 9, 12, 29 (3)
# Affective instability (AFF): 5, 28, 36, 39 (4)
# Difficulty controlling anger (ANG): 6, 8, 40, 47 (4)
# Dissociation and paranoid ideation (DIS): 7, 19, 22, 26, 34, 41, 42, 50 (8)
# Chronic feelings of emptiness (EMP): 14 (1)

# Create empty data frame
BPDC_Project_nodes <- setNames(data.frame(matrix(ncol = 10, nrow = 96)), c("Gender", "ABA", "REL", "IDE", "IMP", "SUI", "AFF", "ANG", "DIS", "EMP"))

# Average the columns belonging to the questions of each node together
BPDC_Project_nodes$Gender <- BPDC_Project$gender # and add gender, because we use gender as a moderator
BPDC_Project_nodes$ABA <- round(rowMeans(BPDC_Project[,c(16,21,24,30,31,46,48)]))
BPDC_Project_nodes$REL <- round(rowMeans(BPDC_Project[,c(35,43,45)]))
BPDC_Project_nodes$IDE <- round(rowMeans(BPDC_Project[,c(10, 13, 17, 18, 23, 33, 37, 49)]))
BPDC_Project_nodes$IMP <- round(rowMeans(BPDC_Project[,c(4, 11, 15, 20, 25, 27, 32, 38, 44)]))
BPDC_Project_nodes$SUI <- round(rowMeans(BPDC_Project[,c(9, 12, 29)]))
BPDC_Project_nodes$AFF <- round(rowMeans(BPDC_Project[,c( 5, 28, 36, 39)]))
BPDC_Project_nodes$ANG <- round(rowMeans(BPDC_Project[,c(6, 8, 40, 47)]))
BPDC_Project_nodes$DIS <- round(rowMeans(BPDC_Project[,c(7, 19, 22, 26, 34, 41, 42, 50)]))
BPDC_Project_nodes$EMP <- BPDC_Project[,14]

# We decided to mean-center the variables as Haslbeck suggests in his blog about MNM's. 
# This makes interpretation more straightforward later on (read https://jonashaslbeck.com/Moderated-Network-Models-for-continuous-data/)
BPDC_Project_nodes_mean <- setNames(data.frame(matrix(ncol = 10, nrow = 96)), c("Gender", "ABA", "REL", "IDE", "IMP", "SUI", "AFF", "ANG", "DIS", "EMP"))
BPDC_Project_nodes_mean$Gender <- BPDC_Project$gender 
BPDC_Project_nodes_mean$ABA <- BPDC_Project_nodes$ABA - round(mean(BPDC_Project_nodes$ABA))
BPDC_Project_nodes_mean$REL <- BPDC_Project_nodes$REL - round(mean(BPDC_Project_nodes$REL))
BPDC_Project_nodes_mean$IDE <- BPDC_Project_nodes$IDE - round(mean(BPDC_Project_nodes$IDE))
BPDC_Project_nodes_mean$IMP <- BPDC_Project_nodes$IMP - round(mean(BPDC_Project_nodes$IMP))
BPDC_Project_nodes_mean$SUI <- BPDC_Project_nodes$SUI - round(mean(BPDC_Project_nodes$SUI))
BPDC_Project_nodes_mean$AFF <- BPDC_Project_nodes$AFF - round(mean(BPDC_Project_nodes$AFF))
BPDC_Project_nodes_mean$ANG <- BPDC_Project_nodes$ANG - round(mean(BPDC_Project_nodes$ANG))
BPDC_Project_nodes_mean$DIS <- BPDC_Project_nodes$DIS - round(mean(BPDC_Project_nodes$DIS))
BPDC_Project_nodes_mean$EMP <- BPDC_Project_nodes$EMP - round(mean(BPDC_Project_nodes$EMP))


## Inspecting data ##

# Check the marginal distribution of the variables
# 3-way (moderated) interactions are more sensitive to extreme values, thus we have to check the distributions
par(mfrow=c(3,3))
for(i in 3:11) {
  barplot(table(BPDC_Project_nodes[, i]), axes = FALSE, xlim = c(0,5), xlab = "", ylim = c(0, 30))
  axis(2, las = 2, c(0, 1000, 2000, 3000))
  title(main = colnames(BPDC_Project_nodes)[i])
}
# We do not see nice normal distributions but right and left skewed marginal distributions, 
# most likely violating the assumption of the MNM's that all variables are conditionally Gaussian. 

# To solve this we can transform the data, but this makes interpretation more difficult. 
# We chose to use the non-transformed variables, and check later the reliability, as is also suggested in the Haslbeck blog


## Estimate the model ##

# library
library(mgm) 
library(qgraph)

# set seed (mgm very sensitive)
set.seed(1)

mgm_mod <- mgm(data = BPDC_Project_nodes_mean,
               type = c("c", rep("g", 9)), # Gender is categorical ('c'), but we treat the other nodes (5-point Likert scale)
                                           # as continuous data ('g')
               level = c(2, rep(1,9)), # The categorical variable has 2 levels, for continuous data we state a 1
               lambdaSel = "CV", # EBIC is more conservative, but we think it might be too conservative for our low power study. 
                                 # Therefore, for our study we choose cross validation (CV).
               ruleReg = "OR", # We choose OR instead of AND, because we don't expect a 'return' effect from the variables to gender
               moderators = 1, # Column 1 is the moderator (gender)
               threshold = "LW", # We think it is to lenient not to use a threshold ("none") - too many false positives
                                 # We use the Low and Wainwright (LW, 2013) threshold, which was the most commonly used threshold in 
                                 # the previous versions. The HW is too strict for our low power study, we use it to check if any  
                                 # moderation effects survive the higher threshold (are stronger)
               pbar = TRUE,      # show a progress bar
               binarySign = TRUE) # As discussed with Tessa in showInteraction() our Sign 'is undefined', Jonas suggested adding this,
                                  # but it does not work.

## Show the pairwise and moderated interactions ##

mgm_mod$interactions$indicator
# 21 pairwise interactions found
# when using threshold = "none": 15 moderation effects
# when using threshold = "LW": 7 moderation effects
# when using threshold = "HW": 1 moderation effects (between ABA and IDE)

# Show pair-wise interaction effects
showInteraction(object = mgm_mod, int = c(2,4)) # fill in between which pair, e.g., 2 (ABA) and 4 (IDE)

# Moderation effect
showInteraction(object = mgm_mod, int = c(1,2,4))
showInteraction(object = mgm_mod, int = c(1,2,6))
showInteraction(object = mgm_mod, int = c(1,2,10))
showInteraction(object = mgm_mod, int = c(1,3,5))
showInteraction(object = mgm_mod, int = c(1,4,7))
showInteraction(object = mgm_mod, int = c(1,5,6))
showInteraction(object = mgm_mod, int = c(1,7,8))
# We do not get the output of Sign, which makes our interpretation more difficult (discussed with Tessa and Jonas, no solution found, 
# discussed that we could continue without Sign)

## Plot the Networks ##

# Separate the networks for each gender
cond0 <- condition(object = mgm_mod, 
                   values = list('1' = 0))
cond1 <- condition(object = mgm_mod, 
                   values = list('1' = 1))

# Find maximum value in both networks
l_cond <- list(cond0,cond1)
max_val <- max(max(l_cond[[1]]$pairwise$wadj),
               max(l_cond[[2]]$pairwise$wadj)) #0.49

# Create name vectors
names = c("Gender", "ABA", "REL", "IDE", "IMP", "SUI", "AFF", "ANG", "DIS", "EMP")

# Create two places for both plots
par(mfrow=c(1,2))

# Create network 1 (male)
network_1 <- qgraph(cond0$pairwise$wadj, 
                    edge.color=l_cond[[1]]$pairwise$edgecolor,
                    labels = names,
                    groups = c(1:10),
                    palette = "pastel",
                    maximum = max_val, 
                    edge.labels = TRUE, 
                    edge.label.cex=1.5,
                    edge.label.margin = 0.01,
                    edge.label.color = "black",
                    title = "Gender = Male",
                    title.cex = 1.5,
                    bg = "transparent"
                    )
# Save lay-out for network 2 (female)
layout_1 <- network_1$layout # to compare

network_2 <- qgraph(cond1$pairwise$wadj, 
                    layout = layout_1,
                    edge.color=l_cond[[2]]$pairwise$edgecolor, 
                    labels = names, 
                    groups = c(1:10),
                    palette = "pastel",
                    maximum = max_val, 
                    edge.labels = TRUE, 
                    edge.label.cex=1.5,
                    edge.label.margin = 0.01,
                    edge.label.color = "black",
                    title = "Gender = Female",
                    title.cex = 1.5,
                    bg = "transparent"
                    )

# We see that in the networks a red connection is shown, indicating that even
# without the Sign output in the showInteraction() function, we can interpret the 
# networks


## Assessing stability of the network by resampling ##

set.seed(1)

# Do the resampling
res_obj <- resample(object = mgm_mod, 
                    data = BPDC_Project_nodes_mean, 
                    nB = 90,
                    pbar = TRUE)

# Plot the stability of the pairwise and moderation effects
plotRes(res_obj, 
        axis.ticks = c(-.6, -.5,-.4,-.3,-.3,-.1, 0, .1, .2, .3, .4, .5,.6), 
        axis.ticks.mod = c(-.6,-.5,-.4,-.3,-.2,-.1, 0, .1,.2,.3,.4,.5,.6), 
        cex.label = 1, 
        labels = names, 
        layout.width.labels = .40)
# The model is not stable at all, we can see this in the large confidence intervals. 
# This is probably the case because of our small sample size.


## Centrality metrics ##

centralityPlot(mgm_mod$pairwise$wadj, include = c("Betweenness", "Closeness","ExpectedInfluence")) # for both networks combined, not relevant for RQ 

centralityPlot(network_1, include = c("Betweenness", "Closeness","ExpectedInfluence"))
centralityPlot(network_2, include = c("Betweenness", "Closeness","ExpectedInfluence"))


