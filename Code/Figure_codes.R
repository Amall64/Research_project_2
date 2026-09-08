install.packages("readxl")
library(spatstat)
library(readxl)

#Load data
Random_data <- read.csv("Random_50.csv")  #data from fiji images 

Clustered_data <- read.csv("Clustered_50.csv")

Even_data <- read.csv("Even_50.csv") 

#Create PPP objects

Random_pattern <- ppp(
  x = Random_data[, 6], #6th column in csv
  y = Random_data[, 7], #7th column in csv
  xrange = c(22.5, 933.5),  #min and max of each co-ordinate 
  yrange = c(14.5, 584.5)
)
plot(Random_pattern, main = "Random pattern", cols = "black", border = "black", pch = 16)


Clustered_pattern <- ppp(
  x = Clustered_data[, 6],
  y = Clustered_data[, 7],
  xrange = c(47, 945),
  yrange = c(52, 543)
)
plot(Clustered_pattern, main = "Clustered pattern", cols = "black", border = "black", pch = 16)



Even_pattern <- ppp(
  x = Even_data[, 6],
  y = Even_data[, 7],
  xrange = c(55, 460),
  yrange = c(55, 415)
)
plot(Even_pattern, main = "Even pattern", cols = "black", border = "black", pch = 16)



#you can check each summary of each point pattern 

#Ripley's K function 

plot(Kest(Random_pattern),    main = "K function - random")
plot(Kest(Clustered_pattern), main = "K function - clustered")
plot(Kest(Even_pattern),      main = "K function - even")

#Ripley's L function 

L_random    <- Lest(Random_pattern)
L_clustered <- Lest(Clustered_pattern)
L_even      <- Lest(Even_pattern)

plot(L_random,    . - r ~ r, main = "L function - random")
plot(L_clustered, . - r ~ r, main = "L function - clustered")
plot(L_even,      . - r ~ r, main = "L function - even")

#Monte carlo envelopes (999)

env_random    <- envelope(Random_pattern,    Lest, nsim = 999, global = TRUE)
env_clustered <- envelope(Clustered_pattern, Lest, nsim = 999, global = TRUE)
env_even      <- envelope(Even_pattern,      Lest, nsim = 999, global = TRUE)

plot(env_random,    . - r ~ r, main = "Envelope - random")
plot(env_clustered, . - r ~ r, main = "Envelope - clustered")
plot(env_even,      . - r ~ r, main = "Envelope - even")



#Adding class labels and creating new columns in each pp df
#the random class, clustered class and even class labels 


Clustered_data$Class_random    <- sample(c(0, 1), nrow(Clustered_data), replace = TRUE)
Clustered_data$Class_Clustered <- ifelse(Clustered_data$X < 496 & Clustered_data$Y > 199, 1, 0)
Clustered_data$Class_even      <- rep_len(c(0, 1), length.out = nrow(Clustered_data)) #to get a repeating pattern of 0,1 for 64 points


Random_data$Class_random    <- sample(c(0, 1), nrow(Random_data),    replace = TRUE)
Random_data$Class_clustered <- ifelse(Random_data$X > 478, 1, 0)
Random_data$Class_even      <- rep_len(c(0, 1), length.out = nrow(Random_data))


Even_data$Class_random      <- sample(c(0, 1), nrow(Even_data),      replace = TRUE)
Even_data$Class_clustered <- ifelse(Even_data$X > 300 & Even_data$Y > 50, 1, 0)
Even_data$Class_even      <- rep_len(c(0, 1), length.out = nrow(Even_data))

write.csv(Even_data, "distribution_0s_1s.csv", row.names = FALSE)

#Lcross() 
#First 0,1 are converted into factors for spatstats to know the labels/categories 
Clustered_pattern_randclass <- ppp(Clustered_data[,6], Clustered_data[,7], c(47,945), c(52,543), marks = as.factor(Clustered_data$Class_random))
Clustered_pattern_clustclass <- ppp(Clustered_data[,6], Clustered_data[,7], c(47,945), c(52,543), marks = as.factor(Clustered_data$Class_Clustered))
Clustered_pattern_evenclass <- ppp(Clustered_data[,6], Clustered_data[,7], c(47,945), c(52,543), marks = as.factor(Clustered_data$Class_even))

Random_pattern_randclass    <- ppp(Random_data$X, Random_data$Y, c(22.5,933.5), c(14.5,584.5), marks = as.factor(Random_data$Class_random))
Random_pattern_clustclass   <- ppp(Random_data$X, Random_data$Y, c(22.5,933.5), c(14.5,584.5), marks = as.factor(Random_data$Class_clustered))
Random_pattern_evenclass    <- ppp(Random_data$X, Random_data$Y, c(22.5,933.5), c(14.5,584.5), marks = as.factor(Random_data$Class_even))

Even_pattern_randclass    <- ppp(Even_data$X, Even_data$Y, c(55, 460), c(55, 415), marks = as.factor(Even_data$Class_random))
Even_pattern_clustclass  <- ppp(Even_data$X, Even_data$Y, c(55, 460), c(55, 415), marks = as.factor(Even_data$Class_clustered))
Even_pattern_evenclass  <- ppp(Even_data$X, Even_data$Y, c(55, 460), c(55, 415), marks = as.factor(Even_data$Class_even))


#Run Lcross on each marked pp, test 9 combinations 
LRR <- Lcross(Random_pattern_randclass, i="0", j="1")
plot(LRR, . - r ~ r)
LRC <- Lcross(Random_pattern_clustclass, i="0", j="1")
plot(LRC, . - r ~ r)
LRE <- Lcross(Random_pattern_evenclass, i="0", j="1")
plot(LRE, . - r ~ r)
LCR <- Lcross(Clustered_pattern_randclass, i="0", j="1")
plot(LCR, . - r ~ r)
LCC <- Lcross(Clustered_pattern_clustclass, i="0", j="1")
plot(LCC, . - r ~ r)
LCE <- Lcross(Clustered_pattern_evenclass, i="0", j="1")
plot(LCE, . - r ~ r)
LER <- Lcross(Even_pattern_randclass, i="0", j="1")
plot(LER, . - r ~ r)
LEC <- Lcross(Even_pattern_clustclass, i="0", j="1")
plot(LEC, . - r ~ r)
LEE <- Lcross(Even_pattern_evenclass, i="0", j="1")
plot(LEE, . - r ~ r)


#Envelopes for each combo

#Random pattern
env_LRR <- envelope(Random_pattern_randclass,  Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LRR, . - r ~ r, main = "Envelope Lcross - Random pattern, random labels")

env_LRC <- envelope(Random_pattern_clustclass, Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LRC, . - r ~ r, main = "Envelope Lcross - Random pattern, clustered labels")

env_LRE <- envelope(Random_pattern_evenclass,  Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LRE, . - r ~ r, main = "Envelope Lcross - Random pattern, even labels")

#Clustered pattern
env_LCR <- envelope(Clustered_pattern_randclass,  Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LCR, . - r ~ r, main = "Envelope Lcross - Clustered pattern, random labels")

env_LCC <- envelope(Clustered_pattern_clustclass, Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LCC, . - r ~ r, main = "Envelope Lcross - Clustered pattern, clustered labels")

env_LCE <- envelope(Clustered_pattern_evenclass,  Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LCE, . - r ~ r, main = "Envelope Lcross - Clustered pattern, even labels")

#Even pattern
env_LER <- envelope(Even_pattern_randclass,  Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LER, . - r ~ r, main = "Envelope Lcross - Even pattern, random labels")

env_LEC <- envelope(Even_pattern_clustclass, Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LEC, . - r ~ r, main = "Envelope Lcross - Even pattern, clustered labels")

env_LEE <- envelope(Even_pattern_evenclass,  Lcross, i="0", j="1", nsim=999, global = TRUE)
plot(env_LEE, . - r ~ r, main = "Envelope Lcross - Even pattern, even labels")

#using a positive control to test this method 

set.seed(42)
n0 <- 50  #generating the 0 class points 
class0_x <- runif(n0, min = 50, max = 450)
class0_y <- runif(n0, min = 50, max = 450)

#generating class 1 near 0 points 
offset_x <- rnorm(n0, mean = 0, sd = 15)
offset_y <- rnorm(n0, mean = 0, sd = 15)

class1_x <- class0_x + offset_x
class1_y <- class0_y + offset_y

all_x <- c(class0_x, class1_x)
all_y <- c(class0_y, class1_y)
labels <- factor(c(rep("0", n0), rep("1", n0)))


true_attraction_pattern <- ppp(all_x, all_y, xrange = c(0, 500), yrange = c(0, 500), marks = labels)
env_true <- envelope(true_attraction_pattern, Lcross, i = "0", j = "1", nsim = 999, global = TRUE)
plot(env_true, . - r ~ r, main = "Envelope Lcross - TRUE attraction (positive control)")


#trying this on the edu dataset ########################################
set.seed(42)
#load and read file into a tibble 
EdU_data <- read_excel("EdU_analysis.xlsx")
EdU_data <- as.data.frame(EdU_data) #turns into df 

image_ids <- sort(unique(EdU_data$ImageID)) #sorts the 26 images 

#storage (lists) of all images across all thresholds 
#each image will be looped in the same sequence as above steps ^^^
Lcross_EdU_results <- list()
envelope_EdU_results <- list()
summary_EdU_rows <- list()

#some images may have too few cells for the envelope, this can be changed. 
min_EdU_cells <- 5  #EdU specifically 
min_total <- 15 #regardless of EdU

for (img in image_ids) {
  sub <- EdU_data[EdU_data$ImageID == img, ]  #filters the table to just image rows 
  #boundaries for this dataset is unknown so window is made from co-ords 
  win <- convexhull.xy(sub$X, sub$Y)
  
  #runs the sequence (ppp, l function, 999 ect...) on both high and low thresholds 
  for (thresh in c("High", "Low")) {
    
    mark_col <- if (thresh == "High") "EdU_Pos_High_threshold" else "EdU_Pos_Low_threshold"
    
    n_pos <- sum(sub[[mark_col]] == 1, na.rm = TRUE) #counts how many edu + cells there are 
    n_tot <- nrow(sub)   #total cell count 
    
    key <- paste0("Image", img, "_", thresh)  #each result will need a label 
    
    if (n_pos < min_EdU_cells || n_tot < min_total) {  #if there isnt enough edu+ cells for a test 
      summary_EdU_rows[[key]] <- data.frame(ImageID = img, Threshold = thresh,  
                                        n_total = n_tot, n_EdUpos = n_pos,  #records what was skipped (if there wasnt enough)
                                        result = "skipped - too few cells")
      next
    }
    
    EdU_pattern <- ppp(sub$X, sub$Y, window = win,    #creates ppp object 
                   marks = as.factor(sub[[mark_col]]))
    
    #Lcross - EdU+ cells vs EdU+ cells (i = j = "1")
    L_obs <- Lcross(EdU_pattern, i = "1", j = "1")  #calculates the L function comparing EdU+ cells to others 
    Lcross_EdU_results[[key]] <- L_obs  #stores results 
    
    #999 envelope  
    #null hypothesis
    env <- envelope(EdU_pattern, Lcross, i = "1", j = "1",    #randomises 0s and 1s across all positions 
                    simulate = expression(rlabel(EdU_pattern)),  
                    nsim = 999, global = TRUE)
    envelope_EdU_results[[key]] <- env   #stores the results 
    
    #plot
    png(filename = paste0("EdU_plot_", key, ".png"), width = 1200, height = 900, res = 150)
    plot(env, . - r ~ r,
         main = paste0("Image ", img, " (", thresh, " threshold) - EdU+ vs EdU+"))
    dev.off()
    
    #classify result: does observed exceed the envelope at any r?
    clustered <- any((env$obs - env$r) > (env$hi - env$r))  #checks if the curve goes above the envelope (more clustered than random)
    dispersed <- any((env$obs - env$r) < (env$lo - env$r))  #check if curve goes below envelope 
    
    result <- if (clustered && !dispersed) {
      "clustered"
    } else if (dispersed && !clustered) {
      "dispersed"
    } else if (clustered && dispersed) {
      "mixed"
    } else {
      "no deviation"
    }
    
    summary_EdU_rows[[key]] <- data.frame(ImageID = img, Threshold = thresh,
                                      n_total = n_tot, n_EdUpos = n_pos,
                                      result = result)
  }
}

summary_df <- do.call(rbind, summary_EdU_rows) #stacks each result in a table 
rownames(summary_df) <- NULL
print(summary_df)

write.csv(summary_df, "EdU_realdata_summary.csv", row.names = FALSE)

#Tally per threshold
table(summary_df$Threshold, summary_df$result)

save.image("EdU_realdata_analysis.RData")






