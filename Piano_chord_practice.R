########## PART 1: Constructing the model ########################################################################################################################################################



tonics <- list("A","Bb","B","C","Db","D","Eb","E","F","Gb","G","Ab") # All tonic notes.

bases <- list("maj", "min", "dim") # Setting possible chord bases to 'maj', 'min', and 'dim'.

extensions <- list("none", "dom7", "maj7") # Setting possible chord extensions to 'none', 'dom7', and 'maj7'.



tabulate_possible_chords <- function(tonics, bases, extensions){
  
  table <- data.frame(tonic = character(0), base = character(0), ext = character(0))
  
  for (i in tonics){
    for (x in bases){
      for (y in extensions){
        table <- rbind(table, data.frame(tonic = i, base = x, ext = y))
      }
    }
  }
  table <- table[!(table$base == "dim" & table$ext %in% c("dom7", "maj7")),] # Removing the combinations of "dim" and "dom7" or "maj7", as these do not form commonly observed chords.
  return(table)
} # Creating a table of all of the possible chords, given the chord components above.

train <- tabulate_possible_chords(tonics, bases, extensions)

possible_chords <- train # For use throughout script.


for (i in 1:nrow(train)){
  train$success[i] <- sample(c(0, 1), size = 1)
} # Assigning 'success' of chord playing trial in a new column named 'success', sampling randomly from c(0,1), i.e., with p(success) = 0.5.

train <- data.frame(lapply(train, as.factor))

model <- glm(success ~ ., data = train, family = binomial) # Training logistic regression model on dataset, where chord components represent predictors of chord trial success. In this model, there should be no relationship between predictor and response variables, as 'success' was assigned randomly.

chords <- c() # Creating a table displaying each possible chord and its predicted likelihood of trial success, as estimated by logistic regression.
likelihoods <- c()

for (i in 1:nrow(possible_chords)){
  newdata <- data.frame(tonic = possible_chords[i,]$tonic, base = possible_chords[i,]$base, ext = possible_chords[i,]$ext)
  chords <- append(chords, paste(possible_chords[i,]$tonic, possible_chords[i,]$base, possible_chords[i,]$ext))
  likelihoods <- append(likelihoods, predict(model, newdata = newdata, type = "response"))
} # Returning each chord's success likelihood.

results <- data.frame(chord = chords, likelihood = likelihoods) # Naming said table 'results'.

results <- results[order(results$likelihood), ] # Sorting by ascending likelihood.



########## PART 2: Now let's train the model on just a subset of the possible chords #############################################################################################################



get_calibration_chords <- function(n){
  
  if (n < length(tonics) + length(bases) + length(extensions) | n > nrow(possible_chords)){
    stop(paste("Please enter a number between", (length(tonics) + length(bases) + length(extensions)), "and", nrow(possible_chords)))
  } else {
  }
  
  minimum_components <- data.frame(tonic = character(0), base = character(0), ext = character(0))
  additional_random_chords <- data.frame(tonic = character(0), base = character(0), ext = character(0))
  
  for (i in tonics){
    minimum_components <- rbind(minimum_components, possible_chords[possible_chords$tonic == i,][sample(nrow(possible_chords[possible_chords$tonic == i,]), 1),])
  }
  for (i in bases){
    minimum_components <- rbind(minimum_components, possible_chords[possible_chords$base == i,][sample(nrow(possible_chords[possible_chords$base == i,]), 1),])
  }
  for (i in extensions){
    minimum_components <- rbind(minimum_components, possible_chords[possible_chords$ext == i,][sample(nrow(possible_chords[possible_chords$ext == i,]), 1),])
  } 
  
  # 18 chords needed to provide minimum chord components. Note that more complex algorithm could cut this down to just 12 (number of tonics, with other components overlapped), however, in the interest of expediency, this was not done here.
  
  additional_random_chords <- rbind(additional_random_chords, possible_chords[sample(1:nrow(possible_chords), (n-18)),])
  
  return(rbind(minimum_components, additional_random_chords))
  
} # Creating function to return a subset of the possible chords for calibration, where n = the desired number of chords to test in calibration (minimum input is set at the minimum number of chords required to cover testing all chord components. Maximum input is set at the number of all possible chords). This function is more complex than simply taking a subset of the possible chords, (continued below)...
                                           # (continued)... as we need to account for the possible condition wherein a component of any subsequently tested chord was not included during calibration. This would result in the model being unable to predict an output, as it would be handling a 'new' level.

train <- get_calibration_chords(50) # Establishing training set for calibration as a subset of all possible chords.

for (i in 1:nrow(train)){
  train$success[i] <- sample(c(0, 1), size = 1)
} # Again, randomly assigning 'success' of each chord playing trial.

train <- data.frame(lapply(train, as.factor))

model <- glm(success ~ ., data = train, family = binomial) 


chords <- c() # Again, creating a table displaying each possible chord and its predicted likelihood of trial success.
likelihoods <- c()

for (i in 1:nrow(possible_chords)){
  newdata <- data.frame(tonic = possible_chords[i,]$tonic, base = possible_chords[i,]$base, ext = possible_chords[i,]$ext)
  chords <- append(chords, paste(possible_chords[i,]$tonic, possible_chords[i,]$base, possible_chords[i,]$ext))
  likelihoods <- append(likelihoods, predict(model, newdata = newdata, type = "response"))
} # Returning each chord's success likelihood.

results <- data.frame(chord = chords, likelihood = likelihoods) # Naming said table 'results'.

results <- results[order(results$likelihood), ] # Sorting by ascending likelihood.



########## PART 3: Now let's implement differential chord success likelihoods as determined additively by separate chord components ###############################################################



train <- get_calibration_chords(50) # Constructing a new calibration training set.

simulate_trials <- function(df){
  for (i in 1:nrow(df)){
    if (df[i,]$tonic %in% c("A", "B", "C", "D", "E", "F")){ ### I.e. if the tonic is a white note, success likelihood is higher.
      tonic_difficulty <- 1
    } else {
      tonic_difficulty <- 2
    }
    if (df[i,]$base == "maj"){
      base_difficulty <- 1
    } else if (df[i,]$base == "min"){
      base_difficulty <- 2
    } else if (df[i,]$base == "dim"){ # Diminished chords are generally considerably more difficult than either major or minor chords.
      base_difficulty <- 4
    }
    if (df[i,]$ext == "none"){
      ext_difficulty <- 1
    } else if (df[i,]$ext == "dom7"){
      ext_difficulty <- 3
    } else if (df[i,]$ext == "maj7"){ # Extensions generally increase chord difficulty.
      ext_difficulty <- 3
    }
    total_difficulty <- tonic_difficulty + base_difficulty + ext_difficulty # Expressing chord difficulty as the sum of their component difficulties.
    df$success[i] <- rbinom(n = 1, size = 1, prob = (1 - ((total_difficulty**2)/100))) # Sampling between success (1) or failure (0), using chord difficulty to determine success probability.
  }
  return(df)
}

train <- simulate_trials(train) # Simulating calibration stage.

train <- data.frame(lapply(train, as.factor))

(nrow(train[train$base == "maj" & train$success == 1, ]) / nrow(train[train$base == "maj", ])) * 100 # Showing as an example that in our simulation, chords built on a major triad have a much higher success rate than those built on a diminished triad.
(nrow(train[train$base == "dim" & train$success == 1, ]) / nrow(train[train$base == "dim", ])) * 100


model <- glm(success ~ ., data = train, family = binomial) ### Building model using calibration dataset.



play_15_chords <- function(){
  
  chords <- c()
  likelihoods <- c()
  
  for (i in 1:nrow(possible_chords)){
    newdata <- data.frame(tonic = possible_chords[i,]$tonic, base = possible_chords[i,]$base, ext = possible_chords[i,]$ext)
    chords <- append(chords, paste(possible_chords[i,]$tonic, possible_chords[i,]$base, possible_chords[i,]$ext))
    likelihoods <- append(likelihoods, predict(model, newdata = newdata, type = "response"))
  }
  
  results <- data.frame(chord = chords, likelihood = likelihoods)
  results <- results[order(results$likelihood), ]
  
  
  
  hardest_five <- results[1:5,] # Returning the top five chords according to difficulty (lowest estimated success likelihoods).
  random_ten <- results[sample((6:nrow(results)),10),] ### Also returning ten random chords from the possible chords. Replace kept at default of FALSE. 'Hardest five' chords excluded from sampling.
  
  output_chords <- rbind(hardest_five, random_ten)
  
  output_chords <- data.frame(output_chords[c(1,6,7,2,8,9,3,10,11,4,12,13,5,14,15),]) ### Rearranging such that the user receives one 'difficult' chord followed by two 'random' chords, and so on.
  
  
  
  output_chords_formatted <- data.frame(tonic = character(0), base = character(0), ext = character(0))
  for (i in 1:nrow(output_chords)){
    output_chords_formatted <- rbind(output_chords_formatted, data.frame(tonic = unlist(strsplit(output_chords[i,1], " "))[1], base = unlist(strsplit(output_chords[i,1], " "))[2], ext = unlist(strsplit(output_chords[i,1], " "))[3]))
  }
  
  new_train <- simulate_trials(output_chords_formatted)
  
  new_train <- data.frame(lapply(new_train, as.factor))
  
  train <- rbind(train, new_train)
  
  train <- data.frame(lapply(train, as.factor))
  
  model <- glm(success ~ ., data = train, family = binomial)
  
  train <<- train
  
  model <<- model
  
  results <<- results
  
  return(output_chords_formatted)
  
} # Function which ranks chords according to difficulty, and returns 15 suitable chords (see comments within function for detail on this). It then simulates the user's success in each chord trial, adds this to the training dataset, and ultimately, updates the model with this new dataset.

play_15_chords() # In addition to functionalities mentioned above, returns the 15 chords given to the user for each iteration, with the first chord given represented by the first row.



########## PART 4: Now let's simulate player learning #############################################################################################################################################



reps <- 200 # Setting number of reps of 15 chord sets to simulate - if resetting reps remember to run from here to reset variables assigned below.

train <- get_calibration_chords(60) # Constructing a new calibration training set.

calibration_chord_number <- nrow(train) # This will be used to ignore the chords presented during the calibration stage for the 'learning' mechanism encoded below.

simulate_trials_with_learning <- function(df){
  for (i in 1:nrow(df)){
    if (df[i,]$tonic %in% c("A", "B", "C", "D", "E", "F")){ # This part is the same as the above function without learning...
      tonic_difficulty <- 1
    } else {
      tonic_difficulty <- 2
    }
    if (df[i,]$base == "maj"){
      base_difficulty <- 1
    } else if (df[i,]$base == "min"){
      base_difficulty <- 2
    } else if (df[i,]$base == "dim"){
      base_difficulty <- 4
    }
    if (df[i,]$ext == "none"){
      ext_difficulty <- 1
    } else if (df[i,]$ext == "dom7"){
      ext_difficulty <- 3
    } else if (df[i,]$ext == "maj7"){
      ext_difficulty <- 3
    }
    total_difficulty <- tonic_difficulty + base_difficulty + ext_difficulty
    prior_appearances <- sum(train$tonic[calibration_chord_number:nrow(train)] == df[i,]$tonic & train$base[calibration_chord_number:nrow(train)] == df[i,]$base & train$ext[calibration_chord_number:nrow(train)] == df[i,]$ext) # As mentioned above, chords shown during calibration counted in prior appearances.
    learning_rate <- 0.1
    total_difficulty <- total_difficulty * exp(-learning_rate * prior_appearances) # Emulating user learning, utilizing exponential decay to decrease effective chord difficulty for every time the user has encountered it. Here we have named what would be the 'decay rate' as 'learning rate' - this can be set at different levels to simulate different rates of player learning.
    df$success[i] <- rbinom(n = 1, size = 1, prob = (1 - ((total_difficulty**2)/100)))
  }
  return(df)
} # Function to simulate trials with learning, taking into account how many times each chord has been encountered before.

train <- simulate_trials(train) # Simulating calibration stage again.

train <- data.frame(lapply(train, as.factor))

model <- glm(success ~ ., data = train, family = binomial) # Building model using calibration data.



play_15_chords_with_learning <- function(){
  chords <- c()
  likelihoods <- c()
  
  for (i in 1:nrow(possible_chords)){
    newdata <- data.frame(tonic = possible_chords[i,]$tonic, base = possible_chords[i,]$base, ext = possible_chords[i,]$ext)
    chords <- append(chords, paste(possible_chords[i,]$tonic, possible_chords[i,]$base, possible_chords[i,]$ext))
    likelihoods <- append(likelihoods, predict(model, newdata = newdata, type = "response"))
  }
  
  results <- data.frame(chord = chords, likelihood = likelihoods)
  results <- results[order(results$likelihood), ]
  
  
  
  hardest_five <- results[1:5,]
  random_ten <- results[sample((6:nrow(results)),10),] ### Replace kept at default of FALSE. 'Hardest five' chords excluded from sampling.
  
  output_chords <- rbind(hardest_five, random_ten)
  
  output_chords <- data.frame(output_chords[c(1,6,7,2,8,9,3,10,11,4,12,13,5,14,15),]) ### Rearranging such that the user receives one 'difficult' chord followed by two 'random' chords.
  
  
  
  output_chords_formatted <- data.frame(tonic = character(0), base = character(0), ext = character(0))
  for (i in 1:nrow(output_chords)){
    output_chords_formatted <- rbind(output_chords_formatted, data.frame(tonic = unlist(strsplit(output_chords[i,1], " "))[1], base = unlist(strsplit(output_chords[i,1], " "))[2], ext = unlist(strsplit(output_chords[i,1], " "))[3]))
  }
  
  new_train <- simulate_trials_with_learning(output_chords_formatted)
  
  new_train <- data.frame(lapply(new_train, as.factor))
  
  train <- rbind(train, new_train)
  
  train <- data.frame(lapply(train, as.factor))
  
  model <- glm(success ~ ., data = train, family = binomial)
  
  train <<- train
  
  results <<- results
  
  model <<- model
  
  return(output_chords_formatted)
} # Similar to earlier function, only factoring in learning in addition.



play_15_chords_with_learning() # In the following we simulate learning and plot the learning curves of the user with respect to 5 different chords - each of differing levels of success likelihood, as estimated from the calibration stage.

initial_likelihoods <- results # For comparison at the end...

hardest_chord <- results$chord[1]
hardest_chord_likelihood <- c(results$likelihood[1])

upper_quartile_chord <- results$chord[round(nrow(results)*0.25)]
upper_quartile_chord_likelihood <- c(results$likelihood[round(nrow(results)*0.25)])

moderate_chord <- results$chord[round(nrow(results)*0.5)]
moderate_chord_likelihood <- c(results$likelihood[round(nrow(results)*0.5)])

lower_quartile_chord <- results$chord[round(nrow(results)*0.75)]
lower_quartile_chord_likelihood <- c(results$likelihood[round(nrow(results)*0.75)])

easiest_chord <- results$chord[nrow(results)]
easiest_chord_likelihood <- c(results$likelihood[nrow(results)])

for (i in 1:(reps - 1)){
  play_15_chords_with_learning()
  
  hardest_chord_likelihood <- append(hardest_chord_likelihood, results[results$chord == hardest_chord,]$likelihood)
  upper_quartile_chord_likelihood <- append(upper_quartile_chord_likelihood, results[results$chord == upper_quartile_chord,]$likelihood)
  moderate_chord_likelihood <- append(moderate_chord_likelihood, results[results$chord == moderate_chord,]$likelihood)
  lower_quartile_chord_likelihood <- append(lower_quartile_chord_likelihood, results[results$chord == lower_quartile_chord,]$likelihood)
  easiest_chord_likelihood <- append(easiest_chord_likelihood, results[results$chord == easiest_chord,]$likelihood)
}

graphing_df <- data.frame(treatment = c(rep(hardest_chord, times = reps), rep(upper_quartile_chord, times = reps), rep(moderate_chord, times = reps), rep(lower_quartile_chord, times = reps), rep(easiest_chord, times = reps)),
                          response = c(hardest_chord_likelihood, upper_quartile_chord_likelihood, moderate_chord_likelihood, lower_quartile_chord_likelihood, easiest_chord_likelihood),
                          reps = rep(1:reps, times = 5)) # Gathering simulation outcome data into a dataframe for graphing below.


represent_as_chord <- function(chord){
  if (unlist(strsplit(chord, ' '))[3] == "none" & (unlist(strsplit(chord, ' '))[2] == "maj" | unlist(strsplit(chord, ' '))[2] == "dim")){
    return(paste0(unlist(strsplit(chord, ' '))[1], unlist(strsplit(chord, ' '))[2]))
  } else if (unlist(strsplit(chord, ' '))[3] == "none" & unlist(strsplit(chord, ' '))[2] == "min"){
      return(paste0(unlist(strsplit(chord, ' '))[1], "m"))
  } else if (unlist(strsplit(chord, ' '))[3] == "dom7" & unlist(strsplit(chord, ' '))[2] == "min"){
    return(paste0(unlist(strsplit(chord, ' '))[1], "m7"))
  } else if (unlist(strsplit(chord, ' '))[3] == "dom7" & unlist(strsplit(chord, ' '))[2] == "min"){
    return(paste0(unlist(strsplit(chord, ' '))[1], "m7"))
  } else if (unlist(strsplit(chord, ' '))[3] == "dom7" & unlist(strsplit(chord, ' '))[2] == "maj"){
    return(paste0(unlist(strsplit(chord, ' '))[1], "7"))
  } else if (unlist(strsplit(chord, ' '))[3] == "maj7" & unlist(strsplit(chord, ' '))[2] == "min"){
    return(paste0(unlist(strsplit(chord, ' '))[1], "min(maj7)"))
  } else if (unlist(strsplit(chord, ' '))[3] == "maj7" & unlist(strsplit(chord, ' '))[2] == "maj"){
    return(paste0(unlist(strsplit(chord, ' '))[1], "maj7"))
  } 
} # Combining all three chord components to form the conventional chord name. This is not quite so straightforward as chord nomenclature conventions are a little convoluted...

for (i in 1:nrow(graphing_df)){
  graphing_df$treatment[i] <- represent_as_chord(graphing_df$treatment[i])
}



library(ggplot2) # Plotting learning trajectory lines for 5 chords of differing initial success likelihoods.

ggplot(graphing_df, aes(x = reps, y = response, color = treatment, group = treatment)) +
  geom_line() +
  labs(x = "Reps", y = "Success Likelihood",
  title = paste("Learning Curves of 5 Different Chords Over", reps, "Reps"),
  subtitle = "Chords simulated represent 5 differing levels of success likelihood as estimated from calibration stage") +
  xlim(1, reps) + ylim(0, 1) +
  scale_color_manual(values = c("#CE9DD9", "#FEC868", "#FD8A8A", "#33B864", "#B4CBF0"), name = "Chords")



final_likelihoods <- results # Comparing initial success likelihoods of each possible chord with those after n rounds of practice.

merging <- merge(initial_likelihoods, final_likelihoods, by = "chord")

merging <- merging[order(merging$likelihood.y), ]

names(merging)[names(merging) == "likelihood.x"] <- "initial likelihood"
names(merging)[names(merging) == "likelihood.y"] <- "final likelihood"

for (i in 1:nrow(merging)){
  merging$chord[i] <- represent_as_chord(merging$chord[i])
}

initial_vs_final_likelihoods <- merging # Comparison table sorted by final success likelihood.


