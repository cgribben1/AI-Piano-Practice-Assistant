
# Piano Practice Assistant
Here, I have designed a program to help guide piano chord practice by prompting the user with the most effective chords to exercise (i.e., the ones requiring the most practice), based on their specific ability. 

In its proposed use, the program is integrated into piano keyboard hardware. Following a brief calibration round in which the user is tasked with playing a series of chords - each trial resulting in a success or a failure -, the program uses logistic regression to return chords with the lowest estimated likelihood of success.

User learning was simulated on 5 different chords of varying estimated difficulty, the graph of this simulation can be found in the file "Piano_practice_learning_curve.png".





## Details

#### Proposed Use
First, an outline of the proposed use of the program. During calibration and subsequent rounds of practice, the user will be given a chord as a prompt. This represents a trial, the result of which will be either success or failure - this depends on whether or not the user plays the chord correctly. Here, we need to define a correctly-played chord. Each trial could come with a time-limit for all action to take place - say 5 seconds. During this time limit, all required keys to form the chord must, at some point, be pressed down in unison, with no other keys having been pressed. If this is achieved, the trial is deemed a success, otherwise, it is deemed a failure.

#### Calibration Stage
The calibration stage is required to build the logistic regression model, and can either be exhaustive (i.e., the user is tasked with playing all possible chords within the context of the practice), or carried out on a subset of the possible chords, so long as each possible chord component (thus, each possible variable level) is tested (more on chord components below).

#### Sequence of Practice
Once calibration is carried out, the initial logistic regression model is constructed. From here, all possible chords are ranked on their predicted success likelihood, as fitted by the model. 15 chords are then given to the user in the next repetition, or 'rep', of trials. Here, we had these 15 chords consist of the 5 chords with the lowest predicted success likelihoods (i.e., the 5 most 'difficult' chords to the user), along with 10 more randomly generated chords (this is done so that the user is not discouraged by facing too many difficult chords in succession). These are supplied in the order: 1 difficult chord followed by 2 randomly generated chords, and so on.

The results of this rep of trials are recorded, this data added to the training data, and the model updated. This process is then repeated indefinitely.

#### Chord Components
Chords were broken down into 3 components: the 'tonic' note (the 'root' note around which the rest of the chord is based), the 'base' of the chord (here, this is either 'major', 'minor', or 'diminished'), and the 'extension' of the chord (here, this is either 'none' (no extension), 'dominant 7', or 'major 7'). Note that any combinations of 'diminished' and 'dominant 7' or 'major 7' were excluded as these do not form commonly observed chords. These combinations represent what might be considered the most common chords required for piano playing; more chord combinations could be added, in turn increasing the range and difficulty of the practice program.

Breaking chords down into 'base' and 'extension' provides the model with additional information regarding potential interactions between these factors. This could also be done using a neural network, however this would require considerably more training data to elucidate these relationships. A major benefit of using simpler regression models is just this - being able to engineer the data such that certain interactions between features are more readily detected, without the requirement of too large a dataset or too intensive computation.

#### Simulations
Finally worth noting is that, owing to the fact that the required user input cannot be obtained without program integration into a piano keyboard, user performance was simulated here. This was done during trials by assigning each chord component a 'difficulty', before summing all component difficulties to form the total 'chord difficulty'. This difficulty was then used to determine the probability of 'success' during sampling from a binomial distribution. Further, in Part IV, user 'learning' was factored into simulations by utilizing the concept of exponential decay to emulate the reduction of chord difficulty depending on how many times the same chord has been encountered by the user over the course of training.

The end product of this script is a visualisation of these simulations, representing a user's learning curve over reps of practice. Here the learning curves corresponding to 5 chords are shown - these represent 5 differing levels of initial success likelihood as estimated by the logistic regression model.
## Deployment

As stated above, this program is intended for use within a piano keyboard system, so that the required input can be received by the user. However, the code here primarily exists as a proof of concept, and comes accompanied by simulations of user input. The entire script - an R script which can be run in RStudio - can be explored to grasp how the algorithm works, otherwise, a suitable takeaway can be found in Part IV, where user learning during the use of the program is simulated and graphed.

The only R package required for running is "ggplot2".

