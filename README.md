Replication Instructions

Study Overview
This study directly replicates and extends the work of Decety et al. (1989), which investigated the temporal correspondence between imagined and executed walking. Participants completed real and imagined walking trials over distances of 5 m, 10 m, and 15 m. The study examined whether imagined and executed movement durations were equivalent using frequentist, Bayesian, and agreement-based approaches.
Repository Structure
Data/
    participants_randomize_trials_data.xlsx
    Data_sorted.xlsx
    Psych-py-questionnaire
Scripts/
    Bland_Altman_script.R
    Graph paper.R
Software Requirements
Analyses were performed using:
•	JASP (Version 0.19)
•	RStudio (Version 2024.04.2+764)
•	R packages:
o	TOSTER
o	ggplot2
o	readxl
Reproducing the Study
Step 1 – Generating the randomized trial order
Before testing each participant, generate a randomized order of the 60 trials.
Each participant completes:
•	10 executed walking trials at 5 m
•	10 imagined walking trials at 5 m
•	10 executed walking trials at 10 m
•	10 imagined walking trials at 10 m
•	10 executed walking trials at 15 m
•	10 imagined walking trials at 15 m
This gives a total of 60 trials per participant. Participants will either start from point A or B. 
The randomized file should include the following information for each trial: participant ID, trial number, condition (executed or imagined), distance (5, 10, 15), starting point (A or B)
The order should be randomized separately for each participant to avoid block effects.
Step 2 – Test participants
Participants first complete the French version of the Movement Imagery Questionnaire-3 (MIQ-3) (Robin et al., 2021)
Then, participants complete the walking task.
For each trial, participants are asked to either actually walk or imagine walking one of three distances: 5 m, 10 m, or 15 m.
For executed walking trials, participants start the stopwatch when initiating the first step and stop it when crossing the target line.
For imagined walking trials, participants close their eyes and imagine themselves walking the indicated distance from a first-person perspective visually and kinesthetically. They start the stopwatch when imagining the first step and stop it when they imagine crossing the target line.
Participants are instructed not to count in their head. 
The stopwatch used was a digital memory stopwatch (500-Memory DT500 Stopwatch USB version Ref:024111). 
Step 3 – Encode the Data
After testing, encode the data in a spreadsheet with one row per trial for each participant. After data entry, compute the mean timing for each participant, condition, and distance. The final dataset used for the main analyses should include one row per participant and one column for each condition-distance combination.
Step 4 – Run statistical analyses with Jasp
Repeated-Measures ANOVA
Run a 2 × 3 repeated-measures ANOVA with:
•	Within-subject factor 1: Condition
o	Executed
o	Imagined
•	Within-subject factor 2: Distance
o	5 m
o	10 m
o	15 m
Dependent variables:
•	ME_5m
•	MI_5m
•	ME_10m
•	MI_10m
•	ME_15m
•	MI_15m
Report:
•	main effect of Condition
•	main effect of Distance
•	Condition × Distance interaction
•	Greenhouse–Geisser corrections when sphericity is violated
•	effect sizes
Bayesian Equivalence Tests
Run Bayesian equivalence paired-samples t-tests for:
•	ME_5m vs MI_5m
•	ME_10m vs MI_10m
•	ME_15m vs MI_15m
Use the predefined equivalence region:
•	δ ∈ [-0.01, 0.01]
Report the Bayes factors for each distance.
Correlations
Run Pearson correlations between:
•	ME_5m and MI_5m
•	ME_10m and MI_10m
•	ME_15m and MI_15m
Also run the correlation between:
•	MIQ-3 total score
•	timing difference between imagined and executed walking
Step 5 – Generate figures in R 
Use RStudio to generate the figures reported in the manuscript.
The R scripts generate:
•	mean walking duration by condition and distance
•	correlations between executed and imagined walking times
•	Bland–Altman plots
•	MIQ-3 correlation plot

Required R packages: install.packages(c("ggplot2", "readxl", "TOSTER"))
