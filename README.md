# git-suppression-LSA2027

This repository contains the models presented in Scontras & Tonhauser "Suppressing projection inferences under a reasoning-based analysis", the analysis scripts used to derive the model predictions and the figures presented in the paper, as well as the code for running and analyzing the results of the two pilots.

## experiments

This folder presents the code for running the two pilots.

## models

This folder contains the models presented in the paper, as well as the R code needed to evaluate the models and generate figures. 

### data

### graphs

### model-1a.wppl

This is the model for sentence (1a); it is identical to the model described in section 3.1 of Scontras & Tonhauser 2026 (Meaning), a simplified version of the model described in section 3 of Scontras & Tonhauser 2025 (SuB).

### model-1b-conditional.wppl

This is the model for the conditional sentence (1b).

### model-1c-conjunction.wppl

This is the model for the conjunction sentence (1c).

### model-evaluation.R

This is the R code used to evaluate the models, generate the model predictions and create Fig.1.

## results

This folder contains the data for the two pilots, as well as the R analysis scripts to analyze the data and generate the result plots (panels a and b of Fig.4 in paper). 

## other files

### helpers.R contains R code needed in both the directory 'models' and 'results'.