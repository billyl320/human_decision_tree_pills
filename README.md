# Pill Shape Classification for Small Data with Human-Machine Hybrid Explainable Model

## Abstract

This paper presents a near perfect interpretable solution for pill shape classification.  Using a human-machine hybrid approach, we were able to achieve an overall classification rate of 97.83%.  The only misclassifications occurred between ovals and capsules.  Our final model used a decision tree, where each node classified meta-classes, or groups of classes, using support vector machines with a polynomial kernel.  While the data was imbalanced between the classes, this model was able to overcome this challenge through the use of meta-classes.  By limiting each node of the decision tree to only two variables to build the model to discriminate the meta-classes, each node is interpretable as the final decision boundaries can be plotted using only two variables.  

## Data

The data branch provides all of the data collected from the python scripts.  Additionally metrics were collected but not used in the final model.

## Code

The code branch provides all of the code to perform the analysis.  The image operators and metric collection was performed in python, while the modeling and plot generation was performed in R.  

## Plots

The plots branch provides many of the R generated scatterplots used to manually determine which variables to use to separate potential meta-classes.

## Results

Overall, we are able to acheive near perfect classification with an overall classification rate of 97.83%.  Our only errors occur between capsule and oval class shapes.  

## 2021 SDSS Short Paper

The referred short paper entitled 'SVM-Based Models for Pill Shape Classification' is also available for download here: https://github.com/billyl320/human_decision_tree_pills/blob/master/SDSS_Format_HMH_Pill_Shape_Classification.pdf. 
