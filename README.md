# ML-performance-microbial-quality
Evaluating the performance of Machine learning approaches to predict the microbial quality of surface waters and to optimize the sampling effort

The directory contains four files:

- ML_Ecoli_Prediction.ipynb : Python file with the 6 machine learning models for the prediction of E. coli concentration. 
The file contains all the steps of data reading, descriptive analysis, division of the dataset into training and test, data standardization and training and testing of the 6 machine learning models. 
The test data and the predictions are then exported. 
The division of the dataset into training and test is done randomly by train_test_split. 
The random_state parameter allows to control the split, so passing an integer allows a reproducible split. 
In order to test several splits, the script was run 10 times changing each time the integer value for the "random_state" parameter from 0 to 9. 

- ML-based_prediction_comparison.R : R file for the descriptive analysis of our dataset.
Visualization by plot of the errors (RMSE, MAE and RDP) to compare the performance of the 6 machine learning models. 

- script-analyse-correlation-rf.R : R file for the analysis of the correlation between predicted values and physicochemical and meteorological parameters.
Determination of the number of reasonable prediction and inaccurate prediction for the 10 random testing datasets.
Analysis of the normality of the data by a shapiro test, the results showed that the data did not have a normal distribution.
So a spearman correlation analysis for the reasonable and inaccurate estimates was then performed.
Visualization of the correlation results by a correplot.
Statistical analysis to compare the correlation between the reasonable and the inaccurate estimates to identify the parameters to be optimized.


- script_identification-parametre_fit.R : R file to identify the set of values that allow a reasonable estimation.
Grouping of the 10 random testing datasets and analysis of the prediction results of the RF-based model. 
Identification of the set of values (physico-chemical and meteorological parameters) that give a reasonable estimate except those that give an inaccurate estimate. 
Visualization of the results for the parameters to be optimized according to the value range of our dataset. 
Additional analysis of the values of the dataset and the 10 random testing datasets. 
