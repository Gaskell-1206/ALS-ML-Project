# Predicting Amyotrophic Lateral Sclerosis Progression with Clinical Trial Data

# Data Cleansing
## General Preprocessing
Certain fields in the raw input data contain invalid or inconsistent values.  Accordingly, we make the following modifications:  

+ We delete “pct_of_Normal_Trial” in FVC and SVC
+ We only consider the “Delta” <= 92 (first 3 months after onset)
+ We recoded variables to binary variables in Demo:
  - sex_male: Male=1, Female=0, no missing
  - race_white: white=1, all others or missing=0
  - race_black: black=1, all others or missing=0
  - race_asian: asian=1, all others or missing=0
  - race_other: 1 include American Indian/Alaska Native, Hawaiian/Pacific Islander, unknown, other, all others=0
  - race_missing: missing=1, no missing=0
  - study_arm_active: Active=1, Placebo=0 (From Treatment.csv)
  - Height_cm and BMI_baseline: values at delta=0 (From vital data)
(Data Cleansing/Data processing_Static_Demo.R)
# Missing Value and Standardization
## Median Imputation
For continuous variables with missing values, we imputed the median of each variable.
In addition to raw values, we also generated data with standardized values for continuous variables (mean=0, sd=1). 
(Data/Data processing_Median Impute_Standardize.R)
## KNN Imputation
For continuous variables with missing values, we imputed the missing value by k-nearest neighbors (k=10). Training and validation data were imputed by the same knn imputer, while test data was imputed separately to avoid data leakage. 
Standardized values for continuous variables (mean=0, sd=1) were also generated here because distance was used in the knn imputer. 

(Data_processing_knn_imputer.ipynb)

# Feature Engineering
## Labs
Selected the test appearing more than 10000 times in the labs.csv, which returned 35 different test, e.g. WhiteBloodCount, Glucose, Creatinine.
For each subject, created the average, minimum, maximum, standard deviation, the number of test, duration of test per test, which returned 35*7 columns in the processed feature table.
(Feature Engineering/dataCleansing_Labs.R)
## FVC and SVC
Forced vital capacity(FVC) is the volume of air that can forcibly be blown out after full inspiration, measured inliters. FVC is available for some of the patients. 
Another measure of lung function is slow vital capacity(SVC). Slow vital capacity is the maximum volume of air that can be exhaled slowly after slow maximum inhalation, also measured in liters.
We ignore records missing either a delta value or a measurement value. In addition, when two records have the same delta value, we keep only the first record
For each time series field, we also calculate a derivative time series of pairwise slopes. That is, for each temporally consecutive pair of measurement values, we compute (second measurement value - first measurement value)/(second delta value - first delta value) and associate this pairwise slope measurement with the delta value (second delta value + first delta value)/2.
For each original time series and each derivative time series we include the following statistics as subject features:   
The maximum measurement value  
The minimum measurement value  
The last measurement value  
The mean measurement value  
The number of measurement values 
The sum of all measurement values  
The delta value associated with the first measurement value  
The delta value associated with the last measurement value  
The mean of the squared values of all of the measurements  
The standard deviation of the measurement values  
The slope of the time series, defined as (last measurement value – first measurement value)/(last delta value – first delta value)

(Feature Engineering/Feature Engineering FVC and SVC.ipynb)

## ALSFRS(first 3 months data)
ALS Functional Rating Scale is frequently used to assess the symptom of severity. 
Removed the samples with less than 2 ALSFRS values in the first 3 months. 
Calculated the slope of change of ALSFRS for the first 3 months using the first ALSFRS score and the last one within 3 month ( Delta<=92). 
## ALS history
Applied one-hot encoding on site of onset, where two sites were considered (bulbar and limb).
Corresponding time (day/delta) of onset was found for each subject 
Applied one-hot encoding on symptom, where ten sites were considered (weakness, speech, cramps, fasciculations, atrophy, swallow, stiffness, sensory changes, gait changes and other)
Corresponding time (day/delta) of symptom was found for each subject 

(Feature Engineering/dataCleansing_AlsHistory.R)

## Family History
Applied one-hot encoding on the neuro disease of families, where five diseases were considered (stroke, Parkinson, ALS, DAT and other).
Created a summary variable called ‘NeuroDisease’, 1 if any family member had any neuro disease mentioned above.
Applied one-hot encoding on the family member who had neuro disease. First-degree, second-degree and third-degree relatives were considered here to reduced the category of family member from 27 to 3.

(Feature Engineering/dataCleansing_FamilyHistory.R)

## Vital (first 3 months data)
Height missing imputed with first non missing and calculated BMI.
Removed 14 static variables: Baseline_** or Endpoint_** (only available for 550 subjects)
Generated min, mean, max, sd, last non missing, first and last delta, number of measurements (_nmeas), slope for features
Dropped missing>80% features (6 features): Supine ** or Standing **
We used Spaghetti plots to visualize longitudinal change of features.
(Feature Engineering/Data processing_Vital.R)

## Riluzole
Recoded variables to binary variables in Demo: patient used Riluzole = 1, patient did not use Riluzole = 0.

## Data merging
We merged all data by “subject_id”. We first merged outcome data and demographic data using inner join and then merged all other data to this main data using left join.
(Data Cleansing/Merge all cleaned data.ipynb)

# Exploratory analysis
We conducted exploratory analysis of the relationship between features and outcome using simple linear regression and visualized top features based on beta coefficient. We used Loess plot to visualize the functional form such as linear or nonlinear associations. We also used lasso regression to identify feature subset.
(Feature Engineering/EDA_Feature Selection.R)
# Model Training
We conducted model training for median imputed and KNN imputed data separately. After importing training, validation and testing data, we separated features and outcome and first trained models using default parameters. Then we used cross-validation and randomized search/grid search to do hyperparameter tuning. We found the best model with lowest validation error and extracted the top 30 most important features. Finally, we combined training and validation set to train models and evaluated performance in testing data.
## Lasso and Elastic Net Regression
Package: sklearn.linear_models.LassoCV, sklearn.linear_models.ElasticNetCV
Hyperparameters: alpha (Lasso), Elastic Net(alpha and l1_ratio)
We also tried feature selection before training lasso model

(Model Training/MLasso and Elastic Net.ipynb)


## SVM, SVR
Package: sklearn.svm.NuSVR
Hyperparameters:
nu = 0.5
C = 0.5
Kernel = ‘rbf’
gamma = ‘auto’
(Model Training/SVR.ipynb)

## Ensemble Learning - Tree Model - Bagging - Random Forests

Package: sklearn.ensemble.RandomForestRegressor
Hyperparameter:
Max depth = 90
min sample split = 10
min samples leaf = 4
Estimators = 237
max features = auto
(Model Training/Random_Forests.ipynb)
## Ensemble Learning - Tree Model - Boosting 
### Adaboosting 
Package: sklearn.ensemble.AdaBoostRegressor
Hyperparameter:
'n_estimators': 100 
'learning_rate': 0.0001
### Gradient Boosting
Package: sklearn.ensemble.GradientBoostingRegressor
Hyperparameter:
'subsample': 0.5
'n_estimators': 100
'min_samples_split': 10
'min_samples_leaf': 2
'max_depth': 1
'learning_rate': 0.1
### XGBoosting
Package: xgboost.XGBRegressor
Hyperparameter:
'subsample': 0.9
'n_estimators': 100
'min_samples_split': 2
'min_samples_leaf': 1
'max_depth': 11
'learning_rate': 1.0
(Model Training/Boosting.ipynb)
## Ensemble Learning - Bayesian Additive Regression Tree
Package: BayesTreeBART.R
Hyperparamter:
ntree = 200
ndpost = 1000
nskip = 100
Sigquant = 0.5
(Model Training/Modeling_BART.R)
## Neural Network
Package: tensorflow.keras
Neural Network Framework:


(Model Training/Neural Network.ipynb)
# Evaluation Metrics
We use following metrics to evaluate model performance: 
+ 𝑅^2: It is a measure of the linear relationship between X and Y. It is interpreted as the proportion of the variance in the dependent variable that is predictable from the independent variable.
+ R2 = metrics.r2_score(y_test, y_pred)}
Adjusted 𝑅^2:The adjusted R-squared compares the explanatory power of regression models that contain different numbers of predictors.
+ MAE: It is the mean of the absolute value of the errors. It measures the difference between two continuous variables, here actual and predicted values of y.
+ MSE: The mean square error (MSE) is just like the MAE, but squares the difference before summing them all instead of using the absolute value.
+ RMSE: The mean square error (MSE) is just like the MAE, but squares the difference before summing them all instead of using the absolute value.
+ Pearson Correlation: Pearson's correlation ρ is a relative measure that evaluates how well a prediction method is able to recover ALSFRS trends across patients. Better predictions lead to a higher value of the correlation, up to 1.0 for the perfect prediction.
(Evaluation Metrics.ipynb)


# Further analysis
We explored the distribution of predicted Y values from different machine learning models and compared it with true Y values, to understand how the extreme values influence model performance. We also generated confusion matrices to understand the clinical implications of the predictions. We defined fast progressors as those with y<-1.1.
(Further explore ALSFRS after predictions.ipynb)

In each model, we consider using zscore > 3 as threshold to remove outliers of y_true. In summary, 57 outliers were dropped. As a result, MSE and correlation have improved significantly.
(codes are inside each model)
