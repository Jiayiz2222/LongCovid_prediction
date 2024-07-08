import pandas as pd
import numpy as np
from lightgbm import LGBMClassifier
from sklearn.model_selection import GridSearchCV
import warnings

# Read data
warnings.filterwarnings('ignore')
RANDOM_STATE = 0
cohort_train = pd.read_csv('../../dataset/inpatient/train.inpatient_18-44.csv')
cohort_test = pd.read_csv('../../dataset/inpatient/test.inpatient_18-44.csv')
deleted_var = ["id", "COVID.test", "inpatient", "major", "ards", "ageg", "pd", "bp", "anxiety", "htn", "cbc.wbc", "cbc.neuphil", "cbc.plt", "inflam.crp", "inflam.ldh"]
cohort_train = cohort_train.drop(deleted_var, axis=1)
cohort_test = cohort_test.drop(deleted_var, axis=1)
cohort_train.loc[cohort_train['sex']=='F', 'sex'] = 0
cohort_train.loc[cohort_train['sex']=='M', 'sex'] = 1
cohort_train['sex'] = pd.to_numeric(cohort_train['sex'])
cohort_test.loc[cohort_test['sex']=='F', 'sex'] = 0
cohort_test.loc[cohort_test['sex']=='M', 'sex'] = 1
cohort_test['sex'] = pd.to_numeric(cohort_test['sex'])
X_train = cohort_train.drop(['death_1y'], axis=1)
y_train = cohort_train['death_1y']
X_test = cohort_test.drop(['death_1y'], axis=1)
y_test = cohort_test['death_1y']
abbr2full = {'sex': 'Sex',
             'Age': 'Age',
             'fully_vacciated': 'Fully vaccinated',
             'major': 'Major CVD',
             'mi': 'Myocardial infarction',
             'hf': 'Heart Failure',
             'stroke': 'Stroke',
             'af': 'Atrial fibrillation',
             'dvt': 'Deep vein thrombosis',
             'pd': 'Psychotic disorder',
             'ee': 'Encephalitis and Encephalopathy',
             'bp': "Bell's Palsy",
             'ild': 'interstitial lung disease',
             'cpd': 'chronic pulmonary disease',
             'ards': 'Acute respiratory distress syndrome',
             'dx.hx.ards': 'hx, Acute respiratory distress syndrome',
             'dx.bw.ards': 'bw, Acute respiratory distress syndrome',
             'pancreatitis': 'pancreatitis',
             'li': 'Liver injury',
             'esr': 'End stage kidney disease',
             'akd': 'Acute kidney injury and failure',
             't1dm': 't1dm',
             't2dm': 't2dm',
             'htn': 'Hypertension',
             'covid.ct': 'COVID CT',
             'inflam.crp': 'C-reactive protein',
             'cbc.wbc': 'WBC (TLC) count',
             'cbc.neuphil': 'Absolute Neutrophil Count',
             'cbc.plt': 'Platelet count',
             'inflam.esr': 'Erythrocyte sedimentation rate',
             'inflam.procalcitonin': 'Procalcitonin',
             'inflam.procalcitonin2': 'Procalcitonin2',
             'inflam.procalcitonin3': 'Procalcitonin3',
             'inflam.ferritin.mol': 'Serum Ferritin',
             'inflam.ldh': 'Lactate Dehydrogenase ',
             'cbc.lympho': 'Lymphocyte',
             'cbc.lympho2': 'Lymphocyte2',
             'cbc.neuphil.pct': 'Neutrophil-lymphocyte ratio (NLR)',
             'anxiety': 'anxiety',
             'ptsd': 'Post-traumatic stress disorder',
             'seizure': 'seizure',
             'covid.ct0.low20':'COVID CT.low20'}

if __name__=='__main__':
    # LightGBM
    method = 'LightGBM'
    print('*********** ' + method + ' ***********')
    lgbm = LGBMClassifier(objective='binary', random_state=RANDOM_STATE)

    # use the inherent feature selection of model
    lgbm.fit(X_train, y_train)
    feature_importances = pd.DataFrame(list(zip(*[lgbm.feature_name_, lgbm.feature_importances_])))
    selected_variables = list(feature_importances[feature_importances[1]>0][0])

    # tune the hyperparameters
    lgbm = LGBMClassifier(objective='binary', random_state=RANDOM_STATE)
    param_grid = {
        'max_depth':[4],
        'min_split_gain':[0.07],
        'subsample':[0.01],
        'colsample_bytree':[0.5],
        'min_child_samples':[19],
        'reg_alpha': [1],
        'reg_lambda': [0.11],
        'learning_rate': [0.1]}
    grid_search = GridSearchCV(estimator=lgbm, param_grid=param_grid, scoring='roc_auc', cv=10, n_jobs=-1, verbose=0)
    grid_search.fit(X_train[selected_variables], y_train)
    print('Tuned Hyperparameters:', grid_search.best_params_)
    model = LGBMClassifier(
        max_depth=grid_search.best_params_['max_depth'],
        min_split_gain=grid_search.best_params_['min_split_gain'],
        subsample=grid_search.best_params_['subsample'],
        colsample_bytree=grid_search.best_params_['colsample_bytree'],
        min_child_samples=grid_search.best_params_['min_child_samples'],
        reg_alpha=grid_search.best_params_['reg_alpha'],
        reg_lambda=grid_search.best_params_['reg_lambda'],
        learning_rate=grid_search.best_params_['learning_rate'],
        verbose=-1, random_state=RANDOM_STATE)

    # fit the best model with selected variables
    model.fit(X_train[selected_variables], y_train)

    # save the predicted values of test set, which are used for performance metrics calculation in R language
    y_score_train = model.predict_proba(X_train[selected_variables])
    y_score_train = pd.Series(y_score_train[:,1])
    pd.DataFrame({'True': y_train, 'Predicted': y_score_train}).to_csv('../../predicted_proba_train/inpatient/predicted_proba_18-44/' + method + '.csv', header=False, index=False)
    y_score_test = model.predict_proba(X_test[selected_variables])
    y_score_test = pd.Series(y_score_test[:,1])
    pd.DataFrame({'True': y_test, 'Predicted': y_score_test}).to_csv('../../predicted_proba_test/inpatient/predicted_proba_18-44/' + method + '.csv', header=False, index=False)

    print('\n')
    for i in selected_variables:
        print(abbr2full[i])