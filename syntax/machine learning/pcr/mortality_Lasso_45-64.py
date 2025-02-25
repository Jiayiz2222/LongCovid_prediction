import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
import warnings

# Read data
warnings.filterwarnings('ignore')
RANDOM_STATE = 0
cohort_train = pd.read_csv('../../dataset/pcr/train.pcr_45-64.csv')
cohort_test = pd.read_csv('../../dataset/pcr/test.pcr_45-64.csv')
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
    # Logistic Regression
    method = 'Lasso'
    print('*********** ' + method + ' ***********')
    model = LogisticRegression(penalty='l1', solver='liblinear', random_state=RANDOM_STATE)
    model.fit(X_train, y_train)

    # save the predicted values of test set, which are used for performance metrics calculation in R language
    y_score_train = model.predict_proba(X_train)
    y_score_train = pd.Series(y_score_train[:,1])
    pd.DataFrame({'True': y_train, 'Predicted': y_score_train}).to_csv('../../predicted_proba_train/pcr/predicted_proba_45-64/' + method + '.csv', header=False, index=False)
    y_score_test = model.predict_proba(X_test)
    y_score_test = pd.Series(y_score_test[:,1])
    pd.DataFrame({'True': y_test, 'Predicted': y_score_test}).to_csv('../../predicted_proba_test/pcr/predicted_proba_45-64/' + method + '.csv', header=False, index=False)

    # print the selected variables
    selected_variables = list(np.array(X_train.columns.tolist())[np.array(model.coef_,dtype=bool)[0]])
    for i in selected_variables:
        print(abbr2full[i])