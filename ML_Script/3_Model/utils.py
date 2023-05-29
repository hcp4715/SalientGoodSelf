import numpy as np
import pandas as pd
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import LogisticRegressionCV
from sklearn.preprocessing import MinMaxScaler, PolynomialFeatures
from sklearn.model_selection import LeaveOneGroupOut, GridSearchCV
from sklearn.inspection import permutation_importance
from sklearn.metrics import make_scorer, roc_auc_score

def score_func(y, y_pred):
    score = roc_auc_score(y, y_pred, multi_class="ovr")# use one vs rest
    return score

my_scores = make_scorer(score_func=score_func, greater_is_better=True, needs_proba=True, needs_threshold=False)

def lr_within_task(X, y, group, source):
    '''
    In the within experiment cross validation, we use leave one subject out cross validation
    All subjects are used for fit the model but one used to test
    '''
    logo = LeaveOneGroupOut()
    df_result = dict(subID=[], score=[], source=[], target=[])# source is for fit，target is for preditt
    feature_importance = []
    feature_coef = []
    for train, test in logo.split(X, y, groups=group):
        test_sub = np.unique(group[test])[0]
        df_result["subID"].append(test_sub)
        
        logi = make_pipeline(
            MinMaxScaler(), 
            PolynomialFeatures(interaction_only=True, include_bias=False),
            LogisticRegressionCV(Cs = np.logspace(-6, 3, 7), cv = 5, class_weight='balanced', 
                                 random_state=123, max_iter=5000, multi_class="ovr"))
        
        logi.fit(X=X[train], y=y[train])
        feature_coef.append(logi.steps[-1][-1].coef_)
        im = permutation_importance(logi, X[test], y[test], scoring=my_scores, n_repeats=20, n_jobs=-1, random_state=123)
        feature_importance.append(im['importances_mean'])
        y_pred = logi.predict_proba(X[test])
        score = roc_auc_score(y[test], y_pred, multi_class='ovr')

        df_result['score'].append(score)
        df_result['source'].append(source)
        df_result['target'].append(source)

    return pd.DataFrame(df_result), feature_importance, feature_coef
