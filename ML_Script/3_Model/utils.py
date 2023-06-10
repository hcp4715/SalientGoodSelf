import numpy as np
import pandas as pd
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import LogisticRegressionCV
from sklearn.preprocessing import MinMaxScaler, PolynomialFeatures
from sklearn.model_selection import LeaveOneGroupOut
from sklearn.dummy import DummyClassifier
from sklearn.inspection import permutation_importance
from sklearn.metrics import make_scorer, roc_auc_score

def score_func(y, y_pred):
    score = roc_auc_score(y, y_pred, multi_class="ovr")# use one vs rest
    return score

#for calculating roc_auc score in permutation importance testing
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


        preprocessing_steps = make_pipeline(MinMaxScaler(),
                                            PolynomialFeatures(degree=3, 
                                                               interaction_only=True)).fit(X[train])
        X_train_pre = preprocessing_steps.transform(X[train])
        X_test_pre = preprocessing_steps.transform(X[test])

        logi = LogisticRegressionCV(Cs = np.logspace(-6, 3, 7), cv = 5, class_weight='balanced', 
                                 random_state=123, max_iter=5000, multi_class="ovr")
        
        logi.fit(X=X_train_pre, y=y[train])
        feature_coef.append(logi.coef_)
        im = permutation_importance(logi, X_test_pre, y[test], scoring=my_scores, n_repeats=20, n_jobs=-1, random_state=123)
        feature_importance.append(im['importances_mean'])
        y_pred = logi.predict_proba(X_test_pre)
        score = roc_auc_score(y[test], y_pred, multi_class='ovr')

        df_result['score'].append(score)
        df_result['source'].append(source)
        df_result['target'].append(source)

    return pd.DataFrame(df_result), feature_importance, feature_coef



def lr_cross_task(X_source, y_source, X_target, y_target, target_group, source_name, target_name):
    '''
    use all training data to fit the model
    '''
    df_result = dict(subID=[], score=[], source=[], target=[])# source拟合的，target预测的condition
    feature_importance = []
    feature_coef = []

    preprocessing_steps = make_pipeline(MinMaxScaler(), PolynomialFeatures(degree=3, interaction_only=True)).fit(X_source)
    X_train_pre = preprocessing_steps.transform(X_source)


    logi = LogisticRegressionCV(Cs = np.logspace(-6, 3, 7), cv = 5, class_weight='balanced', 
                                 random_state=123, max_iter=5000, multi_class="ovr")

    logi.fit(X=X_train_pre, y=y_source)


    for sub in np.unique(target_group):
        idx_sub = target_group == sub
        feature_sub = X_target[idx_sub]
        label_sub = y_target[idx_sub]

        X_test = preprocessing_steps.transform(feature_sub)
        im = permutation_importance(logi, X_test, label_sub, scoring=my_scores, n_repeats=20, n_jobs=-1, random_state=123)
        feature_importance.append(im['importances_mean'])
        feature_coef.append(logi.coef_)
        y_pred = logi.predict_proba(X_test)
        score = roc_auc_score(label_sub, y_pred, multi_class="ovr")

        df_result['subID'].append(sub)
        df_result["score"].append(score)
        df_result["source"].append(source_name)
        df_result["target"].append(target_name)


    return pd.DataFrame(df_result), feature_importance, feature_coef



def dummy_within_task(X, y, group, source):
    df_result = dict(subID=[], score=[], source=[], target=[])# source拟合的，target预测的condition
    logo = LeaveOneGroupOut()
    for train, test in logo.split(X, y, groups=group):
        test_sub = np.unique(group[test])[0]
        df_result["subID"].append(test_sub)
        preprocessing_steps = make_pipeline(MinMaxScaler(),PolynomialFeatures(degree=3, interaction_only=True)).fit(X[train])
        X_train_pre = preprocessing_steps.transform(X[train])
        X_test_pre = preprocessing_steps.transform(X[test])

        dummy_clf = DummyClassifier(strategy="uniform", random_state=123)
        
        dummy_clf.fit(X= X_train_pre, y=y[train])

        y_pred = dummy_clf.predict_proba(X_test_pre)
        score = roc_auc_score(y[test], y_pred, multi_class='ovr')

        df_result['score'].append(score)
        df_result['source'].append(source)
        df_result['target'].append(source)

    return pd.DataFrame(df_result).groupby(["source", "target"]).mean().reset_index()

def lr_cross_task2(X_source, y_source, X_target, y_target, source_group, target_group, source_name, target_name):
    
    '''
    In lr_cross_task2, we can compared this with within cross validation
    use leave one subject out method to train the model
    '''
    df_result = dict(subID=[], score=[], source=[], target=[])# source拟合的，target预测的condition
    feature_importance = []
    feature_coef = []
    logo = LeaveOneGroupOut()
    for train, test in logo.split(X_source, y_source, groups=source_group):

            preprocessing_steps = make_pipeline(MinMaxScaler(), PolynomialFeatures(degree=3, interaction_only=True)).fit(X_source[train])
            X_train_pre = preprocessing_steps.transform(X_source[train])


            logi = LogisticRegressionCV(Cs = np.logspace(-6, 3, 7), cv = 5, class_weight='balanced', 
                                        random_state=123, max_iter=5000, multi_class="ovr")

            logi.fit(X=X_train_pre, y=y_source[train])


            for sub in np.unique(target_group):
                idx_sub = target_group == sub
                feature_sub = X_target[idx_sub]
                label_sub = y_target[idx_sub]

                X_test = preprocessing_steps.transform(feature_sub)
                im = permutation_importance(logi, X_test, label_sub, scoring=my_scores, n_repeats=20, n_jobs=-1, random_state=123)
                feature_importance.append(im['importances_mean'])
                feature_coef.append(logi.coef_)
                y_pred = logi.predict_proba(X_test)
                score = roc_auc_score(label_sub, y_pred, multi_class="ovr")

                df_result['subID'].append(sub)
                df_result["score"].append(score)
                df_result["source"].append(source_name)
                df_result["target"].append(target_name)


    return pd.DataFrame(df_result), feature_importance, feature_coef


def dummy_cross_task(X_source, y_source, X_target, y_target, target_group, source_name, target_name):
    df_result = dict(subID=[], score=[], source=[], target=[])# source拟合的，target预测的condition
    preprocessing_steps = make_pipeline(MinMaxScaler(), PolynomialFeatures(degree=3, interaction_only=True)).fit(X_source)
    X_train_pre = preprocessing_steps.transform(X_source)

    dummy_clf = DummyClassifier(strategy="uniform", random_state=123)

    dummy_clf.fit(X=X_train_pre, y=y_source)


    for sub in np.unique(target_group):
        idx_sub = target_group == sub
        feature_sub = X_target[idx_sub]
        label_sub = y_target[idx_sub]
        X_test = preprocessing_steps.transform(feature_sub)
        y_pred = dummy_clf.predict_proba(X_test)
        score = roc_auc_score(label_sub, y_pred, multi_class="ovr")

        df_result['subID'].append(sub)
        df_result["score"].append(score)
        df_result["source"].append(source_name)
        df_result["target"].append(target_name)


    return pd.DataFrame(df_result).groupby(["source", "target"]).mean().reset_index()

def dummy_cross_task2(X_source, y_source, X_target, y_target, source_group, target_group, source_name, target_name):
    df_result = dict(subID=[], score=[], source=[], target=[])# source拟合的，target预测的condition
    logo = LeaveOneGroupOut()
    for train, test in logo.split(X_source, y_source, groups=source_group):

            preprocessing_steps = make_pipeline(MinMaxScaler(), PolynomialFeatures(degree=3, interaction_only=True)).fit(X_source[train])
            X_train_pre = preprocessing_steps.transform(X_source[train])


            dummy_clf = DummyClassifier(strategy="uniform", random_state=123)

            dummy_clf.fit(X=X_train_pre, y=y_source[train])


            for sub in np.unique(target_group):
                idx_sub = target_group == sub
                feature_sub = X_target[idx_sub]
                label_sub = y_target[idx_sub]

                X_test = preprocessing_steps.transform(feature_sub)
                y_pred = dummy_clf.predict_proba(X_test)
                score = roc_auc_score(label_sub, y_pred, multi_class="ovr")

                df_result['subID'].append(sub)
                df_result["score"].append(score)
                df_result["source"].append(source_name)
                df_result["target"].append(target_name)


    return pd.DataFrame(df_result).groupby(["source", "target"]).mean().reset_index()