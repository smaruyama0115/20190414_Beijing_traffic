# %% ライブラリのimport
import sys
import os
import re
import zipfile
import math
import random
import datetime
import pandas as pd
import numpy as np
import numexpr
%matplotlib inline
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.font_manager import FontProperties
import matplotlib.font_manager as fm
import matplotlib.ticker as tick
import seaborn as sns
sns.set_style('whitegrid')
# prop = fm.FontProperties(fname='/usr/share/fonts/truetype/fonts-japanese-gothic.ttf')
from fastparquet import ParquetFile
from skimage import io
from PIL import Image
#from scipy.misc import imresize
from skimage.transform import rescale, resize
import datetime
from pandas.io.json import json_normalize
import json
import feather
import gc
from sklearn.metrics import roc_auc_score, roc_curve, auc, confusion_matrix, classification_report, precision_recall_fscore_support
from sklearn.model_selection import train_test_split
import ast
import xgboost as xgb
import shap
import time

# %% PICKLE FUNCTION
import pickle
def unpickle(filename):
    with open(filename, 'rb') as fo:
        p = pickle.load(fo)
    return p

def to_pickle(filename, obj):
    with open(filename, 'wb') as f:
        pickle.dump(obj, f, -1)

# %% SQL FUNCTION
import psycopg2
def get_data(query, db='gp6s', pdconv=True):
    if db == 'gp6s':
        conn = psycopg2.connect('dbname=traffic host=192.168.50.1 user=v14_demizu_tsukasa password= port=5432')
    else: pass
    cur = conn.cursor()
    cur.execute(query)
    dat = cur.fetchall()
    cur.close()
    conn.close()
    if pdconv:
        dat = pd.DataFrame(dat)
        dat.columns = [item[0] for item in cur.description]
    return dat

import warnings
warnings.filterwarnings('ignore')

# %% load data
data_dir = './data_set_phase1/'

def loadData(data_dir, is_train=True):
    f_head = 'train' if is_train else 'test'

    # load csv
    dat_que = pd.read_csv(data_dir + f_head + '_queries.csv')
    dat_pln = pd.read_csv(data_dir + f_head + '_plans_expand.csv')

    # transform query data
    dat_que['req_time'] = pd.to_datetime(dat_que['req_time'])
    dat_que['pid'] = dat_que['pid'].fillna(0).astype(int)
    dat_que['o_lon'] = dat_que.apply(lambda x: x['o'].split(',')[0], axis=1)
    dat_que['o_lat'] = dat_que.apply(lambda x: x['o'].split(',')[1], axis=1)
    dat_que['d_lon'] = dat_que.apply(lambda x: x['d'].split(',')[0], axis=1)
    dat_que['d_lat'] = dat_que.apply(lambda x: x['d'].split(',')[1], axis=1)

    # transform plan data
    dat_pln['plan_time'] = pd.to_datetime(dat_pln['plan_time'])

    if is_train: # only training data
        dat_clk = pd.read_csv(data_dir + f_head + '_clicks.csv')
        dat_prf = pd.read_csv(data_dir + 'profiles.csv')

        # transform click data
        dat_clk['click_time'] = pd.to_datetime(dat_clk['click_time'])

        # transform profile
        n_profile = 66
        for i in range(n_profile):
            col = 'p{}'.format(i)
            dat_prf[col] = dat_prf[col].astype(int)

    # return
    if is_train:
        return dat_que, dat_pln, dat_clk, dat_prf
    else:
        return dat_que, dat_pln

dat_que, dat_pln, dat_clk, dat_prf = loadData(data_dir, is_train=True)
test_que, test_pln = loadData(data_dir, is_train=False)

# %% join data

# %% queries & plans
def mergeQueryPlan(dat_que, dat_pln):
    df = pd.merge(dat_que,dat_pln, how='inner', on='sid')

    return df
df = mergeQueryPlan(dat_que, dat_pln)
df_test = mergeQueryPlan(test_que, test_pln)

# %% clicks(only train)
def mergeDfClick(df, dat_clk):
    df = pd.merge(df, dat_clk[['sid','click_mode']],
                  how='left',
                  on='sid')
    df['click_mode'] = df['click_mode'].fillna(0).astype(int)
    return df

df = mergeDfClick(df, dat_clk)

# %% non-click mode
def addNoneClickMode(df):
    # represantate 'non-click class'
    dat_zero = df.copy()

    dat_zero['transport_mode'] = 0
    dat_zero['distance'] = np.NaN
    dat_zero['eta'] = np.NaN
    dat_zero['price'] = np.NaN

    dat_zero = dat_zero.drop_duplicates()

    df = pd.concat([df, dat_zero], axis=0, ignore_index=True)
    df = df.sort_values(by=['req_time','sid'])\
           .reset_index(drop=True)
    return df

df = addNoneClickMode(df)
df_test = addNoneClickMode(df_test)

# %% label
df['target'] = df['transport_mode']==df['click_mode']

# %% Features
# data of 2018-10-02:missing

# %% context
def addContextFeatures(df):
    df['c_dow'] = df['req_time'].dt.dayofweek
    df['c_hour'] = df['req_time'].dt.hour

    # Chinese holiday (国慶節: 10/1-10/7)
    df['is_holiday'] = df['req_time'].dt.date\
                .isin([datetime.date(2018,10,i+1) for i in range(7)])
    return df
df = addContextFeatures(df)
df_test = addContextFeatures(df_test)

# %% plan features
def addPlanFreatures(df):
    # number of transport mode in a plan
    df = pd.merge(df,
                  df.groupby(['sid'],as_index=False)[['transport_mode']]\
                    .count()\
                    .rename(columns={'transport_mode':'num_mode'}),
                 how='left',on='sid')

    # ranking
    df['rank_distance'] = df.groupby(['sid'])['distance'].rank()
    df['rank_eta'] = df.groupby(['sid'])['eta'].rank()
    df['rank_price'] = df.assign(
                            price = lambda df: df['price'].apply(lambda row: row if row>0 else np.NaN)
                       ).groupby(['sid'])['price'].rank()

    # flag of lowest (distance, eta, price) and its combination
    df = pd.merge(df,
                  df.groupby(['sid'],as_index=False)[['rank_distance','rank_eta','rank_price']]\
                      .min()\
                      .rename(columns={'rank_distance':'min_rank_distance'
                                       ,'rank_eta':'min_rank_eta'
                                       ,'rank_price':'min_rank_price'}),
                  how='left',on='sid')

    # single
    df['flag_min_distance'] = df['rank_distance']==df['min_rank_distance']
    df['flag_min_eta'] = df['rank_eta']==df['min_rank_eta']
    df['flag_min_price'] = df['rank_price']==df['min_rank_price']
    # combinatation
    df['flag_min_de'] = (df['rank_distance']==df['min_rank_distance'])&\
                        (df['rank_eta']==df['min_rank_eta'])
    df['flag_min_dp'] = (df['rank_distance']==df['min_rank_distance'])&\
                        (df['rank_price']==df['min_rank_price'])
    df['flag_min_ep'] = (df['rank_eta']==df['min_rank_eta'])&\
                        (df['rank_price']==df['min_rank_price'])
    # triple
    df['flag_lowest'] = (df['rank_distance']==df['min_rank_distance'])&\
                        (df['rank_eta']==df['min_rank_eta'])&\
                        (df['rank_price']==df['min_rank_price'])
    del df['min_rank_distance']
    del df['min_rank_eta']
    del df['min_rank_price']

    # sum of rank
    df['sum_rank_de'] = df['rank_distance'] + df['rank_eta']
    df['sum_rank_dp'] = df['rank_distance'] + df['rank_price']
    df['sum_rank_ep'] = df['rank_eta'] + df['rank_price']
    df['sum_rank'] = df['rank_distance'] + df['rank_eta'] + df['rank_price']

    return df

df = addPlanFreatures(df)
df_test = addPlanFreatures(df_test)

# %% 公共交通機関(Public mode = 1,2,7,9,11)に絞ってeta,priceの最小を求める
#def addPlanFreaturesPub(df):
def addPlanFreaturesPublic(df):
    mode_public = [1,2,7,9,11]
    df_pub = df[df.transport_mode.apply(lambda x: x in mode_public)][['sid','transport_mode','distance','price','eta']]

    df_pub['rank_distance_pub'] = df_pub.groupby(['sid'])['distance'].rank()
    df_pub['rank_eta_pub']      = df_pub.groupby(['sid'])['eta'].rank()
    df_pub['rank_price_pub']    = df_pub.groupby(['sid'])['price'].rank()

    df_pub = pd.merge(df_pub,
                  df_pub.groupby(['sid'],as_index=False)[['rank_distance_pub','rank_eta_pub','rank_price_pub']]\
                      .min()\
                      .rename(columns={'rank_distance_pub':'min_rank_distance_pub'
                                       ,'rank_eta_pub':'min_rank_eta_pub'
                                       ,'rank_price_pub':'min_rank_price_pub'}),
                  how='left',on='sid')

    # single
    df_pub['flag_min_distance_pub'] = df_pub['rank_distance_pub']==df_pub['min_rank_distance_pub']
    df_pub['flag_min_eta_pub'] = df_pub['rank_eta_pub']==df_pub['min_rank_eta_pub']
    df_pub['flag_min_price_pub'] = df_pub['rank_price_pub']==df_pub['min_rank_price_pub']
    # combinatation
    df_pub['flag_min_de_pub'] = (df_pub['rank_distance_pub']==df_pub['min_rank_distance_pub'])&\
                            (df_pub['rank_eta_pub']==df_pub['min_rank_eta_pub'])
    df_pub['flag_min_dp_pub'] = (df_pub['rank_distance_pub']==df_pub['min_rank_distance_pub'])&\
                            (df_pub['rank_price_pub']==df_pub['min_rank_price_pub'])
    df_pub['flag_min_ep_pub'] = (df_pub['rank_eta_pub']==df_pub['min_rank_eta_pub'])&\
                            (df_pub['rank_price_pub']==df_pub['min_rank_price_pub'])
    # triple
    df_pub['flag_lowest_pub'] = (df_pub['rank_distance_pub']==df_pub['min_rank_distance_pub'])&\
                            (df_pub['rank_eta_pub']==df_pub['min_rank_eta_pub'])&\
                            (df_pub['rank_price_pub']==df_pub['min_rank_price_pub'])
    del df_pub['min_rank_distance_pub']
    del df_pub['min_rank_eta_pub']
    del df_pub['min_rank_price_pub']

    # sum of rank
    df_pub['sum_rank_de_pub'] = df_pub['rank_distance_pub'] + df_pub['rank_eta_pub']
    df_pub['sum_rank_dp_pub'] = df_pub['rank_distance_pub'] + df_pub['rank_price_pub']
    df_pub['sum_rank_ep_pub'] = df_pub['rank_eta_pub'] + df_pub['rank_price_pub']
    df_pub['sum_rank_pub'] = df_pub['rank_distance_pub'] + df_pub['rank_eta_pub'] + df_pub['rank_price_pub']

    df = df.merge(df_pub.drop(columns=['distance','price','eta']), how="left", on=["sid","transport_mode"])
    df = df.fillna({
        'rank_distance_pub':7,
        'rank_eta_pub':7,
        'rank_price_pub':7,
        'flag_min_distance_pub':False,
        'flag_min_eta_pub':False,
        'flag_min_price_pub':False,
        'flag_min_de_pub':False,
        'flag_min_dp_pub':False,
        'flag_min_ep_pub':False,
        'flag_lowest_pub':False,
        'sum_rank_de_pub':14,
        'sum_rank_dp_pub':14,
        'sum_rank_ep_pub':14,
        'sum_rank_pub':21
    })

    return df

df = addPlanFreaturesPublic(df)
df_test = addPlanFreaturesPublic(df_test)

# df_pub = df[df.transport_mode.apply(lambda x: x in mode_public)][['sid','transport_mode','eta','price']]
#
#
# df_pub_min_eta_pub = df_pub[df_pub.groupby('sid')['eta'].idxmin()]
# df_pub_min_eta["min_rank_eta_pub"] = 1
#
# df_pub_min_price_pub = df_pub[df_pub.groupby('sid')['eta'].idxmin()]
#
#
# df_pub_min_eta = df_pub[df_pub.groupby('sid',as_index=False)['eta'].idxmin()]
# df_pub_min_eta["min_rank_eta_pub"] = 1
#
# df_pub_min_price = df_pub.loc[df_pub.groupby('sid')['price'].idxmin(),:]
# df_pub_min_price["min_rank_price_pub"] = 1
#
# df_pub
# df_pub_min_eta

# %% user profile
def addUserProfile(df):
    df = pd.merge(df, dat_prf, how='left', on='pid')

    return df
df = addUserProfile(df)
df_test = addUserProfile(df_test)

# %% Node info
split_time = datetime.datetime(2018,11,24,0,0,0)
dat_hist = df[(df.target)&(df.req_time<split_time)]
print(len(dat_hist.o.unique()))
print(len(dat_hist.d.unique()))

dat_hist_o=\
dat_hist.pivot_table(values='sid',
                     index='o',
                     columns='transport_mode',
                     fill_value=0,
                     aggfunc='count')

dat_hist_o.columns = [f'hist_o_{i:02d}' for i in dat_hist_o.columns]

_dat_hist_o_sum = dat_hist_o.sum(axis=1)
dat_hist_o = dat_hist_o.div(_dat_hist_o_sum, axis=0)
dat_hist_o['hist_o_sum'] = _dat_hist_o_sum

dat_hist_d=\
dat_hist.pivot_table(values='sid',
                     index='d',
                     columns='transport_mode',
                     fill_value=0,
                     aggfunc='count')
dat_hist_d.columns = ['hist_d_{0:02d}'.format(i) for i in dat_hist_d.columns]
_dat_hist_d_sum = dat_hist_d.sum(axis=1)
dat_hist_d = dat_hist_d.div(_dat_hist_d_sum, axis=0)
dat_hist_d['hist_d_sum'] = _dat_hist_d_sum

# 過学習を防ぐために、dat_hist_o_sum or dat_hist_d_sum >= 5 のo,d のみに絞る
#dat_hist_o = dat_hist_o[lambda df: df.hist_o_sum >= 5]
#dat_hist_d = dat_hist_d[lambda df: df.hist_d_sum >= 5]

def addNodeInfo(df):
    df = pd.merge(df, dat_hist_o, how='left', on = 'o')
    df = pd.merge(df, dat_hist_d, how='left', on = 'd')

    return df

df = addNodeInfo(df)
df_test = addNodeInfo(df_test)

# %% profiles histogram

# %% pid ごと transport_modeのヒストグラムを作成
# df_for_profiles = df[lambda df: df.pid != 0]
#
# df_for_profiles['target'] = df_for_profiles['target'].astype(int)
#
# dat_hist_profiles = \
# df_for_profiles.pivot_table(
#     values = 'target',
#     index  = 'pid',
#     columns = 'transport_mode',
#     fill_value = 0,
#     aggfunc = 'sum'
# )
#
# dat_hist_profiles.columns = [f'hist_prof_{i:02d}' for i in dat_hist_profiles]
# _dat_hist_profiles = dat_hist_profiles.sum(axis=1)
# dat_hist_profiles  = dat_hist_profiles.div(_dat_hist_profiles, axis=0)
# dat_hist_profiles['hist_prof_sum'] = _dat_hist_profiles
#
# # 過学習を防ぐため、hist_prof_sum >= 5 のprofileのみを使用する。
# dat_hist_profiles = dat_hist_profiles[lambda df: df.hist_prof_sum >= 5]
#
# def addProfilesHist(df,dat_hist_profiles):
#     df = pd.merge(df, dat_hist_profiles, how='left', on = 'pid')
#     return df
#
# df = addProfilesHist(df,dat_hist_profiles)
# df_test = addProfilesHist(df_test,dat_hist_profiles)

# %% to csv
#df.to_csv('./data_set_phase1/df.csv',index=False)
#df_test.to_csv('./data_set_phase1/df_test.csv',index=False)

#df = pd.read_csv('./data_set_phase1/df.csv')
#df_test = pd.read_csv('./data_set_phase1/df_test.csv')


# %% dummy
def addDummyCols(df, col_dummy):
    df_dm = df.copy()

    for col in col_dummy:
        tmp = pd.get_dummies(df[col])
        tmp.columns = [col+'_{}'.format(i) for i in tmp.columns]
        df_dm = pd.concat([df_dm, tmp], axis=1)
        del tmp

    return df_dm

col_dummy = ['transport_mode', 'c_dow', 'c_hour']

df_dm = addDummyCols(df, col_dummy)
df_test_dm = addDummyCols(df_test, col_dummy)

# %% Model

# %% predictor
not_use_col = ['is_train', 'target',
               'sid', 'pid',
               'req_time', 'c_dow', 'c_hour',
               'o', 'd', 'o_lon', 'o_lat', 'd_lon', 'd_lat',
               'plan_time', 'transport_mode', 'click_mode']

# col_for_profiles = ['is_train', 'target',
#                #'sid', 'pid',
#                'req_time', 'c_dow', 'c_hour',
#                'o', 'd', 'o_lon', 'o_lat', 'd_lon', 'd_lat',
#                'plan_time', 'transport_mode', 'click_mode']

col_id = 'sid'
target = 'target'
predictors = [col for col in df_dm.columns if col not in not_use_col]
#predictors_for_profiles = [col for col in df_dm.columns if col not in col_for_profiles]
len(predictors)
np.array(predictors).reshape(-1,1)

# %% validation
SEED = 42
val_size = 0.15

df_dm['is_train'] = df['req_time'] < split_time
X_train, y_train = df_dm[df_dm.is_train][predictors], df_dm[df_dm.is_train][target]
X_test, y_test = df_dm[~df_dm.is_train][predictors], df_dm[~df_dm.is_train][target]

df_dm["flag_min_distance_pub"].dtype

#X_train, y_train = df_dm[df_dm.is_train][predictors_for_profiles], df_dm[df_dm.is_train][target]
#X_test, y_test = df_dm[~df_dm.is_train][predictors_for_profiles], df_dm[~df_dm.is_train][target]

X_train, X_val, y_train, y_val  = train_test_split(X_train, y_train,
                                                   test_size=val_size, random_state=SEED)
print(X_train.values.shape)
print(X_val.values.shape)
print(X_test.values.shape)

# %% XGboost

xgb_train = xgb.DMatrix(X_train, y_train)
xgb_eval  = xgb.DMatrix(X_val, y_val)
# param = {'max_depth':8
#          ,'learning_rate':0.005
#          ,'objective':'binary:logistic'
#          ,'eval_metric':'logloss'
#          ,'evals':[(xgb_train, 'train'),(xgb_eval, 'eval')]
#          ,'seed':42
#          #,'nthread':88
#          #,'tree_method':'gpu_hist'
#          #,'updater':'grow_gpu_hist'
#          #,'device':'gpu'
#          ,'tree_method':'auto'
#          ,'updater':'grow_colmaker'
#          ,'device':'cpu'
#          ,'silent':1
#          ,'early_stopping_rounds':10
#          ,'subsample':.9
#          ,'gamma':.1
#          ,'alpha':0
#          ,'lambda':1
#          ,'min_child_weight':.9
#          ,'colsample_bytree':.9
#         }
# xgb_m = xgb.train(param
#                  ,xgb_train
#                  ,num_boost_round=1000)

# 出水さんパラメータで計算したら重すぎたので、defaultで計算する。
# 100で2時間半程度なので、1000なら25時間程度で終わりそう

# help(xgb.train)

params = {
    # 二値分類問題
    'objective': 'binary:logistic'
    # 評価指標
    ,'eval_metric': 'logloss'
    ,'evals':[(xgb_train, 'train'),(xgb_eval, 'eval')]
    ,'seed':42
}

start = time.time()
xgb_m = xgb.train(params = params, dtrain = xgb_train, num_boost_round=100)
elapsed_time = time.time() - start
print ("elapsed_time:{0}".format(elapsed_time) + "[sec]")

# %% predict
y_pred = xgb_m.predict(xgb.DMatrix(X_test))
df_local = df_dm[~df_dm.is_train]
df_local['pred'] = y_pred

# %% to csv
df_local.to_csv('./data_set_phase1/df_local_hist_over_5_with_public_mode.csv',index=False)
#df_local = pd.read_csv('./data_set_phase1/df_local_hist_over_5_with_public_mode.csv')

# %% plot auc
fpr, tpr, thresholds = roc_curve(y_test, y_pred.reshape([-1,]))
roc_auc = auc(fpr, tpr)

sns.set_style('whitegrid')
plt.figure(figsize=(5,5))
plt.plot(fpr, tpr,)
plt.plot([0,1],[0,1],'k--')
_=plt.title('ROC curve (%0.3f)' % roc_auc)

# %% print classification_report
print(classification_report(y_test * 1, np.round(y_pred), digits=4))

# %% recommend model
y_argmax = df_local.loc[df_local.groupby(['sid'])['pred'].idxmax(),['sid','transport_mode']]
y_argmax.columns = ['sid','recommend_mode']

df_local = pd.merge(df_local, y_argmax, how='left', on='sid')
df_result = df_local[df_local.target]


# there are same mode in one sugest
df_result = df_result.drop_duplicates(subset=['sid','pid','req_time'],keep='first')

# df_result.head(1000)

# %% F1 score @ local
precision_recall_fscore_support(df_result.click_mode,
                                df_result.recommend_mode,
                                average='weighted')

# 出水さんコード：(0.6486888562514959, 0.716253238160911, 0.6662247113296703, None)
# User profileごとのヒストグラムを追加：(0.7430139487418471, 0.7617831783364706, 0.7329085265427089, None) (過学習してそう)
# ↑のover_fit対策後：(0.7081808606577332, 0.7404252939970554, 0.7035039818722723, None)
# over fit対策 + public mode のrankを追加 : (0.6445797399533603, 0.715358667089103, 0.664229244528468, None)
# over fit対策のみ ; (0.6486888562514959, 0.716253238160911, 0.6662247113296703, None)
#

print(classification_report(df_result.click_mode,df_result.recommend_mode,digits=4))

# %% check pivot table

df_result_pivot = df_result.pivot_table(values='sid',
                      index='click_mode',
                      columns='recommend_mode',
                      aggfunc='count',
                      fill_value=0)

df_result_pivot_sum = df_result_pivot.sum(axis=0)
df_result_pivot_rate = df_result_pivot.div(df_result_pivot_sum, axis=1)

df_result_pivot_rate

sns.heatmap(
        df_result_pivot_rate
        ,linewidth=.05, square=True, linecolor='gray',cmap= 'inferno'
   )

df_result_pivot_sum_c = df_result_pivot.sum(axis=1)
df_result_pivot_rate_c = df_result_pivot.div(df_result_pivot_sum_c, axis=0)

df_result_pivot_rate_c

sns.heatmap(
        df_result_pivot_rate_c
        ,linewidth=.05, square=True, linecolor='gray',cmap= 'inferno'
   )

# %% submit

y_sub = xgb_m.predict(xgb.DMatrix(df_test_dm[predictors]))
df_test_dm['pred'] = y_sub
y_sub_argmax = df_test_dm.loc[df_test_dm.groupby(['sid'])['pred'].idxmax(),
                              ['sid','transport_mode']]
y_sub_argmax.columns = ['sid','recommend_mode']
y_sub_argmax.recommend_mode.value_counts().sort_index()

import csv
y_sub_argmax.to_csv('./model/190430_submit_hist_over_5_with_public_mode.csv',
                     index=None,
                     quoting=csv.QUOTE_ALL)

# %% らくがき

# %% 各pidについて、「planでレコメンドされてclickした回数」と「1回でもclickしたか」を特徴量に入れたい。
# dat_que, dat_pln, dat_clk, dat_prf
# test_que, test_pln


# %% dat_queとtest_queの間に重複しているpidはいくつあるか
dat_que_pid_unique  = dat_que['pid'][lambda x: x != 0].unique()
test_que_pid_unique = test_que['pid'][lambda x : x != 0].unique()
intersect_que_pid_unique = np.intersect1d(dat_que_pid_unique,test_que_pid_unique)
print(f'dat_que_pid_unique = {len(dat_que_pid_unique)}')
print(f'test_que_pid_unique = {len(test_que_pid_unique)}')
print(f'intersect = {len(intersect_que_pid_unique)}')
print(f'intersect_rate = {len(intersect_que_pid_unique)/len(test_que_pid_unique)}')

# testデータの70%のユーザーはtrainデータのpidと重複している

# %% dat_que,pln,clkをmerge
# %% queries & plans
df_for_profiles = mergeQueryPlan(dat_que, dat_pln)
df_for_profiles = mergeDfClick(df_for_profiles, dat_clk)
df_for_profiles = df_for_profiles[lambda df: df.pid != 0]

df_for_profiles = \
    df_for_profiles. \
    assign(target = 0). \
    assign(target = lambda df : df.target.mask(df.transport_mode == df.click_mode,1))

dat_hist_profiles = \
df_for_profiles.pivot_table(
    values = 'target',
    index  = 'pid',
    columns = 'transport_mode',
    fill_value = 0,
    aggfunc = 'sum'
)

dat_hist_profiles.columns = [f'hist_prof_{i:02d}' for i in dat_hist_profiles]
_dat_hist_profiles = dat_hist_profiles.sum(axis=1)
dat_hist_profiles  = dat_hist_profiles.div(_dat_hist_profiles, axis=0)
dat_hist_profiles['hist_prof_sum'] = _dat_hist_profiles

dat_hist_profiles

# dat_hist.pivot_table(values='sid',
#                      index='d',
#                      columns='transport_mode',
#                      fill_value=0,
#                      aggfunc='count')
#
# dat_hist_d.columns = ['hist_d_{0:02d}'.format(i) for i in dat_hist_d.columns]
# _dat_hist_d_sum = dat_hist_d.sum(axis=1)
# dat_hist_d = dat_hist_d.div(_dat_hist_d_sum, axis=0)
# dat_hist_d['hist_d_sum'] = _dat_hist_d_sum

# df_for_profiles.groupby(['pid','transport_mode']).agg({'target':['mean','count']})

df_for_profiles.head(10)

df.columns

np.array(df_test.columns).reshape(-1,1)

np.array(df.columns).reshape(-1,1)


df_dm = addDummyCols(df, col_dummy)
df_test_dm = addDummyCols(df_test, col_dummy)







