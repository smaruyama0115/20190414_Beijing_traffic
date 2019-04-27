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
from sklearn.metrics import roc_auc_score, roc_curve, auc, confusion_matrix, classification_report
from sklearn.model_selection import train_test_split
import ast

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

# %% load Data

dat_que = pd.read_csv('./data_set_phase1/train_queries.csv')
dat_pln = pd.read_csv('./data_set_phase1/train_plans.csv')
dat_clk = pd.read_csv('./data_set_phase1/train_clicks.csv')
dat_prf = pd.read_csv('./data_set_phase1/profiles.csv')

# %% check_head
dat_que.head()
dat_pln.head()
dat_clk.head()
# type(dat_clk.click_time[0])
dat_prf.head()

# %% count
print(len(dat_que))
print(len(dat_pln))
print(len(dat_clk))
print(len(dat_prf))

# %% qurty
dat_que.info()

# %% transform

dat_que.req_time = pd.to_datetime(dat_que.req_time)
dat_que.pid = dat_que.pid.fillna(0).astype(int)
dat_que['o_lon'] = dat_que.o.apply(lambda x: x.split(',')[0])
dat_que['o_lat'] = dat_que.o.apply(lambda x: x.split(',')[1])
dat_que['d_lon'] = dat_que.d.apply(lambda x: x.split(',')[0])
dat_que['d_lat'] = dat_que.d.apply(lambda x: x.split(',')[1])

dat_que.head()

# %% plan

dat_pln.head()

dat_pln_ext = dat_pln.copy()
dat_pln_ext['plans'] = dat_pln_ext['plans'].apply(lambda x: re.findall(r'{.+?}',x))
dat_pln_ext['plans'] = dat_pln_ext['plans'].apply(lambda x: list(map(ast.literal_eval,x)))
dat_pln_ext = json_normalize(dat_pln_ext.to_dict("records"),"plans", ["sid","plan_time"])
dat_pln_ext['display_order'] = dat_pln_ext.groupby('sid').cumcount()+1

dat_pln_ext = dat_pln_ext[['sid','plan_time','transport_mode','distance','eta','price','display_order']]
dat_pln_ext['price'] = dat_pln_ext['price'].replace('','0')
dat_pln_ext['price'] = dat_pln_ext['price'].astype(int)

dat_pln_ext.info()
dat_pln_ext.head(10)

# %% dat_pln_ext の保存
dat_pln_ext.to_csv('./data_set_phase1/train_plans_expand.csv')

















