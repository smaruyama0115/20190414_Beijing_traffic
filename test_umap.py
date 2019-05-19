# %% import library

import umap
from sklearn.datasets import load_digits
from scipy.sparse.csgraph import connected_components
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from sklearn.manifold import TSNE
import time
import pandas as pd

# %% test umap
digits = load_digits()
type(digits.target[0])
digits.target = [float(digits.target[i]) for i in range(len(digits.target))]


embedding = umap.UMAP(n_neighbors=5, n_components=2,random_state=42).fit_transform(digits.data)
plt.scatter(embedding[:,0],embedding[:,1],c=digits.target,cmap=cm.tab10)
plt.colorbar()

help(umap.UMAP)

dat_prf = pd.read_csv('./data_set_phase1/profiles.csv')
dat_que = pd.read_csv('./data_set_phase1/train_queries.csv')
dat_pln = pd.read_csv('./data_set_phase1/train_plans_expand.csv')
dat_clk = pd.read_csv('./data_set_phase1/train_clicks.csv')

df = dat_que.merge(dat_pln, how='left', on='sid')
df = df.merge(dat_clk.assign(flag_click = 1),how='left', left_on=['sid','transport_mode'], right_on=['sid','click_mode'])
df["flag_click"] = df["flag_click"].fillna(0)
df = df[df.pid.notnull()]

# 各transport_modeについて、planされた内何割がclickされたか(rate)と、一度でもclickされたか(flag_once)を付与
df = df.groupby(["pid","transport_mode"], as_index=False).agg({"flag_click":["count","sum"]})
df["rate"] = df[("flag_click","sum")]/df[("flag_click","count")]
df["flag_once"] = 0
df["flag_once"] = df.rate.mask(df.rate > 0, 1)
df = df[["pid","transport_mode","rate","flag_once"]]

df_piv = df.pivot(index = "pid", columns = "transport_mode", values = ["rate","flag_once"])
df_piv = df_piv.merge(dat_prf,how='inner',on="pid")

df_piv.head(10)

df_umap = df_piv.filter(regex = "^p\d")
embedding = umap.UMAP(n_neighbors=8, n_components=2,random_state=42).fit_transform(df_umap)

plt.scatter(embedding[:,0],embedding[:,1],c=df_piv[("rate",7.0)])
plt.colorbar()
# %% 以下らくがき

dat_pln

df_piv.reset_index()


df

help(pd.merge)
df.head(10)
df = df.drop(columns = "pid")
embedding = umap.UMAP(n_neighbors=3, n_components=2,random_state=42).fit_transform(df)

plt.scatter(embedding[:,0],embedding[:,1],cmap=cm.tab10)
plt.colorbar()

