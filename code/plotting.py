#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Oct  3 13:37:48 2020

@author: samirkhan
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df = pd.read_csv("industryDataForSamirFixed.csv", encoding='latin1')
df = df[["PercentReturn", "AvgStarScore", "DirectQual", "ActorQual"]]


plt.figure(figsize = (5, 5))
g = sns.PairGrid(df, vars = ["PercentReturn", "AvgStarScore"], 
                 hue = "ActorQual", diag_sharey = False)
g.map_diag(sns.kdeplot, fill = True)
g.map_lower(sns.kdeplot)
g.map_upper(sns.scatterplot, alpha = 0.5, s = 12)
g.add_legend()

plt.savefig("actor.png", dpi = 400)

#sns.pairplot(df, vars = ["PercentReturn", "AvgStarScore"], 
             #hue = "DirectQual", kind = "kde")

# x_vars = ["PercentReturn", "AvgStarScore", "DirectQual", "ActorQual"]
# y_vars = ["PercentReturn", "AvgStarScore"]
# g = sns.PairGrid(df, diag_sharey = False)

# sns.histplot(x = df["PercentReturn"], ax = g.axes[0][0], stat = "density")
# sns.histplot(x = df["AvgStarScore"], ax = g.axes[1][1])
# sns.countplot(x = df["DirectQual"], ax = g.axes[2][2])
# sns.countplot(x = df["ActorQual"], ax = g.axes[3][3])

# #g.axes[2][2].set(xlabel = None, ylabel = None)
# #g.axes[3][3].set(ylabel = None)

# sns.stripplot(x = df["DirectQual"], y = df["PercentReturn"], ax = g.axes[0][2])

#g.map_diag(sns.histplot, color=".3")
#g.map_offdiag(sns.scatterplot, color=".3")

#sns.stripplot(x = g.axes[0][3])
#g.map_offdiag(sns.scatterplot)

