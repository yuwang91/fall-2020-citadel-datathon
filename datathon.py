# -*- coding: utf-8 -*-
"""
Created on Tue Sep 29 19:26:52 2020

@author: yuwan
"""

import pandas as pd
import numpy as np

df = pd.read_csv('C:/Users/yuwan/Dropbox/Stanford/Year 2/datathon/movie_lense/movie_lense/movies.csv')

out = []
for i in range(len(df)):
    l = df['genres'][i].split('|')
    mId = df['movieId'][i]
    for j in l:
        out.append([mId,j])
    if i%1000 == 0:
        print i
        
out_df = pd.DataFrame(out)
out_df[2] = 1

pivot_df = out_df.pivot(index = 0, columns = 1, values = 2).fillna(0)
pivot_df['movieId'] = pivot_df.index
cols = pivot_df.columns.tolist()
cols = cols[-1:] + cols[:-1]
pivot_df = pivot_df[cols]

pivot_df.to_csv('C:/Users/yuwan/Dropbox/Stanford/Year 2/datathon/movie_lense/movie_lense/movies_pivot.csv', index = False)