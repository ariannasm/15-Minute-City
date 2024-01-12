## INPUTS
## -----------
## outcomes_urb_clean_popweighted.csv
## outcomes_blckgrp_clean_popweighted.csv

## OUTPUTS
## -----------
## TableS2.tex

import pandas as pd
import numpy as np
from scipy.stats import spearmanr
import geopandas as gpd
from shapely.geometry import Point
import math
from tqdm import tqdm
from wquantiles import median
from haversine import haversine

folder = 'path/to/replication'

idx = pd.read_csv(folder+'data/clean/outcomes_blckgrp_clean_popweighted.csv')
idx_urb = pd.read_csv(folder+'data/clean/outcomes_urb_clean_popweighted.csv')


measure_label_dict = {'usage_pct_all_cats_home_bg_based': '15 min. usage',
                      'usage_10': '10 min. usage',
                      'usage_20': '20 min. usage',
                      'usage_25': '25 min. usage',
                      'med_dist': 'Med. trip dist. (km)'}

df = pd.DataFrame()
for measure in ['usage_pct_all_cats_home_bg_based','usage_10','usage_20','usage_25','med_dist']:
    corr_urb = np.round(spearmanr(idx_urb.usage_pct_all_cats_home_bg_based,idx_urb[measure],nan_policy='omit')[0],3)
    corr_bg = np.round(spearmanr(idx.usage_pct_all_cats_home_bg_based,idx[measure],nan_policy='omit')[0],3)
    mn_urb = np.mean(idx_urb[measure])
    mn_bg = np.mean(idx[measure])
    df = pd.concat([df, pd.DataFrame.from_records([{"Alternate measure":measure_label_dict[measure], 
                    'Neighborhood level correlation':corr_bg, 'Neighborhood level mean':mn_bg,
                    'Urban area level correlation':corr_urb, 'Urban area level mean':mn_urb}])], ignore_index=True)
bg_obs = '{:,}'.format(len(idx))
urb_obs = '{:,}'.format(len(idx_urb))



df = pd.concat([df, pd.DataFrame.from_records([{"Alternate measure":'Observations', 
                    'Neighborhood level correlation':bg_obs, 'Neighborhood level mean':bg_obs,
                    'Urban area level correlation':urb_obs, 'Urban area level mean':urb_obs}])], ignore_index=True)


df = df.set_index('Alternate measure')

latex_table = df.to_latex(column_format = 'C{4cm}C{3cm}C{3cm}C{3cm}C{3cm}',caption ='Correlation of various measures of local living with 15 min. usage', label = 'tbl:corr cut-offs', float_format="{:0.3f}".format, index=True,index_names=False)
latex_table = latex_table.replace('\\label{tbl:corr cut-offs}','\\label{tbl:corr cut-offs} \n\\resizebox{\\textwidth}{!}{')
latex_table = latex_table.replace('\end{tabular}','\end{tabular}}')
latex_table = latex_table.replace('& Urban area level mean \\\\','& Urban area level mean \\\\\n & (1) & (2) & (3) & (4)\\\\')

text_file = open(folder+"output/TableS2.tex", "w")
text_file.write(latex_table)
text_file.close()
