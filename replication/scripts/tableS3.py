## INPUTS
## -----------
## outcomes_blckgrp_clean_popweighted.csv

## OUTPUTS
## -----------
## TableS3.tex

import pandas as pd

# ## read in data
folder = 'path/to/replication'

outcomes = pd.read_csv(folder+'/data/clean/outcomes_blckgrp_clean_popweighted.csv')

readable_names_dict = {'idx_h':'Healthcare',
                       'idx_p':'Parks',
                       'idx_res':'Restaurants',
                       'idx_sch':'Schools',
                       'idx_ser':'Services',
                       'idx_g':'Groceries',
                       'idx_d':'Drugstore',
                       'idx_rel':'Religious',
                       'idx_ac':'Arts and Culture',
                       'idx_all_cats':'All',
                       'access_idx_crosscity':'Equal-Weighted Access'}

## select columns of interest
cols = [c for c in outcomes.columns if ((c.count('_')==1)|(c=='idx_all_cats'))&(c.startswith('idx_'))]
cols.remove('idx_all_cats')
cols.insert(0,'idx_all_cats')

cols.insert(0,'access_idx_crosscity')
cols.insert(0,'access_idx_crosscity_wt')


## find correlations
correlation_df = pd.DataFrame(outcomes[cols].corr('spearman')['access_idx_crosscity_wt']).transpose()
correlation_df = correlation_df.drop('access_idx_crosscity_wt',axis=1)
correlation_df = correlation_df.rename(columns=readable_names_dict)
correlation_df.index = (['Correlation with Access'])

## create latex formatting
latex_table = correlation_df.to_latex(column_format = 'lrrrrrrrrrrr',caption ='Comparison of local access metrics', label = 'access metrics corr', float_format="{:0.3f}".format, index=True,index_names=False)

## customize formatting to match other supplementary tables
latex_table = latex_table.replace('\\label{access metrics corr}','\\label{access metrics corr} \n\\resizebox{\\textwidth}{!}{')
latex_table = latex_table.replace('\end{tabular}','\end{tabular}}')
col_nums = ''
for i in range(1,len(correlation_df.columns)+1):
    col_nums += f'& ({i}) '
latex_table = latex_table.replace('\\midrule',f'{col_nums}\\\\ \\midrule')
latex_table = latex_table.replace("\\toprule",'\\toprule\n& &\\multicolumn{10}{c}{Counts of Amenities within a 15-minute walk (CBG-level)} \\\\\n\\cmidrule(l{3pt}r{3pt}){3-12}')

## save to a .tex file for easy input into the manuscript
text_file = open(folder+"output/TableS3.tex", "w")
text_file.write(latex_table)
text_file.close()