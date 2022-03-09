from csv import writer
import numpy as np
import sys
import pandas as pd

arg=sys.argv

lines = []
with open('/shared/projects/abc_fish/tree_topology/data/'+str(arg[2])+'/'+str(arg[2])+'_proba_LDhelmet_'+str(arg[1])+'.txt') as f:
    lines = f.readlines()


with open('/shared/ifbstor1/projects/abc_fish/tree_topology/data/'+str(arg[2])+'/ancestral_'+str(arg[1])+'_test.tsv','a') as f:
  f.write("{}\t{}\t{}\n".format("#CHROM","POS","AA"))


NUC=['A','C','G','T']
for line in lines:
    a=line.rstrip("\n").split(' ')
    b=list(np.float_(a[2:6]))
    AA=NUC[b.index(np.max(b))]
    ll="{}\t{}\t{}\n".format(a[0],a[1],AA)
    with open('/shared/ifbstor1/projects/abc_fish/tree_topology/data/'+str(arg[2])+'/ancestral_'+str(arg[1])+'_test.tsv','a+') as f:
      _ = f.write(ll)

ancestral=pd.read_csv('/shared/ifbstor1/projects/abc_fish/tree_topology/data/'+str(arg[2])+'/ancestral_'+str(arg[1])+'_test.tsv', sep='\t')
ancestral=ancestral.assign(chrom_pos=ancestral["#CHROM"].astype(str)+ancestral["POS"].astype(str))
ancestral=ancestral.drop_duplicates("chrom_pos")
del ancestral["chrom_pos"]
ancestral.to_csv('/shared/ifbstor1/projects/abc_fish/tree_topology/data/'+str(arg[2])+'/ancestral_'+str(arg[1])+'_test.tsv',sep='\t',index=False)
