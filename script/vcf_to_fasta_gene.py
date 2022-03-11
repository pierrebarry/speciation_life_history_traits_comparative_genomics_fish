import matplotlib.pyplot as plt
import allel
import numpy as np
#print(allel.__version__)
import sys
import seaborn as sns
import os
import gzip
import warnings
import math
from tqdm import tqdm
import pandas as pd

	
#sys.argv=['/home/labosea1/R_SCRIPT/VCF_TO_FASTA.py','Dlabr','/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/Dlabr/pixy/Dlabr_LG1A.vcf.gz','LG1A','10000','50000']
#arg=sys.argv

sys.argv=['/home/labosea1/R_SCRIPT/VCF_TO_FASTA.py','Dlabr']
arg=sys.argv

genes_to_abc=pd.read_csv("/home/labosea1/ABC/genes_to_abc.csv")
genes_to_abc=genes_to_abc[genes_to_abc["SP"]==arg[1]]


for genes in range(genes_to_abc.shape[0]):
	vcf=allel.read_vcf('/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/Dlabr/pixy/Dlabr_'+str(genes_to_abc.iloc[genes]["CHROM"])+'.vcf.gz',fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT','variants/QUAL', 'variants/DP','calldata/DP'],region=str(genes_to_abc.iloc[genes]["CHROM"])+':'+str(genes_to_abc.iloc[genes]["START"])+'-'+str(genes_to_abc.iloc[genes]["END"])) #import vcf
	#vcf=allel.read_vcf('/DATA/sdb1/Pierre/VCF/Dlabr/Dlabr_indel5bp_snponly.vcf.gz',fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT','variants/QUAL', 'variants/DP','calldata/DP'],
	#region=str(genes_to_abc.iloc[genes]["CHROM"])+':'+str(genes_to_abc.iloc[genes]["START"])+'-'+str(genes_to_abc.iloc[genes]["END"])) #import vcf
	
	species=str(arg[1])
	samples = vcf['samples']
	pos=vcf['variants/POS']
	gt = allel.GenotypeArray(vcf['calldata/GT'])
	dp= vcf['calldata/DP']
	for sam in range(len(samples)):
		for allele in [0,1]:
			sequence=''
			for pos_1 in range(len(pos)):
				if vcf['variants/REF'][pos_1]=='N' or vcf['variants/ALT'][pos_1,1]!='' or len(vcf['variants/REF'][pos_1])>1 or len(vcf['variants/ALT'][pos_1,0])>1:
					sequence=sequence+'N'
				else:
					if gt[pos_1,sam][allele]==-1:
						sequence=sequence+'N'
					else:
						if dp[pos_1,sam]>5:
							if gt[pos_1,sam][allele]==0:
								sequence=sequence+vcf['variants/REF'][pos_1]
							else:
								sequence=sequence+vcf['variants/ALT'][pos_1,gt[pos_1,sam][allele]-1]
						else:
							sequence=sequence+'N'				
			newline=">"+str(genes_to_abc.iloc[genes]["CHROM"])+':'+str(genes_to_abc.iloc[genes]["START"])+'-'+str(genes_to_abc.iloc[genes]["END"])+'|'+str(species)+'|'+samples[sam]+'|'+'allele'+str(allele+1)
			with open(species+"_DILS.fasta","ab") as f:
				np.savetxt(f,[newline],fmt='%s',delimiter=";",newline="\n")
			newline=sequence
			with open(species+"_DILS.fasta","ab") as f:
				np.savetxt(f,[newline],fmt='%s',delimiter=";",newline="\n")

