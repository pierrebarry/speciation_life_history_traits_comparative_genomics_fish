##bcftools view -H /DATA/sdb1/Pierre/VCF/Dlabr/Dlabr_indel5bp_snponly.vcf.gz | awk '{print $1,$2,$4,$9}' > info_vcf
## sudo scp -r -p pagagnaire@162.38.66.126:/media/pagagnaire/DATA/Pierre-Alexandre.Gagnaire/CoGeDiv/Marion/Variant_Orientation/Scine/est-sfs/Dlabr_proba_LDhelmet.txt /DATA/sdb1/Pierre/Phased/Scine/ancestral_state_full.txt


import matplotlib.pyplot as plt
import allel
import numpy as np
#print(allel.__version__)
import sys
import seaborn as sns
import os
import gzip
import warnings
import pandas as pd
from tqdm import tqdm
import collections
import subprocess
from sys import exit
import csv
import re

warnings.filterwarnings("ignore",category=RuntimeWarning)

sys.argv=['','Lbude']
species=1
os.mkdir("/home/labosea1/Introgression/DSUITE/"+str(sys.argv[species]))
os.system("bcftools view -H /DATA/sdb1/Pierre/VCF/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_indel5bp_snponly.vcf.gz | awk '{print $1,$2,$4,$5,$9}' > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/info_vcf")

#ancestral=pd.read_csv("/DATA/sdb1/Pierre/Phased/"+str(sys.argv[species])+"/ancestral_state.tsv.gz",sep="\t")
ancestral=pd.read_csv("/DATA/sdb1/Pierre/Phased/"+str(sys.argv[species])+"/ancestral_state_full.txt",sep=" ",header=None)
ancestral.columns=["CHROM","POS","A","C","G","T"]
ancestral=ancestral.assign(chrom_pos=ancestral["CHROM"].astype(str)+ancestral["POS"].astype(str))
ancestral=ancestral.drop_duplicates("chrom_pos")

info_vcf=pd.read_csv("/home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/info_vcf",sep=" ",header=None)
info_vcf.columns=['CHROM','POS','REF','ALT','INFO']
row_before=info_vcf.shape[0]
row_after=info_vcf.drop_duplicates().merge(ancestral.drop_duplicates(),how="left",on=["CHROM","POS"]).shape[0]
if row_before==row_after:
	info_vcf=info_vcf.drop_duplicates().merge(ancestral.drop_duplicates(),how="left",on=["CHROM","POS"])
else:
	print("Oh no ..., we lose some rows of info_vcf")
	exit()

tmp=info_vcf[["A","C","G","T"]]

AA=[]
for i in tqdm(range(info_vcf.shape[0])):
	AA+=[tmp.iloc[i].idxmax()]

info_vcf["AA"]=AA

outgroup=[]
for i in tqdm(range(info_vcf.shape[0])):
	if info_vcf["INFO"].iloc[i]=='GT:AD:DP:GQ:PL':
		if info_vcf["REF"].iloc[i]==info_vcf["AA"].iloc[i]:
			outgroup+=['0/0:20,20:20:20:0,30,345']
		elif info_vcf["ALT"].iloc[i]==info_vcf["AA"].iloc[i]:
			outgroup+=['1/1:20,20:20:20:0,30,345']
		else:
			outgroup+=['./.:20,20:20:20:.:.:0,30,345:.']
	elif info_vcf["INFO"].iloc[i]=='GT:AD:DP:GQ:PGT:PID:PL:PS':
		if info_vcf["REF"].iloc[i]==info_vcf["AA"].iloc[i]:
			outgroup+=['0/0:20,20:20:20:.:.:0,30,345:.']
		elif info_vcf["ALT"].iloc[i]==info_vcf["AA"].iloc[i]:
			outgroup+=['1/1:20,20:20:20:.:.:0,30,345:.']
		else:
			outgroup+=['./.:20,20:20:20:.:.:0,30,345:.']
		

data_outgroup=pd.DataFrame(
{
	"outgroup":outgroup
}
)

data_outgroup.to_csv("/home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_outgroup.csv",index=False)

samples=subprocess.check_output("bcftools query -l /DATA/sdb1/Pierre/VCF/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_indel5bp_snponly.vcf.gz",shell=True)
samples=samples.decode().split('\n')[:-1]

pop=[]
for i in range(len(samples)):
	pop+=[samples[i][5:7]]	

samples+=["OUTGROUP"]
pop+=["Outgroup"]

data_pop=pd.DataFrame(
{
	"samples":samples,
	"pop": pop
}
)

data_pop.to_csv("/home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_pop.txt",sep="\t",header=False,index=False)

os.chdir("/home/labosea1/Introgression/DSUITE/"+str(sys.argv[species]))
os.system("tail -n +2 '/home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_outgroup.csv' > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_outgroup_remove.csv")
os.system("sed 's/\"//g' /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_outgroup_remove.csv > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_outgroup_remove_noquote.csv")
os.system("bcftools view -H /DATA/sdb1/Pierre/VCF/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_indel5bp_snponly.vcf.gz > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/tmp_"+str(sys.argv[species])+".vcf")
os.system("bcftools view -h /DATA/sdb1/Pierre/VCF/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_indel5bp_snponly.vcf.gz > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/tmp_"+str(sys.argv[species])+"_header.vcf")
os.system("paste /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/tmp_"+str(sys.argv[species])+".vcf /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/"+str(sys.argv[species])+"_outgroup_remove_noquote.csv > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/vcf_with_outgroup.vcf")
os.system("rm /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/tmp_"+str(sys.argv[species])+".vcf")
os.system("sed '${s/$/\tOUTGROUP/}' /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/tmp_"+str(sys.argv[species])+"_header.vcf > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/tmp_"+str(sys.argv[species])+"_header_withoutgroup.vcf")
os.system("cat /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/tmp_"+str(sys.argv[species])+"_header_withoutgroup.vcf /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/vcf_with_outgroup.vcf > /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/vcf_with_outgroup_with_header.vcf")
os.system("rm /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/vcf_with_outgroup.vcf")
os.system("bgzip /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/vcf_with_outgroup_with_header.vcf")
os.system("tabix -p vcf /home/labosea1/Introgression/DSUITE/"+str(sys.argv[species])+"/vcf_with_outgroup_with_header.vcf.gz")
