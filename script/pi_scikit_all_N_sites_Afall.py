#import matplotlib.pyplot as plt
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
import array
import os.path

#sys.argv=['','scaff0.vcf','10000']
#vcf=allel.read_vcf("/DATA/sdb1/Pierre/pixy/filter/test_invariant.vcf.gz",fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT'],log=sys.stdout) #import vcf

if str(sys.argv[3])=="Lbude":
	print("Lbude")
	vcf=allel.read_vcf(sys.argv[1],fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT'],samples=['LbudeFa1','LbudeFa2','LbudeFa4','LbudeFa5','LbudeFa6','LbudeGa1','LbudeGa2','LbudeGa3','LbudeGa4','LbudeGa6','LbudeLi1','LbudeLi2','LbudeLi3','LbudeLi5','LbudeLi6','LbudeMu1','LbudeMu3','LbudeMu4']) #import vcf	
elif str(sys.argv[3])=="Hgutt":
	print("Hgutt")
	vcf=allel.read_vcf(sys.argv[1],fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT'],samples=['HguttFa2','HguttFa3','HguttFa4','HguttFa5','HguttFa6','HguttGa9','HguttGa6','HguttGa3','HguttGa7','HguttGa11','HguttLi1','HguttLi3','HguttLi4','HguttLi5','HguttLi6','HguttMu1','HguttMu2','HguttMu3','HguttMu4','HguttMu5']) #import vcf	
elif str(sys.argv[3])=="Dpunt":
	print("Dpunt")
	vcf=allel.read_vcf(sys.argv[1],fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT'],samples=['DpuntFa1','DpuntFa2','DpuntFa3','DpuntFa4','DpuntFa5','DpuntGa1','DpuntGa2','DpuntGa3','DpuntGa4','DpuntGa5','DpuntLi1','DpuntLi2','DpuntLi3','DpuntLi4','DpuntLi5','DpuntMu1','DpuntMu2','DpuntMu3','DpuntMu4'])
elif str(sys.argv[3])=="Peryt":
	print("Peryt")
	vcf=allel.read_vcf(sys.argv[1],fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT'],samples=['PerytFa2','PerytFa3','PerytFa4','PerytFa5','PerytGa1','PerytGa2','PerytGa3','PerytGa4','PerytGa6','PerytLi1','PerytLi2','PerytLi3','PerytLi4','PerytLi5','PerytMu1','PerytMu2','PerytMu3','PerytMu5','PerytMu6',])
elif str(sys.argv[3])=="Ssard":
        print("Ssard")
        vcf=allel.read_vcf(sys.argv[1],fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT'],samples=['SsardFa2','SsardFa4','SsardFa5','SsardFa6','SsardGa1','SsardGa2','SsardGa3','SsardGa4','SsardGa6','SsardLi1','SsardLi2','SsardLi3','SsardLi4','SsardLi5','SsardMu1','SsardMu2','SsardMu3','SsardMu4','SsardMu5',])
else:
	print("Normal")
	vcf=allel.read_vcf(sys.argv[1],fields=['samples', 'calldata/GT', 'variants/CHROM', 'variants/POS', 'variants/REF','variants/ALT']) #import vcf
samples = vcf['samples']

Li=[x for x in range(len(samples)) if 'Li' in samples[x]]
Mu=[x for x in range(len(samples)) if 'Mu' in samples[x]]
Fa=[x for x in range(len(samples)) if 'Fa' in samples[x]]
Ga=[x for x in range(len(samples)) if 'Ga' in samples[x]]
# Per poulations
subpops={	
	'all': range(len(vcf['samples'])),
	'Li': Li,
	'Mu': Mu,
	'Fa' : Fa,
	'Ga' : Ga,
	'Med' : Li+Mu,
	'Atl' : Fa+Ga,
	'Li-Mu' : Li+Mu,
	'Li-Fa' : Li+Fa,
	'Li-Ga' : Li+Ga,
	'Mu-Fa' : Mu+Fa,
	'Mu-Ga' : Mu+Ga,
	'Fa-Ga' : Fa+Ga,
	'Li-Mu-Fa' : Li+Mu+Fa,
	'Li-Mu-Ga' : Li+Mu+Ga,
	'Li-Fa-Ga' : Li+Fa+Ga,
	'Mu-Fa-Ga' : Mu+Fa+Ga
}

# All samples
gt = allel.GenotypeArray(vcf['calldata/GT']) #Convert vcf in GenotypeArray
h = gt.to_haplotypes #Convert in Haplotype format
ac=gt.count_alleles_subpops(subpops)

### PI
name_file=sys.argv[1].split('/')[-1]

if os.path.isfile("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/pi_window_"+str(name_file)+".txt")==False:
	new_line=['CHROM','START','END','POP','TOTAL_SITES','N_SITES','NUM_ALL','DENOM_ALL','PI_ALL','NUM_REMOVE_MULTI','DENOM_REMOVE_MULTI','PI_REMOVE_MULTI','NUM_REMOVE_INDEL','DENOM_REMOVE_INDEL',
	'PI_REMOVE_INDEL','NUM_REMOVE_MULTI_INDEL','DENOM_REMOVE_MULTI_INDEL','PI_REMOVE_MULTI_INDEL']
	with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/pi_window_"+str(name_file)+".txt","ab") as f:
		np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")
		
ss=np.arange(int(sys.argv[2]),ac['all'].shape[0],int(sys.argv[2]))
ss=np.append(ss,ac['all'].shape[0])
#print(ss)
for cc in ss:
	
	a=0
	N_ref=0
	nan_count=0
	
	num_count_all=0
	denom_count_all=0
	
	num_count_remove_multi=0
	denom_count_remove_multi=0
	
	num_count_remove_indel=0
	denom_count_remove_indel=0
	
	num_count_remove_multi_indel=0
	denom_count_remove_multi_indel=0
	
	if cc % int(sys.argv[2]) == 0:
		stt=(cc-int(sys.argv[2]))
	else:
		stt= cc - (cc % int(sys.argv[2]))
	for i in range(stt,(cc)):
		if vcf['variants/REF'][i]!='N':
			n=np.sum(ac['all'][i,:])
			all_genotypes=np.concatenate(gt[i,]).tolist()
			all_genotypes=[kkk for kkk in all_genotypes if kkk != -1]
			if len(all_genotypes)>0:
				a=a+1
				for one in range(0,len(all_genotypes)-1):
					for two in range(one+1,len(all_genotypes)):
						if all_genotypes[one]==all_genotypes[two]:
							denom_count_all=denom_count_all+1
						else:
							#print("OK")
							denom_count_all=denom_count_all+1
							num_count_all=num_count_all+1
						if vcf['variants/REF'][i]!='N' and len(''.join(vcf['variants/ALT'][i,:]))<=1:
							if all_genotypes[one]==all_genotypes[two]:
								denom_count_remove_multi=denom_count_remove_multi+1
							else:
								denom_count_remove_multi=denom_count_remove_multi+1
								num_count_remove_multi=num_count_remove_multi+1
						if vcf['variants/REF'][i]!='N' and len(vcf['variants/REF'][i])==1:
							if all_genotypes[one]==all_genotypes[two]:
								denom_count_remove_indel=denom_count_remove_indel+1
							else:
								denom_count_remove_indel=denom_count_remove_indel+1
								num_count_remove_indel=num_count_remove_indel+1
						if vcf['variants/REF'][i]!='N' and len(vcf['variants/REF'][i])==1 and len(''.join(vcf['variants/ALT'][i,:]))<=1:
							if all_genotypes[one]==all_genotypes[two]:
								denom_count_remove_multi_indel=denom_count_remove_multi_indel+1
							else:
								denom_count_remove_multi_indel=denom_count_remove_multi_indel+1
								num_count_remove_multi_indel=num_count_remove_multi_indel+1			
		else:
			N_ref=N_ref+1
	if denom_count_all!=0:
		pi_all=num_count_all*100/denom_count_all
	else:
		pi_all="nan"
	if denom_count_remove_multi!=0:
		pi_remove_multi=num_count_remove_multi*100/denom_count_remove_multi
	else:
		pi_remove_multi="nan"
	if denom_count_remove_indel!=0:
		pi_remove_indel=num_count_remove_indel*100/denom_count_remove_indel
	else:
		pi_remove_indel="nan"
	if denom_count_remove_multi_indel!=0:
		pi_remove_multi_indel=num_count_remove_multi_indel*100/denom_count_remove_multi_indel
	else:
		pi_remove_multi_indel="nan"
	new_line=[vcf['variants/CHROM'][0],stt,int(cc),'All',int(a),int(N_ref),
	int(num_count_all),int(denom_count_all),pi_all,
	int(num_count_remove_multi),int(denom_count_remove_multi),pi_remove_multi,
	int(num_count_remove_indel),int(denom_count_remove_indel),pi_remove_indel,
	int(num_count_remove_multi_indel),int(denom_count_remove_multi_indel),pi_remove_multi_indel]
	with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/pi_window_"+str(name_file)+".txt","ab") as f:
		np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")


pop=['Li','Fa','Ga']
ind_pop=["" for x in range(len(vcf['samples']))]
for i in range(len(vcf['samples'])):
	ind_pop[i]=str(vcf['samples'][i][5:7])

ss=np.arange(int(sys.argv[2]),ac['all'].shape[0],int(sys.argv[2]))
ss=np.append(ss,ac['all'].shape[0])

#print(ss)
for pop1 in range(0,3):
	
	for cc in ss:
		
		a=0
		N_ref=0
		nan_count=0
		
		num_count_all=0
		denom_count_all=0
		
		num_count_remove_multi=0
		denom_count_remove_multi=0
		
		num_count_remove_indel=0
		denom_count_remove_indel=0
		
		num_count_remove_multi_indel=0
		denom_count_remove_multi_indel=0
		
		if cc % int(sys.argv[2]) == 0:
			stt=(cc-int(sys.argv[2]))
		else:
			stt= cc - (cc % int(sys.argv[2]))
		#print(stt)
		for i in range(stt,(cc)):
			if vcf['variants/REF'][i]!='N':
				n=np.sum(ac['all'][i,:])
				pp=[jj for jj in range(len(ind_pop)) if ind_pop[jj] == pop[pop1]]
				n=np.sum(ac['all'][i,:])
				all_genotypes=np.concatenate(gt[i,pp]).tolist()
				all_genotypes=[jj for jj in all_genotypes if jj != -1]
				if len(all_genotypes)>0:
					a=a+1
					for one in range(0,len(all_genotypes)-1):
						for two in range(one+1,len(all_genotypes)):
							if all_genotypes[one]==all_genotypes[two]:
								denom_count_all=denom_count_all+1
							else:
								denom_count_all=denom_count_all+1
								num_count_all=num_count_all+1
							if vcf['variants/REF'][i]!='N' and len(''.join(vcf['variants/ALT'][i,:]))<=1:
								if all_genotypes[one]==all_genotypes[two]:
									denom_count_remove_multi=denom_count_remove_multi+1
								else:
									denom_count_remove_multi=denom_count_remove_multi+1
									num_count_remove_multi=num_count_remove_multi+1
							if vcf['variants/REF'][i]!='N' and len(vcf['variants/REF'][i])==1:
								if all_genotypes[one]==all_genotypes[two]:
									denom_count_remove_indel=denom_count_remove_indel+1
								else:
									denom_count_remove_indel=denom_count_remove_indel+1
									num_count_remove_indel=num_count_remove_indel+1
							if vcf['variants/REF'][i]!='N' and len(vcf['variants/REF'][i])==1 and len(''.join(vcf['variants/ALT'][i,:]))<=1:
								if all_genotypes[one]==all_genotypes[two]:
									denom_count_remove_multi_indel=denom_count_remove_multi_indel+1
								else:
									denom_count_remove_multi_indel=denom_count_remove_multi_indel+1
									num_count_remove_multi_indel=num_count_remove_multi_indel+1			
			else:
				N_ref=N_ref+1
		if denom_count_all!=0:
			pi_all=num_count_all*100/denom_count_all
		else:
			pi_all="nan"
		if denom_count_remove_multi!=0:
			pi_remove_multi=num_count_remove_multi*100/denom_count_remove_multi
		else:
			pi_remove_multi="nan"
		if denom_count_remove_indel!=0:
			pi_remove_indel=num_count_remove_indel*100/denom_count_remove_indel
		else:
			pi_remove_indel="nan"
		if denom_count_remove_multi_indel!=0:
			pi_remove_multi_indel=num_count_remove_multi_indel*100/denom_count_remove_multi_indel
		else:
			pi_remove_multi_indel="nan"
		new_line=[vcf['variants/CHROM'][0],stt,int(cc),pop[pop1],int(a),int(N_ref),
		int(num_count_all),int(denom_count_all),pi_all,
		int(num_count_remove_multi),int(denom_count_remove_multi),pi_remove_multi,
		int(num_count_remove_indel),int(denom_count_remove_indel),pi_remove_indel,
		int(num_count_remove_multi_indel),int(denom_count_remove_multi_indel),pi_remove_multi_indel]
		with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/pi_window_"+str(name_file)+".txt","ab") as f:
			np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")	
	
## dxy

if os.path.isfile("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/dxy_window_"+str(name_file)+".txt")==False:
	new_line=['CHROM','START','END','POP1','POP2','TOTAL_SITES','N_SITES','NUM_ALL','DENOM_ALL','DXY_ALL','NUM_REMOVE_MULTI','DENOM_REMOVE_MULTI','DXY_REMOVE_MULTI','NUM_REMOVE_INDEL','DENOM_REMOVE_INDEL',
	'DXY_REMOVE_INDEL','NUM_REMOVE_MULTI_INDEL','DENOM_REMOVE_MULTI_INDEL','DXY_REMOVE_MULTI_INDEL']
	with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/dxy_window_"+str(name_file)+".txt","ab") as f:
			np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")

pop=['Li','Fa','Ga']
ind_pop=["" for x in range(len(vcf['samples']))]
for i in range(len(vcf['samples'])):
	ind_pop[i]=str(vcf['samples'][i][5:7])

ss=np.arange(int(sys.argv[2]),ac['all'].shape[0],int(sys.argv[2]))
ss=np.append(ss,ac['all'].shape[0])

for pop1 in range(0,2):
	for pop2 in range((pop1+1),3):
		for cc in ss:		
			a=0
			N_ref=0
			nan_count=0
			
			num_count_all=0
			denom_count_all=0
			
			num_count_remove_multi=0
			denom_count_remove_multi=0
			
			num_count_remove_indel=0
			denom_count_remove_indel=0
			
			num_count_remove_multi_indel=0
			denom_count_remove_multi_indel=0
			
			if cc % int(sys.argv[2]) == 0:
				stt=(cc-int(sys.argv[2]))
			else:
				stt= cc - (cc % int(sys.argv[2]))
			#print(stt)
			for i in range(stt,(cc)):
				if vcf['variants/REF'][i]!='N':
					pp1=[jj for jj in range(len(ind_pop)) if ind_pop[jj] == pop[pop1]]
					pp2=[jj for jj in range(len(ind_pop)) if ind_pop[jj] == pop[pop2]]
					n=np.sum(ac['all'][i,:])
					all_genotypes_pop1=np.concatenate(gt[i,pp1]).tolist()
					all_genotypes_pop1=[jj for jj in all_genotypes_pop1 if jj != -1]
					all_genotypes_pop2=np.concatenate(gt[i,pp2]).tolist()
					all_genotypes_pop2=[jj for jj in all_genotypes_pop2 if jj != -1]
					if len(all_genotypes_pop1)>0 and len(all_genotypes_pop2)>0:
						a=a+1
						for one in range(0,len(all_genotypes_pop1)):
							for two in range(0,len(all_genotypes_pop2)):
								if all_genotypes_pop1[one]==all_genotypes_pop2[two]:
									denom_count_all=denom_count_all+1
								else:
									denom_count_all=denom_count_all+1
									num_count_all=num_count_all+1
								if vcf['variants/REF'][i]!='N' and len(''.join(vcf['variants/ALT'][i,:]))<=1:
									if all_genotypes_pop1[one]==all_genotypes_pop2[two]:
										denom_count_remove_multi=denom_count_remove_multi+1
									else:
										denom_count_remove_multi=denom_count_remove_multi+1
										num_count_remove_multi=num_count_remove_multi+1
								if vcf['variants/REF'][i]!='N' and len(vcf['variants/REF'][i])==1:
									if all_genotypes_pop1[one]==all_genotypes_pop2[two]:
										denom_count_remove_indel=denom_count_remove_indel+1
									else:
										denom_count_remove_indel=denom_count_remove_indel+1
										num_count_remove_indel=num_count_remove_indel+1
								if vcf['variants/REF'][i]!='N' and len(vcf['variants/REF'][i])==1 and len(''.join(vcf['variants/ALT'][i,:]))<=1:
									if all_genotypes_pop1[one]==all_genotypes_pop2[two]:
										denom_count_remove_multi_indel=denom_count_remove_multi_indel+1
									else:
										denom_count_remove_multi_indel=denom_count_remove_multi_indel+1
										num_count_remove_multi_indel=num_count_remove_multi_indel+1							
				else:
					N_ref=N_ref+1
			if denom_count_all!=0:
				dxy_all=num_count_all*100/denom_count_all
			else:
				dxy_all="nan"
			if denom_count_remove_multi!=0:
				dxy_remove_multi=num_count_remove_multi*100/denom_count_remove_multi
			else:
				dxy_remove_multi="nan"
			if denom_count_remove_indel!=0:
				dxy_remove_indel=num_count_remove_indel*100/denom_count_remove_indel
			else:
				dxy_remove_indel="nan"
			if denom_count_remove_multi_indel!=0:
				dxy_remove_multi_indel=num_count_remove_multi_indel*100/denom_count_remove_multi_indel
			else:
				dxy_remove_multi_indel="nan"
			new_line=[vcf['variants/CHROM'][0],stt,int(cc),pop[pop1],pop[pop2],int(a),int(N_ref),
			int(num_count_all),int(denom_count_all),dxy_all,
			int(num_count_remove_multi),int(denom_count_remove_multi),dxy_remove_multi,
			int(num_count_remove_indel),int(denom_count_remove_indel),dxy_remove_indel,
			int(num_count_remove_multi_indel),int(denom_count_remove_multi_indel),dxy_remove_multi_indel]
			with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/dxy_pi/dxy_window_"+str(name_file)+".txt","ab") as f:
					np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")

	
