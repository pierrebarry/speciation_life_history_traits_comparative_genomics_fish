from Bio import SeqIO
import numpy as np
import pandas as pd
import os
import itertools
import sys

sp=sys.argv[1]
stat=sys.argv[2]
fasta_species=sys.argv[3]

#fasta_sequences = SeqIO.parse(open('/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/'+str(sp)+'/ABC/'+str(sp)+'_DILS.fasta'),'fasta')
fasta_sequences = SeqIO.parse(open(fasta_species),'fasta')

locus_sequence = []
pop_sequence = []
seq_sequence = []
for fasta in fasta_sequences:
	name, sequence = fasta.id, str(fasta.seq)
	name=name.split('|')
	pop_sequence += [name[2][5:7]]
	locus_sequence += [name[0]]
	seq_sequence += [sequence]

gene=pd.DataFrame(
{
	"LOCUS":locus_sequence,
	"POP":pop_sequence,
	"SEQUENCE":seq_sequence
}
)

print(gene)

def pi(locus):
	tmp=gene[gene["LOCUS"]==locus]
	tmp=tmp.reset_index(drop=True)
	for pop1 in range(0,4):
		a=0
		N_ref=0
		nan_count=0
		
		num_count_all=0
		denom_count_all=0
		
		for i in range(len(tmp["SEQUENCE"][0])):
				pp=[jj for jj in range(tmp.shape[0]) if tmp["POP"][jj] == pop[pop1]]
				all_genotypes_pop = []
				for ee in pp:
					all_genotypes_pop += [tmp["SEQUENCE"][ee][i]]
				all_genotypes_pop=[jj for jj in all_genotypes_pop if jj != "N"]
				if len(all_genotypes_pop)>0:
					a=a+1
					for i in range(0,len(all_genotypes_pop)-1):
						for j in range(i+1,len(all_genotypes_pop)):
							if all_genotypes_pop[i]==all_genotypes_pop[j]:
								denom_count_all=denom_count_all+1
							else:
								denom_count_all=denom_count_all+1
								num_count_all=num_count_all+1					
				else:
					N_ref=N_ref+1
		if denom_count_all!=0:
			pi_all=num_count_all*100/denom_count_all
		else:
			pi_all="nan"
		new_line=[locus,pop[pop1],int(a),int(N_ref),
		int(num_count_all),int(denom_count_all),pi_all]
		with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+str(sp)+"/pi_gene_"+str(sp)+".txt","ab") as f:
				np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")
	a=0
	N_ref=0
	nan_count=0
	
	num_count_all=0
	denom_count_all=0
	
	for i in range(len(tmp["SEQUENCE"][0])):
			pp=[jj for jj in range(tmp.shape[0]) if 11 == 11]
			all_genotypes_pop = []
			for ee in pp:
				all_genotypes_pop += [tmp["SEQUENCE"][ee][i]]
			all_genotypes_pop=[jj for jj in all_genotypes_pop if jj != "N"]
			if len(all_genotypes_pop)>0:
				a=a+1
				for i in range(0,len(all_genotypes_pop)-1):
					for j in range(i+1,len(all_genotypes_pop)):
						if all_genotypes_pop[i]==all_genotypes_pop[j]:
							denom_count_all=denom_count_all+1
						else:
							denom_count_all=denom_count_all+1
							num_count_all=num_count_all+1					
			else:
				N_ref=N_ref+1
	if denom_count_all!=0:
		pi_all=num_count_all*100/denom_count_all
	else:
		pi_all="nan"
	new_line=[locus,"all",int(a),int(N_ref),
	int(num_count_all),int(denom_count_all),pi_all]
	with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+str(sp)+"/pi_gene_"+str(sp)+".txt","ab") as f:
			np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")

def dxy(locus):
	tmp=gene[gene["LOCUS"]==locus]
	tmp=tmp.reset_index(drop=True)
	for pop1 in range(0,3):
		for pop2 in range((pop1+1),4):	
			
			a=0
			N_ref=0
			nan_count=0
			
			num_count_all=0
			denom_count_all=0
			
			
			for i in range(len(tmp["SEQUENCE"][0])):
					pp1=[jj for jj in range(tmp.shape[0]) if tmp["POP"][jj] == pop[pop1]]
					pp2=[jj for jj in range(tmp.shape[0]) if tmp["POP"][jj] == pop[pop2]]
					all_genotypes_pop1 = []
					for ee in pp1:
						all_genotypes_pop1 += [tmp["SEQUENCE"][ee][i]]
					all_genotypes_pop2 = []
					for ee in pp2:
						all_genotypes_pop2 += [tmp["SEQUENCE"][ee][i]]
					all_genotypes_pop1=[jj for jj in all_genotypes_pop1 if jj != "N"]
					all_genotypes_pop2=[jj for jj in all_genotypes_pop2 if jj != "N"]
					if len(all_genotypes_pop1)>0 and len(all_genotypes_pop2)>0:
						a=a+1
						for i in range(0,len(all_genotypes_pop1)):
							for j in range(0,len(all_genotypes_pop2)):
								if all_genotypes_pop1[i]==all_genotypes_pop2[j]:
									denom_count_all=denom_count_all+1
								else:
									denom_count_all=denom_count_all+1
									num_count_all=num_count_all+1					
					else:
						N_ref=N_ref+1
			if denom_count_all!=0:
				dxy_all=num_count_all*100/denom_count_all
			else:
				dxy_all="nan"
			new_line=[locus,pop[pop1],pop[pop2],int(a),int(N_ref),
			int(num_count_all),int(denom_count_all),dxy_all]
			with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+str(sp)+"/dxy_gene_"+str(sp)+".txt","ab") as f:
					np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")

## RUN
if stat=="pi":
	if os.path.isfile("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+str(sp)+"/pi_gene_"+str(sp)+".txt")==False:
		new_line=['GENE','POP','TOTAL_SITES','N_SITES','NUM_ALL','DENOM_ALL','PI_ALL']
		with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+str(sp)+"/pi_gene_"+str(sp)+".txt","ab") as f:
			np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")
	pop=np.unique(gene["POP"])
	print(pop)
	for locus in np.unique(gene["LOCUS"]):
		pi(locus)
	#processed_list = Parallel(n_jobs=5)(delayed(pi)(locus) for locus in np.unique(gene["LOCUS"]))

if stat=="dxy":
	if os.path.isfile("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+str(sp)+"/dxy_gene_"+str(sp)+".txt")==False:
		new_line=['GENE','POP1','POP2','TOTAL_SITES','N_SITES','NUM_ALL','DENOM_ALL','DXY_ALL']
		with open("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/"+str(sp)+"/dxy_gene_"+str(sp)+".txt","ab") as f:
				np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")
	pop=np.unique(gene["POP"])
	print(pop)
	for locus in np.unique(gene["LOCUS"]):
		dxy(locus)
	#processed_list = Parallel(n_jobs=5)(delayed(dxy)(locus) for locus in np.unique(gene["LOCUS"]))


run = 0

if run == 1:

	## PI

	if os.path.isfile("/home/labosea1/gene_stat/pi_gene_"+str(sp)+".txt")==False:
		new_line=['GENE','POP','TOTAL_SITES','N_SITES','NUM_ALL','DENOM_ALL','PI_ALL']
		with open("/home/labosea1/gene_stat/pi_gene_"+str(sp)+".txt","ab") as f:
			np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")

	pop=np.unique(gene["POP"])

	for locus in np.unique(gene["LOCUS"]):
		tmp=gene[gene["LOCUS"]==locus]
		tmp=tmp.reset_index(drop=True)
		for pop1 in range(0,4):
			a=0
			N_ref=0
			nan_count=0
			
			num_count_all=0
			denom_count_all=0
			
			for i in range(len(tmp["SEQUENCE"][0])):
					pp=[jj for jj in range(tmp.shape[0]) if tmp["POP"][jj] == pop[pop1]]
					all_genotypes_pop = []
					for ee in pp:
						all_genotypes_pop += [tmp["SEQUENCE"][ee][i]]
					all_genotypes_pop=[jj for jj in all_genotypes_pop if jj != "N"]
					if len(all_genotypes_pop)>0:
						a=a+1
						for i in range(0,len(all_genotypes_pop)-1):
							for j in range(i+1,len(all_genotypes_pop)):
								if all_genotypes_pop[i]==all_genotypes_pop[j]:
									denom_count_all=denom_count_all+1
								else:
									denom_count_all=denom_count_all+1
									num_count_all=num_count_all+1					
					else:
						N_ref=N_ref+1
			if denom_count_all!=0:
				pi_all=num_count_all*100/denom_count_all
			else:
				pi_all="nan"
			new_line=[locus,pop[pop1],int(a),int(N_ref),
			int(num_count_all),int(denom_count_all),pi_all]
			with open("/home/labosea1/gene_stat/pi_gene_"+str(sp)+".txt","ab") as f:
					np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")
		a=0
		N_ref=0
		nan_count=0
		
		num_count_all=0
		denom_count_all=0
		
		for i in range(len(tmp["SEQUENCE"][0])):
				pp=[jj for jj in range(tmp.shape[0]) if 11 == 11]
				all_genotypes_pop = []
				for ee in pp:
					all_genotypes_pop += [tmp["SEQUENCE"][ee][i]]
				all_genotypes_pop=[jj for jj in all_genotypes_pop if jj != "N"]
				if len(all_genotypes_pop)>0:
					a=a+1
					for i in range(0,len(all_genotypes_pop)-1):
						for j in range(i+1,len(all_genotypes_pop)):
							if all_genotypes_pop[i]==all_genotypes_pop[j]:
								denom_count_all=denom_count_all+1
							else:
								denom_count_all=denom_count_all+1
								num_count_all=num_count_all+1					
				else:
					N_ref=N_ref+1
		if denom_count_all!=0:
			pi_all=num_count_all*100/denom_count_all
		else:
			pi_all="nan"
		new_line=[locus,"all",int(a),int(N_ref),
		int(num_count_all),int(denom_count_all),pi_all]
		with open("/home/labosea1/gene_stat/pi_gene_"+str(sp)+".txt","ab") as f:
				np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")


	## DXY
	if os.path.isfile("/home/labosea1/gene_stat/dxy_gene_"+str(sp)+".txt")==False:
		new_line=['GENE','POP1','POP2','TOTAL_SITES','N_SITES','NUM_ALL','DENOM_ALL','DXY_ALL']
		with open("/home/labosea1/gene_stat/dxy_gene_"+str(sp)+".txt","ab") as f:
				np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")
				
	pop=np.unique(gene["POP"])

	for locus in np.unique(gene["LOCUS"]):
		tmp=gene[gene["LOCUS"]==locus]
		tmp=tmp.reset_index(drop=True)
		for pop1 in range(0,3):
			for pop2 in range((pop1+1),4):	
				
				a=0
				N_ref=0
				nan_count=0
				
				num_count_all=0
				denom_count_all=0
				
				
				for i in range(len(tmp["SEQUENCE"][0])):
						pp1=[jj for jj in range(tmp.shape[0]) if tmp["POP"][jj] == pop[pop1]]
						pp2=[jj for jj in range(tmp.shape[0]) if tmp["POP"][jj] == pop[pop2]]
						all_genotypes_pop1 = []
						for ee in pp1:
							all_genotypes_pop1 += [tmp["SEQUENCE"][ee][i]]
						all_genotypes_pop2 = []
						for ee in pp2:
							all_genotypes_pop2 += [tmp["SEQUENCE"][ee][i]]
						all_genotypes_pop1=[jj for jj in all_genotypes_pop1 if jj != "N"]
						all_genotypes_pop2=[jj for jj in all_genotypes_pop2 if jj != "N"]
						if len(all_genotypes_pop1)>0 and len(all_genotypes_pop2)>0:
							a=a+1
							for i in range(0,len(all_genotypes_pop1)):
								for j in range(0,len(all_genotypes_pop2)):
									if all_genotypes_pop1[i]==all_genotypes_pop2[j]:
										denom_count_all=denom_count_all+1
									else:
										denom_count_all=denom_count_all+1
										num_count_all=num_count_all+1					
						else:
							N_ref=N_ref+1
				if denom_count_all!=0:
					dxy_all=num_count_all*100/denom_count_all
				else:
					dxy_all="nan"
				new_line=[locus,pop[pop1],pop[pop2],int(a),int(N_ref),
				int(num_count_all),int(denom_count_all),dxy_all]
				with open("/home/labosea1/gene_stat/dxy_gene_"+str(sp)+".txt","ab") as f:
						np.savetxt(f,[new_line],fmt='%s',delimiter=";",newline="\n")
