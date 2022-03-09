import msprime
import tskit
import tsinfer
import tsdate
import json
import pandas as pd
import numpy as np
import sys

arg=sys.argv
#arg=['infer_tree.py','ts']
print(arg)
dated_ts=tskit.load(arg[1])

#dated_ts=tskit.load("Scant_120787_dated.trees")

def local_gnn(ts, focal, reference_sets):
	reference_set_map = np.zeros(ts.num_nodes, dtype=int) - 1
	for k, reference_set in enumerate(reference_sets):
		for u in reference_set:
			if reference_set_map[u] != -1:
				raise ValueError("Duplicate value in reference sets")
			reference_set_map[u] = k
	
	K = len(reference_sets)
	A = np.zeros((len(focal), ts.num_trees, K))
	lefts = np.zeros(ts.num_trees, dtype=float)
	rights = np.zeros(ts.num_trees, dtype=float)
	parent = np.zeros(ts.num_nodes, dtype=int) - 1
	sample_count = np.zeros((ts.num_nodes, K), dtype=int)
	
	# Set the intitial conditions.
	for j in range(K):
		sample_count[reference_sets[j], j] = 1
	
	for t, ((left, right),edges_out, edges_in) in enumerate(ts.edge_diffs()):
		for edge in edges_out:
			parent[edge.child] = -1
			v = edge.parent
			while v != -1:
				sample_count[v] -= sample_count[edge.child]
				v = parent[v]
		for edge in edges_in:
			parent[edge.child] = edge.parent
			v = edge.parent
			while v != -1:
				sample_count[v] += sample_count[edge.child]
				v = parent[v]
		
		# Process this tree.
		for j, u in enumerate(focal):
			focal_reference_set = reference_set_map[u]
			p = parent[u]
			lefts[t] = left
			rights[t] = right
			while p != tskit.NULL:
				total = np.sum(sample_count[p])
				if total > 1:
					break
				p = parent[p]
			if p != tskit.NULL:
				scale = 1 / (total - int(focal_reference_set != -1))
				for k, reference_set in enumerate(reference_sets):
					n = sample_count[p, k] - int(focal_reference_set == k)
					A[j, t, k] = n * scale
	return (A, lefts, rights)
	
## Whole - gnn

#### Extract statistic 
samples_listed_by_population = [
  dated_ts.samples(population=pop_id)
  for pop_id in range(dated_ts.num_populations)
  ]

gnn = dated_ts.genealogical_nearest_neighbours(
  dated_ts.samples(), samples_listed_by_population
)

# Tabulate GNN nicely using a Pandas dataframe with named rows and columns
sample_nodes = [dated_ts.node(n) for n in dated_ts.samples()]
sample_ids = [n.id for n in sample_nodes]
sample_names = [
  json.loads(dated_ts.individual(n.individual).metadata)["name"]
  for n in sample_nodes
  ]
sample_pops = [
  json.loads(dated_ts.population(n.population).metadata)["population"]
  for n in sample_nodes
  ]
gnn_table = pd.DataFrame(
  data=gnn,
  index=[
    pd.Index(sample_ids, name="Sample node"),
    pd.Index(sample_names, name="Individual"),
    pd.Index(sample_pops, name="Pop"),
    ],
  columns=[json.loads(p.metadata)["population"] for p in dated_ts.populations()],
)

#print(gnn_table)
# Summarize GNN for all birds from the same country
#print(gnn_table.groupby(level="Pop").mean())

#gnn_table.to_csv("/shared/projects/abc_fish/tree_topology/output/Scant/GNN_"+arg[1]+"_4pop.csv")


###

#### Extract statistic 
samples_listed_by_population = [
  np.concatenate((dated_ts.samples(population=0),dated_ts.samples(population=1))),
  np.concatenate((dated_ts.samples(population=2),dated_ts.samples(population=3)))
  ]

gnn = dated_ts.genealogical_nearest_neighbours(
  dated_ts.samples(), samples_listed_by_population
)

# Tabulate GNN nicely using a Pandas dataframe with named rows and columns
sample_nodes = [dated_ts.node(n) for n in dated_ts.samples()]
sample_ids = [n.id for n in sample_nodes]
sample_names = [
  json.loads(dated_ts.individual(n.individual).metadata)["name"]
  for n in sample_nodes
  ]
sample_pops = [
  json.loads(dated_ts.population(n.population).metadata)["population"]
  for n in sample_nodes
  ]
gnn_table_2 = pd.DataFrame(
  data=gnn,
  index=[
    pd.Index(sample_ids, name="Sample node"),
    pd.Index(sample_names, name="Individual"),
    pd.Index(sample_pops, name="Pop"),
    ],
  columns=['Atlantic Ocean','Mediterranean Sea'],
)

df_all = gnn_table.join(gnn_table_2)

df_all["LENGTH"]=dated_ts.get_sequence_length()
df_all.to_csv("/shared/projects/abc_fish/tree_topology/output/"+arg[2]+"/GNN_"+arg[3]+".csv")

## Local GNN


## 4 pops

samples_listed_by_population = [
    dated_ts.samples(population=pop_id)
    for pop_id in range(dated_ts.num_populations)
]

IND=[]
LEFT=[]
RIGHT=[]
CHROM=[]
ALGARVE=[]
BAY_OF_BISCAY=[]
GULF_OF_LION=[]
COSTA_CALIDA=[]
num=[]

#local_gnn(dated_ts,[0],samples_listed_by_population)
for ind in range(0,39):
  A, left, right = local_gnn(dated_ts, [ind], samples_listed_by_population)
  regions=['Algarve','Bay of Biscay','Gulf of Lion', 'Costa Calida']
  df = pd.DataFrame(data=A[0], columns=regions)
  df["left"] = left
  df["right"] = right
  # Remove rows with no difference in GNN to next row
  #keep_rows = ~(df.iloc[:, 0:4].diff(axis=0) == 0).all(axis=1)
  #df = df[keep_rows]
  num+=list(np.repeat(ind,len(list(df["Algarve"]))))
  ALGARVE+=list(df["Algarve"])
  BAY_OF_BISCAY+=list(df["Bay of Biscay"])
  GULF_OF_LION+=list(df["Gulf of Lion"])
  COSTA_CALIDA+=list(df["Costa Calida"])
  IND+=list(np.repeat(sample_names[ind],len(list(df["Algarve"]))))
  CHROM+=list(np.repeat(arg[3],len(list(df["Algarve"]))))
  LEFT+=list(df["left"])
  RIGHT+=list(df["right"])
  #df.to_csv("/shared/projects/abc_fish/tsinfer/Dlabr"+str(ind)+"_local_gnn.csv")

df2 = pd.DataFrame(
  {
    "num":  num,
    "IND": IND,
    "CHROM": CHROM,
    "left": LEFT,
    "right": RIGHT,
    "Algarve": ALGARVE,
    "Bay of Biscay": BAY_OF_BISCAY,
    "Gulf of Lion": GULF_OF_LION,
    "Costa Calida": COSTA_CALIDA
  }
)

df2=df2.set_index(["num","IND","CHROM","left","right"])


## 2 pops

samples_listed_by_population = [
  np.concatenate((dated_ts.samples(population=0),dated_ts.samples(population=1))),
  np.concatenate((dated_ts.samples(population=2),dated_ts.samples(population=3)))
]

num=[]
IND=[]
LEFT=[]
RIGHT=[]
CHROM=[]
ATL=[]
MED=[]

#local_gnn(dated_ts,[0],samples_listed_by_population)
for ind in range(0,39):
  A, left, right = local_gnn(dated_ts, [ind], samples_listed_by_population)
  regions=['Atlantic Ocean','Mediterranean Sea']
  df = pd.DataFrame(data=A[0], columns=regions)
  df["left"] = left
  df["right"] = right
  # Remove rows with no difference in GNN to next row
  #keep_rows = ~(df.iloc[:, 0:4].diff(axis=0) == 0).all(axis=1)
  #df = df[keep_rows]
  ATL+=list(df["Atlantic Ocean"])
  MED+=list(df["Mediterranean Sea"])
  IND+=list(np.repeat(sample_names[ind],len(list(df["Atlantic Ocean"]))))
  CHROM+=list(np.repeat(arg[3],len(list(df["Atlantic Ocean"]))))
  LEFT+=list(df["left"])
  RIGHT+=list(df["right"])
  num+=list(np.repeat(ind,len(list(df["Atlantic Ocean"]))))
  #df.to_csv("/shared/projects/abc_fish/tsinfer/Dlabr"+str(ind)+"_local_gnn_2pop.csv")

df4 = pd.DataFrame(
  {
    "num":  num,
    "IND": IND,
    "CHROM": CHROM,
    "left": LEFT,
    "right": RIGHT,
    "Atlantic Ocean": ATL,
    "Mediterranean Sea": MED
  }
)

df4=df4.set_index(["num","IND","CHROM","left","right"])

df_all = df2.join(df4)

df_all.to_csv("/shared/projects/abc_fish/tree_topology/output/"+arg[2]+"/Local_GNN_"+arg[3]+".csv")
