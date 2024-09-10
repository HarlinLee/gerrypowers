import pickle
import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from gerrychain import (GeographicPartition, Graph)
import utils
from argparse import ArgumentParser

def main(args):
    fname = args.fn
    state = fname.split('/')[-2]

    if fname.startswith("biased_chains"):
        if 'shortburst' in fname:
            with open(fname, 'rb') as f:
                all_assignments, all_election_results, all_scores, races = pickle.load(f)
        else:
            with open(fname, 'rb') as f:
                all_assignments, all_election_results, all_scores, races, _, _ = pickle.load(f)
   
    else:
        with open(fname, 'rb') as f:
            all_assignments, all_election_results, races = pickle.load(f)
    
    idx = args.i
    if idx is None:
        idxs = [np.random.choice(len(all_assignments))] 
    elif len(idx.split(',')) == 1:
        idxs = [int(idx)]
    else:
        idxs = [int(i) for i in idx.split(',')]
    
    print(f'map index is {idxs}')
 
    for idx in idxs:
        
        graph = utils.read_geodata(f"./data/{state}/{state}.shp")
        partition = GeographicPartition(graph, all_assignments[idx])
        partition.plot(cmap="tab20b")
        plt.axis('off')
        plt.title(f'{idx}th map in chain')
        plt.tight_layout()
        plt.savefig(f'{fname[:-4]}_{idx}_map.pdf', dpi=300)

        print(f'figure saved at {fname[:-4]}_{idx}_map.pdf')

if __name__== "__main__":
    parser = ArgumentParser(description="Plot maps")
    parser.add_argument("--fn", type=str, default='biased_chains/NC/shortburst_PRES16_Republican_mean_median_10000_1719427359.pkl')
    parser.add_argument("--i", default=None, help='If none provided, plot random index. You can also supply multiple indices separated by comma.')
    args = parser.parse_args()

    print(args)
    main(args)
