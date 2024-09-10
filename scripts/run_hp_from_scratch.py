import numpy as np
import pickle
from functools import partial
import pandas as pd
from gerrychain import (GeographicPartition, Graph, MarkovChain,
                        updaters, constraints, Election, accept)
from gerrychain.proposals import reversible_recom, propose_chunk_flip, propose_random_flip
import utils

def main(args):
    map_of_choice = args.map
    prop = args.proposal

    label_function = args.bias
    k = args.k
    alpha = args.a
    epsilon = args.e
    m = args.m 

    fname = args.fn
    
    biased = fname.startswith("biased_chains")
    
    state = fname.split("/")[1]
    election_name = fname.split("_")[2]
    if biased:
        party_to_favor = fname.split("_")[3]
    else:
        party_to_favor = "Republican"

    graph = utils.read_geodata(f"./data/{state}/{state}.shp")
    my_updaters = {"population": updaters.Tally("TOTPOP", alias="population")}
    my_updaters.update(utils.get_elections(party_to_favor, election_name, state))

    if state in ['NC', 'WI', 'MD']:
        assignment = 'CD'
    elif state == 'PA':
        assignment = 'CD_2011'
    elif state == 'TX':
        assignment = 'USCD'
    else:
        raise ValueError("I don't know the column name for this state")

    initial_partition = GeographicPartition(graph, assignment=assignment, updaters=my_updaters)


    if biased:
        # find the map of interest
        with open(fname, 'rb') as f:
            all_assignments, all_election_results, all_scores, races = pickle.load(f)
        
        all_scores = np.array(all_scores)
        sb_ol = all_scores.shape[0] # 10000
        sb_il = all_scores.shape[1] # 5

        if map_of_choice == "max":

            max_idx_ol = np.argmax(all_scores.max(axis=1))
            max_idx_il = np.argmax(all_scores[max_idx_ol,:])

            print(all_scores[max_idx_ol,:])
            print(max_idx_ol, max_idx_il)

            map_idx = max_idx_ol*sb_il + max_idx_il
        
        elif map_of_choice == "min":

            min_idx_ol = np.argmin(all_scores.min(axis=1))
            min_idx_il = np.argmin(all_scores[min_idx_ol,:])

            print(all_scores[min_idx_ol,:])
            print(min_idx_ol, min_idx_il)

            map_idx = min_idx_ol*sb_il + min_idx_il
        
        elif map_of_choice == "random":
            map_idx = np.random.choice(all_scores.size)

        else:
            raise ValueError("Only max, min, or random is implemented.")
    else:
        # find the map of interest
        with open(fname, 'rb') as f:
            all_assignments, all_election_results, races = pickle.load(f)
        
        if map_of_choice == "random":
            map_idx = np.random.choice(len(all_assignments))
        else:
            raise ValueError("Only random is implemented.")
        
        print('biased map from', fname)
        print('index of map investigated', map_idx)

    # set up Markov chain starting with initial map
    initial_partition = GeographicPartition(graph, all_assignments[map_idx], updaters=my_updaters)

    ideal_population = sum(initial_partition["population"].values()) / len(initial_partition)

    if prop == 'recom':
        proposal = partial(reversible_recom,
                       pop_col="TOTPOP",
                       pop_target=ideal_population,
                       epsilon=0.02,
                       node_repeats=2
                      )
        interval = 100
    elif prop == 'random':
        proposal = propose_random_flip
        interval = 10000
    elif prop == 'chunk':
        proposal = propose_chunk_flip
        interval = 100
    else:
        raise ValueError("only recom, random, or chunk flip")

    compactness_bound = constraints.UpperBound(
            lambda p: len(p["cut_edges"]),
            2*len(initial_partition["cut_edges"])
            )

    pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, 0.02)
    
    metric_names = ["efficiency gap", "mean median", "partisan bias", "partisan gini", "mean thirdian", "safe seats"]
    all_metric_values = []
    all_counts = []
    all_percents = []
    rho_lower = np.zeros(len(metric_names))
    rho_upper = np.zeros(len(metric_names))
    
    for traj in range(m):
        print('trajectory', traj)
        print('=================')
        chain = MarkovChain(
            proposal=proposal,
            constraints=[
                pop_constraint,
                compactness_bound
            ],
            accept=accept.always_accept,
            initial_state=initial_partition,
            total_steps=k+1
            )
        
        partisan_metric_values = [] # keep track of the partisan metrics
        safe_seats = []
        percents = [] # keep track of the democratic percentages
        
        for idx, partition in enumerate(chain):
            curr = partition.updaters[election_name](partition)
            partisan_metric_values.append([
                curr.efficiency_gap(), curr.mean_median(), curr.partisan_bias(), curr.partisan_gini(), curr.mean_thirdian()
                ])
            safe_seats.append(len([x for x in partition[election_name].percents(party_to_favor) if x > 0.53]))
            percents.append(sorted(partition[election_name].percents(party_to_favor)))
            
            if idx % interval == 0:
                print(idx)

        # each column in this matrix is a metric and
        # each row is a step in the chain 
        w_vals = np.hstack((np.array(partisan_metric_values), np.array(safe_seats).reshape(-1, 1)))
        all_metric_values.append(w_vals)
        all_percents.append(percents)
        
        w0_vals = w_vals[0] # metric values of the map in the begining of the trajectory
        print('initial values', w0_vals)
        w1_vals = w_vals[1:]
        
        counts = np.sum(w1_vals < w0_vals, axis=0)
        print('counts', counts)
        all_counts.append(counts)

        # if epsilon-outlier in lower tail, increase rho by 1
        rho_lower +=  counts < epsilon*(k+1)
        
        # if epsilon-outlier in upper tail, increase rho by 1
        rho_upper +=  counts > (1-epsilon)*(k+1)

        print('rho_lower', rho_lower, 'rho_upper', rho_upper)
        print()

    # calculate p-value from Theorem 3.1
    r = rho_lower - m*np.sqrt(2*epsilon/alpha)
    p_lower = np.minimum(1, np.exp(-np.minimum((r**2)*np.sqrt(alpha/2/epsilon)/3/m, r/3)))

    r = rho_upper - m*np.sqrt(2*epsilon/alpha)
    p_upper = np.minimum(1, np.exp(-np.minimum((r**2)*np.sqrt(alpha/2/epsilon)/3/m, r/3)))
    print('================')
    print(args)
    print(map_idx)
    print(metric_names)
    print('initial values', w0_vals)
    print('p_lower', p_lower, 'p_upper', p_upper)
    
    res = {'w0': w0_vals, 'rho_lower':rho_lower, 'rho_upper':rho_upper, 'p_lower':p_lower, 'p_upper':p_upper, 'counts':all_counts}

    # save results
    from time import time
    fname = 'hp/'+fname[:-4]+'_'+str(map_idx)+'_'+str(int(time()))
    
    with open(fname +'.pkl', 'wb') as f:
        pickle.dump((args, map_idx, metric_names, all_metric_values, all_percents, res), f, pickle.HIGHEST_PROTOCOL)
    
    print('w vals saved to', fname)

if __name__ == "__main__":
    from argparse import ArgumentParser

    parser = ArgumentParser(description="hypothesis test")
    parser.add_argument("--map", type=str, default="random", help="max, min, random")
    parser.add_argument("--k", type=int, default=100000,
        help="number of steps in Markov chain")
    parser.add_argument("--m", type=int, default=32,
        help="number of independent trajectories")
    parser.add_argument("--bias", default="mean_median",
        help="label function")
    parser.add_argument("--a", type=float, default=0.009, help="alpha")
    parser.add_argument("--e", type=float, default=0.003, help="epsilon")
    parser.add_argument("--fn", type=str, default='biased_chains/PA/shortburst_PRES12_Republican_mean_median_10000_1695552174.pkl', help="path to biased chain file")
    parser.add_argument("--proposal", type=str, default="random")
    args = parser.parse_args()

    print(args)
    main(args)
