import numpy as np
import pickle
import random
from functools import partial
import pandas as pd
from gerrychain import (GeographicPartition, Graph, MarkovChain,
                        updaters, constraints, Election, accept)
from gerrychain.proposals import reversible_recom, propose_chunk_flip, propose_random_flip
import utils

"""
Run it like this:

PYTHONHASHSEED=0 python -u ./run_hp_from_scratch.py --k 200000 --m 32 --map random --fn unbiased_chains/NC/unbiased_PRES16_50000.pkl --proposal reversible --seed 1
"""

def fast_reversible_propose(partition):
    """
    Propose a flip uniformly over (boundary node, neighbouring district) pairs, the same
    distribution as gerrychain's slow_reversible_propose but without building the set of
    pairs. A random cut edge and endpoint v pick the pair (v, d) with probability
    a/(2|cut edges|), where a is the number of edges from v to district d; accepting
    with probability 1/a makes all pairs equally likely, otherwise we try again.
    """
    cut_edges = tuple(partition["cut_edges"])
    if len(cut_edges) == 0:
        return partition
    mapping = partition.assignment.mapping
    while True:
        edge = random.choice(cut_edges)
        index = random.choice((0, 1))
        flipped_node, other_node = edge[index], edge[1 - index]
        district = mapping[other_node]
        a = sum(1 for u in partition.graph.neighbors(flipped_node) if mapping[u] == district)
        if random.random() * a < 1:
            return partition.flip({flipped_node: district})

# epsilons and chain lengths recorded in the per-test summary, as in the power analysis
SUMMARY_EPS = [0.005, 0.003, 0.001, 0.0005, 0.0001, 0.00005, 0.000009]
SWEEP_EPS = [0.005, 0.003, 0.001]
SWEEP_K = [1000] + list(range(10000, 200000, 10000))

def save_summary(path, args, map_idx, w_vals, alpha=0.05):
    """
    Save which label functions reject the null for this test, for each epsilon (full
    chain) and for shorter chains (k sweep), computed with read_hp_results.test_hypothesis.
    One small file per test, so partial results can be reported at any time without
    reading the full results.
    """
    import json
    import os
    from read_hp_results import test_hypothesis

    def outcome(eps, k=None):
        (_, p_upper), (_, p_lower) = test_hypothesis(w_vals, eps, alpha, verbose=False, k=k)
        return {'upper': [int(x) for x in p_upper <= alpha], 'lower': [int(x) for x in p_lower <= alpha]}

    summary = {
        'fn': args.fn, 'map_idx': int(map_idx), 'seed': args.seed, 'proposal': args.proposal,
        'k': args.k, 'm': args.m, 'alpha': alpha, 'w0': [float(x) for x in w_vals[0, 0]],
        'full': {str(eps): outcome(eps) for eps in SUMMARY_EPS},
        'sweep': {str(eps): {str(k): outcome(eps, k) for k in SWEEP_K if k < args.k} for eps in SWEEP_EPS},
    }
    tmp = path + '.tmp'
    with open(tmp, 'w') as f:
        json.dump(summary, f)
    os.replace(tmp, path)

def main(args):
    # gerrychain draws from python's random module and ignores GERRYCHAIN_RANDOM_SEED
    if args.seed is not None:
        random.seed(args.seed)
        np.random.seed(args.seed)

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


    if args.plans is not None:
        # starting plans extracted from the chain file beforehand ({map index: assignment}),
        # so the large chain file does not have to be loaded
        with open(args.plans, 'rb') as f:
            all_assignments = pickle.load(f)
        map_idx = args.map_idx
    elif biased:
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

    # a given map index overrides --map, e.g. to re-test the maps of an earlier run
    if args.map_idx is not None:
        map_idx = args.map_idx
        print('index of map investigated (given by --map_idx)', map_idx)

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
    elif prop == 'reversible':
        # with the contiguity constraint below this chain is reversible (Theorem 3.1)
        proposal = fast_reversible_propose
        interval = 10000
    elif prop == 'chunk':
        proposal = propose_chunk_flip
        interval = 100
    else:
        raise ValueError("only recom, random, reversible, or chunk flip")

    compactness_bound = constraints.UpperBound(
            lambda p: len(p["cut_edges"]),
            2*len(initial_partition["cut_edges"])
            )

    pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, 0.02)

    # flip proposals do not check that the district losing a node stays connected,
    # so reject (and redraw) any proposal that splits a district
    if prop in ('random', 'reversible'):
        contiguity = constraints.single_flip_contiguous
    else:
        contiguity = constraints.contiguous

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
                compactness_bound,
                contiguity
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
            if args.save_percents:
                percents.append(sorted(partition[election_name].percents(party_to_favor)))

            if idx % interval == 0:
                print(idx)

        # each column in this matrix is a metric and
        # each row is a step in the chain 
        w_vals = np.hstack((np.array(partisan_metric_values), np.array(safe_seats).reshape(-1, 1)))
        all_metric_values.append(w_vals)
        if args.save_percents:
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

    if args.summary is not None:
        save_summary(args.summary, args, map_idx, np.array(all_metric_values))
        print('summary saved to', args.summary)

if __name__ == "__main__":
    from argparse import ArgumentParser

    parser = ArgumentParser(description="hypothesis test")
    parser.add_argument("--map", type=str, default="random", help="max, min, random")
    parser.add_argument("--map_idx", type=int, default=None,
        help="index of the map to investigate; overrides --map")
    parser.add_argument("--k", type=int, default=100000,
        help="number of steps in Markov chain")
    parser.add_argument("--m", type=int, default=32,
        help="number of independent trajectories")
    parser.add_argument("--bias", default="mean_median",
        help="label function")
    parser.add_argument("--a", type=float, default=0.009, help="alpha")
    parser.add_argument("--e", type=float, default=0.003, help="epsilon")
    parser.add_argument("--fn", type=str, default='biased_chains/PA/shortburst_PRES12_Republican_mean_median_10000_1695552174.pkl', help="path to biased chain file")
    parser.add_argument("--proposal", type=str, default="reversible",
        help="reversible (the flip chain of Theorem 3.1), random (not reversible, kept to reproduce the original results), recom, chunk")
    parser.add_argument("--seed", type=int, default=None, help="random seed")
    parser.add_argument("--save_percents", type=int, default=1,
        help="1 to save the sorted vote shares of every step, 0 to skip them")
    parser.add_argument("--plans", type=str, default=None,
        help="pickle of {map index: assignment} to read the starting plan from instead of --fn (needs --map_idx)")
    parser.add_argument("--summary", type=str, default=None,
        help="json file for a small summary of this test's outcomes")
    args = parser.parse_args()

    print(args)
    main(args)
