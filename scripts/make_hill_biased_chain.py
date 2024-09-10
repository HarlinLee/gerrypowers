import pickle
from functools import partial
import pandas as pd
from argparse import ArgumentParser
from gerrychain import (GeographicPartition, Graph, MarkovChain,
                        updaters, constraints, Election, accept)
from gerrychain.proposals import recom
from gerrychain.tree import bipartition_tree
from gerrychain.meta.diversity import collect_diversity_stats
import utils
import numpy as np
import matplotlib.pyplot as plt
from biased_accept import get_biased_accept

def main(args):
    party_to_favor = args.party
    election_name = args.election
    state = args.state
    bias_measure = args.bias
    num_steps = args.n
    
    ## get data
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
    ideal_population = sum(initial_partition["population"].values()) / len(initial_partition)

    # set up constraints and chain
    proposal = partial(recom,
                       pop_col="TOTPOP",
                       pop_target=ideal_population,
                       epsilon=0.02,
                       node_repeats=2,
                       method=partial(
                           bipartition_tree,
                           max_attempts=100,
                           warn_attempts=100,
                           allow_pair_reselection=True
                       ))

    compactness_bound = constraints.UpperBound(
        lambda p: len(p["cut_edges"]),
        2*len(initial_partition["cut_edges"])
    )

    pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, 0.02)

    
    all_assignments = [] # something pickle-able
    all_election_results = []
    all_scores = []
    all_stats = []
    
    chain = MarkovChain(
        proposal=proposal,
        constraints=[
            pop_constraint,
            compactness_bound
            ],
        accept=get_biased_accept(bias_measure, election_name, party_to_favor),
        #accept=accept.always_accept,
        initial_state=initial_partition,
        total_steps=num_steps
    )

    if args.diversity:
        chain = collect_diversity_stats(chain)

    for idx, partition in enumerate(chain):
        if args.diversity:
            partition, stats = partition

        curr = partition.updaters[election_name](partition)

        if bias_measure == "mean_median":
            score = curr.mean_median()
        elif bias_measure == "efficiency_gap":
            score = curr.efficiency_gap()
        elif bias_measure == "partisan_bias":
            score = curr.partisan_bias()
        elif bias_measure == "partisan_gini":
            score = curr.partisan_gini()
        elif bias_measure == "safe_seats":
            score = len([x for x in partition[election_name].percents(party_to_favor) if x > 0.53])

        all_scores.append(score)
        all_assignments.append(partition.assignment)
        elec_result = partition[election_name]
        all_election_results.append(elec_result.totals_for_party)

        if idx % 200 == 0:
            if args.diversity:
                print(idx, stats)
                all_stats.append(stats)
            else:
                print(idx)
        

    from time import time
    fname = f'biased_chains/{state}/'+'_'.join(["hill", election_name, party_to_favor, bias_measure, str(num_steps),str(int(time()))])

    with open(fname +'.pkl', 'wb') as f:
        pickle.dump((all_assignments,
                     all_election_results,
                     all_scores,
                     elec_result.election.parties_to_columns, args, all_stats), f, pickle.HIGHEST_PROTOCOL)

    print('assignments saved to', fname)

    all_scores = np.array(all_scores)
    print(all_scores.shape)

    plt.figure(figsize=(6, 10))
    plt.plot(all_scores, np.arange(len(all_scores)), color='tab:blue', alpha=0.5)
    plt.xlabel('score')
    plt.ylabel('steps')
    plt.tight_layout()
    plt.savefig(fname+'_lines.pdf')

    #data = pd.DataFrame(all_percents) #convert to a pd dataframe

    #utils.plot_bias_metrics(data, partisan_metric_values, all_safe_seats, all_party_seats, fname)



if __name__ == "__main__":
    parser = ArgumentParser(description="shortburst biased chain")
    parser.add_argument("--party", type=str, default="Republican", 
        help="party to favor")
    parser.add_argument("--election", type=str, default="PRES16", 
        help="election name")
    parser.add_argument("--bias", default="efficiency_gap", 
        help="bias measure")
    parser.add_argument("--n", type=int, default=10000, 
        help="number of steps")
    parser.add_argument("--diversity", type=int, default=0, 
        help="diversity stats 0 or 1 only")
    parser.add_argument("--state", type=str, default="NC", help="state")
    args = parser.parse_args()
    
    print(args)
    
    main(args)
