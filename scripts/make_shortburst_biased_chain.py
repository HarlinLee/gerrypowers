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

def main(args):
    party_to_favor = args.party
    election_name = args.election
    state = args.state
    bias_measure = args.bias
    num_steps = args.n
    short_burst_steps = args.sb
    
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

    start_partition = initial_partition

    for outer_step in range(num_steps):
    
        # short burst chain
        chain = MarkovChain(
            proposal=proposal,
            constraints=[
                pop_constraint,
                compactness_bound
            ],
            accept=accept.always_accept,
            initial_state=start_partition,
            total_steps=short_burst_steps
        )

        inner_partitions = []
        inner_scores = []
        
        if args.diversity:
            chain = collect_diversity_stats(chain)

        for idx, partition in enumerate(chain):
            if args.diversity:
                partition, stats = partition

            curr = partition.updaters[election_name](partition)
            inner_partitions.append(partition)

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

            inner_scores.append(score)
            all_assignments.append(partition.assignment)
            elec_result = partition[election_name]
            all_election_results.append(elec_result.totals_for_party)

        start_partition = inner_partitions[np.array(inner_scores).argmax()]
        all_scores.append(inner_scores)

        if outer_step % 100 == 0:
            if args.diversity:
                print(outer_step, stats)
            else:
                print(outer_step)
        
    if args.diversity:
        print(stats)

    from time import time
    fname = f'biased_chains/{state}/'+'_'.join(["shortburst", election_name, party_to_favor, bias_measure, str(num_steps),str(int(time()))])

    with open(fname +'.pkl', 'wb') as f:
        pickle.dump((all_assignments,
                     all_election_results,
                     all_scores,
                     elec_result.election.parties_to_columns), f, pickle.HIGHEST_PROTOCOL)

    print('assignments saved to', fname)

    all_scores = np.array(all_scores)
    print(all_scores.shape)

    plt.figure(figsize=(6, 10))
    plt.plot(all_scores.T, np.tile(range(all_scores.shape[0]), [all_scores.shape[1], 1]), color='tab:blue', alpha=0.5)
    plt.xlabel('score')
    plt.ylabel('steps')
    plt.tight_layout()
    plt.savefig(fname+'_lines.pdf')

    #data = pd.DataFrame(all_percents) #convert to a pd dataframe

    #utils.plot_bias_metrics(data, partisan_metric_values, all_safe_seats, all_party_seats, fname)



if __name__ == "__main__":
    parser = ArgumentParser(description="shortburst biased chain")
    parser.add_argument("--party", type=str, default="Democratic", 
        help="party to favor")
    parser.add_argument("--election", type=str, default="PRES16", 
        help="election name")
    parser.add_argument("--bias", default="efficiency_gap", 
        help="bias measure")
    parser.add_argument("--n", type=int, default=10000, 
        help="outer loop number of steps")
    parser.add_argument("--sb", type=int, default=5, 
        help="inner loop (short burst) number of steps")
    parser.add_argument("--state", type=str, default="PA", help="state")
    parser.add_argument("--diversity", type=int, default=0, help="diversity stats only 0 or 1")
    args = parser.parse_args()
    
    print(args)
    
    main(args)
