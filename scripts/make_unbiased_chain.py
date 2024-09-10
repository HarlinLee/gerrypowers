import pickle
from functools import partial
from argparse import ArgumentParser
from gerrychain import (GeographicPartition, Graph, MarkovChain,
                        updaters, constraints, Election, accept)
from gerrychain.proposals import recom
from gerrychain.meta.diversity import collect_diversity_stats
from gerrychain.tree import bipartition_tree
import utils

def main(args):
    election_name = args.election
    state = args.state
    num_steps = args.n
    diversity = args.diversity

    ## get data
    graph = utils.read_geodata(f"./data/{state}/{state}.shp")
    my_updaters = {"population": updaters.Tally("TOTPOP", alias="population")}
    my_updaters.update(utils.get_elections("Republican", election_name, state))
    
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
                       )                 
                        )

    compactness_bound = constraints.UpperBound(
        lambda p: len(p["cut_edges"]),
        2*len(initial_partition["cut_edges"])
    )

    pop_constraint = constraints.within_percent_of_ideal_population(initial_partition, 0.02)

    chain = MarkovChain(
        proposal=proposal,
        constraints=[
            pop_constraint,
            compactness_bound
        ],
        accept=accept.always_accept,
        initial_state=initial_partition,
        total_steps=num_steps
        )
    
    all_assignments = [] # something pickle-able
    all_election_results = []
    
    if diversity:
        chain = collect_diversity_stats(chain)
    
    for idx, partition in enumerate(chain):
        if diversity:
            partition, stats = partition

        all_assignments.append(partition.assignment)
        elec_result = partition[election_name]
        all_election_results.append(elec_result.totals_for_party)

        if idx % 1000 == 0:
            if diversity:
                print(idx, stats)
            else:
                print(idx)
    if diversity:
        print(stats)
        
    fname = f'unbiased_chains/{state}/'+'_'.join(["unbiased", election_name, str(num_steps)])

    with open(fname +'.pkl', 'wb') as f:
        pickle.dump((all_assignments,
                     all_election_results,
                     elec_result.election.parties_to_columns), f, pickle.HIGHEST_PROTOCOL)

    print('assignments saved to', fname)

if __name__ == "__main__":
    parser = ArgumentParser(description="unbiased chain")
    parser.add_argument("--election", type=str, default="PRES16", 
        help="election name")
    parser.add_argument("--n", type=int, default=50000, 
        help="outer loop number of steps")
    parser.add_argument("--state", type=str, default="PA", help="state")
    parser.add_argument("--diversity", type=int, default=0, help="collect diversity stats")
    args = parser.parse_args()
    
    print(args)
    
    main(args)
