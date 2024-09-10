import pickle
from gerrychain import (GeographicPartition, Graph, MarkovChain, accept,
                        updaters, constraints, Election)
from gerrychain.proposals import propose_random_flip
from gerrychain.updaters.election import ElectionResults
import glob
import utils
import os
import numpy as np
import pandas as pd
from argparse import ArgumentParser

#### copy pasted and modified from https://gerrychain.readthedocs.io/en/latest/_modules/gerrychain/metrics/partisan.html?highlight=gerrychain.metrics.partisan
def mean_median(election_results, first_party):
    """
    Computes the Mean-Median score for the given ElectionResults.
    A positive value indicates an advantage for the first party listed
    in the Election's parties_to_columns dictionary.
    """
    data = election_results.percents(first_party)

    return np.median(data) - np.mean(data)



def mean_thirdian(election_results, first_party):
    """
    Computes the Mean-Median score for the given ElectionResults.
    A positive value indicates an advantage for the first party listed
    in the Election's parties_to_columns dictionary.

    The motivation for this score is that the minority party in many
    states struggles to win even a third of the seats.
    """
    data = election_results.percents(first_party)

    thirdian_index = round(len(data) / 3)
    thirdian = sorted(data)[thirdian_index]

    return thirdian - np.mean(data)


def efficiency_gap(results, parties):
    """
    Computes the efficiency gap for the given ElectionResults.
    A positive value indicates an advantage for the first party listed
    in the Election's parties_to_columns dictionary.
    """
    party1, party2 = [results.counts(party) for party in parties]
    wasted_votes_by_part = map(wasted_votes, party1, party2)
    total_votes = results.total_votes()
    numerator = sum(waste2 - waste1 for waste1, waste2 in wasted_votes_by_part)
    return numerator / total_votes



def wasted_votes(party1_votes, party2_votes):
    """
    Computes the wasted votes for each party in the given race.
    :party1_votes: the number of votes party1 received in the race
    :party2_votes: the number of votes party2 received in the race
    """
    total_votes = party1_votes + party2_votes
    if party1_votes > party2_votes:
        party1_waste = party1_votes - total_votes / 2
        party2_waste = party2_votes
    else:
        party2_waste = party2_votes - total_votes / 2
        party1_waste = party1_votes
    return party1_waste, party2_waste



def partisan_bias(election_results, first_party):
    """
    Computes the partisan bias for the given ElectionResults.
    The partisan bias is defined as the number of districts with above-mean
    vote share by the first party divided by the total number of districts,
    minus 1/2.
    """
    party_shares = np.array(election_results.percents(first_party))
    mean_share = np.mean(party_shares)
    above_mean_districts = len(party_shares[party_shares > mean_share])
    return (above_mean_districts / len(party_shares)) - 0.5




def run(first_party, election_name, election, my_updaters, all_assignments, all_election_results, graph):
    partisan_metric_values = [] # keep track of the partisan metrics
    all_safe_seats = []
    all_percents = [] # keep track of the democratic percentages
    all_party_seats = []

    if first_party == 'Republican':
        parties = [first_party, 'Democratic']
    else:
        parties = [first_party, 'Republican']

    for idx in range(len(all_assignments)):
        elec_result = ElectionResults(election, all_election_results[idx], all_assignments[idx].parts)
        partition = GeographicPartition(graph, all_assignments[idx], updaters=my_updaters)

        partisan_metric_values.append([
            efficiency_gap(elec_result, parties), 
            mean_median(elec_result, first_party), 
            partisan_bias(elec_result, first_party),
            elec_result.partisan_gini(),
            mean_thirdian(elec_result, first_party)
            ])

        safe_seats = len([x for x in partition[election_name].percents(first_party) if x > 0.53])
        all_safe_seats.append(safe_seats)
        all_percents.append(sorted(partition[election_name].percents(first_party)))
        all_party_seats.append(elec_result.seats(first_party))
        
        if idx % 1000 == 0:
            print(idx)
        
    return partisan_metric_values, all_safe_seats, all_percents, all_party_seats, elec_result




def main(args):
    state = args.state
    
    for folder in [f"biased_chains/{state}", f"unbiased_chains/{state}"]:

        biased = folder.startswith("biased_chains/")
        print(folder, biased)
        if biased:
            fns = glob.glob(os.path.join(folder, "hill_*_50000_*[0-9].pkl"))
            fns += glob.glob(os.path.join(folder, "shortburst_*_10000_*[0-9].pkl"))
        else:
            fns = glob.glob(os.path.join(folder, "unbiased_*_50000.pkl"))
        print(fns)
        
        ## get data
        graph = utils.read_geodata(f"./data/{state}/{state}.shp")
        
        for fname in fns:
            if 'metrics' in fname:
                continue
                
            fname = fname[:-4]
            fname = fname.split('/')[-1]
            
            print(fname)
            
            if biased and 'shortburst' in fname:
                with open(os.path.join(folder, fname+'.pkl'), 'rb') as f:
                    all_assignments, all_election_results, all_scores, races = pickle.load(f)
                
                L = fname.split('_')
                election_name, party_to_favor = L[1], L[2]
            elif biased and 'hill' in fname:
                with open(os.path.join(folder, fname+'.pkl'), 'rb') as f:
                    all_assignments, all_election_results, all_scores, races, args, all_stats = pickle.load(f)
                
                L = fname.split('_')
                election_name, party_to_favor = L[1], L[2]
            else:
                with open(os.path.join(folder, fname+'.pkl'), 'rb') as f:
                    all_assignments, all_election_results, races = pickle.load(f)

                L = fname.split('_')
                election_name  = L[1]
                party_to_favor = "Republican"
            
            print(election_name, party_to_favor, races)
            
            election = Election(election_name, races)
            my_updaters = {"population": updaters.Tally("TOTPOP", alias="population")}
            my_updaters.update(utils.get_elections(party_to_favor, election_name, state))
               
            # calculate w.r.t. OG party   
            partisan_metric_values, all_safe_seats, all_percents, all_party_seats, elec_result = run(
                party_to_favor, election_name, election, my_updaters, all_assignments, all_election_results, graph)
            
            with open(os.path.join(folder, fname + '-metrics.pkl'), 'wb') as f:
                pickle.dump((partisan_metric_values, 
                         all_safe_seats, 
                         all_percents,
                         all_party_seats,
                         elec_result.election.parties_to_columns), f, pickle.HIGHEST_PROTOCOL)
            print('metrics saved to', fname + '-metrics.pkl')

            # calculate w.r.t. the OTHER party
            if party_to_favor == 'Republican':
                party_to_favor = 'Democratic'
            elif party_to_favor == 'Democratic':
                party_to_favor = 'Republican'
            else:
                print('something is going wrong')
                  
            partisan_metric_values, all_safe_seats, all_percents, all_party_seats, elec_result = run(
                party_to_favor, election_name, election, my_updaters, all_assignments, all_election_results, graph)
            
            with open(os.path.join(folder, fname + f'-{party_to_favor}-metrics.pkl'), 'wb') as f:
                pickle.dump((partisan_metric_values, 
                         all_safe_seats, 
                         all_percents,
                         all_party_seats,
                         elec_result.election.parties_to_columns), f, pickle.HIGHEST_PROTOCOL)
            print('metrics saved to', folder, fname + f'-{party_to_favor}-metrics.pkl')
            
if __name__ == "__main__":
    parser = ArgumentParser(description="calculate and save metrics")
    parser.add_argument("--state", type=str, default="NC",
        help="state")
    args = parser.parse_args()
    
    print(args)
    
    main(args)
