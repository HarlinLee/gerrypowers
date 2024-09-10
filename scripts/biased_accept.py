import numpy as np
from functools import partial

def get_biased_accept(bias_measure, election_name, party_to_favor):
    accept_dict = {
        'safe_seats' : partial(increase_safe_seats, election_name=election_name, party_to_favor=party_to_favor),
        'efficiency_gap' : partial(increase_efficiency_gap, election_name=election_name),
        'mean_median' : partial(increase_mean_median, election_name=election_name),
        'partisan_bias' : partial(increase_partisan_bias, election_name=election_name),
        'partisan_gini' : partial(increase_partisan_gini, election_name=election_name)
    }

    return accept_dict[bias_measure]

# this is from the Duchin, Needham, Weighill paper
def increase_safe_seats(partition, election_name, party_to_favor):
    parent = partition.parent
    DEMseats = len(
        [x for x in partition[election_name].percents(party_to_favor) if x > 0.53]
    )
    DEMseatsparent = len(
        [x for x in parent[election_name].percents(party_to_favor) if x > 0.53]
    )
    alpha = np.exp(2*(DEMseats-DEMseatsparent))
    doaccept = (np.random.random() < alpha)
    return doaccept

def get_elec_results(x, election_name):
    # get ElectionResults for a map
    return x.updaters[election_name](x)

def increase_efficiency_gap(partition, election_name):
    parent = partition.parent

    curr = get_elec_results(partition, election_name)
    prev = get_elec_results(parent, election_name)

    ## change here if we are interested in different "bias" metric
    # https://gerrychain.readthedocs.io/en/latest/api.html#module-gerrychain.metrics
    delta = curr.efficiency_gap() - prev.efficiency_gap()
    beta = 300
        
    # accept immediately if "bias" is not decreasing
    # else: accept with some probability, Metropolis style
    return np.random.random() < np.exp(beta*delta)

def increase_mean_median(partition, election_name):
    parent = partition.parent

    curr = get_elec_results(partition, election_name)
    prev = get_elec_results(parent, election_name)

    delta = curr.mean_median() - prev.mean_median()
    beta = 300 # this should change with the bias metric
    
    # accept immediately if "bias" is not decreasing
    # else: accept with some probability, Metropolis style
    return np.random.random() < np.exp(beta*delta)

def increase_partisan_bias(partition, election_name):
    parent = partition.parent

    curr = get_elec_results(partition, election_name)
    prev = get_elec_results(parent, election_name)

    delta = curr.partisan_bias() - prev.partisan_bias()
    beta = 50 # this should change with the bias metric
        
    # accept immediately if "bias" is not decreasing
    # else: accept with some probability, Metropolis style
    return np.random.random() < np.exp(beta*delta)


def increase_partisan_gini(partition, election_name):
    parent = partition.parent

    curr = get_elec_results(partition, election_name)
    prev = get_elec_results(parent, election_name)

    delta = curr.partisan_gini() - prev.partisan_gini()
    beta = 300 # this should change with the bias metric
        
    # accept immediately if "bias" is not decreasing
    # else: accept with some probability, Metropolis style
    return np.random.random() < np.exp(beta*delta)
