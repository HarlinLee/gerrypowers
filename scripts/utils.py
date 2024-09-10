import matplotlib.pyplot as plt
from gerrychain import (GeographicPartition, Graph, MarkovChain,
                        updaters, constraints, Election)
from gerrychain.proposals import recom
import geopandas as gpd
import numpy as np

def read_geodata(path):
    # read in the shapefile as a dataframe
    vtds = gpd.read_file(path)
    print(path, 'has columns:', vtds.columns)
    # convert the dataframe to a gerrychain-viable object
    graph = Graph.from_geodataframe(vtds, ignore_errors=True)
    return graph

def get_elections(party_to_favor, election_name, state="PA"):
    if state == "PA":
        elections_dict = {
            "SEN10" : {"Republican": "SEN10R", "Democratic": "SEN10D"},
            "SEN12" : {"Republican": "USS12R", "Democratic": "USS12D"},
            "SEN16" : {"Republican": "T16SENR", "Democratic": "T16SEND"},
            "PRES12" : {"Republican": "PRES12R", "Democratic": "PRES12D"},
            "PRES16" : {"Republican": "T16PRESR", "Democratic": "T16PRESD"}
        }
    elif state == "TX":
        elections_dict = {
            "SEN12" : {"Republican": "SEN12R", "Democratic": "SEN12D"},
            "SEN14" : {"Republican": "SEN14R", "Democratic": "SEN14D"},
            "PRES12" : {"Republican": "PRES12R", "Democratic": "PRES12D"},
            "PRES16" : {"Republican": "PRES16R", "Democratic": "PRES16D"}
        }
    elif state == "MD":
        elections_dict = { 
            "SEN12" : {"Republican": "SEN12R", "Democratic": "SEN12D"},
            "SEN16" : {"Republican": "SEN16R", "Democratic": "SEN16D"},
            "SEN18" : {"Republican": "SEN18R", "Democratic": "SEN18D"},
            "PRES12" : {"Republican": "PRES12R", "Democratic": "PRES12D"},
            "PRES16" : {"Republican": "PRES16R", "Democratic": "PRES16D"}
        }
    elif state == "WI":
        elections_dict = {
            "SEN12" : {"Republican": "SEN12R", "Democratic": "SEN12D"},
            "SEN16" : {"Republican": "SEN16R", "Democratic": "SEN16D"},
            "SEN18" : {"Republican": "SEN18R", "Democratic": "SEN18D"},
            "PRES12" : {"Republican": "PRES12R", "Democratic": "PRES12D"},
            "PRES16" : {"Republican": "PRES16R", "Democratic": "PRES16D"}
        }
    elif state == "NC":
       elections_dict = { 
            "SEN14" : {"Republican": "EL14G_USS_", "Democratic": "EL14G_US_1"},
            "SEN16" : {"Republican": "EL16G_USS_", "Democratic": "EL16G_US_1"},
            "PRES12" : {"Republican": "EL12G_PR_R", "Democratic": "EL12G_PR_D"},
            "PRES16" : {"Republican": "EL16G_PR_R", "Democratic": "EL16G_PR_D"}
        }
    else:
        raise Exception

    if party_to_favor == "Democratic":
        tmp = {}
        for k, v in elections_dict.items():
            tmp[k] = {"Democratic": v["Democratic"], "Republican": v["Republican"]}
        elections_dict = tmp.copy()

    elections = [Election(election_name, elections_dict[election_name])]
    print([(e.name, e.parties) for e in elections])

    # Election updaters, for computing election results using the vote totals
    # from our shapefile.

    election_updaters = {election.name: election for election in elections}

    return election_updaters
 
    
def plot_bias_metrics(data, partisan_metric_values, all_safe_seats, all_party_seats, fname, party_to_favor=None):

    fig = plt.figure(figsize=(15, 5))

    ax = fig.add_subplot(1,3,1)
    # Draw 50% line
    ax.axhline(0.5, color="#cccccc")
    # Draw boxplot
    data.boxplot(ax=ax, positions=range(len(data.columns)))
    # Draw initial plan's Democratic vote %s (.iloc[0] gives the first row)
    plt.plot(data.iloc[0], "ro")
    # Annotate
    ax.set_title("Comparing the plan to an ensemble")
    ax.set_ylabel(f"{party_to_favor} vote %")
    ax.set_xlabel("Sorted districts")
    ax.set_ylim(0, 1)
    ax.set_yticks([0, 0.25, 0.5, 0.75, 1])

    ax = fig.add_subplot(1,3,2)
    ax.plot(np.arange(0, len(partisan_metric_values), 10), partisan_metric_values[::10], alpha=0.5)
    ax.set_title(f'Partisan metrics (calculated wrt {party_to_favor})')
    ax.set_xlabel('Steps on MCMC chain')
    ax.legend(['efficiency gap', 'mean median', 'partisan bias', 'partisan gini', 'mean thirdian'])

    ax = fig.add_subplot(1,3,3)
    ax.plot(np.arange(0, len(partisan_metric_values), 10), all_safe_seats[::10], alpha=0.5)
    ax.plot(np.arange(0, len(partisan_metric_values), 10), all_party_seats[::10], alpha=0.5)
    ax.legend(['safe seats', 'seats won'])
    ax.set_xlabel('Steps on MCMC chain')
    ax.set_title(f'# of seats for {party_to_favor}')

    plt.tight_layout()
    plt.savefig(fname+'.pdf', dpi=300, format='pdf')
    plt.close()
