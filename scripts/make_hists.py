from argparse import ArgumentParser
import pickle
import utils
import glob
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os

def get_metrics_files(args):
    party, folder, ufolder, prefix, election, metric1 = args['party'], args['folder'], args['ufolder'], args['prefix'], args['election'], args['metric1']

    if party == 'Rep':
        Rep = glob.glob(os.path.join(folder, f"{prefix}_{election}_Republican_{metric1}_*-metrics.pkl"))
        Dem = glob.glob(os.path.join(folder, f"{prefix}_{election}_Democratic_{metric1}_*-Republican-metrics.pkl"))
        Neutral = glob.glob(os.path.join(ufolder, f"unbiased_{election}_50000-metrics.pkl"))
        Rep = [r for r in Rep if len(r.split('-'))==2]
    else:
        Rep = glob.glob(os.path.join(folder, f"{prefix}_{election}_Republican_{metric1}_*-Democratic-metrics.pkl"))
        Dem = glob.glob(os.path.join(folder, f"{prefix}_{election}_Democratic_{metric1}_*-metrics.pkl"))
        Neutral = glob.glob(os.path.join(ufolder, f"unbiased_{election}_50000-Democratic-metrics.pkl"))
        Dem = [d for d in Dem if len(d.split('-'))==2]

    assert len(Rep) == 1
    assert len(Dem) == 1
    assert len(Neutral) == 1
    print(Rep, Dem, Neutral)

    rep_values = load_metrics_file(Rep[0], args, True)
    dem_values = load_metrics_file(Dem[0], args, True)
    neu_values = load_metrics_file(Neutral[0], args, False)

    return rep_values, dem_values, neu_values

def load_metrics_file(fname, args, plot=True):
    with open(fname, 'rb') as f:
            partisan_metric_values, all_safe_seats, all_percents, all_party_seats, races = pickle.load(f)
    metric_values = np.array(partisan_metric_values)
    metric_values = np.hstack((metric_values, np.array(all_safe_seats).reshape(-1,1)))
    
    print(metric_values.shape)
    
    if plot:
        utils.plot_bias_metrics(pd.DataFrame(all_percents), partisan_metric_values, all_safe_seats, all_party_seats, f'{fname[:-4]}-plot', args['party'])
    return metric_values

def plot_hist(r, d, ne, metric1, metric2, path, args):
    party, state, election, prefix = args['party'], args['state'], args['election'], args['prefix']

    fig, ax = plt.subplots(1, figsize=(10, 8))
    kwargs = {'alpha': 0.3}

    min_v = min(r.min(), d.min(), ne.min())
    max_v = max(r.max(), d.max(), ne.max())
    print(min_v, max_v)

    if metric2 in ['efficiency_gap', 'mean_median', 'mean_thirdian', 'partisan_gini']:
        kwargs.update({'bins': 200, 'range': (min_v, max_v)})
    elif metric2 in ['partisan_bias', 'safe_seats']:
        kwargs.update({'range': (min_v, max_v), 'bins':20})

    ax.hist(r, label=f'Republican (calculated wrt {party})', color='r', **kwargs)
    ax.hist(d, label=f'Democrat (calculated wrt {party})', color='b', **kwargs)
    ax.hist(ne, label=f'Unbiased (calculated wrt {party})', color='k', **kwargs)

    ax.axvline(ne[0], label=f'{state} CD', color='k', alpha=1)
    plt.title(f"{election}, biased for {metric1}. comparing {metric2}", fontsize=16)
    plt.legend()
    plt.tight_layout()
    
    plot_f = os.path.join(path, f'{prefix}-{metric2}-{party}.pdf')
    plt.savefig(plot_f, dpi=300)
    plt.close()
    
    print(plot_f)
        
def main(args):
    s = args.s
    state = args.state

    args = vars(args)
    
    elections = ['PRES16', 'PRES12']
    biased_metrics = ['efficiency_gap', 'mean_median', 'partisan_bias', 'partisan_gini',  'safe_seats']
    compared_metrics = ['efficiency_gap', 'mean_median', 'partisan_bias', 'partisan_gini',
            'mean_thirdian', 'safe_seats'] 
    
    folder = f"biased_chains/{state}"
    ufolder = f"unbiased_chains/{state}"
    
    args.update({"folder": folder, "ufolder": ufolder, "elections" : elections, "biased_metrics": biased_metrics, "compared_metrics": compared_metrics})

    for prefix in ["shortburst", "hill"]:        
        args["prefix"] = prefix

        for party in ["Rep", "Dem"]:
            args["party"] = party
        
            for election in elections:
                args["election"] = election

                # save correlation here
                cor_r = np.zeros((len(biased_metrics), len(compared_metrics)))
                cor_d = np.zeros_like(cor_r)
                cor_n = np.zeros_like(cor_d)

                # create scatter plot fig
                fig, ax = plt.subplots(len(compared_metrics), len(biased_metrics), figsize=(12, 8))

                for i, metric1 in enumerate(biased_metrics):
                    args["metric1"] = metric1
                    
                    print(args)

                    hist_f = os.path.join(folder, election, metric1)
                    os.makedirs(hist_f,  exist_ok=True)

                    # get data
                    rep_values, dem_values, neu_values = get_metrics_files(args)
                    
                    if metric1 != 'safe_seats':
                        ri = rep_values[:, i]
                        di = dem_values[:, i]
                        ni = neu_values[:, i]
                    else:
                        ri = rep_values[:, -1]
                        di = dem_values[:, -1]
                        ni = neu_values[:, -1]
                    
                    for j, metric2 in enumerate(compared_metrics):
                        rj = rep_values[:, j]
                        dj = dem_values[:, j]
                        nj = neu_values[:, j]

                        # get correlation values
                        print('correlation', np.corrcoef(ri, rj))
                        cor_r[i, j] = np.corrcoef(ri, rj)[0,1]
                        cor_d[i, j] = np.corrcoef(di, dj)[0,1]
                        cor_n[i, j] = np.corrcoef(ni, nj)[0,1]
                        
                        # plot histogram
                        plot_hist(rj, dj, nj, metric1, metric2, hist_f, args)

                        # plot scatterplots
                        kwargs = {'alpha': 0.1}
                    
                        if metric1 != metric2:
                            ax[j, i].scatter(ni[::s], nj[::s], label=f'Unbiased (calculated wrt {party})', color='k', **kwargs)
                            ax[j, i].scatter(ri[::s], rj[::s], label=f'Republican (calculated wrt {party})', color='r', **kwargs)
                            ax[j, i].scatter(di[::s], dj[::s], label=f'Democrat (calculated wrt {party})', color='b', **kwargs)

                            ax[j, i].set_ylabel(metric2, fontsize=8)
                        else:
                            # histograms if metric1 = metric2
                            ax[j, i].hist(ni, label=f'Unbiased (calculated wrt {party})', color='k', bins=50, **kwargs)
                            ax[j, i].hist(ri, label=f'Republican (calculated wrt {party})', color='r', bins=50,  **kwargs)
                            ax[j, i].hist(di, label=f'Democrat (calculated wrt {party})', color='b', bins=50, **kwargs)
                    
                        ax[j, i].set_xlabel(metric1, fontsize=8)
                        ax[j, i].tick_params(axis='both', which='major', labelsize=6)
                        ax[j, i].tick_params(axis='both', which='minor', labelsize=6)

                # finish and save scatter plots
                plt.suptitle(f"{election}. chains are biased for metric on the x-axis", fontsize=16)
                plt.tight_layout()
         
                plot_f = os.path.join(folder, election, f"{prefix}-{party}-scatter.pdf")
                plt.savefig(plot_f, dpi=300)
                plt.close()
                print(plot_f)

                # plot correlation heatmaps
                for label, cor in zip(['Republican', 'Democrat', 'Unbiased'], [cor_r, cor_d, cor_n]):
            
                    fig, ax = plt.subplots()
                    im = ax.imshow(cor)

                    ax.set_xticks(np.arange(len(compared_metrics)), labels=compared_metrics)
                    ax.set_yticks(np.arange(len(biased_metrics)), labels=biased_metrics)

                    plt.setp(ax.get_xticklabels(), rotation=45, ha="right",
                 rotation_mode="anchor")

                    for i in range(len(biased_metrics)):
                        for j in range(len(compared_metrics)):
                            text = ax.text(j, i, np.around(cor[i, j], 2),
                               ha="center", va="center", color="w")

                    ax.set_title(election + ', ' +label, fontsize=16)
                    ax.set_xlabel('compared')
                    ax.set_ylabel('biased')
                   
                    plot_f = os.path.join(folder, election, f"{prefix}-{party}-{label}-correlation.pdf")
                    plt.savefig(plot_f, dpi=300)
                    plt.close()
                    print(plot_f)

if __name__ == "__main__":
    parser = ArgumentParser(description="make plots of metrics")
    parser.add_argument("--s", type=int, default=40, 
        help="plot 1 every s numbers for the scatter plot")
    parser.add_argument("--state", type=str, default="NC", help="state")
    args = parser.parse_args()
    
    print(args)
    
    main(args)
    print('done!')
