# Empirical Power Analysis of a Statistical Test to Quantify Gerrymandering

Work by Ranthony A. Clark, Susan Glenn, Harlin Lee, and Soledad Villar.

We generate biased MCMC chains using hill climbing[^fn1] and short burst[^fn2], then run an emprirical power analysis of the outlier test in Theorem 3.1 (Chikina, Frieze, Mattingly & Pegden)[^fn3].

## Software

```python
module load python/3.11.6
python -m venv .venv
source .venv/bin/activate
pip install gerrychain
pip install shapely==2.0.1 # this line might not be necessary but untested.
pip install -r 'https://raw.githubusercontent.com/mggg/GerryChain/main/docs/requirements.txt'
pip install descartes
pip list
```

## Structure
This code relies heavily on consistent naming of the chain files.
  1. Making unbiased and biased chains
     - `make_unbiased_chains.sh` runs `make_unbiased_chain.py`.
         - Saves `unbiased_chains/{state}/unbiased_{election}_{n}.pkl`. 
     - `make_hill_chains.sh` runs `make_hill_chains.py`.
         - Saves chain data in `biased_chains/{state}/hill_{election}_{party}_{bias}_{n}_{id}.pkl`. `id` comes from current time.
         - Saves bias metric values in `biased_chains/{state}/hill_{election}_{party}_{bias}_{n}_{id}_lines.pdf`.
     - `make_shorburst_chains.sh` runs `make_shortburst_chain.py`.
         - Files saved are the same as hill climbing, except they start with `shortburst` instead of `hill`.
           
  3. For a given state, calcualte metrics and generate figures for all chains
     - `calculate_metrics.sh` runs `calculate_metrics.py` and `make_hists.py`.
     - Calculated metrics are saved in `biased_chains/{state}/hill_{election}_{party}_{bias}_{n}_{id}-metrics.pkl` or `biased_chains/{state}/hill_{election}_{party}_{bias}_{n}_{id}-{the other party}-metrics.pkl`.
     - Plot of metrics are in `biased_chains/{state}/hill_{election}_{party}_{bias}_{n}_{id}-plot.pdf`.
     - Histograms, correlation heatmaps and scatter plots are in `biased_chains/{state}/{folder}/{metric biased towards}/{shortburst}-{metric used to compare histograms}.pdf`, `biased_chains/{state}/{folder}/{party}-correlation.pdf` and `biased_chains/{state}/{folder}/scatter.pdf`.
    
    
  5. Run multiple trajectories for hypothesis test
     - `run_hp_from_scratch.sh` runs `run_hp_from_scratch.py`.
     - Sample 100 maps from the chain in `fn` (an output of first step), then save the results in `hp/{fn}_{map_idx}_{id}.pkl`. Each of this file contains `m` trajectories.
       
  7. Read results from multiple trajectories and perform hypothesis test
     - `read_hp_results.sh` runs `read_hp_results.py`.
     - Read the trajectories saved from the earlier step and save results in `{fn}_{ep}_{alpha}.csv`.

## Common parameters
| Parameter             | Explanation | Examples |
| :---------------- | :------ | :---- |
| `state`        |   State name | NC, PA, etc [^1]  |
| `election`          |   Election name   | PRES16, PRES12, SEN10, etc [^1] |
| `n` | Number of steps in MCMC chain | 50000, 10000|
| `bias`    |  Bias metric   | mean_median, efficiency_gap, partisan_bias, partisan_gini, safe_seats |
| `party` | Party to favor | Republican or Democratic |
| `diversity` | Collect diversity statistics [^2]  | 0 or 1 |
| `s` | Plot only 1 out of every `s` numbers for readability [^3] | 50 |
 
### Hypothesis test parameters
Shows up in `run_hp_from_scratch.py` and `read_hp_results.py`. 

| Parameter             | Explanation | Examples |
| :---------------- | :------ | :---- |
| `e` | Epsilon for hypothesis test | 0.0005 |
| `a` | Alpha for hypothesis test | 0.05 |
| `m` | Number of trajectories | 32 |
| `k` | Number of steps in MCMC chain | 100000 |
| `proposal` | MCMC chain generation method | recom (reversible), random (flipnode), chunk (chunk flip) [^4]|
| `map` | Which maps to investigate | random (randomly select from chain), max (map with max value), min |
| `fn` | File name | See below |

#### Notes on `run_hp_from_scratch.py`:
  - Where do we control parameter 100, i.e. how many maps to sample from a given chain? In the header of `run_hp_from_scratch.sh`, there's a slurm parameter `#SBATCH --array=1-100%50`. This means run 100 of the same script in parallel but no more than 50 at a time[^5].
- `fn` should be the path to where the (un)biased chain is. For example, `biased_chains/NC/shortburst_PRES16_Republican_partisan_gini_10000_1719440281.pkl` or `unbiased_chains/NC/unbiased_PRES16_50000.pkl`.
  
#### Notes on `read_hp_results.py`:
  - Note that `e` and `a` can take in multiple values separated by comma. For example, `--e 0.015,0.01,0.005,0.003,0.001,0.0005 --a 0.05`.
  - `fn` should be the filename for trajectories from `run_hp_from_scratch.py`. It can be a single pkl file, regex that matches multiple files, or a folder name. For example, `*` in `hp/biased_chains/NC/shortburst_PRES12_Republican_mean_median*.pkl` is a wildcard and can take any value. This should match 100 filenames.

[^fn1]: Duchin, M., Needham, T., Weighill, T. (2022). The (homological) persistence of gerrymandering. Foundations of Data Science, 2022, 4(4): 581-622. doi: 10.3934/fods.2021007
[^fn2]: Cannon, S., Goldbloom-Helzner, A., Gupta, V. et al. (2023). Voting Rights, Markov Chains, and Optimization by Short Bursts. Methodol Comput Appl Probab 25, 36. https://doi.org/10.1007/s11009-023-09994-1
[^fn3]: Chikina, M., Frieze, A., Mattingly, J. C., & Pegden, W. (2020). Separating Effect From Significance in Markov Chain Tests. Statistics and Public Policy, 7(1), 101–114. https://doi.org/10.1080/2330443X.2020.1806763
[^1]: See `get_elections` function in `utils.py` for options.
[^2]: https://gerrychain.readthedocs.io/en/latest/api/#gerrychain.meta.diversity.collect_diversity_stats
[^3]: Only used for scatter plots.
[^4]: https://gerrychain.readthedocs.io/en/latest/api/#module-gerrychain.proposals
[^5]: https://slurm.schedmd.com/job_array.html



