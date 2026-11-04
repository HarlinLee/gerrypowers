# Results of the corrected outlier test

These are the results of the re-run of all 2,200 tests of the paper with the corrected
outlier test: the reversible flip proposal with the contiguity constraint, 32 trajectories
of 200,000 steps from each starting plan, α = 0.05. The tests start from the same 100 plans
per chain as the original tests (20 short-burst chains and 2 neutral chains), with seed
100000 + task number and `PYTHONHASHSEED=0`. See `jobs/README.md` for how they were run.

- `summaries/task_N.json`: one file per test, N being the line of `jobs/all_tests.txt`.
  - `fn`, `map_idx`, `seed`, `proposal`, `k`, `m` and `alpha` identify the test.
  - `w0` holds the metric values of the starting plan, in the order efficiency gap,
    mean-median, partisan bias, partisan gini, mean-thirdian, safe seats.
  - `full[ε]["upper"]` and `full[ε]["lower"]` are 0/1 rejection indicators per metric, in
    the same order, for the upper and lower tails at k = 200,000.
  - `sweep[ε][k]` gives the same indicators for the first k steps of each trajectory, for
    ε = 0.005, 0.003, 0.001 and k = 1,000 and 10,000 to 190,000.
- `../df_power_total.csv`: the input of `power_analysis.R`, with the same rows and layout as
  the original file, now in `../df_power_total_original.csv`. The counts are rejections out
  of 100 tests.
- `df_power_partial.csv`: the same counts for every chain and ε, plus the k sweep of chain 13
  (2012, Democratic, efficiency gap), with a column N for the number of tests.
- `power_by_chain.csv`: power (or Type I error, for the neutral chains) per chain and ε, with
  its binomial standard error, next to the original test's value on the same starting plans.
- `power_tables.txt`: the power tables of Section 5.2 of the paper. Each entry is the mean
  power over the chains of a metric, year or party, with the standard error of that mean.

All of these can be rebuilt from the summaries, except the comparison with the original test
in `power_by_chain.csv`, which needs the original tests' csv files. Write to another folder so
the committed files are not overwritten:

```
python scripts/report_power.py --summaries results/summaries --out check --like df_power_total_original.csv
python scripts/power_tables.py results/summaries
```
`check/df_power_total.csv` and `check/df_power_partial.csv` are then identical to
`df_power_total.csv` and `results/df_power_partial.csv`.
