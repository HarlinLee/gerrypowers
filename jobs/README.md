# Re-running the outlier test

These are the steps used for the re-run of all 2,200 tests with the corrected outlier test
(reversible flip proposal with the contiguity constraint, see `scripts/run_hp_from_scratch.py`).
Run everything from the repository root.

1. **The list of tests.** `jobs/all_tests.txt` has one test per line:
   `<chain file> <map index> <original result>`. It covers the 100 starting plans of each of
   the 20 short-burst chains and the 2 neutral chains, the same plans as the original tests,
   so new and old results can be compared plan by plan. The lines go round-robin over the 22
   chains, so any prefix of the list is balanced.
2. **Extract the starting plans** once, so that each test loads a small file instead of the
   whole chain file. This also checks that every district of every starting plan is connected.

   ```
   python scripts/extract_start_plans.py --tests jobs/all_tests.txt --out start_plans
   ```
3. **Run the tests** on a Slurm cluster, one test per array task, with seed
   100000 + task number and `PYTHONHASHSEED=0`. Each test took about 1.7 core-hours.

   ```
   mkdir -p logs summaries hp/biased_chains/NC hp/unbiased_chains/NC
   sbatch --array=1-2200%100 jobs/run_hp_all.sbatch
   ```
   Each test saves its trajectories' metric values in `hp/` and a small summary of its
   outcomes in `summaries/task_N.json`.
4. **Report power and Type I error** from the summaries, at any time.

   ```
   python scripts/report_power.py --summaries summaries --out results --like df_power_total.csv
   python scripts/power_tables.py summaries
   ```
   `report_power.py` writes `power_by_chain.csv` and `df_power_partial.csv`. With
   `--like df_power_total.csv` it also writes `results/df_power_total.csv` with the same rows,
   in the same order, as the given file, which is the input of `power_analysis.R`.
   `power_tables.py` prints the power tables of Section 5.2 of the paper, with standard errors.
5. **Compare with the original test** on the same starting plans, given the `read_hp_results.py`
   csv files of the original tests (laid out as `{biased,unbiased}_chains/NC/`):

   ```
   python scripts/report_power.py --summaries summaries --out results --original <folder>
   python scripts/paired_check.py --summaries summaries --original <folder>
   ```

`scripts/aggregate_power.py` builds `df_power_total.csv` the other way, from the csv files of
`read_hp_results.py` rather than from the summaries.
