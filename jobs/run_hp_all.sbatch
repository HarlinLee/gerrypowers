#!/bin/bash -l
#SBATCH --job-name=hp
#SBATCH --time=4:00:00
#SBATCH --mem=3800M
#SBATCH --output=logs/hp_%A_%a.out
# Add the --account and --partition lines your cluster needs.

# Re-run of the outlier test: reversible flip proposal with the contiguity constraint,
# 32 trajectories of 200,000 steps from the starting plan of one original test. Line N of
# jobs/all_tests.txt is "<chain file> <map index> <original result>". The lines go
# round-robin over the 22 chains, so any prefix of the run is balanced. The starting plan
# is read from start_plans/ (made by scripts/extract_start_plans.py), which keeps memory
# under 4 GB, i.e. one core. Each finished test also writes a small summary to
# summaries/task_N.json, so partial results can be reported at any time.
#
# Submit from the repository root:
#   mkdir -p logs summaries hp/biased_chains/NC hp/unbiased_chains/NC
#   sbatch --array=1-2200%100 jobs/run_hp_all.sbatch
read FN IDX ORIG < <(sed -n "${SLURM_ARRAY_TASK_ID}p" jobs/all_tests.txt)
if [ -z "$IDX" ]; then echo "no test on line $SLURM_ARRAY_TASK_ID"; exit 1; fi
SEED=$((100000 + SLURM_ARRAY_TASK_ID))

# an environment with the packages in requirements.txt, see the README
source .venv/bin/activate
export PYTHONHASHSEED=0
echo "task $SLURM_ARRAY_TASK_ID: fn $FN map_idx $IDX seed $SEED (original test: $ORIG)"
echo "code $(git rev-parse --short HEAD) + uncommitted: $(git status --short scripts | tr '\n' ' ')"

/usr/bin/time -v python -u scripts/run_hp_from_scratch.py --k 200000 --m 32 --map_idx "$IDX" \
    --fn "$FN" --plans "start_plans/$(basename "$FN")" --proposal reversible \
    --seed "$SEED" --save_percents 0 --summary "summaries/task_${SLURM_ARRAY_TASK_ID}.json"
