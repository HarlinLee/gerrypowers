"""Power tables of Section 5.2 of the paper, from all tested maps and from the second half of each
biased ensemble.

Each biased ensemble is a short-burst chain of 50,000 saved plans, and its 100 tested maps
were drawn uniformly from all of them. For each ensemble this computes the power at
eps = 0.001 and k = 200,000 (corrected test, upper tail, for the metric the ensemble was
biased for), using all its tested maps or only those with map index >= 25,000. For the
ensembles of each metric, year and party it reports the mean of these powers, the standard
error of that mean, and the standard deviation of the powers between ensembles. The number
of flagged maps in an ensemble is binomial, so its power p_i has variance p_i(1 - p_i)/n_i,
and the mean of K ensembles has standard error sqrt(sum_i p_i(1 - p_i)/n_i) / K. The paper's
tables use all tested maps and this standard error. It also checks that the second-half
maps are more biased than the first-half ones.

Run it like this, from the repository root:

python scripts/power_tables.py results/summaries
"""
import glob
import json
import math
import os
import re
import statistics as st
import sys
from collections import defaultdict

SUMMARIES = sys.argv[1] if len(sys.argv) > 1 else "results/summaries"
CUT = 25000
EPS = "0.001"
POS = {"efficiency_gap": 0, "mean_median": 1, "partisan_bias": 2, "partisan_gini": 3, "safe_seats": 5}
CHAIN = re.compile(r"shortburst_PRES(\d+)_(\w+?)_(efficiency_gap|mean_median|partisan_bias|partisan_gini|safe_seats)_")

outcomes = {"all": defaultdict(list), "second half": defaultdict(list)}
labels = defaultdict(lambda: ([], []))
for path in glob.glob(os.path.join(SUMMARIES, "task_*.json")):
    with open(path) as f:
        s = json.load(f)
    m = CHAIN.match(os.path.basename(s["fn"]))
    if not m:
        continue
    key = m.groups()
    j = POS[key[2]]
    late = s["map_idx"] >= CUT
    labels[key][late].append(s["w0"][j])
    outcomes["all"][key].append(s["full"][EPS]["upper"][j])
    if late:
        outcomes["second half"][key].append(s["full"][EPS]["upper"][j])

more_biased = sum(st.mean(late) > st.mean(early) for early, late in labels.values())
print(f"ensembles whose late maps have a larger mean label than their early maps: {more_biased} of {len(labels)}")
for subset, res in outcomes.items():
    power = {key: sum(v) / len(v) for key, v in res.items()}
    print(f"\n== {subset}: {len(power)} ensembles, {min(map(len, res.values()))}-{max(map(len, res.values()))} tested maps each ==")
    for name, i in (("Metric", 2), ("Year", 0), ("Party", 1)):
        groups = defaultdict(list)
        for key, p in power.items():
            groups[key[i]].append((p, len(res[key])))
        for g, v in sorted(groups.items()):
            ps = [p for p, _ in v]
            se = math.sqrt(sum(p * (1 - p) / n for p, n in v)) / len(v)
            print(f"{name:6s} {g:15s} mean {st.mean(ps):.4f}  se {se:.4f}  sd {st.stdev(ps):.3f}  ({len(v)} ensembles)")
