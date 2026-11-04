"""Power and Type I error of the outlier test, from the per-test summaries.

run_hp_from_scratch.py --summary writes one small json per test. This script reads them
(it can be run at any time, also while tests are still running) and reports, for each
biased chain, the number of finished tests and the power (share of tests rejecting in the
upper tail for the metric the chain was biased for) at each epsilon, with its standard
error. For the two neutral chains it reports the Type I error rate (share of tests
rejecting in either tail) per metric. With --original, it also reports the power of the
original test on the same starting plans, from the csv files of read_hp_results.py.

Writes to --out:
  power_by_chain.csv     one row per chain (and metric, for the neutral chains) and epsilon
  df_power_partial.csv   the format of df_power_total.csv plus a column N (finished
                         tests); divide the counts by N, not 100.
With --like, it also writes df_power_total.csv with the rows, in the same order, of the
given csv (for example the original df_power_total.csv).

Run it like this, from the repository root:

python scripts/report_power.py --summaries results/summaries --out results
"""
import csv
import glob
import json
import math
import os
import re
from argparse import ArgumentParser
from collections import defaultdict

from aggregate_power import INDEX, METRICS

EPS = ["0.005", "0.003", "0.001", "0.0005", "0.0001", "5e-05", "9e-06"]
METRIC_POS = {"efficiency_gap": 0, "mean_median": 1, "partisan_bias": 2,
              "partisan_gini": 3, "safe_seats": 5}
CHAIN = re.compile(r"shortburst_PRES(12|16)_(Democratic|Republican)_([a-z_]+)_10000_\d+$")


def load_summaries(folder):
    """chain name -> list of test summaries."""
    summaries = defaultdict(list)
    for path in glob.glob(os.path.join(folder, "task_*.json")):
        with open(path) as f:
            s = json.load(f)
        summaries[os.path.basename(s["fn"])[:-4]].append(s)
    return summaries


def original_outcomes(original, name, eps):
    """map index -> (upper, lower) rejection indicators of the original test."""
    if original is None:
        return {}
    sub = "unbiased_chains" if name.startswith("unbiased") else "biased_chains"
    prefix = re.sub(r"_(10000|50000)(_\d+)?$", "", name)
    out = {}
    for path in glob.glob(f"{original}/{sub}/NC/{glob.escape(prefix)}[*]_{eps}_0.05.csv"):
        with open(path, encoding="utf-8") as f:
            for row in list(csv.reader(f))[2:]:
                if row and row[0] not in ("", "total"):
                    out[int(row[0])] = ([int(float(x)) for x in row[8:14]],
                                        [int(float(x)) for x in row[22:28]])
    return out


def se(p, n):
    return math.sqrt(p * (1 - p) / n) if n else float("nan")


def main(args):
    summaries = load_summaries(args.summaries)
    os.makedirs(args.out, exist_ok=True)
    rows, df_rows = [], []
    total = sum(len(v) for v in summaries.values())
    print(f"{total} finished tests over {len(summaries)} chains\n")

    for name in sorted(summaries):
        tests = summaries[name]
        n = len(tests)
        match = CHAIN.match(name)
        for eps in EPS:
            orig = original_outcomes(args.original, name, eps)
            if match:  # biased chain: power for its own metric, upper tail
                year, party, metric = match.groups()
                j = METRIC_POS[metric]
                new = sum(t["full"][str(float(eps))]["upper"][j] for t in tests)
                paired = [orig[t["map_idx"]][0][j] for t in tests if t["map_idx"] in orig]
                rows.append({"chain": f"{year} {party} {metric}", "epsilon": eps, "n": n,
                             "power": new / n, "se": se(new / n, n),
                             "original_power_same_plans": sum(paired) / len(paired) if paired else "",
                             "n_paired": len(paired)})
                counts = [sum(t["full"][str(float(eps))]["upper"][i] for t in tests) for i in range(6)]
                df_rows.append(counts + [f"{float(eps):.6f}".rstrip("0"), year, metric.replace("_", "."),
                                         "Democrat" if party == "Democratic" else party,
                                         INDEX[(year, party, metric)], 200000, n])
                if INDEX[(year, party, metric)] == 13 and str(float(eps)) in tests[0]["sweep"]:
                    for k in tests[0]["sweep"][str(float(eps))]:
                        counts = [sum(t["sweep"][str(float(eps))][k]["upper"][i] for t in tests) for i in range(6)]
                        df_rows.append(counts + [f"{float(eps):.6f}".rstrip("0"), year, metric.replace("_", "."),
                                                 "Democrat" if party == "Democratic" else party, 13, int(k), n])
            else:  # neutral chain: Type I error per metric, either tail
                for i, metric in enumerate(METRICS):
                    up = sum(t["full"][str(float(eps))]["upper"][i] for t in tests)
                    lo = sum(t["full"][str(float(eps))]["lower"][i] for t in tests)
                    paired = [max(orig[t["map_idx"]][0][i], orig[t["map_idx"]][1][i])
                              for t in tests if t["map_idx"] in orig]
                    rows.append({"chain": f"{name} (Type I, {metric})", "epsilon": eps, "n": n,
                                 "power": (up + lo) / n, "se": se((up + lo) / n, n),
                                 "original_power_same_plans": sum(paired) / len(paired) if paired else "",
                                 "n_paired": len(paired)})

    with open(os.path.join(args.out, "power_by_chain.csv"), "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    header = METRICS + ["Epsilon", "PRES", "Metric", "Party", "Index", "K"]
    with open(os.path.join(args.out, "df_power_partial.csv"), "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(header + ["N"])
        writer.writerows(df_rows)
    if args.like is not None:
        # same rows and order as the given csv, counts from this run
        counts = {tuple(str(x) for x in r[6:12]): r[:6] for r in df_rows}
        with open(args.like, newline="") as f:
            template = list(csv.reader(f))[1:]
        with open(os.path.join(args.out, "df_power_total.csv"), "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow(header)
            writer.writerows(counts[tuple(r[6:12])] + r[6:12] for r in template)

    print(f"{'chain':42s} {'n':>4s}  {'power (eps=0.001)':>18s}  {'original, same plans':>21s}")
    for r in rows:
        if r["epsilon"] == "0.001" and "Type I" not in r["chain"]:
            orig = r["original_power_same_plans"]
            print(f"{r['chain']:42s} {r['n']:4d}  {r['power']:10.2f} ± {r['se']:.2f}  "
                  f"{orig if orig == '' else f'{orig:21.2f}'}")
    print("\nType I error rate at eps=0.001 (neutral chains, either tail):")
    for r in rows:
        if r["epsilon"] == "0.001" and "Type I" in r["chain"]:
            print(f"  {r['chain']:60s} {r['power']:.3f}  (n={r['n']})")
    print(f"\nsaved to {args.out}")


if __name__ == "__main__":
    parser = ArgumentParser(description="power and Type I error from the per-test summaries")
    parser.add_argument("--summaries", type=str, default="results/summaries",
        help="folder with the task_*.json files written by run_hp_from_scratch.py --summary")
    parser.add_argument("--original", type=str, default=None,
        help="folder with the read_hp_results.py csv files of the original test, laid out as "
             "{biased,unbiased}_chains/NC/, to compare on the same starting plans")
    parser.add_argument("--like", type=str, default=None,
        help="csv in the format of df_power_total.csv whose rows to reproduce in df_power_total.csv")
    parser.add_argument("--out", type=str, default="results", help="output folder")
    main(parser.parse_args())
