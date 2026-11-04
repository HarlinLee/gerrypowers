"""
Build df_power_total.csv, the input of power_analysis.R, from the CSVs written by
read_hp_results.py.

Each output row is one biased chain, one epsilon and one k. The six metric columns hold
the number of tested maps (out of 100) on which the test rejects in the upper tail, i.e.
the "p-value upper" counts on the "total" line of the corresponding CSV.

Run it like this, from the repository root:

python scripts/aggregate_power.py --dir hp/biased_chains/NC --out df_power_total.csv
"""
import csv
import glob
import os
import re
from argparse import ArgumentParser

METRICS = ['efficiency gap', 'mean median', 'partisan bias', 'partisan gini', 'mean thirdian', 'safe seats']

# chain index used in the original df_power_total.csv: (year, party, metric) -> index
INDEX = {
    ('16', 'Republican', 'efficiency_gap'): 1,
    ('16', 'Republican', 'mean_median'): 2,
    ('16', 'Republican', 'partisan_bias'): 3,
    ('12', 'Republican', 'efficiency_gap'): 4,
    ('12', 'Republican', 'mean_median'): 5,
    ('12', 'Republican', 'partisan_bias'): 6,
    ('12', 'Republican', 'partisan_gini'): 7,
    ('16', 'Republican', 'partisan_gini'): 8,
    ('16', 'Republican', 'safe_seats'): 9,
    ('12', 'Republican', 'safe_seats'): 10,
    ('16', 'Democratic', 'efficiency_gap'): 11,
    ('16', 'Democratic', 'mean_median'): 12,
    ('12', 'Democratic', 'efficiency_gap'): 13,
    ('12', 'Democratic', 'mean_median'): 14,
    ('12', 'Democratic', 'partisan_bias'): 15,
    ('12', 'Democratic', 'partisan_gini'): 16,
    ('16', 'Democratic', 'partisan_gini'): 17,
    ('16', 'Democratic', 'partisan_bias'): 18,
    ('16', 'Democratic', 'safe_seats'): 19,
    ('12', 'Democratic', 'safe_seats'): 20,
}

# e.g. shortburst_PRES12_Democratic_efficiency_gap*_0.001_0.05_10000.csv
# (the * is part of the name; no k suffix means the full chain)
NAME = re.compile(r'^shortburst_PRES(12|16)_(Democratic|Republican)_([a-z_]+)\*_'
                  r'([0-9.e-]+)_([0-9.]+)(?:_(\d+))?\.csv$')


def upper_counts(path):
    with open(path, encoding='utf-8') as f:
        rows = [r for r in csv.reader(f) if r]
    total = rows[-1]
    assert total[0] == 'total', f"no total line in {path}"
    return [int(float(x)) for x in total[8:8 + len(METRICS)]]


def main(args):
    out = []
    for path in sorted(glob.glob(os.path.join(args.dir, '*.csv'))):
        match = NAME.match(os.path.basename(path))
        if match is None:
            continue
        year, party, metric, eps, alpha, k = match.groups()
        if float(alpha) != args.a:
            continue
        out.append(upper_counts(path) + [
            f"{float(eps):.6f}".rstrip('0'),
            year,
            metric.replace('_', '.'),
            'Democrat' if party == 'Democratic' else party,
            INDEX[(year, party, metric)],
            int(k) if k else args.k,
        ])

    out.sort(key=lambda r: (r[10], r[11], float(r[6])))
    with open(args.out, 'w', newline='', encoding='utf-8') as f:
        writer = csv.writer(f)
        writer.writerow(METRICS + ['Epsilon', 'PRES', 'Metric', 'Party', 'Index', 'K'])
        writer.writerows(out)
    print(len(out), 'rows saved to', args.out)


if __name__ == "__main__":
    parser = ArgumentParser(description="aggregate hypothesis test results for power_analysis.R")
    parser.add_argument("--dir", type=str, default="hp/biased_chains/NC",
        help="folder with the csv files from read_hp_results.py")
    parser.add_argument("--a", type=float, default=0.05, help="alpha")
    parser.add_argument("--k", type=int, default=200000,
        help="k to record for csv files without a k suffix (full chain)")
    parser.add_argument("--out", type=str, default="df_power_total.csv")
    args = parser.parse_args()

    print(args)
    main(args)
