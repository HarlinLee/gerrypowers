"""Per-plan metrics of the North Carolina biased chains, for Figures 4-6 and Section 4.2 of the paper.

Reads the *-metrics.pkl files written by calculate_metrics.py next to each chain in
{root}/{biased,unbiased}_chains/NC. Their rows follow the saved
plans of the chain, and their columns are efficiency gap, mean-median, partisan bias,
partisan gini and mean-thirdian (computed with respect to a party), plus a separate list
of safe seats. A hill climbing chain saves one plan per step (50,000 steps). A short burst
chain saves 5 plans per burst (its starting plan and 4 Recom proposals), so plan k was
saved after 4 * (k // 5) + k % 5 Recom proposals.

Writes the metrics of all chains to --out, and the part that the figures use to
--figure_data (data/figure_metrics.npz, small enough to keep in the repository). Prints
the numbers quoted in Section 4.2.

Run it like this, from the repository root (or with --root pointing to the folder that
holds biased_chains/ and unbiased_chains/):

python scripts/extract_chain_metrics.py --out chain_metrics.npz --figure_data data/figure_metrics.npz

With --from_npz chain_metrics.npz it only writes --figure_data from an existing file.
"""
import glob
import os
import pickle
from argparse import ArgumentParser

import numpy as np

METRICS = ["efficiency_gap", "mean_median", "partisan_bias", "partisan_gini", "safe_seats"]
COLUMNS = ["efficiency_gap", "mean_median", "partisan_bias", "partisan_gini", "mean_thirdian", "safe_seats"]
OTHER = {"Democratic": "Republican", "Republican": "Democratic"}


def load(path):
    with open(path, "rb") as f:
        values, safe_seats, _, _, _ = pickle.load(f)
    return np.hstack([np.asarray(values, dtype=float), np.asarray(safe_seats, dtype=float)[:, None]])


def chain_metrics(root, method, year, party, metric, wrt):
    """Metrics of one chain with respect to party wrt (rows = saved plans, columns = COLUMNS)."""
    n = 50000 if method == "hill" else 10000
    suffix = "-metrics.pkl" if wrt == party else f"-{wrt}-metrics.pkl"
    paths = glob.glob(f"{root}/biased_chains/NC/{method}_PRES{year}_{party}_{metric}_{n}_*[0-9]{suffix}")
    assert len(paths) == 1, (method, year, party, metric, wrt, paths)
    return load(paths[0]), os.path.basename(paths[0])


def proposals(method, n_plans):
    k = np.arange(n_plans)
    return k if method == "hill" else 4 * (k // 5) + k % 5


# arrays of chain_metrics.npz used by make_paper_figures.py
FIGURE_KEYS = (
    [f"{m}_16_{p}_efficiency_gap_wrt{p}" for m in ("hill", "shortburst") for p in ("Democratic", "Republican")]
    + [f"hill_16_{p}_mean_median_wrtRepublican" for p in ("Republican", "Democratic")]
    + [f"shortburst_16_{p}_{m}_wrtRepublican" for p in ("Republican", "Democratic") for m in METRICS
       if not (m == "efficiency_gap" and p == "Republican")]
    + ["unbiased_16_wrtRepublican"]
)


def extract(root):
    out = {}
    files = []
    for method in ("hill", "shortburst"):
        for year in ("12", "16"):
            for party in ("Democratic", "Republican"):
                for metric in METRICS:
                    for wrt in ("Democratic", "Republican"):
                        m, fn = chain_metrics(root, method, year, party, metric, wrt)
                        out[f"{method}_{year}_{party}_{metric}_wrt{wrt}"] = m.astype(np.float32)
                        files.append(fn)
    for year in ("12", "16"):
        out[f"unbiased_{year}_wrtRepublican"] = load(
            f"{root}/unbiased_chains/NC/unbiased_PRES{year}_50000-metrics.pkl").astype(np.float32)
    print(f"read {len(files)} biased metrics files")
    return out


def report(out):
    # Section 4.2: extremes of each chain for the metric it was biased for, with respect to the favored party
    print("\nmost extreme value of the biased metric (w.r.t. the favored party) and Recom proposals to first reach it")
    for year in ("12", "16"):
        for metric in METRICS:
            for party in ("Republican", "Democratic"):
                row = []
                for method in ("hill", "shortburst"):
                    v = out[f"{method}_{year}_{party}_{metric}_wrt{party}"][:, COLUMNS.index(metric)]
                    best = v.max()
                    first = proposals(method, len(v))[np.flatnonzero(v >= best - 1e-9)[0]]
                    row.append(f"{method:10s} {best:8.4f} at {first:6d}")
                print(f"PRES{year} {metric:15s} {party:10s} | " + " | ".join(row))

    # Figure 4 and the plateau paragraph: efficiency gap chains of 2016
    print("\n2016 efficiency gap chains (w.r.t. the favored party)")
    for party in ("Democratic", "Republican"):
        for method in ("hill", "shortburst"):
            v = out[f"{method}_16_{party}_efficiency_gap_wrt{party}"][:, 0]
            p = proposals(method, len(v))
            cross = {lev: int(p[np.flatnonzero(v >= lev)[0]]) for lev in (0.15, 0.2, 0.23, 0.24, 0.25, 0.3, 0.35) if (v >= lev).any()}
            print(f"{party:10s} {method:10s} start {v[0]:.4f} max {v.max():.4f} first reached: "
                  + ", ".join(f"{k} at {c}" for k, c in cross.items()))
            if method == "hill":
                # plateaus: median efficiency gap between consecutive jumps of more than 0.03
                jumps = np.flatnonzero(np.abs(np.diff(v)) > 0.03) + 1
                edges = [0, *jumps, len(v)]
                for a, b in zip(edges[:-1], edges[1:]):
                    if b - a > 500:
                        print(f"    steps {a:6d}-{b:6d}: median efficiency gap {np.median(v[a:b]):.4f}")


def main(args):
    if args.from_npz is not None:
        d = np.load(args.from_npz)
        out = {k: d[k] for k in d.files if k != "columns"}
    else:
        out = extract(args.root)
        np.savez_compressed(args.out, columns=np.array(COLUMNS), **out)
        print(f"wrote {args.out}")
        report(out)
    if args.figure_data is not None:
        np.savez_compressed(args.figure_data, columns=np.array(COLUMNS), **{k: out[k] for k in FIGURE_KEYS})
        print(f"wrote {args.figure_data}")


if __name__ == "__main__":
    parser = ArgumentParser(description="per-plan metrics of the NC chains, for the paper's figures")
    parser.add_argument("--root", type=str, default=".",
        help="folder that holds biased_chains/ and unbiased_chains/")
    parser.add_argument("--out", type=str, default="chain_metrics.npz", help="metrics of all chains")
    parser.add_argument("--figure_data", type=str, default=None,
        help="also write the arrays the figures use to this file")
    parser.add_argument("--from_npz", type=str, default=None,
        help="read the metrics of all chains from this file instead of the chain folders")
    main(parser.parse_args())
