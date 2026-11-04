"""Type I errors by tail, and a paired comparison of power with the original test.

For each biased chain, compares the corrected test with the original test on the same
starting plans at epsilon = 0.001: the number of plans rejected only by the original,
only by the corrected test, and an exact two-sided McNemar (sign) test on those.

Run it like this, from the repository root:

python scripts/paired_check.py --summaries results/summaries --original <folder>

where <folder> holds the read_hp_results.py csv files of the original test, laid out as
{biased,unbiased}_chains/NC/.
"""
import math
from argparse import ArgumentParser

from report_power import CHAIN, METRIC_POS, load_summaries, original_outcomes


def main(args):
    S = load_summaries(args.summaries)

    print("Type I at eps=0.001, rejections per metric, out of the finished tests")
    for name in sorted(S):
        if name.startswith("unbiased"):
            up = [sum(t["full"]["0.001"]["upper"][i] for t in S[name]) for i in range(6)]
            lo = [sum(t["full"]["0.001"]["lower"][i] for t in S[name]) for i in range(6)]
            print(f"  {name}: upper tail {up}, lower tail {lo}")

    print("\nPaired comparison at eps=0.001 (same starting plans)")
    print(f"{'chain':36s} {'new':>4s} {'orig':>5s} {'only orig':>10s} {'only new':>9s} {'p (McNemar)':>12s}")
    for name in sorted(S):
        m = CHAIN.match(name)
        if not m:
            continue
        j = METRIC_POS[m.group(3)]
        orig = original_outcomes(args.original, name, "0.001")
        pairs = [(t["full"]["0.001"]["upper"][j], orig[t["map_idx"]][0][j])
                 for t in S[name] if t["map_idx"] in orig]
        only_orig = sum(1 for n, o in pairs if o and not n)
        only_new = sum(1 for n, o in pairs if n and not o)
        d = only_orig + only_new
        k = min(only_orig, only_new)
        p = min(1.0, 2 * sum(math.comb(d, i) for i in range(k + 1)) / 2 ** d) if d else 1.0
        label = f"{m.group(1)} {m.group(2)} {m.group(3)}"
        print(f"{label:36s} {sum(n for n, o in pairs):4d} {sum(o for n, o in pairs):5d} "
              f"{only_orig:10d} {only_new:9d} {p:12.3f}")


if __name__ == "__main__":
    parser = ArgumentParser(description="paired comparison with the original test")
    parser.add_argument("--summaries", type=str, default="results/summaries",
        help="folder with the task_*.json files written by run_hp_from_scratch.py --summary")
    parser.add_argument("--original", type=str, required=True,
        help="folder with the read_hp_results.py csv files of the original test")
    main(parser.parse_args())
