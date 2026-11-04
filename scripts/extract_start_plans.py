"""Extract the starting plans of all tests from the chain files, once.

For each chain in the list of tests, load the chain file a single time and save
{map index: assignment as a plain dict} for the plans the tests start from, after
checking that every district of every plan is connected. Each test then loads this
small file (run_hp_from_scratch.py --plans) instead of the whole chain file, so it fits
in one core's memory.

Run it like this, from the repository root:

python scripts/extract_start_plans.py --tests jobs/all_tests.txt --out start_plans
"""
import os
import pickle
from argparse import ArgumentParser
from collections import defaultdict

import networkx as nx

import utils


def main(args):
    graph = utils.read_geodata(args.shp)
    wanted = defaultdict(set)
    with open(args.tests) as f:
        for line in f:
            fn, idx, _ = line.split()
            wanted[fn].add(int(idx))

    os.makedirs(args.out, exist_ok=True)
    for fn, idxs in sorted(wanted.items()):
        with open(fn, "rb") as f:
            all_assignments = pickle.load(f)[0]
        plans = {}
        for i in sorted(idxs):
            mapping = dict(all_assignments[i].mapping)  # node -> district
            parts = defaultdict(list)
            for v, d in mapping.items():
                parts[d].append(v)
            split = [d for d, nodes in parts.items() if not nx.is_connected(graph.subgraph(nodes))]
            assert not split, f"{fn} map {i}: split districts {split}"
            plans[i] = mapping
        out = os.path.join(args.out, os.path.basename(fn))
        with open(out, "wb") as f:
            pickle.dump(plans, f, pickle.HIGHEST_PROTOCOL)
        print(f"{fn}: {len(plans)} starting plans, all districts connected -> {out}", flush=True)
        del all_assignments


if __name__ == "__main__":
    parser = ArgumentParser(description="extract the starting plans of the tests")
    parser.add_argument("--tests", type=str, default="jobs/all_tests.txt",
        help="list of tests, one '<chain file> <map index> <original result>' per line")
    parser.add_argument("--out", type=str, default="start_plans", help="output folder")
    parser.add_argument("--shp", type=str, default="./data/NC/NC.shp", help="state shapefile")
    main(parser.parse_args())
