"""Figures 4, 5 and 6 of the paper, from data/figure_metrics.npz (made by extract_chain_metrics.py).

Figure 4: efficiency gap along the 2016 chains biased for efficiency gap, with respect to
the favored party, against the number of Recom proposals. A hill climbing chain makes one
proposal per step. A short burst chain makes 4 proposals per burst and is drawn as one
horizontal segment per burst, from the smallest to the largest score among its 5 saved plans.

Figure 5: histograms of mean-median, with respect to the Republican vote share, of the 2016
chains biased for mean-median (hill climbing and short bursts) and of the neutral chain,
with the value of the enacted plan, from which all chains start. The legend is a separate file.

Figure 6: every pair of metrics for the 2016 short burst chains, computed with respect to
the Republican vote share, every 50th saved plan (histograms on the diagonal). Points are
rasterized; axes and text stay vector.

Run it like this, from the repository root:

python scripts/make_paper_figures.py --data data/figure_metrics.npz --out plots/paper
"""
import os
from argparse import ArgumentParser

import numpy as np
import matplotlib
matplotlib.use("pdf")
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from matplotlib.ticker import MaxNLocator

NAMES = {"efficiency_gap": "efficiency gap", "mean_median": "mean-median", "partisan_bias": "partisan bias",
         "partisan_gini": "partisan gini", "mean_thirdian": "mean-thirdian", "safe_seats": "safe seats"}


def figure4(d, columns, out):
    plt.rcParams.update({"font.size": 9, "axes.labelsize": 9, "xtick.labelsize": 8, "ytick.labelsize": 8})
    xticks = {"Democratic": [-0.05, 0.05, 0.15, 0.25], "Republican": [0.05, 0.15, 0.25, 0.35]}
    for party, short in (("Democratic", "Dem"), ("Republican", "Rep")):
        for method in ("hill", "shortburst"):
            eg = d[f"{method}_16_{party}_efficiency_gap_wrt{party}"][:, columns.index("efficiency_gap")]
            fig, ax = plt.subplots(figsize=(1.9, 2.9))
            if method == "hill":
                ax.plot(eg, np.arange(len(eg)) / 1000, color="tab:blue", alpha=0.6, lw=0.6)
            else:
                bursts = eg.reshape(-1, 5)
                y = 4 * np.arange(len(bursts)) / 1000
                segs = np.stack([np.c_[bursts.min(1), y], np.c_[bursts.max(1), y]], axis=1)
                ax.add_collection(LineCollection(segs, colors="tab:blue", alpha=0.5, lw=0.3))
                ax.set_xlim(bursts.min() - 0.02, bursts.max() + 0.02)
            ax.set_ylim(-2, 52)
            ax.set_yticks(range(0, 51, 10))
            if method == "hill":
                ax.set_xticks(xticks[party])
            else:
                ax.xaxis.set_major_locator(MaxNLocator(4))
            ax.set_xlabel("efficiency gap")
            ax.set_ylabel("Recom proposals (thousands)")
            fig.tight_layout(pad=0.3)
            fig.savefig(os.path.join(out, f"NC_eg_{short}_{method}_proposals.pdf"))
            plt.close(fig)


def figure5(d, columns, out):
    plt.rcParams.update({"font.size": 7, "axes.labelsize": 7, "xtick.labelsize": 7, "ytick.labelsize": 7})
    mm = columns.index("mean_median")
    neutral = d["unbiased_16_wrtRepublican"][:, mm]
    for method, xticks in (("hill", [-0.04, 0, 0.04, 0.08]), ("shortburst", [-0.08, -0.04, 0, 0.04, 0.08])):
        rep = d[f"{method}_16_Republican_mean_median_wrtRepublican"][:, mm]
        dem = d[f"{method}_16_Democratic_mean_median_wrtRepublican"][:, mm]
        # as in make_hists.py: 200 bins over the range of all three chains
        kwargs = {"alpha": 0.3, "bins": 200, "histtype": "stepfilled", "lw": 0, "range": (min(rep.min(), dem.min(), neutral.min()),
                                                       max(rep.max(), dem.max(), neutral.max()))}
        fig, ax = plt.subplots(figsize=(2.58, 1.95))
        for v, col in ((rep, "r"), (dem, "b"), (neutral, "k")):
            ax.hist(v, color=col, weights=np.full(len(v), 1e-3), **kwargs)
        ax.axvline(neutral[0], color="k", lw=0.6)  # the enacted plan, where every chain starts
        ax.set_xticks(xticks)
        ax.set_xlabel("mean-median")
        ax.set_ylabel("number of plans (thousands)")
        fig.tight_layout(pad=0.3)
        fig.savefig(os.path.join(out, f"{method}-mean_median-Rep.pdf"))
        plt.close(fig)
    fig = plt.figure(figsize=(4.8, 0.2))
    handles = [Patch(color="r", alpha=0.3, label="Biased for Republicans"),
               Patch(color="b", alpha=0.3, label="Biased for Democrats"),
               Patch(color="k", alpha=0.3, label="Neutral ensemble $\\Sigma^0$"),
               Line2D([], [], color="k", lw=0.8, label="Enacted plan")]
    fig.legend(handles=handles, loc="center", ncol=4, frameon=False, fontsize=6.5,
               handlelength=1.5, columnspacing=1.2)
    fig.savefig(os.path.join(out, "mean_median_legend.pdf"), bbox_inches="tight", pad_inches=0.01)
    plt.close(fig)


def figure6(d, columns, out):
    plt.rcParams.update({"font.size": 8, "axes.labelsize": 8, "xtick.labelsize": 6, "ytick.labelsize": 6})
    biased = ["efficiency_gap", "mean_median", "partisan_bias", "partisan_gini", "safe_seats"]
    every = 50
    neutral = d["unbiased_16_wrtRepublican"]
    fig, axes = plt.subplots(len(columns), len(biased), figsize=(7.6, 4.6), sharex="col", layout="constrained")
    fig.get_layout_engine().set(w_pad=0.02, h_pad=0.02, wspace=0.03, hspace=0.03)
    for i, m1 in enumerate(biased):
        rep = d[f"shortburst_16_Republican_{m1}_wrtRepublican"]
        dem = d[f"shortburst_16_Democratic_{m1}_wrtRepublican"]
        c1 = columns.index(m1)
        for j, m2 in enumerate(columns):
            ax = axes[j, i]
            c2 = columns.index(m2)
            if m1 == m2:
                lo = min(neutral[:, c1].min(), rep[:, c1].min(), dem[:, c1].min())
                hi = max(neutral[:, c1].max(), rep[:, c1].max(), dem[:, c1].max())
                bins = np.arange(lo - 0.5, hi + 1) if m1 == "safe_seats" else np.linspace(lo, hi, 51)
                for v, col in ((neutral, "k"), (rep, "r"), (dem, "b")):
                    ax.hist(v[:, c1], bins=bins, color=col, alpha=0.3)
                ax.set_yticks([])
            else:
                for v, col in ((neutral, "k"), (rep, "r"), (dem, "b")):
                    ax.scatter(v[::every, c1], v[::every, c2], s=1.5, color=col, alpha=0.15, lw=0, rasterized=True)
                ax.yaxis.set_major_locator(MaxNLocator(3, integer=m2 == "safe_seats"))
            ax.xaxis.set_major_locator(MaxNLocator(3, integer=m1 == "safe_seats"))
            ax.tick_params(length=2, pad=1)
            if j == 0:
                ax.set_title(f"biased for {NAMES[m1]}", fontsize=8)
            if j == len(columns) - 1:
                ax.set_xlabel(NAMES[m1])
            if i == 0:
                ax.set_ylabel(NAMES[m2].replace(" ", "\n").replace("-", "-\n"), fontsize=7, linespacing=0.9)
    fig.savefig(os.path.join(out, "shortburst-Rep-scatter.pdf"), dpi=400)
    plt.close(fig)


def main(args):
    d = np.load(args.data)
    columns = list(d["columns"])
    os.makedirs(args.out, exist_ok=True)
    figure4(d, columns, args.out)
    figure5(d, columns, args.out)
    figure6(d, columns, args.out)
    print(f"wrote Figures 4-6 to {args.out}")


if __name__ == "__main__":
    parser = ArgumentParser(description="Figures 4-6 of the paper")
    parser.add_argument("--data", type=str, default="data/figure_metrics.npz")
    parser.add_argument("--out", type=str, default="plots/paper")
    main(parser.parse_args())
