#!/usr/bin/env python3
"""Reproduces every figure quoted in "Are AFL Grand Finals just another game, or
a different kettle of fish?" (The Conversation, 2026), in the order they appear
in the piece. Reads afl_grand_final_data.xlsx in the same folder.

Needs: pandas, openpyxl, statsmodels
    pip install pandas openpyxl statsmodels
Run (from anywhere):
    python code/basic_stats.py
"""
import os
import pandas as pd
import statsmodels.formula.api as smf

# the data file lives in ../data relative to this script, regardless of where
# you run it from
DATA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "data",
                     "afl_grand_final_data.xlsx")
ORDER = ["Home & Away", "Elimination Final", "Qualifying Final",
         "Semi Final", "Preliminary Final", "Grand Final"]
FINALS = ORDER[1:]

modern = pd.read_excel(DATA, sheet_name="2012-2025 matches")
modern["round_type"] = pd.Categorical(modern.round_type, ORDER, ordered=True)
modern["cp_rate"] = 100 * modern.contestedPossessions / modern.totalPossessions
modern["uncontestedMarks"] = modern.marks - modern.contestedMarks

old = pd.read_excel(DATA, sheet_name="2000-2011 matches")
old["is_final"] = (old.round_type != "Home & Away").astype(int)

quarters = pd.read_excel(DATA, sheet_name="Quarter scores 2000-2025")
quarters["round_type"] = pd.Categorical(quarters.round_type, ORDER, ordered=True)


def section(title):
    print(f"\n{'=' * 78}\n{title}\n{'=' * 78}")


# ------------------------------------------------------------------------
section("1. What actually changes in a final (2012-2025, all matches)")
# "Teams average 347 disposals in a Grand Final against 361 in a home-and-away game"
M = ["disposals", "kicks", "handballs", "uncontestedPossessions", "cp_rate",
     "marks", "uncontestedMarks", "contestedMarks", "disposalEfficiency"]
print(modern.groupby("round_type", observed=True)[M].mean().round(1).to_string())
print("\nArticle quotes Grand Final vs Home & Away specifically:")
print(modern.groupby("round_type", observed=True)[M].mean().round(1)
      .loc[["Home & Away", "Grand Final"]].to_string())


# ------------------------------------------------------------------------
section("2. Which final is the hardest (2012-2025)")
CONTACT = ["tackles", "contestedPossessions"]
t = modern.groupby("round_type", observed=True)[CONTACT + ["score"]].mean().round(1)
print(t.to_string())
print("\n'Rank each September's nine finals': Grand Final's rank on contested "
      "possessions, one season at a time (1 = most contested final that year)")
f = modern[modern.is_final == 1]
g = (f.groupby(["season", "match_id", "round_type"], observed=True)
       .contestedPossessions.sum().reset_index())
ranks = []
for s, sub in g.groupby("season"):
    sub = sub.sort_values("contestedPossessions", ascending=False).reset_index(drop=True)
    if "Grand Final" not in sub.round_type.values:
        continue
    gi = sub.index[sub.round_type == "Grand Final"][0]
    ranks.append({"season": s, "gf_rank": gi + 1, "n_finals": len(sub)})
r = pd.DataFrame(ranks)
print(r.to_string(index=False))
print(f"\nSeasons where GF ranked 1st on contested possessions since 2011: "
      f"{r[(r.season > 2011) & (r.gf_rank == 1)].season.tolist() or 'none'}")


# ------------------------------------------------------------------------
section("3. Grand Finals did not always look like this")
# The article's method: rank the 9 (or 10, in 2010) finals of a season on a
# combined-team total, and compare the Grand Final with the MEDIAN of the rest.
# The 2010 Grand Final was drawn and replayed - both games count as finals that
# season, and the drawn game (not an average of the two) is "the Grand Final".
both = pd.concat([
    old[["season", "round_type", "is_final", "team", "opponent",
         "tackles", "contestedPossessions", "match_url"]].assign(gid=old.match_url),
    modern[["season", "round_type", "is_final", "team", "opponent",
            "tackles", "contestedPossessions", "match_id"]].rename(
                columns={"match_id": "gid"})
], ignore_index=True)
finals = both[both.is_final == 1]
combined = (finals.groupby(["season", "gid", "round_type"], observed=True)
                   [["tackles", "contestedPossessions"]].sum().reset_index())

for metric in ("tackles", "contestedPossessions"):
    rows = []
    for s, sub in combined.groupby("season"):
        gfs = sub[sub.round_type == "Grand Final"].sort_values("gid")
        if gfs.empty or len(sub) < 9:
            continue
        gf = gfs.iloc[0]  # the game played on the day; the 2010 replay is left in "rest"
        rest = sub.drop(gf.name)
        rows.append({"season": s, "grand_final": gf[metric],
                     "other_finals_median": rest[metric].median(),
                     "gap_pct": round(100 * (gf[metric] - rest[metric].median())
                                      / rest[metric].median(), 1)})
    d = pd.DataFrame(rows)
    print(f"\n--- {metric}: Grand Final vs the median of its own season's other finals ---")
    print(d[d.season.isin([2011, 2015])].to_string(index=False))
    d["block"] = d.season.map(lambda y: f"{2000+((y-2000)//2*2)}-{2000+((y-2000)//2*2)+1}")
    print("\ntwo-year blocks:")
    print(d.groupby("block").gap_pct.mean().round(1).to_string())


# ------------------------------------------------------------------------
section("4. Are they close games? (all 5,111 games, 2000-2025)")
# one row per game (quarters carries a row per team; a game's two rows are
# mirror images of the same margin, so this avoids any double-counting)
q = quarters.drop_duplicates("game_code").copy()
for k in range(1, 5):
    q[f"gap{k}"] = q[f"lead_after_q{k}"].abs()

print("average margin at each break, EVERY round type (one row per game):")
by_round = q.groupby("round_type", observed=True)[["gap1", "gap2", "gap3", "gap4"]].mean().round(1)
by_round.columns = ["Quarter time", "Half time", "Three-qtr time", "Full time"]
by_round["pct_within_2_goals"] = (100 * q.groupby("round_type", observed=True)
                                   .gap4.apply(lambda x: (x <= 12).mean())).round(1)
print(by_round.to_string())
gf = q[q.round_type == "Grand Final"]
print(f"\nGrand Final median full-time margin: {gf.gap4.median():.1f} points "
      f"(mean {gf.gap4.mean():.1f}, n={len(gf)})")

q["group"] = q.round_type.map(
    lambda r: "Grand Final" if r == "Grand Final"
    else ("Home & Away" if r == "Home & Away" else "Other finals"))
q["growth"] = q.gap4 - q.gap3
print("\naverage margin GROWTH in the final quarter, by group:")
print(q.groupby("group").growth.mean().round(1).to_string())


# ------------------------------------------------------------------------
section("5. Even the umpiring looks different")
for era, df, seasoncol in ((old.season.min(), old, "season"),):
    pass
old["frees_per_100"] = 100 * old.freesFor / old.disposals
modern["frees_per_100"] = 100 * modern.freesFor / modern.disposals
o_gf = old[old.round_type == "Grand Final"].frees_per_100
o_ha = old[old.round_type == "Home & Away"].frees_per_100
print(f"2000-2011: Grand Final {o_gf.mean():.2f} vs Home & Away {o_ha.mean():.2f} "
      "frees per 100 disposals")
m_gf = modern[modern.round_type == "Grand Final"].frees_per_100
m_of = modern[(modern.is_final == 1) & (modern.round_type != "Grand Final")].frees_per_100
print(f"2012-2025: Grand Final {m_gf.mean():.2f} vs other finals {m_of.mean():.2f} "
      "frees per 100 disposals")


# ------------------------------------------------------------------------
section("6. Methods note: season fixed-effects model behind the round-type claims")
dd = modern[["round_type", "season", "match_id", "tackles"]].dropna()
fit = smf.ols("tackles ~ C(round_type, Treatment('Home & Away')) + C(season)",
              data=dd).fit(cov_type="cluster", cov_kwds={"groups": dd.match_id})
print("Tackles by round type vs Home & Away, adjusted for season, "
      "SEs clustered by match:\n")
for rt in FINALS:
    k = f"C(round_type, Treatment('Home & Away'))[T.{rt}]"
    if k in fit.params.index:
        ci = fit.conf_int().loc[k]
        print(f"  {rt:20s} {fit.params[k]:+.2f} tackles  "
              f"(95% CI {ci[0]:+.2f} to {ci[1]:+.2f}, p={fit.pvalues[k]:.4f})")
