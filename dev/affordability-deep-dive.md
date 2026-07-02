# The affordability index vs. the question it exists to answer — a deep dive

**Date:** 2026-07-02. **Scope:** how wrong the original `afford()` (v1) was, what the correct
measure is, and how far the rebuilt engine (`afford_index()` v2, branch `afford-index-v2`) is from
it. Builds on [affordability-index-review.md](affordability-index-review.md) (the v1 code autopsy)
and [affordability-index-design.md](affordability-index-design.md) (the 3-gate funnel). Every
number here was computed this session by `dev/afford_v1_gap.R` (King County WA, VLI renters,
ACS 2020–2024, HUD FY2024 limits) unless another source is cited.

---

## 0. TL;DR

**The question** (Tim, 2026-07-02): *if everyone had free will and could move wherever they wanted
without paying more than 30% (and, stretching, 50%) of their income in rent — where are those
places, how many units are available, and how many people could move there?*

1. **v1 didn't just have bugs — it answered a different question.** It measured *"what share of
   the occupied stock would be cheap for a mis-measured income group"*: no availability, no people
   counts, no 50% stretch line, a home-made "AMI." Measured on King County VLI renters, its income
   cutoff was **25.6% too low** ($60,000 vs HUD's $75,350), it **understated affordable rental
   supply by ~16 percentage points at the median** (15% vs 36%), and **1 in 5 tracts would get the
   wrong verdict** on an affordable/not map. Its ownership side leaned the other way: at 2024 rates
   the true income needed to buy is **~48% higher** than v1 assumed.
2. **The fix is a three-part measure**, all from the same engine: (i) a tract **verdict** —
   *affordable / roughly affordable / not affordable to VLI* — anchored on the 30% and 50% burden
   lines; (ii) **unit counts that are open, not just cheap** (turnover/vacancy now; an
   NLIHC-style "not occupied by someone richer" adjustment next); (iii) **people arithmetic** —
   affordable units per 100 VLI renter households, and the shortfall.
3. **v2 already is most of the fix.** The hard parts — HUD AMI, interpolation, renter-matched
   demand, rate-aware ownership, Gate-2 availability — are built, tested, and QA'd. What's missing
   is thin: the 50% burden line, the categorical verdict layer (current maps are continuous — the
   exact "looks mid, no problem here" failure), and the per-100/shortfall framing. One genuinely
   new piece: the competition adjustment (CHAS). §4 sizes each.

The free-will answer v2 can already give for King County VLI renters: **~188,400 VLI renter
households; ~163,700 units they could rent at ≤30% of income (86.8 per 100 — a structural deficit
of ~24,800 even with perfect free sorting); of those units only ~46,700 open up in a year (24.8
per 100) and only ~8,700 are vacant right now (4.6 per 100).** v1 could produce none of these
sentences.

---

## 1. The question, made operational

Three quantities, one presentation rule.

| Quantity | Operationalization |
|---|---|
| **Where** are the places a group could live at ≤30% / ≤50% burden? | Per tract: share + count of rentals at gross rent ≤ `0.30·Y/12` and ≤ `0.50·Y/12`, where `Y` = the tier's HUD income ceiling (VLI = pilot group), household-size adjusted |
| **How many units are available** — not just cheap? | Affordable count × opening rate (turnover = annual flow; vacancy = open now), then competition-adjusted (a cheap unit occupied by a higher-income household is not available — NLIHC's "affordable **and available**") |
| **How many people could move there**? | Capacity counts: per-tract affordable+available units ≈ households the tract can absorb; regionally, units per 100 tier households and the **shortfall** = tier households − affordable units |

**Presentation rule (applies across the board, recorded to memory):** the public and policymakers
read maps by color; a continuous gradient that renders a place "mid" reads as "no problem." End
products must classify: **not affordable / roughly affordable / affordable to VLI households**.
The two burden lines Tim named give the classes their plain-English meaning:

- **Affordable** — a VLI household lands here at ≤30% of income (the HUD standard);
- **Roughly affordable** — attainable only by stretching to 30–50% of income (cost-burdened but
  short of HUD's "severe" line at 50%);
- **Not affordable** — even at half their income, most of the stock is out of reach.

A tract-majority rule (≥50% of units) makes each class a **median-unit sentence**: "the typical
rental in this neighborhood would cost a VLI family more than half its income." That is the
defensible, explainable cut. (Decision point for Tim: majority share is the recommended rule;
alternatives are a fair-share rule — location quotient ≥ 1 — or a different share threshold. The
deployed SLC/San Diego reports already used a 3-class "Not / Less / More Affordable" scheme, so
this restores the deployed presentation on a corrected engine.)

---

## 2. How wrong the original was

### 2.1 Against the question (the conceptual misses)

These hold even if every line of v1's code were bug-free:

1. **It measured the occupied stock.** v1 counts units affordable among *all* rentals (`B25063`,
   occupied universe). Nobody can move into an occupied unit. It never asked "is anything open?" —
   for a migration theory, the central omission (review §3.4.1). Scale of the miss, from v2's
   Gate 2: only **28.5%** of King's VLI-affordable units turn over in a year (46,671 of 163,657)
   and **5.3%** are vacant-for-rent right now — v1's implicit "all of it is up for grabs" is off
   by a factor of ~4 (flow) to ~19 (stock-now).
2. **It reported shares and rates, never people.** Outputs were `supply` (a share), a location
   quotient, and units per 100k. Nothing answered "how many households could actually move here" —
   no capacity, no shortfall. The deployed reports had to hand-derive their "fewer than 1
   affordable unit per 3 ELI households" headline.
3. **One burden line.** The 30% rule only; no 50% severe-burden line, so no "roughly affordable"
   middle class — the map could not distinguish "stretch" from "impossible."
4. **The wrong people in the denominator.** Demand = *all* households under the cutoff (222,678 in
   King), not *renter* households under it (188,438). Renters skew poorer: v1's group was 23.7% of
   all households where the true VLI-renter share is **44.9% of renters** — so its location
   quotient flattered tracts by roughly **1.9×**.
5. **A home-made "AMI."** Median of tract median *household* incomes — not HUD's family-income,
   size-adjusted, high-cost-adjusted limit that every partner (HPRM, LIHTC, the reports) means by
   "50% AMI."

### 2.2 Against correctness (measured damage, King County, VLI renters)

Full code autopsy in [affordability-index-review.md](affordability-index-review.md) (the `0.188`
constant, universe-mixing, recycling bug, positional `rep()`, snapping, fake "Jenks"). What those
defects *cost*, measured this session (`dev/afford_v1_gap.R`, n = 494 joined tracts):

| Distortion | v1 | Correct (v2/HUD) | Damage |
|---|---|---|---|
| VLI income cutoff | $64,224 → **snapped to $60,000** (0.5 × median-of-tract-medians $128,447) | **$75,350** (HUD VLI, 4-person, FY24) | cutoff **25.6% too low** |
| Median tract VLI-affordable rental share | **15.1%** | **35.5%** | understated **16.5 pp** at the median (mean −17.3 pp) |
| Tract-level agreement | — | — | correlation **0.84**; **70%** of tracts off by >10 pp, **41%** off by >20 pp — the *ranking* is distorted, not just the level |
| Verdict stability (majority rule at 30%) | — | — | **21.9% of tracts flip** the affordable/not verdict |
| Ownership: income needed to buy | price × 0.188 (≈3.8% rate, 0% down, no tax/ins) | price × **0.279** (6.9% 2024 rate, 10% down, 1.25% tax+ins) | v1 understates required income — the true requirement is **1.48×** v1's (afford 5.3× income vs 3.6×) |
| Demand universe | 222,678 all-households ≤ $60k (23.7%) | 188,438 renter households ≤ HUD VLI (44.9% of renters) | fair-share ratio inflated ≈ **1.9×** |

Two directions at once: **too pessimistic on rent** (cutoff too low → tracts look less affordable
than they are), **too optimistic on ownership** (0.188 → homes look ~48% cheaper to reach than
they are), and **silent on availability and people**. That pattern can't be patched with a
calibration factor; it required the rebuild.

*(Caveat: the 25.6%-low cutoff is King-specific — median-of-medians vs HUD can err in either
direction by region; Atlanta's county-vs-metro divergence runs the other way, per the QA page §6.
The point is not the sign but that v1's income line was untethered from the HUD standard everyone
else uses.)*

### 2.3 Does the published v1 finding survive the corrected engine? Yes — in its defensible form

The SLC (Wasatch) and San Diego EDR reports, built on v1, headlined *"no 'more affordable'
neighborhoods outside displacement-risk areas."* Replication test (`dev/slc_sd_replication.R`,
2026-07-02): both engines re-run on **San Diego County** (the exact published region) and **Salt
Lake County** (core of the 4-county Wasatch region; the HUD-API outage kept Davis/Weber out),
VLI renters, crossed with HPRM v5-2022 `dis_value` bands. v1's cutoff error replicates in both
places (SD $60,000 vs HUD $75,750 = **26.3% low**; SLC $50,000 vs $57,750 = **15.5% low**).

The published claim has two readings, and they fare differently:

1. **"Everywhere that's displacing is a place VLI renters can reach" — replicates, essentially
   exactly.** Under the v2 verdict, displacement tracts (`dis_value < 0`) land in the
   affordable-or-roughly classes **36 of 36** times in Salt Lake (22 affordable, 14 roughly,
   **0 in "not affordable"**) and **156 of 157** in San Diego. Under v1's classes the same
   pattern: 31 of SLC's 36 displacement tracts sat in its "More Affordable" class. Displacement
   pressure lives inside the reachable set — the highways-of-migration claim — and it survives
   the corrected engine.
2. **"Everywhere VLI renters can reach is displacing" — does not hold, and mostly never did.**
   Under v2, 55% of San Diego's affordable-verdict tracts and 61% of Salt Lake's show *no* net
   outflow. v1 exaggerated toward this absolutist reading mechanically: its ~16–26%-low cutoff
   shrank "More Affordable" to a sliver (San Diego: **10 tracts** of ~730) at the extreme bottom
   of the market — precisely the displacement-pressured slice — so the map *read* as "affordable
   = displacing."

The gradient is strong either way, and **stronger in the published regions than in King/Bay**:
`cor(supply, dis_value)` = **−0.55** (SD) and **−0.60** (SLC); affordable-verdict tracts are
under net outflow at 3.0× (SD: 45% vs 15%) to 5.2× (SLC: 39% vs 7%) the rate of the rest; median
`dis_value` in the affordable class sits at break-even (+11, +27) against strong low-income
growth elsewhere (+379, +615). **For the San Mateo report, the defensible headline is the
exhaustive form of reading 1 plus the gradient — not the absolute form of reading 2.**
*(Vintage caveat: replicated against HPRM v5 2022, not the report-era HPRM release.)*

---

## 3. The fix: the measure the question demands

For the pilot group (VLI renters; generalizes to any tier × tenure), per tract `t`:

```
Y            = HUD VLI income limit (county, FY, household-size adjusted)   [ami_cutoffs()]
afford30(t)  = units with gross rent ≤ 0.30·Y/12     -- the standard        [built: accessible]
afford50(t)  = units with gross rent ≤ 0.50·Y/12     -- the stretch line    [MISSING: burden param]
open(t)      = afford30(t) × turnover_rate(t)         -- flow per year      [built: available_turnover]
               (vacancy_rate for "open right now")                          [built: available_vacancy]
avail(t)     = afford30(t) not occupied by higher-income households         [MISSING: CHAS layer]

verdict(t)   = affordable          if afford30(t)/total(t) ≥ ½              [MISSING: verdict layer]
               roughly affordable  else if afford50(t)/total(t) ≥ ½
               not affordable      otherwise

people:      per-tract capacity  = open(t)  (≈ households/yr the tract can absorb)
             regional            = Σ afford30 per 100 VLI renter households  [MISSING: framing only]
             shortfall           = N_VLI − Σ afford30
```

Design commitments carried over from [affordability-index-design.md](affordability-index-design.md):
always show **price-only next to available** (the paper-vs-real wedge is itself a finding), and the
verdict map is the headline while continuous surfaces move to tooltips/appendix.

**The competition adjustment (the one genuinely new piece).** Cheap units are not reserved for
low-income households. NLIHC's Gap methodology is the national standard for exactly this: of the
7.2M units affordable to ELI renters nationally, **only 3.8M are "affordable and available"** —
the rest are occupied by higher-income households — yielding the famous **35 affordable-and-available
homes per 100 ELI renter households** (NLIHC, *The Gap*, March 2026, pp. 4, 11; method: 2024 ACS
PUMS, units categorized by the rent affordable at ≤30% of income *without regard to the incomes of
current tenants*, then matched to occupants — p. 31; verified this session,
[nlihc.org/gap](https://nlihc.org/sites/default/files/gap/2026/gap-report_2026_english.pdf)).
PUMS is PUMA-geography, so for **tract**-level work the source is HUD **CHAS** (the ACS special
tabulation: households by income × units by affordability at 30/50/80% HAMFI, published down to
tract parts — huduser.gov/portal/datasets/cp.html; latest vintage appeared to be 2017–2021 as of
this check — **confirm current vintage before building**). Until that layer exists, our per-100
numbers are honest about being *affordability-only*: King's 86.8 per 100 would drop materially
after competition adjustment — nationally for ELI the adjustment cuts the stock roughly in half
(7.2M → 3.8M).

**What the answer looks like (King County, VLI renters, computable today):**

> King County has **~188,400 VLI renter households**. At ≤30% of income, the county's rental stock
> holds **~163,700 units they could afford — 86.8 per 100 households**, a structural deficit of
> **~24,800 units even if everyone could re-sort freely**. Only **~46,700** of those units open up
> in a year (**24.8 per 100**) and **~8,700 are vacant right now (4.6 per 100)**. By tract verdict:
> **139 tracts (28%) are affordable** to a VLI family, **315 (64%) only roughly affordable**
> (stretching past 30% of income), **40 (8%) not affordable even at half their income**. 46 tracts
> have less than one affordable opening a year — affordable on paper, closed in practice (only 4
> of them sit in the "affordable" verdict class).

*(Notes: tiers are cumulative on both sides — VLI units include ELI-priced units, VLI households
include ELI households — so the ratio is internally consistent. The open/vacant counts assume
opening units share the occupied stock's affordability mix — stated in the Gate-2 code — and are
not yet competition-adjusted.)*

---

## 4. How far the current work is from the fix

**Built and verified** (code read + QA'd this session; `R/afford_index.R`, `dev/afford_qa.html`):

| Piece | Where | Status |
|---|---|---|
| HUD AMI anchor (snapshot → API), hh-size adjusted | `ami_cutoffs()`, `ami_source` ladder | ✅ validated cell-by-cell vs HUD (QA §6) |
| Label-parsed brackets + within-bracket interpolation | `.afi_parse_label`, `.afi_interp_le` (`afford_index.R:59`) | ✅ kills snapping + positional `rep()` |
| Renter-matched demand (+ all/owner/likelihood universes) | `demand=` (`afford_index.R:306–338`) | ✅ QA §2 |
| Rate-aware ownership (both bases; 0.188 dead) | `own_current`/`own_buyin` | ✅ QA §3 |
| Gate-2 availability: vacancy + turnover, `available_*` counts | `.afi_availability` (`afford_index.R:160`), joined at `:390` | ✅ QA §5 |
| Cumulative↔banded tiers | `afford_bands()` | ✅ QA §7 |
| Stability join (Gate-3 partial: displacement/eviction) | `afford_stability()` | ✅ tested; P1 walkthrough |

**Gaps** (items 1–3 **built 2026-07-02**, same session as this doc):

| # | Gap | Why it matters | Status |
|---|---|---|---|
| 1 | **50% burden line** | no "roughly affordable" class without it | ✅ `afford_index(burden = 0.30, stretch = 0.50)` → `accessible_stretch`/`supply_stretch`; `afford_bands()` differences them |
| 2 | **Verdict layer + categorical maps** — the walkthrough maps were `type = "continuous"`, the exact "reads mid → no problem" failure | the presentation rule; the headline product | ✅ `afford_verdict()` (majority/median-unit rule, `share_cut` arg); walkthrough headline maps flipped to green/orange/red verdicts |
| 3 | **People arithmetic** — per-100, shortfall, capacity framing | it's the actual question | ✅ `afford_capacity()` (per-100 affordable/stretch/open, shortfall, `tracts_lt1_open`); free-will table in the walkthrough |
| 4 | **Competition adjustment** — affordable ≠ available when higher-income households occupy the cheap stock | NLIHC: nationally ~half of ELI-affordable stock is occupied richer | **Open, Medium** — CHAS tract join (or PUMS at county for calibration); the one real methodology addition |
| 5 | MOE / small-tract reliability flags (review §3.2.5) | a 2-unit tract shouldn't read like a 500-unit tract; verdicts near the 0.5 line can flip within the MOE | **Open, Medium** |

Unit tests cover the verdict rule, capacity arithmetic, and stretch banding
(`tests/testthat/test-afford_verdict.R`); the packaged functions reproduce this doc's hand-computed
King numbers exactly (139/315/40; 86.8 per 100; shortfall 24,780).

## 5. Remaining build order

1. CHAS competition layer → true "affordable **and available** per 100" (NLIHC-comparable).
2. MOE/reliability flags (and a lean band or dual display for verdicts within MOE of the 0.5 line).
3. Gate the walkthrough's bivariate (afford × stability) on the absolute verdict instead of the
   median split.

The verdict layer feeds the existing chain unchanged: verdict × `afford_stability()` = "can
afford it, can get in, can stay" — the good-place spine.

---

*Provenance: all King County numbers — `dev/afford_v1_gap.R` (this session; joins
`dev/afford_qa_data.rds` v2 baseline to a fresh legacy `afford()` run; internal check: recomputed
supply reproduces the packaged v2 supply at r > 0.999). v1 mechanics — `R/afford.R` (read).
v2 mechanics — `R/afford_index.R` (read). v1 code autopsy — `dev/affordability-index-review.md`.
NLIHC Gap 2026 — PDF fetched and read this session (pp. 4, 11, 31). CHAS — huduser.gov search this
session; vintage flagged unconfirmed. Continuous-map check — `dev/p1_eval.Rmd:52–58` (grep).*
