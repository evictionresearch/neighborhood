# Measuring residential choice: the affordability index v2 design

**Question we're really trying to answer (Tim's framing):** *which groups can move where they
want, and which groups are limited in choice?* This is the "highways of migration" theory made
measurable. This doc is the critical design for the v2 engine and the "fluid calculator," building
on [affordability-index-review.md](affordability-index-review.md) and
[hud-income-limits-architecture.md](hud-income-limits-architecture.md).

---

## 1. The core construct: choice is a three-gate funnel

A household does not get to live somewhere because a unit there is *cheap*. It gets to live there
only if it clears **three gates in sequence** — and those three gates are exactly the three forces
in the theory:

| Gate | Question | Theory force | Data |
|---|---|---|---|
| **1. Afford** | Can they pay for it? | **Affordability** | ACS rent `B25063` / owner cost `B25094` / value `B25075`; AMI tiers (ACS or HUD) |
| **2. Available** | Is there an open unit to move into? | (the physical "highway capacity") | vacancy `B25004`, mover/turnover flows `B07013`, **Data Axle** panel |
| **3. Allowed** | Will they actually be accepted? | **Discrimination + records** ("scarlet E", credit) | voucher acceptance / SOI law, ERN **EER** eviction-record exposure, audit-study discount, `ntdf()` race overlay |

**Choice = what survives all three gates.** "Limited in choice" means the funnel collapses at gate
2 or 3 *even when gate 1 passes* — the household can afford the place but can't get in. The current
`afford()` measures a partial **Gate 1 only**. Everything below is about measuring all three and,
crucially, the *gap between them*.

This funnel framing is the design's organizing principle: every metric exists in (at least) two
versions — **price-only** (gate 1) and **attainable** (gates 1·2·3) — and the *difference between
them is the measurement of constraint*.

---

## 2. Be critical: why the obvious measures are wrong

- **"Count the tracts a group can afford" is not choice.** It ignores vacancy (you can't move into
  an occupied unit), competition (if a tier is 40% of households but can afford 20% of units, they
  face a 2:1 squeeze), and acceptance (affordable ≠ rentable to someone with a record).
- **Affordability is monotonic in income, so raw counts are trivial.** Higher income can afford
  everything lower income can, plus more — "rich have more options" is not a finding. The
  *interesting* signal is the **gradient and the cliffs**: at which AMI tier does access collapse,
  and for which racial groups does it collapse *faster than income alone predicts*?
- **Breadth without quality is a mirage.** Being able to access 30% of tracts means little if all 30%
  are high-poverty, high-displacement-risk neighborhoods. Choice confined to distressed places is
  *constrained* choice. Access must be **weighted by opportunity**.
- **Supply ≠ flow.** Tract-level vacancy (`B25004`) is a noisy snapshot of *current* openings, not
  annual *turnover*. The honest move is to treat gate 2 as an estimate with uncertainty and
  **calibrate it against observed moves** (§5).
- **One affordability number per region hides the squeeze.** The whole point is comparison *across
  groups* and *across space*; the unit of analysis is (group × tract), summarized to (group).

---

## 3. The proposed measures

Notation: groups `g` = AMI tier (ELI/VLI/LI/MI/HI) — optionally crossed with race via `ntdf()`.
Tenure handled separately (rent; own with options A and B from the review). Per tract `t`:

### 3.1 Attainable units (the funnel applied)
```
afford(g,t)      = units in t affordable to g                          # gate 1
available(g,t)   = afford(g,t) × vacancy_or_turnover_rate(t)           # gate 2
attainable(g,t)  = available(g,t) × barrier_pass_rate(g,t)             # gate 3
```
`barrier_pass_rate` starts at 1 (price-only) and is discounted by voucher rejection, SOI law,
eviction-record screening (EER), and — where modeling race — an audit-study discrimination factor.
Report **price-only** and **attainable** side by side everywhere.

### 3.2 Opportunity Access Index (OAI) — the headline
Classify tracts into opportunity strata (configurable: poverty, UDP/HPRM displacement risk, jobs
access, schools, environmental burden). For group `g`:
```
OAI(g) = Σ_t attainable(g,t) · opportunity_weight(t)  /  Σ_t attainable(g,t)
```
i.e. *of the housing a group can actually attain, how opportunity-rich is it, on average?* High OAI
= real choice into good neighborhoods; low OAI = confined to distressed ones. Compute it both
price-only and attainable.

### 3.3 Constraint Gap — "limited in choice," quantified
Two complementary gaps, both designed to strip out the trivial "income buys options" effect:
```
Constraint Gap (vs top)   = OAI(highest tier)  −  OAI(g)          # spatial-opportunity penalty of being poorer
Constraint Gap (barriers) = OAI_priceonly(g)   −  OAI_attainable(g)   # how much discrimination/records shrink choice
```
The **barrier gap is the discrimination/record tax**: how much smaller a group's real choice set is
than their *income alone* would allow. For Black/Latinx renters with eviction exposure it should be
large; for high-income white owners ≈ 0. That contrast *is* the theory's payload.

### 3.4 Competition adjustment (optional, for access not just opportunity)
A location-quotient on attainable supply vs. demand:
```
access_LQ(g) = [attainable_units(g) / all_available_units]  /  [households(g) / all_households]
```
`< 1` = the group can attain less than its population share — structural undersupply, independent of
where those units are.

### 3.5 A single, simple readout (the "pie chart")
For each group, a 0–1 **Choice Score** = OAI_attainable(g) scaled against the region, plus the two
gap numbers. Sophisticated funnel underneath; three numbers on top (Choice Score, opportunity gap,
barrier gap). That's the "Pie Chart with a Bayesian Chaser."

---

## 4. The fluid calculator

Inputs the user slides: **income or AMI tier** (or a raw dollar income) × **tenure** (rent / own) ×
**owner cost basis** (A = current monthly cost, default; B = cost to *buy in today* at this year's
rate) × **year** × optional **race** × optional **household size** (default 4). It returns:

- a **map** of attainable tracts (price-only vs. attainable toggle),
- the **Choice Score + two gaps** for the selected group,
- a **gradient panel**: the same metric across *all* tiers at once — a slope/line chart showing
  where choice collapses. This is the answer to "which groups can move where they want": the flat
  top (HI/MI roam freely), the dropping line (LI/VLI), the floor (ELI confined), and the *second
  line* (barrier-adjusted) peeling away below it for groups facing records/discrimination.

The headline figure for a report is that gradient panel, faceted by race — it shows, in one image,
free-choice groups vs. constrained groups, and the price-vs-barrier wedge.

Mechanically the calculator is just the v2 engine evaluated over a grid of inputs and cached; the
"fluidity" is a thin UI (leaflet + a few controls), not new methodology.

---

## 5. The Bayesian chaser: calibrate against observed moves

We have something most affordability tools don't: ERN's **Data Axle panel** observes *actual* moves
by income tier, tract-to-tract. Use it to validate and calibrate:

- Our funnel predicts *where a group could attain housing*. Data Axle shows *where they actually
  moved*. **Where observed moves are systematically below predicted attainability, that residual is
  empirical evidence of gate-3 constraint** (discrimination/records) beyond what price and vacancy
  explain — a far stronger claim than asserting the barrier multipliers a priori.
- This also lets us *fit* the barrier_pass_rate rather than guess it: treat observed/predicted as
  the signal, partially pooled across regions (hence "Bayesian chaser"). Ties the index directly
  into the HPRM/EDR modeling stack.

---

## 6. Data sources & honest caveats

| Need | Source | Caveat |
|---|---|---|
| Afford (gate 1) | ACS `B25063`/`B25094`/`B25075`, AMI tiers | MOE large at tract level; own-side basis choice (A/B) matters |
| Available (gate 2) | `B25004`, `B07013`, Data Axle | snapshot vacancy noisy; turnover better; treat as uncertain |
| Allowed (gate 3) | voucher/SOI policy, ERN EER, audit studies, `ntdf()` | barrier rates are estimates; report price-only alongside always |
| Opportunity weights | poverty, HPRM/UDP risk, jobs, schools | value-laden → make configurable, never hard-code one index |
| Calibration | Data Axle | proprietary; document the model, ship the derived rates not the raw panel |

Two non-negotiables: (1) **always show price-only next to attainable** so the barrier assumptions
are auditable, and (2) **carry MOE / flag low-count tracts** so a 1-of-2-units tract isn't read like
a 500-of-1000 tract.

---

## 7. Phased build

1. **Gate 1 done right** — rebuild supply on county/HUD AMI (`B19113`), within-bracket
   interpolation, rent + both owner bases (A default, B move-in), all family sizes (default 4),
   `ami_source` = acs/hud/hud_acs. Output the tidy per-(tract×tier) table. *(Replaces today's
   `afford()`.)*
2. **Gate 2** — add availability (`B25004`/turnover); introduce `attainable` and `access_LQ`.
3. **Opportunity layer + OAI + Constraint Gap (vs top)** — the headline gradient.
4. **Gate 3 barriers + barrier Constraint Gap** — vouchers, EER, race overlay; price-only vs
   attainable everywhere.
5. **Calculator UI + Data Axle calibration.**

Ship 1–3 as the defensible core (pure ACS/HUD, fully reproducible); 4–5 are the research frontier
that makes it ERN's, not a generic affordability map.
