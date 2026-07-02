# A Good Place — the stable-destination / segregation-destinations layer (design)

**Successor to** `dev/affordability-index-design.md` (the original 3-gate funnel). **Theory & full
argument:** `~/git/evictionresearch/hprm/manuscripts/a-good-place.md`. **Status:** design locked
(this conversation); build not started. The Gate-1/2 afford engine is committed on branch
`afford-index-v2`.

> **Citation discipline (per `~/.claude/CLAUDE.md`):** §"Citation status" at the end separates what was
> *verified this session* from what is *from the HPRM manuscript (refs to confirm)* and what is *not yet
> verified*. Do not promote a flagged citation to verified without reading the source.

---

## 1. The question — and why *stability*, not opportunity, is the spine

**The question.** Highways of migration / economic-segregation destinations: *which census tracts can
each AMI × race group afford, reach, stay in, and be allowed and able to enter — if everyone were free
to move?* The answer is not a property of a tract. **A good place is a *relation* between a household
and the opportunity structure: the set of places it can afford, reach, stay in, and be allowed/able to
access.** Displacement is the involuntary *contraction* of that set; segregation is its structural
*constraint*.

**Why stability is the spine — and the strong case for why it is *not all Chetty* (this argument must
also live in the neighborhood package's roxygen/vignette, not only here):**

1. **Chetty measures individual *outcomes*, aggregated to tracts — not the systems that produce
   inequality.** The Opportunity Atlas is a tract aggregate of *predicted* adult outcomes for children
   raised ~1978–92; its own authors concede it cannot isolate any single causal neighborhood feature
   (a correlated "bundle"; CHK 2020).
2. **Systems create inequality, not individuals.** Chapple (2017) locates contemporary displacement in
   *structural* forces — widening income inequality, real-wage stagnation, the transformation of
   metropolitan labor markets — making precarity near-universal for lower-income renters rather than a
   local neighborhood effect. The vital things to measure are therefore the **external/structural
   constraints** on where a household can live (affordability, displacement/eviction pressure,
   discrimination, network access) — *not* aggregated individual opportunity outcomes.
3. **Residents agree.** Reid (2019) finds LIHTC residents locate their barriers in labor- and
   housing-*market* structures, not neighborhood amenities; the opportunity-map domains do not predict
   their employment and weakly/counter-intuitively track their own perceptions.
4. **Relocation to "opportunity" is not the lever.** MTO produced no adult economic gains (Kling,
   Liebman & Katz 2007) and movers *reproduced concentrated disadvantage* (Sampson 2008) — which is
   also the basis of the segregation-recreation worry (§2).
5. **Displacement is itself a harm.** Being forced to move — *especially out of a good, stable place* —
   carries documented individual consequences (health, economic, schooling, network disruption). So
   stability is doubly the spine: a stable place is the good, and *losing* it is the harm.

**Therefore:** stability (can a household afford to *stay* without being displaced or evicted) is the
spine; **opportunity is an optional, clearly-labeled side-layer.** The one defensible opportunity
signal is the **employment rate of resident adults** ("how many people around have jobs" — the
verified predictor of mobility, *not* commute/job-proximity), which lives in that side-layer, off by
default.

## 2. Theoretical foundation (and the ecological-fallacy commitment)

The spine rests on five verified pillars:

- **Reid (2019)** — opportunity maps encode a suburban-white definition of "good" (Goetz 2017/2018,
  cited within Reid), poorly match lived experience, and don't predict residents' outcomes; residents
  value affordability, stability, networks, and *staying*.
- **Carlson (2020)** — displacement is measurement-dependent (population / individual / motivational
  proxies rank tracts differently; gentrification predicts displacement *only* under the motivational
  measure).
- **Hepburn, Louis & Desmond (2024)** — most forced moves occur in *non-gentrifying* low-SES tracts →
  never use gentrification as the displacement proxy.
- **Chapple (2017); Chapple & Song (2024)** — structural/systemic drivers; the ecological fallacy in
  population-approach analysis yields spurious conclusions.
- **Sampson (2008)** — MTO was an *individual* mobility experiment whose movers reproduced concentrated
  disadvantage; experiments obscure the structures that sort people.

**The ecological-fallacy commitment.** We will *not* build the displacement signal from
aggregate-compositional inference (Robinson 1950; the modifiable areal unit problem, Openshaw 1984).
We use ERN's individual/longitudinal stack — EDR from the Data Axle household panel (Carlson's
*individual* approach), EER from eviction court records. Any ACS variable is labeled a
**place-descriptor, not an individual-outcome claim.**

**The segregation concern (load-bearing).** Sampson (2008) reinforces the worry that routing
low-income households of color to "affordable + stable" places could *recreate* segregation — place
stratification dressed as community preservation. This is *why* the measure is the
*constraint on the choice set*, not a prescribed destination (§3). The segregation/attainment
literature that frames this — spatial assimilation, place stratification, preferences, and especially
**Krysan & Crowder's *Cycles of Segregation* (networks/search)** — is summarized in the manuscript and
is **flagged below as not yet verified this session.**

## 3. The measure

**Four-gate funnel,** per group `g` = AMI tier × race, per tract `t`:
**Afford → Available → Allowed (stability: HPRM EDR + EER, + optional barrier proxies) → Known (the
Data Axle observed-move residual; *Cycles of Segregation* network/search constraint).**

**Combine HPRM with the afford × available set three ways — all reported:**
- (a) **report-alongside** — where each group can go × how precarious each tract is;
- (b) **hard "stable destination" filter** — affordable + reachable + HPRM below a threshold (mirrors
  the Opportunity Atlas's own affordable-*and*-stable screen) — available as an option;
- (c) **continuous stability-weighted** — the **default**.

Headline = the **stable-destination set**; default weighting = (c) continuous; (b) hard filter offered
as an option. Always show **price-only (Gate 1) next to stable-attainable** so the stability
assumptions are auditable.

**Headline metric: the Constraint Gap (OAI-style), by AMI × race (default; AMI-only an option).** How
much smaller / more disadvantaged a group's attainable-stable set is than the unconstrained geography —
a segregation measure grounded in individual displacement/mobility data, **reported disaggregated by
gate** for legibility.

**Optional opportunity lens** (off by default, clearly labeled "predicted/correlational
place-descriptor"): resident-adult employment rate (the verified signal); optionally COI/Atlas with
their vintage/noise/sorting caveats surfaced.

**Resident-valued attributes to surface as side variables / validation criteria** (Reid): affordability,
rent predictability, unit quality, lack of stigma, proximity to work, social networks, and the ability
to stay put. These inform what "good" means to residents; they are context, not the score.

## 4. Operationalization, and the four decisions as resolved

**Data.** `neighborhood::afford_index(demand="matched", ami_source="hud")` → afford × available set +
AMI tiers; `ntdf()` (and `B25118`/`B19001A–I`) → race. **HPRM** → EDR + EER joined on `GEOID`. Data
Axle panel → Known gate (P3).

**Stability signal (Gate 3), operationalized** (verified against the bundled file
`/Users/buffalo/data/evictionresearch/hprm/hprm_v5_full_2022.parquet`; join on `geoid`, 99.9% field
completeness, 2022 vintage / 2020-census tracts):

- **EDR = `dis_value`** — the validated displacement composite (sums only the AMI tiers at/below the
  +50 BART-uncertainty buffer, so a confidently in-migrating tier can't mask a displacing one) →
  `s_edr = clamp01((dis_value + 300)/350)` — published anchors `+50 → 1.0`, `−300` (Extreme) `→ 0`.
- **EER = `ev_value`** — filing-rate ratio (1.0 = state avg) → `s_eer = clamp01((2 − ev_value)/1.2)` —
  `0.8 → 1.0`, `2.0` (Extreme) `→ 0`. *CA caveat: EER training excluded CA, so CA `ev_value` is
  out-of-sample — usable but flagged.*
- **`stability_source` parameter** — `"both"` (default; equal-weighted average `0.5·s_edr + 0.5·s_eer`),
  `"edr"`, or `"eer"`. **These are different harms:** EDR is *net out-migration* (low-income households
  actually **leaving** — the displacement itself); EER is the *added precarity of the formal-eviction
  channel*. They diverge under HPRM's policy-substitution effect (tenant protections suppress filings
  while market displacement intensifies). The afford↔stability trade-off is **EDR-first** (San Diego
  −0.48 with both, −0.45 EDR-only), so `"edr"` is the robust choice where EER is out-of-sample (CA).

**Small-tract reliability.** Empirical-Bayes shrinkage / suppression + surfaced uncertainty, so the
stability signal is not noisier than the opportunity maps it critiques.

**Geography / scale (MAUP).** The tract is a unit of convenience; opportunity/constraint structures are
supra-tract (Reid: transit, school districts, labor markets). Label accordingly; don't read individual
fate off a tract value.

**The honest tension you flagged — aggregating EDR/EER risks re-entering the ecological fallacy.** True,
and worth stating plainly:
- *Defense:* EDR/EER are **estimated from individual data** (the Data Axle panel + court records =
  Carlson's individual approach), so the *signal* avoids the population/compositional fallacy the whole
  critique targets — a categorical improvement over tract-change measures.
- *Residual:* the *output* is a tract-level rate, and using it to characterize a destination for a
  specific household is an aggregate→individual application. We mitigate, not eliminate: (i) label it a
  **place-level risk/rate descriptor**, never an individual guarantee; (ii) use the **Gate-4 Data Axle
  observed-move residual** as the individual-level check/calibration (the "Bayesian chaser"); (iii)
  don't overclaim. Any tract index ultimately characterizes places — the commitment is to *build from
  individual data and label honestly*. A bounded, acknowledged tension, not a solved one.

**Four decisions (resolved, with your refinements):**
1. **Spine = stability (HPRM)**; opportunity = optional, labeled, off-by-default side-layer.
2. **Displacement is the core Allowed signal**, from HPRM forced-move/eviction data — *never*
   gentrification proxies.
3. **Individual/longitudinal measurement** + place-descriptor labeling; small-tract shrinkage.
4. **AMI × race by default**, with an **AMI-only option**; **continuous stability-weight default**,
   **hard filter optional**.

## 5. Honest limits (carry in every output)

- Measures *command over residential space*, **not** the wage/labor-market structures where inequality
  is actually produced (Chapple 2017) and where residents locate their deepest barriers (Reid 2019).
  Housing stability is a platform for mobility, not a substitute.
- **Aggregate-output ecological-fallacy residual** (§4) — bounded and labeled, not eliminated.
- **Segregation-recreation risk** — the choice-set / Constraint-Gap framing (and the by-race, by-gate
  reporting) is the guard; the index must never be used to justify confinement.
- **Informal displacement** uncaptured (court records are a subset of forced moves); **HPRM vintage**
  is 2019 native / 2022 conservative lower bound.
- The **Known gate** is a research frontier, not a settled metric.
- The **displacement-consequence** literature (forced moves harm individuals, esp. from good places)
  motivates the spine but is **flagged below as not yet verified this session.**

---

## Citation status (per the factual-accuracy bar)

- **Verified this session** (read or adversarially fetched, with locations on file): Reid (2019,
  *Housing Policy Debate*, doi:10.1080/10511482.2019.1582549 — PDF read pp.1–22; Goetz 2017/2018 cited
  *within* Reid, p.16); Sampson (2008, *AJS* 114(1):189–231, doi:10.1086/589843); Carlson (2020,
  *City & Community* 19(3):573–592, doi:10.1111/cico.12482); Hepburn, Louis & Desmond (2024,
  *Social Forces* 102(3):880–901, doi:10.1093/sf/soad123); Beck & Martin (2024, *City & Community*,
  doi:10.1177/15356841241264266); Chetty et al. Opportunity Atlas (NBER w25147 / AER 2024); Chetty,
  Hendren & Katz (2020, *Econ Journal Watch* 17(2):299–304); Aliprantis & Martin (2020, Cleveland Fed
  WP 20-37); Kling, Liebman & Katz (2007, *Econometrica*); Kaestner (2020, *Econ Journal Watch* 17(2),
  contested — cite narrowly).
- **From the HPRM manuscript read this session — confirm full refs before publishing:** Chapple (2017);
  Chapple & Song (2024, *JAPA*, doi:10.1080/01944363.2024.2319293 — fetched, no claim survived adversarial
  verification); Robinson (1950); Openshaw (1984).
- **NOT verified this session — confirm before relying:** the segregation/attainment classics
  (Massey & Denton 1993; Logan & Molotch 1987; Massey 1985; Logan 1978 / Alba & Logan 1993;
  Schelling 1971; Charles 2003; **Krysan & Crowder, *Cycles of Segregation*, 2017**); the
  displacement-consequence literature (Desmond eviction-consequences and related).
