# Affordability Index (`afford()`) — Deep Review & Remediation Plan

**Author:** Review prepared for Tim Thomas / ERN–CiDR Lab
**Date:** 2026-06-01
**Scope:** `R/afford.R` in the `neighborhood` package (v1.0.6), evaluated against the
intended use case (the "highways of migration" theory) and against how the index is
deployed in the Salt Lake City (Wasatch) and San Diego (SANDAG) EDR reports.
**Status:** Review + plan. No changes to `afford()` *logic* have been made on this branch;
only documentation has been added. Implementation should follow sign-off on the design
decisions flagged below.

---

## 0. TL;DR

The `afford()` function answers a genuinely important and well-posed question — *for a given
income group, how much of each tract's housing stock is within reach, and how does that
compare to the region's share of households in that income group?* — and the core idea (a
supply-to-demand **location quotient** per tract) is the right conceptual engine for the
migration theory. **But the current implementation has one disqualifying methodological
black box, several latent correctness bugs, and is missing the three dimensions the theory
actually cares about.** In its present form I would not put its *ownership* numbers in a
report, and I would caveat the *rental* numbers.

The single most important problems, in order:

1. **The home-ownership affordability test is a hard-coded constant (`× 0.188`) that silently
   bakes in a ~3.8% mortgage interest rate and a 0%-down, no-tax, no-insurance loan.** At 2024
   interest rates this overstates ownership affordability by roughly 30–40%. (§3.1.1)
2. **"AMI" is computed as the median of tract median incomes** — a median-of-medians, un-weighted,
   single-number-for-the-whole-region proxy that is neither HUD AMI nor a population-weighted
   regional median. (§3.1.2)
3. **The ownership stock is built by summing two different census universes** (owner-occupied
   *value*, B25075, plus vacant-for-sale *price asked*, B25085), which conflates occupied stock
   with for-sale flow. (§3.1.3)
4. **The index measures affordability of the *occupied* stock, not *availability*** — it never
   asks whether an affordable unit is actually open to move into (vacancy/turnover). For a
   migration model this is the central omission. (§3.4.1)
5. **It is blind to the non-price barriers in the theory** — discrimination, source-of-income
   rejection, and screening on eviction records / credit ("the scarlet E"). (§3.4.2)
6. **Several latent bugs**: a vector-recycling hazard in the `class_pop` filter, total reliance
   on census rows arriving in a fixed order, a `state="CA"` (name) path that silently returns
   nothing, and mislabeled "Jenks" categories that are actually hard-coded cut points. (§3.2)

The good news: **the organization already has a better AMI engine** in
`evictionresearch/library/code/d_calc_ami_incomplete.R`, which uses HUD-style ELI/VLI/LI/MI/HI
tiers built from county AMI with *linear interpolation within income brackets*. The
affordability index should be rebuilt on top of that engine. (§4)

---

## 1. What `afford()` actually does

Call signature:

```r
afford(state, counties, ami_limit, year = 2024, geometry = FALSE, ...)
```

Plain-language pipeline (line numbers refer to `R/afford.R`):

1. **Demand.** Pull county-level household-income distribution `B19001` (17 brackets), filter to
   the requested counties (L29–36).
2. **Region AMI.** Pull tract-level median household income `B19013_001` and take
   `ami <- median(med_inc$estimate)` — i.e. the **median of tract medians** (L38–44, L120).
3. **Supply — ownership.** Pull two tract-level tables and stack them (L48–86):
   - `B25085` "price asked" (vacant-for-sale / sold-not-occupied), called `price`;
   - `B25075` "value" (owner-occupied units), called `value`.
4. **Supply — rental.** Pull tract-level gross rent `B25063`, called `rent` (L88–106).
5. **Bracket → required income.** Attach a hand-built numeric vector to each table by *position*
   (`rep(..., times = nrow/len)`), then convert each housing-cost bracket into the income needed
   to afford it (L108–124):
   - ownership: `income_limit = price_or_value_limit * 0.188`
   - rental: `income_limit = (rent_limit / 0.3) * 12`
6. **Affordability threshold.** For the requested `ami_limit` (e.g. `0.5`), the income cutoff is
   `ami_limit * ami`, **snapped to the nearest income bracket boundary** via `closest()` (L25–27,
   L128).
7. **Accessible counts.** Per tract, sum the units whose required income is at or below the
   threshold → `tr_own_accessible`, `tr_rent_accessible`; the bracket with `income_limit == 0`
   is treated as the tract total (L126–160).
8. **Region demand share.** `class_prop = class_pop / total_pop`, the regional share of households
   at or below the AMI cutoff (L162–174).
9. **Three per-tract metrics** (L177–203):
   - `tr_*_supply = accessible / total` — share of the tract's stock affordable to the group;
   - `tr_*_ratio = supply / class_prop` — a **location quotient**: >1 means the tract carries more
     than its "fair share" of affordable supply for that group;
   - `tr_*_rate = (accessible / class_pop) * 100000` — accessible units per 100k regional
     group-households.
10. **Categories + popups + optional geometry** (L205–273). The `*_jenks_cat` fields are built from
    fixed cut points (≤100, 100–200, >200 per 100k), **not** a Jenks/natural-breaks classification.

**The conceptual heart is step 9's `ratio`** — a supply/demand location quotient per tract. That is
exactly the right primitive for "where can group X actually go?" It is also the most
under-developed part of the output (buried among raw counts, never surfaced as *the* index).

---

## 2. How the index is used in the field — and the gap to the code

Both deployed reports describe the **same** conceptual method, in nearly identical language:

> *"the total number of low-income households in [the region] and compares the respective
> population size to the number of rental units in a tract that these families can afford
> without spending more than 30% of their income on rent."*
> — SANDAG and SLC EDR reports

Observations from the two deployments:

| Aspect | SLC (Wasatch) & San Diego reports | `afford()` as written |
|---|---|---|
| Tenure emphasized | **Rental only** ("affordable rental unit", "rental stock") | Computes both, but **ownership** is the broken half |
| AMI tiers | **HUD bands**: ELI <30%, VLI <50%, LI <80% | Single continuous `ami_limit`; "AMI" = median of tract medians |
| Threshold | 30% of income on **rent** | 30% rule for rent ✔; ownership via `× 0.188` ✗ |
| Headline framing | "fewer than 1 affordable unit per 3 ELI households" — a **supply:demand ratio** | `tr_rent_ratio` is exactly this, but unlabeled and not foregrounded |
| Map classes | "Not / Less / More Affordable" (3 classes) | `*_jenks_cat` 3 classes, but mislabeled and on a per-100k rate, not the ratio |
| Key finding | *"no 'more affordable' neighborhoods outside displacement-risk areas"* — i.e. **nowhere to move** | The index can show this, but only via the rental side |

**Takeaways:**

- The **published work leans entirely on the rental side and on HUD bands**, while the code's
  distinctive (and broken) feature is the ownership side and a non-HUD AMI. The code and the
  reports have drifted apart.
- The reports' headline — "fewer than one affordable unit per three ELI households" — is precisely
  `tr_rent_ratio` (or its inverse). The index should **make the ratio the primary output**, named
  in plain language, instead of leading with raw rates.
- The reports' central conclusion is a **migration conclusion** ("nowhere affordable to move that
  isn't already under displacement pressure"). That is the highways-of-migration theory stated
  empirically — and it requires the *availability* and *barrier* dimensions the code currently
  lacks (§3.4).

---

## 3. Critique

### 3.1 Conceptual & methodological

#### 3.1.1 The `× 0.188` ownership constant is a black box that bakes in an interest rate **[P0]**

```r
price$income_limit <- price$limit * 0.188   # required income to "afford" a home of this price
```

There is no comment, no parameter, no citation. Reverse-engineering it:

> `0.188 ≈ 0.0564 / 0.30`, where **0.0564 is the annual mortgage constant** for a 30-year
> fixed-rate loan at **≈3.8% interest** (monthly payment factor × 12), and **0.30** is the
> 30%-of-income housing-cost ceiling.

So the formula assumes a household can afford a home whose **fully-financed** mortgage payment, at
a **~late-2010s interest rate**, with **0% down payment** and **no property tax, insurance, PMI, or
HOA**, equals 30% of income. Equivalent to "you can afford a house ≈5.3× your income."

Why this is disqualifying:

- **Interest-rate sensitivity.** The required-income factor is extremely sensitive to rates. At
  ~7% (where 30-year rates sat through 2023–2024), the mortgage constant is ≈0.0798, so the factor
  should be ≈0.266, not 0.188. **Using 0.188 in a 7% year makes homes look ~40% more affordable
  than they are.** The index is being run for `year = 2024` with a ~2018 interest-rate assumption.
- **Ignores the real cost stack.** Property taxes, insurance, PMI, and HOA/condo fees are a large
  and *place-varying* share of ownership cost. A constant multiplier cannot capture this.
- **Ignores down payment / wealth.** Affordability of ownership is gated by wealth, not just
  income — exactly the kind of barrier the migration theory is about.

**Fix options (in order of preference):**
1. **Replace value/price with a monthly-cost table.** Use `B25094` (Selected Monthly Owner Costs,
   dollar brackets) — the owner analog to `B25063` gross rent — and apply the *same* 30%-rule used
   for renters: `income_limit = (monthly_owner_cost / 0.3) * 12`. This removes the magic number
   entirely and treats both tenures consistently. **(Recommended.)**
2. If a price-based test must be kept, **make the mortgage assumptions explicit parameters**
   (`interest_rate`, `term_years`, `down_pct`, `tax_ins_rate`) and compute the constant from the
   study year's actual rate.

#### 3.1.2 "AMI" is a median-of-medians, not AMI **[P0]**

```r
ami <- stats::median(med_inc$estimate, na.rm = TRUE)   # median of tract median incomes
```

Three problems:

- **Statistically biased.** The median of tract medians is not the regional median household income.
  It throws away the within-tract distribution and is sensitive to how many small vs. large tracts
  there are.
- **Un-weighted.** A 40-household tract counts the same as a 6,000-household tract.
- **Not HUD AMI.** HUD's published Area Median Income is **household-size-adjusted, metro/county
  defined, and released annually**, and is the standard the SLC/SD reports invoke (ELI/VLI/LI). A
  single region-wide number also misbehaves for **multi-county regions** (e.g. the 5-county Bay Area
  example bundles very different income geographies into one cutoff).

**Fix:** Adopt the county-AMI approach already used in `library/code/d_calc_ami_incomplete.R`:
`co_ami = B19013_001` at the **county** level, with tiers at `0.3 / 0.5 / 0.8 / 1.2 × co_ami`, each
tract scored against *its own county's* AMI. Optionally offer a `use_hud_ami = TRUE` path that
pulls the official HUD income limits. (§4)

#### 3.1.3 Ownership stock sums two different census universes **[P1]**

```r
tract_counts <- dplyr::bind_rows(price_counts, value_counts) %>%
  dplyr::group_by(GEOID) %>% dplyr::summarize_all(~sum(.))
```

`B25075` (value) has universe **owner-occupied** units; `B25085` (price asked) has universe
**vacant-for-sale / sold-not-yet-occupied** units. Summing them yields a denominator
(`tr_own_total`) that is *occupied owner stock + units on the market* — not a coherent universe.
For a migration question this is doubly wrong: the occupied units are by definition **not available
to move into**, and they swamp the (small) for-sale flow that actually represents opportunity.

**Fix:** Decide what `afford()` is measuring and pick one universe:
- For **"how affordable is the existing owned stock"** → `B25075` alone.
- For **"where could a mover actually buy"** → the for-sale flow (`B25085`) — closer to the
  theory, but small-count/noisy at the tract level.
Do not sum them. (See §3.4.1 on availability.)

#### 3.1.4 Threshold snapping and no within-bracket interpolation **[P1]**

`closest()` snaps the income cutoff to the nearest of 17 bracket boundaries (so an 80%-AMI cutoff
of \$64,000 becomes \$60,000), and units are counted by **whole bracket** — a unit-bracket is fully
in or fully out. The org's own AMI helper already does the right thing: **apportion fractional
households/units across a tier boundary** using
`(top − cutoff) / (top − bottom)`. Carrying that interpolation into `afford()` removes both the
snapping error and the all-or-nothing bracket error.

#### 3.1.5 Cumulative vs. banded tiers is undocumented **[P2]**

`afford()` counts units affordable **at or below** `ami_limit` (cumulative), so `0.8` includes
everything affordable to `0.5`. That matches the reports' "affordable to households making less than
50%" phrasing, but it should be **stated**, and the package should also expose **banded** tiers
(e.g. the 50–80% slice) by differencing, since HPRM and the reports both think in bands.

### 3.2 Correctness bugs & fragility

#### 3.2.1 `class_pop` filter relies on silent vector recycling **[P1]**

```r
class_pop <- sum(income %>%
  dplyr::filter(limit <= closest(ami_limit*ami, income_limit) & income_limit > 0) %>%
  dplyr::select(estimate))
```

Inside `filter()`, `limit` is a **column** (length = `nrow(income)`), but `income_limit` is the
**global 17-element vector** defined at L108 (there is no `income_limit` column on `income`). So
`income_limit > 0` is length 17 and gets **recycled** against the per-row condition. It only
produces the right answer because `nrow(income)` happens to be a multiple of 17 *and* the rows
arrive in `B19001_001..017` order so the recycled `FALSE` lands on each county's total row. This is
a correctness landmine: any change in row order or county count breaks it silently. **Fix:** filter
on an explicit column (e.g. `limit > 0`) and never reference the bare global vector inside `filter()`.

#### 3.2.2 Total reliance on census row order via positional `rep()` **[P1]**

```r
price$limit <- rep(price_limit, times = nrow(price)/length(price_limit))
```

Every bracket→limit mapping assumes each tract returns **exactly** the full set of variables in a
**fixed order**. If `get_acs()` returns a different order, drops a suppressed bracket, or the table
definition shifts between vintages, the limits **mis-align to the wrong brackets with no error** —
the worst kind of failure. The `arrange(GEOID, variable)` mitigates this only because the variable
codes are zero-padded. **Fix:** join limits to variables **by name** (a small lookup table keyed on
the `B250xx_0nn` code), never by position.

#### 3.2.3 `state` as a name (`"CA"`) silently returns nothing **[P1]**

```r
income <- get_acs(...) %>% filter(GEOID %in% paste0(state, counties))
```

`tidycensus` accepts `state = "CA"` and `county = "Marin"`, but this filter builds GEOIDs by
**string-pasting** `state` and `counties`, which only works for numeric FIPS (`"06"`, `"041"`). The
file's own trailing example, `afford("CA", "Marin", .8, year = 2022)`, would make `income` empty →
`class_pop`/`class_prop` collapse → wrong output, **no error raised**. **Fix:** normalize inputs to
FIPS up front (or build the county-GEOID list from the returned data), and validate.

#### 3.2.4 `*_jenks_cat` is not Jenks **[P2]**

The categories use fixed cut points (≤0.1% / 0.1–0.2% / >0.2%), not a Jenks natural-breaks
algorithm. The name is misleading in code, popups, and any downstream legend. **Fix:** rename to
`*_cat` (or actually compute Jenks via `classInt::classIntervals`, which also adapts to each
region's distribution rather than fixed national cut points).

#### 3.2.5 Smaller correctness/robustness items **[P2]**
- `rep(rent_limit, time = nrow(rent)/25)` uses `time=` (works only via partial-arg matching of
  `times=`) and a hard-coded `25` instead of `length(rent_limit)`.
- Margins of error are dropped everywhere. Tract-level housing-cost brackets often have MOEs as
  large as the estimate; the index should at least flag low-reliability tracts (e.g. small
  denominators) rather than presenting `0/3 = 0%` and `1/2 = 50%` as equally trustworthy.
- `class_prop == 0` is guarded, but `tr_own_total == 0` / `tr_rent_total == 0` produce `NaN`
  supplies that flow into the categories unguarded.

### 3.3 Software engineering & package hygiene

| Item | Issue | Fix | Pri |
|---|---|---|---|
| Chatty `print()` | `print("Return dataframe")` etc. violate the house style (no chatty prints) and pollute output | remove, or `cli::cli_inform()` gated by `verbose` | P2 |
| Implicit joins | several `left_join()` without `by=` → noisy messages, risk of wrong keys | always specify `by=` | P2 |
| No input validation | no checks on `ami_limit` range, FIPS format, year availability | add `stopifnot()`/`cli::cli_abort()` guards | P1 |
| `@return` is wrong | docs say "Returns a spatial file" but a data.frame is returned unless `geometry=TRUE` | fix roxygen (done on this branch) | P2 |
| No tests | `afford()` has zero test coverage | add tests with a tiny fixture / mocked `get_acs` | P1 |
| Split `get_acs()` calls | the 1–24 / 25–27 split is legacy (old variable-count cap) and adds fragility | single call per table | P2 |
| Performance | re-downloads `load_variables()` 3× and pulls every bracket every call | pull once; consider caching | P2 |
| Naming | `afford()` is terse for an exported API; ownership/rental outputs are inconsistently prefixed | consider `afford_index()` + tidy output schema | P2 |

### 3.4 What's missing for the "highways of migration" theory

The theory is that movement is constrained by **(a) affordability, (b) discrimination, and
(c) records** (the scarlet E, credit/criminal screening). `afford()` addresses a *partial* version
of (a) and nothing of (b) or (c). The deployed reports already gesture at the real target — *there
is nowhere to move that isn't already under displacement pressure* — which the current index can
only weakly support.

#### 3.4.1 No availability / vacancy dimension **[P0 for the theory]**

Affordability of **occupied** stock ≠ ability to **move in**. A tract can be 100% affordable and
have zero open units. A migration model needs the **opening rate**: vacant-for-rent (`B25004`) and
vacant-for-sale units, ideally combined with turnover. The index should report not just
"share affordable" but "**affordable AND available** units per group-household." This is the literal
operationalization of a "highway" — a route with actual capacity.

#### 3.4.2 No barrier/exclusion dimension **[P1 for the theory]**

To capture discrimination and records, layer tract-level proxies onto the affordability surface:
- **Source-of-income / voucher acceptance** and Housing Choice Voucher saturation (HUD Picture of
  Subsidized Households).
- **Eviction-record screening** — directly connectable to ERN's own filing data (EER): tracts that
  are "affordable on paper" but whose landlords screen out the scarlet E are not real destinations.
- **Credit / criminal screening** proxies; **racial composition / steering** signals (the package's
  own `ntdf()` typologies are a natural join).

Even a simple **"affordable-but-barriered" flag** would make the index say something the theory
predicts and the raw price model cannot.

#### 3.4.3 Not integrated with the ERN model stack **[P1]**

The index uses a bespoke continuous `ami_limit` and a bespoke AMI, while HPRM / EDR / EER use
standardized **ELI/VLI/LI** tiers and county AMI. To be the affordability layer *of the ecosystem*
it must (i) emit the same tiers, (ii) key on `GEOID` cleanly, and (iii) be joinable to EDR
(displacement risk) and EER (eviction risk) so one can ask the theory's actual question:
**"of the tracts a 50%-AMI household can afford, how many are *also* available, *also* low-barrier,
and *not already* under displacement/eviction pressure?"** That intersection *is* the highways map.

#### 3.4.4 The index itself is under-articulated **[P1]**

Right now the function returns ~12 columns of intermediates and no single, defensible "affordability
index." Define the index explicitly. A defensible v1 for group *g* in tract *t*:

```
opportunity(t, g) = affordable_available_units(t, g)  /  regional_households(g)
```

reported as a per-10k rate and as a location quotient vs. the regional mean — with the
barrier/displacement adjustments as optional multipliers. Lead with that; keep the rest as
supporting columns.

---

## 4. The org already has a better AMI engine — use it

`evictionresearch/library/code/d_calc_ami_incomplete.R` (the `ami_fun()` routine) is markedly more
correct than `afford()`'s AMI handling and should be the foundation:

- **County AMI** from `B19013_001`, with `co_30 / co_50 / co_80 / co_120` cutoffs — not a
  median-of-medians.
- **Linear interpolation within income brackets** to apportion households into ELI/VLI/LI/MI/HI —
  the right fix for §3.1.4.
- **Renter vs. all-household** universes (`B25118` vs `B19001`) — matters because the reports key on
  *renter* affordability.
- **Race/ethnicity breakouts** (`B19001A–I`) — enables the equity cuts the reports feature and the
  theory's discrimination lens.

It is named `_incomplete` and is demand-side only (it builds the *households-by-tier* denominators,
not the *units-by-tier* supply). The natural architecture: **promote `ami_fun()`'s logic into the
package as the shared demand/tier engine, and rebuild `afford()`'s supply side (units by tier, with
interpolation) on the same scaffold.**

---

## 5. Direct answers to the three questions

**Does the current model do this right?**
Partially. The *skeleton* is right — a per-tract supply-vs-demand location quotient by income group
is the correct primitive, and the **rental** side (30%-of-gross-rent) is methodologically sound
modulo the AMI base and bracket snapping. The **ownership** side is not usable as written (the
`0.188` constant and the mixed-universe stock). The **AMI** definition is weak. And the thing is
**mislabeled** in places ("Jenks", `@return`).

**Could it be done better?**
Yes, and mostly by adopting methods the org *already owns*: county-AMI + within-bracket
interpolation from `d_calc_ami`, a consistent monthly-cost test for both tenures (`B25094`/`B25063`),
named-variable joins instead of positional `rep()`, and surfacing the location quotient as the
headline index.

**What's missing?**
The three things the migration theory is actually about: **availability** (vacancy/turnover — you
can't move into an occupied unit), **barriers** (discrimination, vouchers, the scarlet E, credit),
and **integration** with EDR/EER so affordability can be intersected with displacement and eviction
pressure. Also missing: uncertainty handling (MOEs), input validation, and tests.

---

## 6. Prioritized remediation plan

**P0 — correctness/credibility blockers (do before any new report uses ownership numbers):**
1. Replace `× 0.188` with a monthly-owner-cost test (`B25094`) or explicit, year-specific mortgage
   parameters. (§3.1.1)
2. Replace median-of-tract-medians AMI with **county AMI** (optionally HUD AMI). (§3.1.2)
3. Add an **availability** measure (vacant-for-rent/-sale) so the index reflects movability. (§3.4.1)

**P1 — methodology & latent bugs:**
4. Stop summing `B25075 + B25085`; choose one universe. (§3.1.3)
5. Within-bracket interpolation; drop `closest()` snapping. (§3.1.4)
6. Fix the `class_pop` recycling filter. (§3.2.1)
7. Join limits to variables **by name**, not position. (§3.2.2)
8. Normalize/validate `state`/`counties` to FIPS; guard divide-by-zero. (§3.2.3, §3.2.5)
9. Emit standardized **ELI/VLI/LI** tiers; make output joinable to EDR/EER. (§3.4.3)
10. Add a barrier/voucher/eviction-record overlay (even a simple flag). (§3.4.2)

**P2 — hygiene & polish:**
11. Remove chatty `print()`s; specify all `by=`; rename `*_jenks_cat`. (§3.2.4, §3.3)
12. Add tests + a fixture; consolidate `get_acs()` calls; fix `@return`. (§3.3)
13. Surface a single named **opportunity index** column as the headline. (§3.4.4)

---

## 7. Proposed R package design

The user's stated goal is "an R package that calculates the affordability index." Two viable homes:

- **(A) Keep it in `neighborhood`** as a first-class, rebuilt `afford_index()` family. Lowest
  overhead; sits next to `ntdf()`/`get_co_puma()`, which it will want to join to.
- **(B) Spin out a dedicated `affordr`/`amindex` package** that depends on a shared ` erntools`/AMI
  core. Cleaner if the affordability index is going to grow the availability + barrier + EDR/EER
  integrations, which pull in more data sources than `neighborhood` should carry.

**Recommendation:** Build the engine **inside `neighborhood` first** (A) — promote `ami_fun()` to a
documented internal, rebuild `afford()` on it — and only spin out (B) once the availability/barrier
layers are real. Either way, target this public surface:

```r
# demand/tier engine (from d_calc_ami)
ami_tiers(state, counties, year, by_race = FALSE, tenure = c("all","renter"))

# supply by tier, with availability
afford_supply(state, counties, year, tenure = c("rent","own"),
              cost_basis = c("monthly","value"))     # monthly => B25063/B25094, no magic number

# the index: supply ⨯ demand ⨯ (optional) availability/barrier adjustments
afford_index(state, counties, year,
             ami_tiers = c("ELI","VLI","LI"),         # standardized, not a bare ratio
             availability = TRUE,                      # B25004 / vacant-for-sale
             barriers = NULL,                          # optional overlay (vouchers, EER, ...)
             geometry = FALSE)
```

Design rules:
- **One supply universe per call**, explicitly chosen; never sum stock + flow.
- **No hidden constants** — every economic assumption (interest rate, 30% ceiling, AMI basis) is a
  named, defaulted, *documented* argument.
- **Tidy output** with a stable schema and a single headline `opportunity_index` column plus the
  supporting `supply`, `ratio`, `available`, `barrier_adj` columns.
- **Tested** against a small bundled fixture so CI doesn't need a live Census API key.
- **Joinable** to `ntdf()`, EDR, and EER on `GEOID` + tier.

---

## Appendix A — derivation of the `0.188` constant

For a fully-financed 30-year fixed mortgage at annual rate *r*, the monthly payment per dollar
borrowed is `m = (r/12) / (1 − (1 + r/12)^−360)`; the **annual mortgage constant** is `12m`.
Setting required income so that the annual payment is 30% of income gives
`income = price × (12m) / 0.30`.

| Rate *r* | annual constant 12m | factor = 12m / 0.30 |
|---|---|---|
| 3.8% | ≈0.0564 | **≈0.188**  ← the hard-coded value |
| 4.0% | ≈0.0573 | ≈0.191 |
| 5.0% | ≈0.0644 | ≈0.215 |
| 6.0% | ≈0.0719 | ≈0.240 |
| 7.0% | ≈0.0798 | ≈0.266 |

The constant ignores down payment, property tax, insurance, PMI, and HOA, all of which raise the
required income further. A 2024 run at ~7% should use ≈0.27, not 0.188 — hence the ~30–40%
over-statement of ownership affordability.

## Appendix B — census tables referenced

| Table | Meaning | Universe | Used / proposed |
|---|---|---|---|
| `B19013_001` | Median household income | households | AMI base (use **county**, not median-of-tracts) |
| `B19001` | Household income (16 brackets) | households | demand (households by income) |
| `B19001A–I` | Household income by race/ethnicity | households | demand, equity cuts (from `d_calc_ami`) |
| `B25118` | Household income by tenure | occupied units | renter-specific demand (from `d_calc_ami`) |
| `B25075` | **Value** | **owner-occupied** units | ownership stock (don't mix with B25085) |
| `B25085` | **Price asked** | **vacant-for-sale / sold** units | for-sale flow (don't mix with B25075) |
| `B25063` | Gross rent ($) | renter-occupied units | rental supply (30% rule ✔) |
| `B25094` | Selected monthly owner costs ($) | owner-occupied units | **proposed** owner analog → kills `0.188` |
| `B25004` | Vacancy status | vacant units | **proposed** availability measure |
| `B25070` | Gross rent as % income | renter-occupied | optional rent-burden cross-check |
| `B25091` | Owner costs as % income (by mortgage status) | owner-occupied | optional owner-burden cross-check |
