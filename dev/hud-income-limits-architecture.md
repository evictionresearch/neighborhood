# HUD Income Limits: pulling, reproducing, and surviving HUD going dark

**Purpose:** decide how the affordability index sources Area Median Income (AMI) tiers
(ELI/VLI/LI), how we *reproduce* HUD's official limits ourselves, and how we keep the whole
thing working if HUD deprecates the API or — a live risk under the current administration —
pulls the data offline. Builds on [affordability-index-review.md](affordability-index-review.md).

---

## 1. The decision: three AMI sources, default ACS

`ami_source` argument, in order of dependency on HUD:

| Mode | What it is | HUD dependency | Matches HUD? |
|---|---|---|---|
| **`"acs"`** *(default)* | tiers = fractions × **county ACS median family income** (B19113); ELI/VLI/LI = 0.3/0.5/0.8, with within-bracket interpolation | **none** (Census only) | In *typical* areas yes; **diverges in high-cost & very-low-income areas** (see §3) |
| **`"hud"`** | official published HUD limits, pulled live via `hudr`, **with a bundled annual snapshot fallback** | API at run time, but snapshot makes it durable | Exact (it *is* HUD) |
| **`"hud_acs"`** | our **full reproduction** of HUD's cascade from ACS + FMR + poverty guidelines + the CBO trend factor | none at run time (needs the inputs, all archivable) | Within rounding, *including* high-cost areas — this is the longevity insurance |

Default `"acs"` keeps the package self-contained and transparent. `"hud"` is for reports that
must match HUD's published numbers verbatim. `"hud_acs"` is what we fall back to if HUD's API/site
disappears and we still need HUD-equivalent limits.

**Important correction to the earlier review:** HUD anchors on **median *family* income (ACS
`B19113`)**, not household income (`B19013`). The `"acs"` mode should use `B19113` for AMI tiers to
stay comparable to HUD, even though the *demand* side (households by income, `B19001`) and the
deployed reports think in households. Document the family-vs-household distinction explicitly.

---

## 2. How HUD actually builds the limits (the architecture to replicate)

Verified against HUD's annual methodology PDFs (FY2022–FY2026). Everything cascades from the
**4-person Very Low-Income (50%) limit**:

1. **Area MFI** = (ACS `B19113` median family income for the area, vintage = **FY − 2**) × **trend
   factor**, rounded to nearest $100.
   - Trend factor = **CBO per-capita wage-growth forecast** for **FY2025+**; CBO **CPI-U** forecast
     for FY2015–FY2024 (BLS CPI-U the FY2022 exception). *This single national scalar is the #1
     reproduction risk.* FY2024 = **1.062**; FY2025 = **1.08**.
   - 1-yr ACS if MOE < ½ estimate and ≥100 responses; else current+prior-two **5-yr** averaged;
     else fall up to the containing geography (county → state non-metro).
2. **VLI(4)** = 50% MFI, then **in order**: high-housing-cost bump → high-income reduction →
   state non-metro floor → year-over-year cap/floor.
   - High-cost bump: `VLI = max(VLI, monthly_2BR_FMR × 29.1429)` (= FMR×12×0.85/0.35).
   - Cap/floor: ≤5% drop; rise ≤ max(5%, 2×national-MFI-change), absolute cap **10%** (FY2024+).
3. **LI(4)** = **1.6 × VLI(4)**, capped at the **US median family income** (FY2024 = $97,800;
   FY2026 = $107,900) *unless high housing costs justify exceeding it*; then its own cap/floor.
4. **ELI(4)** = `min( VLI, max( 0.6 × VLI, HHS_poverty_guideline ) )`. Poverty floor added FY2014;
   AK/HI use different guidelines; territories excluded.
5. **Family sizes** from the 4-person base: **70 / 80 / 90 / 100 / 108 / 116 / 124 / 132 %** for
   1–8 persons; +8 pts/person beyond 8. ELI follows poverty-dollar steps when the floor binds.
6. **Round every published figure up to nearest $50.**

Geography = **FMR areas** (metros, HUD-defined HMFA sub-areas, and non-metro counties). Every
county maps to exactly one limit area. OMB delineation vintage changed at **FY2025** (Bulletin
18-04 → 23-01, incl. the Connecticut planning-region restructure) — area definitions are *not*
stable across that boundary.

Authoritative sources (download and archive these — see §5):
`huduser.gov/portal/datasets/il/il{YY}/IncomeLimitsMethodology-FY{YY}.pdf` and
`.../Medians-Methodology-FY{YY}.pdf`.

---

## 3. Validation: how close is our reproduction? (test run, FY2024)

Reproduced 4-person limits from 2022 1-yr ACS `B19113` × 1.062, vs. published figures:

| Area | Repro MFI | Official AMI | Naïve VLI (0.5·MFI) | Official VLI(4) | Note |
|---|---:|---:|---:|---:|---|
| **San Diego, CA** (area = county) | **$119,600** | **$119,500** | $59,800 | **$75,750** | MFI exact (0.08%); VLI **+27%** from FMR high-cost bump |
| **Fresno, CA** | $84,300 | $87,900¹ | $42,150 | $43,950¹ | ¹state non-metro **floor** — in HUD's own limits (see §7.5) |
| **Kern, CA** | $78,600 | $87,900¹ | $39,300 | $43,950¹ | ¹same floor (note Fresno=Kern exactly) |

Findings:

- **The MFI engine is right.** Where the HUD area equals the county and no floor binds (San Diego),
  `ACS B19113 × CBO trend` reproduces HUD's median to **0.08%**. The architecture works.
- **High-cost areas need the FMR bump — and it's large.** San Diego's real VLI is **27% above**
  naïve 50%-of-MFI. A simple `0.5 × ACS median` (the tempting `"acs"` shortcut) **materially
  understates affordability thresholds in exactly the high-cost regions ERN works in** (Bay Area,
  San Diego, coastal UT). So: in high-cost regions prefer `"hud"`/`"hud_acs"`, or implement the FMR
  bump in `"acs"` mode and label it.
- **The cascade checks out.** Official LI(4) $121,250 ≈ 1.6 × $75,750 — confirming HUD anchors LI on
  VLI (and here even *exceeds* the US-median cap via the high-cost exception).
- **The non-metro floor is HUD's, and state mirrors add their own layers.** Fresno and Kern both
  land on **$87,900** because HUD floors low-income areas to the state non-metro median — HUD's own
  published Fresno VLI(4) is 43,950 = 0.5 × 87,900, so the floor is in HUD's files, not a state
  edit (corrected 2026-08-17; see §7.5). What state republications DO add: extra categories (CA:
  acutely-low, moderate) and hold-harmless floors that can hold a state table above HUD in down
  years (verified for FY2025, §7.4). **Validate against HUD's own files, never a state mirror** —
  and know which schedule the program you are modeling actually uses. This is also why `"hud"`
  mode must hit HUD (or our HUD snapshot), not a convenience source.

**Validated against the live HUD API** (token now configured). HUD's FY2024 San Diego figures:
`median_income 119,500`, `VLI(4) 75,750`, `LI(4) 121,250`, `ELI(4) 45,450`. These confirm: our ACS
`median_income` reproduction is within 0.08%; the cascade exactly holds (`LI = 1.6 × VLI`,
`ELI = 0.6 × VLI`); the high-cost bump puts VLI at 0.63 × MFI (not 0.50); and family-size scaling
matches HUD (size-1 VLI 53,050 = 0.70×, size-6 87,900 = 1.16×). [Corrected 2026-08-17: this
paragraph previously called CA HCD's "Extremely Low" a state construct by holding a $58,750 figure
against HUD's San Diego ELI ($45,450) — but $58,750 is the SF-HMFA/San Mateo 4-person ELI, a
different area. HCD's 2024 San Diego row (ELI 45,450 / VLI 75,750 / LI 121,250) is identical to
HUD's; HCD's ELI/VLI/LI are HUD's numbers except where a state floor or hold-harmless binds. The
genuinely state-only categories are Acutely Low and Moderate. See §7.4–7.5.]

---

## 4. The R packages: what we found

- **`hudr`** (CRAN) installs cleanly on R 4.6 and exposes the right surface: `get_hud_il_data()`,
  `get_hud_il_statedata()`, `get_hud_fmr_data()`, `get_hud_chas_data()`, plus list/lookup helpers.
  It reads `HUD_API_KEY` from the environment (exactly like `tidycensus`/`CENSUS_API_KEY`).
- **`rhud`** (the ropensci-reviewed one) **failed to install on R 4.6** ("not available for this
  version of R") — treat as unmaintained; do not depend on it.
- **Neither package ships any offline data** — both are thin clients over `huduser.gov/hudapi`.
  *If HUD's API goes away, these packages return nothing.* That is the core longevity problem.

So: use `hudr` for the live `"hud"` path (in `Suggests`, optional), but **do not rely on it for
durability** — durability comes from our own snapshots and reproduction (§5).

---

## 5. Longevity plan (assume HUD may go dark)

Layered, so each layer fails over to the next:

1. **Bundle annual snapshots in the package.** Each year, pull the full national IL + FMR tables
   once and commit them as compressed data (`data/hud_il_2024.rda`, …) with provenance (pull date,
   source URL, methodology-PDF hash). The `"hud"` path reads the snapshot when the API is
   unreachable. *This alone guarantees every year we've ever captured keeps working forever.*
2. **Archive the primary sources, now.** Mirror, per year: the IL data file, the FMR file, the two
   methodology PDFs, the HHS poverty guidelines, and the CBO trend factor — into a versioned store
   (our own S3/Zenodo deposit with a DOI, mirroring the package's existing Zenodo practice). HUD's
   site is already behind a bot-wall that blocks scripted download (it returns HTTP 202 with no
   body), so *capture proactively while the data is still public.*
3. **Keep the reproduction engine (`"hud_acs"`) in code and tested.** As long as we have ACS
   (`B19113`), FMR, poverty guidelines, and the trend factor, we can regenerate HUD-equivalent
   limits without HUD. ACS is a separate agency (Census) — diversifying the dependency.
   - If **FMR** also disappears, approximate the high-cost bump from ACS gross-rent percentiles
     (`B25063`) — document it as an approximation, validated against the last good FMR year.
   - If the **CBO trend factor** is the only gap, it's a single national number per year — trivially
     archivable and even forecastable from public CPI/wage series.
4. **Snapshot test.** A CI test asserts our bundled snapshots + `"hud_acs"` reproduction still match
   the last-known-good HUD figures within rounding, so drift/regressions surface early.

Net: `"acs"` needs only Census; `"hud_acs"` needs Census + archived FMR/poverty/trend; `"hud"`
needs the API *or* our snapshot. **No single point of failure is HUD's live site.**

---

## 6. Status: `hud` source wired, validated, and snapshot-ready

Done:

- `HUD_API_KEY` configured (in `~/.Renviron`, alongside `CENSUS_API_KEY`).
- `ami_cutoffs(..., ami_source = "hud")` and `afford_index(..., ami_source = "hud")` implemented.
  ELI/VLI/LI come straight from HUD's per-household-size limits (caps/floors/high-cost baked in);
  tiers with no HUD limit (e.g. MI = 120%) are derived and labelled as such. Household sizes 1-8
  (default 4) select HUD's published column directly.
- The path is **snapshot-first, API-fallback**: it reads a bundled `hud_il_<year>` dataset when
  present (offline, deterministic) and only calls the live API for counties/years not in it.
- API errors are caught and re-raised **without** the original message, so a leaked URL can't expose
  the key.
- Snapshot builder written: `data-raw/build_hud_il.R` (one county per API call; saves
  `data/hud_il_<year>.rda`).

Remaining for full longevity:

1. **Run the snapshot build** (`Rscript data-raw/build_hud_il.R 2024`) and commit the `.rda`. This is
   the durable artifact — once bundled, `"hud"` works with no API and no key.
2. **Archive the primary sources** (IL/FMR files, methodology PDFs, poverty guidelines, CBO factor)
   per §5.2.
3. **Implement `"hud_acs"`** (full-cascade reproduction incl. the FMR high-cost bump) as the
   no-HUD-at-all insurance — now straightforward since every constant and rule is validated above.

---

## 7. Definitions, statute, and the family-size mechanics (verified 2026-08-17)

Re-derived from primary sources on 2026-08-17 during the San Mateo engagement
(`cidrlab/hprm_sanmateo`); every figure below was fetched or recomputed that day. The §3
corrections dated the same day are resolved here.

### 7.1 What the tiers are (and where they come from)

- **AMI** as housing programs use it = HUD's **median family income (MFI)** estimate for the FMR
  area containing the county (metro, HMFA sub-area, or non-metro county) — not a Census
  household-income median. §2 covers how MFI is built (B19113, FY−2, trend factor).
- **LI / VLI are statutory.** 42 U.S.C. § 1437a(b)(2): low-income families are those "whose
  incomes do not exceed 80 per centum of the median income for the area, as determined by the
  Secretary with adjustments for smaller and larger families"; very low-income is the same wording
  at 50 percent. 24 CFR 5.603(b) carries both into program regulations. Both clauses also let HUD
  set ceilings "higher or lower" on findings about area incomes or housing costs — that clause is
  what makes published limits lawful even when they are not plain fractions of the median
  (high-cost bumps, floors, caps; §2).
- **ELI** entered with the Quality Housing and Work Responsibility Act of 1998 (a 30% standard),
  was tied to the VLI limits in 1999, and was redefined by the Consolidated Appropriations Act,
  2014 as the **greater of** the size-adjusted 30% limit **or the HHS poverty guideline for that
  family size**, **capped at the VLI limit** (HUD FY24 methodology PDF p. 6; FY26 p. 5).
- **MI (120%)** is not a HUD Section 8 limit — HUD publishes 30/50/80 only. Our MI is a derived
  convenience tier (fraction × median × size factor), and the code labels it as such.

### 7.2 Four-person anchor, size-dynamic application

The answer to "is 'low income' a family-of-four number, or dynamic by household size?" is
**both, in a specific order**:

- **Construction is 4-person-anchored.** HUD computes the 4-person VLI, applies every adjustment
  and cap there (§2), then generates sizes 1–8 by fixed factors —
  **70 / 80 / 90 / base / 108 / 116 / 124 / 132%**, +8 points per person past eight — with every
  published figure **rounded UP to the nearest $50**. "HUD makes no additional adjustments to
  limits for families of more than or less than four persons for the very low-income limit,
  except that these are then rounded up to the nearest $50" (FY24 PDF p. 8; factor table p. 7).
  Since FY2024, size-adjusted results are re-tested against the cap/floor rules.
- **Application is size-dynamic.** Eligibility is determined at the household's **actual** size
  against that 1–8 schedule (CA HCD instructions page: "Determine eligibility based on actual
  number of persons in household"). The 4-person column is the quoting convention because it IS
  the base (MFI ≈ 2 × VLI(4)).
- **Mechanical check (FY2024, San Mateo / SF HMFA):** all 24 published values (ELI/VLI/LI ×
  sizes 1–8) reproduce exactly from the three published 4-person bases via
  `round50_up(base × factor)` plus the ELI rule in §7.3. Scope note: this verifies the size
  schedule GIVEN the published 4-person bases; the bases themselves carry HUD's
  caps/floors/high-cost adjustments and their own rounding and are not plain fractions — e.g.
  Yuba's published 30% base is 26,350 where 0.6 × VLI(4) = 26,370 (a nearest-$50, not up-$50,
  artifact).

### 7.3 ELI and the poverty line

ELI is the one tier with a **dollar floor** instead of pure percentages:

    ELI(size) = min( VLI(size), max( round50_up( base30(4) × factor(size) ), poverty_guideline(size) ) )
    where base30(4) is HUD's PUBLISHED 4-person 30% limit (≈ 0.6 × VLI(4) under HUD's own rounding)

The HHS poverty guidelines step in **flat dollars per person** ($5,380/person in 2024, 48 states
+ DC; Alaska and Hawaii have their own schedules, e.g. AK +$6,730/person), while the size factors
step in percentages — so wherever the floor binds, the ELI row stops tracking the factor table.
Verified against published FY2024 rows (all 8 sizes each):

| Area | Poverty floor binds at | Reading |
|---|---|---|
| San Mateo / SF HMFA | never | 30% of a $186,600 median clears poverty at every size |
| Alpine County (AMI $119,300) | sizes 5–8 | percent steps fall below poverty's flat steps for larger households |
| Yuba County (AMI $87,900, the non-metro floor) | sizes 3–8 | low-income area; 4-person ELI = the poverty line exactly ($31,200) |

Reading: in affluent areas ELI behaves like a pure 30% tier; in poor areas ELI **converges to the
poverty line** — which was the 2014 amendment's purpose: 30% of a low median can fall below
poverty, and the floor keeps deeply poor families inside the eligibility system. The VLI cap
covers places so poor that poverty exceeds VLI (HUD then sets ELI = VLI; it elects this for all
of Puerto Rico since FY2023 — FY26 PDF p. 5). Two consequences for us: `.afi_poverty_guideline`
must stay year-versioned and would need AK/HI schedules if the index ever leaves the lower 48;
and `.afi_cascade_row` computes the 30% base inline as 0.6 × VLI(4) without HUD's base rounding,
a ≤$50 divergence in `hud_acs` reproduction mode (within its documented tolerance, but known).

### 7.4 The state republication layer (California as the worked example)

States commonly republish HUD's limits with statutory add-ons. California (HCD "State Income
Limits," Cal. Code Regs. tit. 25 § 6932):

- **Adds categories HUD does not have**: Acutely Low (15% of AMI, H&SC 50063.5) and Moderate
  (120%, H&SC 50093). These ARE state constructs.
- **Keeps ELI/VLI/LI = HUD's figures** except where a state adjustment binds: the non-metro
  floor (also inside HUD's own limits — §7.5) and HCD's 2013 **hold-harmless** policy (post-2009
  values never decrease year-over-year).
- **Hold-harmless divergence, verified**: HUD's FY2025 SF-HMFA 4-person limits FELL (ELI 58,000 /
  VLI 96,700 / LI 154,700) while HCD's 2025 San Mateo row held at the 2024 values (58,750 /
  97,900 / 156,650). Same county, same year, same category names, different numbers — federal and
  state program eligibility genuinely diverged in 2025. (HUD's FY2026 then jumped past both:
  63,050 / 105,050 / 168,100.)
- **Which schedule governs is a program attribute.** San Mateo County's own limits sheet prints
  the federal and state schedules side by side: federal programs use the federal schedule,
  state/local programs the state one, mixed funding "the more stringent income levels."
- **Rent-setting is a third thing.** CA sets affordable *rents* at a benchmark household size
  derived from bedrooms (bedrooms + 1 unassisted; 1.5 × bedrooms federally assisted; H&SC
  50052.5/50053), never the tenant's actual size. Eligibility basis ≠ rent basis.

Whether the other 49 states floor, hold harmless, relabel, or add categories is an open
question — issue #21 (filed 2026-08-17) recommends an agent sweep to build a state registry.

### 7.5 Corrections log + source ledger (2026-08-17)

Corrected in §3: (a) the Fresno/Kern footnote — the $87,900 floor is present in HUD's OWN
published limits (HUD FY24 HOME/IL: Fresno VLI(4) = 43,950 = 0.5 × 87,900; $87,900 is HUD's CA
non-metro median per the HCD memo's HUD figures), HCD mirrors it; (b) the closing ELI sentence —
it compared San Diego's HUD ELI ($45,450) to San Mateo's ELI ($58,750), two different areas.

Primary sources (all fetched or re-derived 2026-08-17):

- 42 U.S.C. § 1437a(b)(2) — law.cornell.edu/uscode/text/42/1437a
- 24 CFR 5.603(b) — law.cornell.edu/cfr/text/24/5.603
- HUD FY2024 IL methodology — huduser.gov/portal/datasets/il/il24/IncomeLimitsMethodology-FY24.pdf
  (high-cost trigger 35%/85% of 2BR FMR: PDF p. 2; LI = 1.6 × VLI + US-median cap $97,800: p. 4;
  ELI / poverty floor / 2014 act: p. 6; factor table + round-up-$50: p. 7;
  no-additional-adjustments: p. 8)
- HUD FY2026 IL methodology — same path il26/…-FY26.pdf (same items at PDF pp. 2, 4, 5, 6, 7;
  US-median cap $107,900; Puerto Rico ELI=VLI election: p. 5)
- HUD Adjusted HOME income limits, CA, 2024/2025/2026 —
  huduser.gov/portal/datasets/home-datasets/files/HOME_IncomeLmts_State_CA_{year}.pdf (SF HMFA
  rows on PDF p. 3 in each; HOME 30/50/80 == Section 8 ELI/VLI/LI verified line-for-line for
  FY2024 against the SMC federal sheet; the FY25/26 equivalence is by construction, not
  re-verified)
- HHS 2024 poverty guidelines — acf.gov "2024 HHS Poverty Guidelines" PDF (48-state schedule,
  +$5,380/person; AK/HI separate)
- CA HCD State Income Limits, 2024 (memo 2024-05-09) and 2025 (memo 2025-04-23) —
  hcd.ca.gov/…/income-limits-{2024,2025}.pdf
- San Mateo County 2024 income limits sheet (rev. 2024-05-14) — smcgov.org/media/149150/download
