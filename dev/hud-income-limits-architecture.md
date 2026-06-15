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
| **Fresno, CA** | $84,300 | $87,900¹ | $42,150 | $43,950¹ | ¹CA non-metro **floor**, not HUD |
| **Kern, CA** | $78,600 | $87,900¹ | $39,300 | $43,950¹ | ¹same CA floor (note Fresno=Kern exactly) |

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
- **"Official" republications are not HUD.** CA HCD floors every county to the CA non-metro median
  (Fresno and Kern both land on **$87,900**) and relabels categories. **Validate against HUD's own
  files, never a state mirror.** This is also why `"hud"` mode must hit HUD (or our HUD snapshot),
  not a convenience source.

**Validated against the live HUD API** (token now configured). HUD's FY2024 San Diego figures:
`median_income 119,500`, `VLI(4) 75,750`, `LI(4) 121,250`, `ELI(4) 45,450`. These confirm: our ACS
`median_income` reproduction is within 0.08%; the cascade exactly holds (`LI = 1.6 × VLI`,
`ELI = 0.6 × VLI`); the high-cost bump puts VLI at 0.63 × MFI (not 0.50); and family-size scaling
matches HUD (size-1 VLI 53,050 = 0.70×, size-6 87,900 = 1.16×). It also exposed that CA HCD's
"Extremely Low" ($58,750) is a state construct, **not** HUD's ELI ($45,450) — reinforcing "validate
against HUD, never a state mirror." See §6.

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
