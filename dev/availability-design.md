# Pane 2 — Availability: "can they actually get in?"

**Date:** 2026-07-03. **Status:** design + verified feasibility; build after Tim reviews the
pane-1 evaluation maps. **Context:** the product is **three panes — affordability →
availability → stability** (decided 2026-07-03; screening/records — the scarlet-E gate — stays a
documented future overlay, not a fourth pane yet). Pane 1 (the verdict map, `afford_map()` +
`afford_caption()`) answers *"what would they pay if they could move in?"* at incumbent prices.
Pane 2 answers the two things pane 1 deliberately assumes away: **is anything open, and at what
price does the door actually open?**

---

## 1. Three components

### A. Openings — BUILT
Already in the engine (`afford_index(availability = TRUE)`): per tract, **turnover** (`B07013`
past-year movers → `available_turnover`, the annual flow) and **vacancy** (`B25004`
vacant-for-rent → `available_vacancy`, open right now), with the per-100/shortfall arithmetic in
`afford_capacity()`. King County VLI: 86.8 affordable units per 100 households on paper; **24.8
per 100 open in a year; 4.6 per 100 open now.**

### B. Entry price — NEW (the Zillow layer)
Pane 1 prices tracts at what *current tenants* pay (ACS `B25063` standing gross rents). Movers
face **asking rents**, which run higher — the locked descriptor's "entry prices run higher
still." This component measures that wedge and re-scores affordability at the ticket window.

**Source, verified live 2026-07-03:** Zillow Observed Rent Index (ZORI), ZIP-level public CSV —
`https://files.zillowstatic.com/research/public_csvs/zori/Zip_zori_uc_sfrcondomfr_sm_month.csv`
— **8,444 ZIPs, monthly 2015-01 through 2026-05** (file last modified 2026-06-16), smoothed
typical asking rent, single-family + condo + multifamily.

**The wedge is large. King County teaser (computed 2026-07-03):** all 69 King ZIPs carry ZORI;
median ZIP asking rent **$2,230**/mo. Against the VLI lines (HUD VLI-4 $75,350 → 30% =
$1,884/mo, 50% = $3,140/mo): **6 of 69 ZIPs** are asking-affordable at 30%, 62 of 69 at the 50%
stretch. Compare pane 1: **28% of tracts** (139/494) read "affordable" at standing rents vs
**~9% of ZIPs** at asking — most of the green class shrinks materially at the ticket window.
*(ZIP ≠ tract and ZORI is the typical listing, not the cheap tail — read as direction + rough
magnitude until the tract build below.)*

**Method — ratio-shift, not levels.** ZORI is a smoothed *mean of listed units* (mid-market,
multifamily-weighted); using its level as "the tract's entry rent" would mis-state the cheap
segment. Instead estimate each tract's **entry premium** and shift the standing distribution:

```
premium(zip)   = ZORI(zip, latest) / standing_rent(zip)      # ACS B25063 median, ZIP-aggregated
premium(tract) = block-HU-weighted average of its ZIPs' premiums
entry_cost(tract, bracket) = standing bracket bounds × premium(tract)
entry_supply30/50, entry_verdict = the same 30%/50% verdict grammar, at entry prices
```

**ZIP → tract:** adapt the package's own block-exact machinery (`nt_areal_weight()`): 2020
census blocks nest within both 2020 tracts and 2020 ZCTAs, so (zcta × tract) housing-unit
weights are exact, no areal approximation — generalize the place-crosswalk internals to take
ZCTA polygons (`tigris::zctas()`), or use HUD's USPS ZIP–tract crosswalk when the HUD API is
back (it is down as of 2026-07-02).

**Validation / triangulation:**
- ACS **recent-mover rents** (gross rent by year householder moved in — an ACS-native entry-rent
  signal at tract level; *verify the table id (likely B25113) and tract availability before
  building on it*);
- HUD **FMR** (by construction a 40th-percentile *recent-mover* gross rent) at metro/ZIP scale,
  when the API returns;
- face checks: premiums highest in hot cores (Seattle, SF), ≈1 in soft markets; premium < 1
  flagged for review (possible in declining submarkets, but rare).

### C. Competition — PLANNED (the CHAS layer)
An affordable unit occupied by a higher-income household is not available. NLIHC's Gap
(verified from the March 2026 PDF): of 7.2M units affordable to ELI renters nationally, only
**3.8M are "affordable and available."** Tract source: HUD **CHAS** (households by income ×
units by affordability, 30/50/80% HAMFI, tract-part geography; confirm current vintage —
2017–2021 as of the last check). Produces the competition-adjusted per-100 —
NLIHC-comparable — and closes gap #4 from `affordability-deep-dive.md`.

## 2. The pane-2 product

- **Map:** the availability verdict — the same three-class grammar as pane 1, but at **entry
  prices and open units**: green = a VLI household could realistically land here this year at
  ≤30% of income; orange = only by stretching; red = effectively closed. A diagnostic **entry
  premium** layer (asking ÷ standing) shows where pane 1 most understates.
- **Numbers (the capacity line):** "of every 100 VLI renter households, X could find an open
  unit this year at ≤30% of income at today's asking rents" — pane 1's 86.8/100 stepped down
  by openings (24.8) and entry pricing (TBD), then by competition (CHAS).
- **Reading + what-ifs**, mirroring `afford_caption()`: *this is whether a [tier] household can
  actually get in this year* — assuming they are accepted (screening still excluded, stated).

## 3. API sketch

```r
zori <- afford_zori()                        # download + cache the ZIP CSV (verified URL)
w    <- nt_zcta_weights(state, counties)     # block-exact zcta x tract HU weights
x    <- afford_index(..., geometry = TRUE) |> afford_verdict()
x    <- afford_entry(x, zori, weights = w)   # + entry_premium, entry_supply30/50, entry_verdict
afford_capacity(x)                           # gains per-100-at-entry columns
afford_map(x, verdict = "entry_verdict")     # pane-2 map (afford_map gains a column switch)
```

## 4. Phasing

1. `nt_zcta_weights()` (generalize the block crosswalk) + `afford_zori()` (download/cache/tidy).
2. `afford_entry()` — premium, shifted verdict, capacity columns; King + Bay validation incl.
   the recent-mover ACS cross-check.
3. Pane-2 map + caption; add to the evaluation document next to pane 1.
4. CHAS competition layer (component C) → the NLIHC-comparable per-100.

**Honest caveats to carry:** ZORI covers listed units (thin in rural/small markets — fall back
to county premium where a tract's ZIPs lack ZORI); smoothed index ≠ lease-level; ZCTA ≠ USPS
ZIP exactly; standing rents are a 5-year ACS average while ZORI is monthly — the premium mixes
a timing gap with a true mover premium (both push entry costs up; disentangle with the
recent-mover table); premiums applied uniformly within a tract's rent distribution (the cheap
tail may reprice faster in gentrifying areas — flag, don't hide).
