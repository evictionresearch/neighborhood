# CLAUDE.md — neighborhood (R package)

The shared engine for all HPRM/ERN maps and charts (the `nt_*` functions: HPRM
maps, choropleths, affordability index, PD plots, profile widgets). Consumers
include `evictionresearch/hprm`, the state report repos, and
`cidrlab/hprm_sanmateo` — this package is the engine; those repos are consumers.
Code standards live canonically in `~/git/evictionresearch/library/CODE_PHILOSOPHY.md`
and `CODE_CONVENTIONS.md`.

<!-- BEGIN hprm-lineage (synced from evictionresearch/library/standards/claude-md-hprm-lineage.md — edit there, then run library/scripts/sync_claude_md_standard.py) -->
## HPRM lineage

When introducing the HPRM in any prose: decades of scholarship on displacement,
gentrification, segregation, urban planning, and demography, operationalized with
modern Bayesian tools (BART); developed by Tim Thomas and collaborators at UC
Berkeley's Urban Displacement Project during the pandemic, carried forward by ERN.
Public theory families (four only): place stratification and discrimination,
economic/affordability, household preference, and network theories of neighborhood
change. "Highways of migration" and ranking superlatives stay internal. Canonical
narrative: `cidrlab/library/ORG.md → HPRM Lineage`.
<!-- END hprm-lineage -->

<!-- BEGIN eviction-measurement (synced from evictionresearch/library/standards/claude-md-eviction-measurement.md — edit there, then run library/scripts/sync_claude_md_standard.py) -->
## What an eviction filing count does and does not measure

Court records are what the ERN mostly holds, and a filing count is the weakest
of the numbers we publish. Every rate built on one undercounts displacement,
by a factor that varies by place. State it on any page that reports filing
rates, and never let a low filing rate be read as housing security.

**Most forced moves never reach a court.** The 2017 American Housing Survey
puts the national ratio at **5.5 informal evictions for every formal one**.
Informal evictions were 72.3% of forced moves and formal evictions 13.1%; the
rest were fear of eviction after a missed payment (6.4%), landlord foreclosure
(5.0%), and condemnation (3.2%). The AHS national formal eviction rate of 0.8%
runs 65% below the 2.3% produced from court records.
— Gromis & Desmond (2021), *Estimating the Prevalence of Eviction in the
United States: New Data from the 2017 American Housing Survey*, Cityscape
23(2), pp. 279–290.
<https://www.huduser.gov/portal/periodicals/cityscpe/vol23num2/ch15.pdf>

**The ratio is not a constant, and that is the useful part.** Milwaukee ran at
2:1 informal-to-formal; the New York City/Newark MSA at 2.5:1 by one measure,
while a second NYC source found formal evictions almost twice as common as
informal. Gromis and Desmond attribute the reversal to "New York City's
uniquely robust tenant protections, which incentivize tenants threatened with
eviction to defend their case in court." **Where tenants have counsel and
protections, more displacement passes through the court and the filing count
measures more of it. Where they do not, the count measures less.** A
cross-place comparison of filing rates is therefore partly a comparison of
tenant-protection regimes, not only of housing precarity.

**The Milwaukee survey figures**, for the version that counts moves rather
than cases: 48% of forced moves were informal evictions, 24% formal, 23%
landlord foreclosure, 5% condemnation. Its authors concluded that estimates
built on eviction court records "are considerable underestimates."
— Desmond (2015), *Unaffordable America: Poverty, housing, and eviction*, IRP
Fast Focus 22-2015, p. 3.
<https://www.irp.wisc.edu/publications/fastfocus/pdfs/FF22-2015.pdf>

**Who this undercount falls on.** Renters without a written lease, and renters
for whom the courts carry immigration risk, are least likely to appear in a
court record at all. That is why Latine filing rates commonly read at or below
white rates in court data while displacement runs the other way. The full
treatment, with verified quotations and the countervailing evidence that some
of the gap is protective rather than hidden, is in
`library/standards/latine-filing-rates-note.md`. Read it before writing any
profile section that reports rates by race.

**How to write it.** Filings, judgments, notices, and lockouts are separate
stages and are not interchangeable. A rate per 1,000 renter households counts
filings and not households, so a unit filed on repeatedly counts each time.
Say what the number counts, say what it misses, and give the reader the ratio
rather than leaving the undercount implicit.
<!-- END eviction-measurement -->

<!-- BEGIN factual-accuracy (synced from evictionresearch/library/standards/claude-md-factual-accuracy.md — edit there, then run library/scripts/sync_claude_md_standard.py) -->
## Factual accuracy — non-negotiable

Everything must be factually true. This overrides helpfulness, completeness, and the urge to sound finished.

- **Never fabricate** — no invented numbers, citations, file paths, function names, API behaviors, or results. If you don't know, say so.
- **Verify before asserting** — ground every factual claim in something checked this session (a file read, a command run, a source fetched). Don't assert from memory when the answer is checkable.
- **Label thought exercises** — open any speculation or hypothetical with an explicit **[Thought exercise]** marker so it's never mistaken for fact.
- **Mark confidence when it matters** — for consequential claims you couldn't fully verify, flag the uncertainty and how to confirm it. Distinguish *verified* (checked) from *inferred* (reasoned) from *assumed* (unchecked).
- **Build only on solid ground** — analysis must rest on prior facts or analysis already established correct; flag unverified dependencies before building on them.
- **Report outcomes honestly** — failures, skipped steps, and partial results get stated plainly with evidence. Never round a partial result up to "done."
<!-- END factual-accuracy -->
