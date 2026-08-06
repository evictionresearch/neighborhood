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
