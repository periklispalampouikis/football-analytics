# Debrief 04: Which side do they attack down?

**Match:** Kestrel Bay FC 2–0 Redfern Athletic (fictional practice data from The Portal)
**Report:** [`width-report-kestrel-bay.pdf`](width-report-kestrel-bay.pdf)
**Code:** [`04-width.R`](04-width.R), one script that produces all figures

## Main findings (open play only)

- Kestrel Bay enter the final third most often down the left (76 of 194 entries, 39%), just ahead of the centre (70, 36%).
- The left depends on one player: winger Niall Sorrell makes 37 of the 76 left entries, 34 of them carries.
- Counting passes only, the centre leads. The left-side finding depends on including carries.
- Central possessions reach the box more often (56% vs 39%), but shot rates are the same (12% vs 11%). With 22 to 28 possessions per channel, this is a hint, not a finding.

## Method

- Final-third entry: completed pass or carry ending at x ≥ 80 (120 × 80 pitch).
- Channels: cut at y = 80/3 and 160/3.
- Box and shot rates per possession, with 95% Jeffreys intervals.
- Sensitivity: four ways of counting (all entries, passes only, carries only, one per possession).

Data is not included. Put the CSVs in `data/raw/portal/` to run the script.
