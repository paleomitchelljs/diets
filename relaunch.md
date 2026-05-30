# RELAUNCH — running the Bayesian fits

## CURRENT STATE (2026-05-29)

- `data/fit_simple.rds` — **done** (4 chains × 2000 post-warmup, finished 07:36 May 29).
  Summarize with `Rscript code/x04_summariseFit.R data/fit_simple.rds`.
- `data/fit_phylo.rds` — **running** under `nohup caffeinate -is` (started ~07:36 May 29).
  R PID ~93334 (verify via `lsof data/fit_phylo.log`). Expected wall-clock ~40h baseline,
  ~2 days with load spikes; ETA roughly ~31 May. Survives Claude session closure.

**To pick up in a fresh Claude session** (the original task ID `bzmvptelh` is session-scoped
and gone, but the process is still alive):

```bash
cd "/Users/jmitchell/Library/CloudStorage/Dropbox/Research/DietProject"
ls -la data/fit_phylo.rds           # exists ⇒ finished
stat -f '%Sm' data/fit_phylo.log    # last write
lsof data/fit_phylo.log | head -3   # still attached ⇒ alive
tail -5 data/fit_phylo.log          # latest iter print
```

When `fit_phylo.rds` appears, summarize with:
```bash
Rscript code/x04_summariseFit.R data/fit_phylo.rds
```

---



> **Delete this file once the fits have run successfully.** It's a temporary
> runbook, not part of the project.

## Why the fits aren't done yet

On 2026-05-26/27 we launched the two Stan fits and they **stalled** — the
phylogenetic fit sat at 20% warmup for ~8 hours without advancing. The cause was
**not the models**: this is a shared host (`COE\Domain Users`) and it was
saturated, **load average ~130–150**. The Stan chains were alive but starved of
CPU. Running *two* fits at once (8 chains) on top of that external load made it
worse. We stopped both jobs cleanly; nothing usable was written (brms only saves
on completion).

**Lesson for relaunch:** check the machine load first, and run the fits **one at
a time**, not concurrently.

## Pre-flight (10 seconds)

```bash
cd "/Users/jmitchell/Library/CloudStorage/Dropbox/Research/DietProject"
uptime    # look at the 1-minute load average (first number)
```

- 1-min load **< ~8**: good to go.
- 1-min load **high (dozens+)**: wait — the box is busy with other users' jobs;
  the fit will crawl. Re-check later.

The pipeline inputs already exist (`data/longform.csv`, `data/phylo_A.rds`, etc.).
You only need to rebuild them if `data/raw_data.csv` or `data/mass_overrides.csv`
changed:

```bash
Rscript code/x00_reformatData.R       # rebuilds data/longform.csv
```

## Run the fits (one at a time)

The scripts use brms's default **rstan** backend, which works on this machine.
(Do **not** switch to cmdstanr — its subprocess launch is blocked here.)
During compilation you'll see harmless `xcrun ... couldn't create cache file`
warnings — ignore them, the compile succeeds.

**Option A — simple model first (faster, good smoke test):**
```bash
RUN_BRMS=TRUE Rscript code/x01_simpleAnalysis.R   # -> data/fit_simple.rds
```
4 chains × 3000 iter (2000 post-warmup). Expect tens of minutes on a free machine.

**Option B — the phylogenetic model (the main result):**
```bash
RUN_BRMS=TRUE Rscript code/x02_phyloModel.R       # -> data/fit_phylo.rds
```
4 chains × 2000 iter (1000 post-warmup), `adapt_delta=0.95`. This is the heavy one
— budget **a few hours** even on a free machine (171k rows + 167×167 phylogenetic
covariance + correlated clade slopes).

**Recommended:** run them sequentially so they don't fight for cores. To launch
the phylo fit in the background of your own terminal and keep it alive after you
log out:
```bash
nohup bash -c 'RUN_BRMS=TRUE Rscript code/x02_phyloModel.R' > data/fit_phylo.log 2>&1 &
tail -f data/fit_phylo.log      # watch progress; Ctrl-C just stops watching
```

A run is healthy if the log keeps printing new `Iteration: N / ...` lines. If the
timestamp on `data/fit_phylo.log` stops advancing for a long time, the machine is
overloaded again — check `uptime`.

## After a fit finishes

Summarise it (fixed effects, group SDs with Rhat/ESS, phylogenetic-signal lambda,
and a flag for any Rhat > 1.05):

```bash
Rscript code/x04_summariseFit.R data/fit_phylo.rds
Rscript code/x04_summariseFit.R data/fit_simple.rds
```

Then tell Claude "the fits are done" — it can load the `.rds` files and produce
full posterior summaries, diagnostics, clade-slope contrasts, and plots.

## Convergence caveats to expect

- **Few-level groups.** `clade` and `obstype` each have only **4 levels**, so
  their group-SD parameters are intrinsically hard to estimate and may show
  elevated Rhat even with plenty of iterations. If so, the fix is a **tighter
  prior** on those SDs (e.g. `student_t(3, 0, 2.5)` instead of the current
  `student_t(3,0,5)` / `cauchy(0,5)`), not just more sampling.
- The first simple-fit run used too few iterations (200 draws → Rhat ~1.48). The
  current `x01` settings (4×3000) fix that; don't revert them.

## What each output is

| file | what |
|------|------|
| `data/fit_simple.rds` | simple Bayesian fit (x01) |
| `data/fit_phylo.rds`  | phylogenetic partial-pooling fit (x02) — main result |
| `data/phylo_tree.rds`, `data/phylo_A.rds` | taxonomy placeholder tree + correlation matrix (swap in a real tree at `data/tree.tre`) |
| `data/phylo_model.stan` | generated Stan code (for inspection) |
| `data/mass_overrides.csv`, `data/mass_gaps.csv` | mass-integration outputs (x03) |
