# Time-varying parameter composition: interface design

Working design note for the `GP()`/`RW()` state work (#1451). Captures the
interface we converged on, why, the decisions still open, and a rough
implementation sketch. Not user-facing documentation.

## Problem

The released model composes a random walk (breakpoints) *and* a Gaussian process
on the same Rt trajectory: `update_Rt` builds `logR = log(R0) + bp + gp`, adding
both terms when both are present, and this is the default when `rw` is set with
the GP left on. The state rework replaces `update_Rt` with
`get_state_trajectory`, which dispatches on a single scalar `state_type`, so a
trajectory can now be a random walk *or* a GP but not both. That is a regression
from `main`, not a missing future feature. The redesign below restores
composition and generalises it.

## Core abstraction

Every time-varying quantity is one shape:

```
value[t] = inv_link( level + Σ components[t] )
```

- **level** — a single baseline (for Rt this is R0). Either a fixed number or a
  sampled parameter with a prior. Always present; it is not itself a component.
- **components** — a (possibly empty) list of mean-zero deviations that vary over
  time. Each is centred to average zero, so the level owns the mean and the
  components own the departures. Centring composes: a sum of mean-zero vectors is
  mean-zero, so the level stays identified regardless of how many components sit
  on top.
- **inv_link** — determined by the parameter (log for Rt and the observation
  scale, etc.), not chosen by the user.

Because `get_state_trajectory` already returns a `vector[t]` for every parameter,
"fixed", "constant but uncertain" and "time-varying" are the same structure with
different fillings:

| specification | level | components |
| --- | --- | --- |
| `Fixed(1)` | known number | none |
| `LogNormal(2, 0.2)` | sampled (prior) | none |
| `constant(LogNormal(2, 0.2)) + GP()` | sampled (prior) | one or more |

Fixed and constant collapse to the same thing (zero components); they differ only
in whether the level is known or sampled. This removes the fixed/constant/varying
trichotomy: there is one specification type, and the R code no longer needs to
branch on `is_state_spec(x) || x != Fixed(1)`.

The same abstraction is parameter-agnostic. `obs_opts(scale = ...)` and
`obs_opts(dispersion = ...)` already accept state specs and route through
`get_state_trajectory`, so the grammar below applies unchanged to the observation
scale (ascertainment) and dispersion, and to back-calculation infections, with
each parameter supplying its own link and its own sensible defaults.

## Interface

`constant()`/`initial()` wrap a baseline distribution; components compose onto
it with `+` (a bare distribution cannot be the left operand of `+` — see
below).

```r
# constant Rt (baseline, no components)
rt_opts(prior = constant(LogNormal(2, 0.2)))

# single GP, default (mean/stationary) anchor — unchanged single-component sugar
rt_opts(prior = GP(mean = LogNormal(2, 0.2)))

# composing needs a baseline wrapper or an anchored component to combine with
rt_opts(prior = constant(LogNormal(2, 0.2)) + GP())

# weekly random walk
rt_opts(prior = constant(LogNormal(1, 1)) + RW(period = 7))

# date-anchored breakpoints
rt_opts(prior = constant(LogNormal(1, 1)) +
  RW(knots = as.Date(c("2020-03-23", "2020-06-08"))))

# composed: baseline + GP + breakpoints (the regression this restores)
rt_opts(prior = initial(LogNormal(1, 1)) + GP() + RW(knots = bp_dates))

# equivalently, the baseline can come from one anchored component instead of
# an explicit wrapper
rt_opts(prior = GP(mean = LogNormal(2, 0.2)) + RW(period = 7))

# init-anchored: the prior describes the initial value, not the average
rt_opts(prior = initial(LogNormal(1, 1)) + GP())

# the same grammar on another parameter
obs_opts(scale = constant(LogNormal(0, 0.2)) + GP())
```

This reads as "Rt is a baseline plus a GP plus breakpoints", which is the
sentence a modeller says out loud describing the model.

### The `+` operator

`dist_spec` (a single value, constant or uncertain) and `state_spec` (a
trajectory built from one or more time-varying components) are deliberately
two different types, not one with a "sometimes composable" mode. Blurring
them — letting a bare distribution silently double as a baseline — would make
it unclear whether an expression names one value or a list of things that
vary. `constant()`/`initial()` are the explicit bridge: they turn a
`dist_spec` into a `state_spec`'s baseline, so `+` only ever combines
`state_spec`s, never a `dist_spec` directly.

This also happens to sidestep a mechanical clash for free: `+.dist_spec`
already means convolution (used for delays), and R's dispatch has no way to
let a bare distribution also resolve to a second `+` method (see Alternatives
considered). Because `constant()`/`initial()` return a `state_spec`, neither
operand of `+` is ever a bare `dist_spec`, so that clash never arises.

A trajectory's baseline may come from **either** `constant()`/`initial()`
**or** a single component's own `mean =`/`init =` (as in `GP(mean = ...) +
RW()`) — whichever operand of `+` carries an anchor becomes the baseline; the
other operand(s) must be "bare" (`GP()`/`RW()` with neither `mean` nor `init`).
Combining two baselines is an error ("a trajectory can have only one
baseline"). `GP()`/`RW()` used bare, alone (not composed), is a valid object
but errors with a clear message if it reaches model-building without ever
picking up a baseline.

`+.state_spec` is a single S3 method (one class hierarchy: `GP()`/`RW()`
produce `state_spec`; `constant()`/`initial()` and any composed result produce
`trajectory_spec`, which also inherits `state_spec`), so `component + component`,
`baseline + component`, `component + baseline` and `trajectory + component` all
dispatch to the same method — no need for a separate method per combination.
The earlier draft of this note proposed a single `baseline(x, anchor = )`
object instead of the `constant()`/`initial()` pair; both are the same idea
under different names, and `constant()`/`initial()` reads better at the call
site (no `anchor = "mean"/"init"` string to get right).

### The anchor (mean vs init)

The anchor states which feature of the trajectory the baseline prior describes:

- **mean** (stationary): the prior describes the time-average value. Since the
  components are centred at zero, the level is that average.
- **init** (non-stationary): the prior describes the initial value `value[1]`;
  the trajectory evolves as departures from that start. A Jacobian transfers the
  prior from the sampled level onto the derived initial value.

The anchor is a property of the baseline (there is one average and one initial
value per trajectory, whatever the component count), so it travels with the
baseline distribution:

- **`constant(dist)` = mean/stationary anchor** (the same anchor `GP(mean =
  ...)`/`RW(mean = ...)` already give a single component).
- **`initial(dist)` = init-anchored**, matching `GP(init = ...)`/`RW(init =
  ...)`. `constant`/`initial` are chosen because they describe the meaning
  directly and shadow no base function (unlike `mean`).

Attaching the anchor to the baseline (rather than to `rt_opts(anchor = )`) means a
reusable fragment such as `trend <- initial(LogNormal(1, 1)) + GP()` keeps its
anchor when spliced across regions in `opts_list()`.

### Two kinds of prior

The composed object is the full specification, and every term contributes priors:

- the **baseline** prior on the level (and its anchor);
- each **component's own** priors: the GP lengthscale and magnitude, the RW step
  standard deviation.

`GP()`/`RW()` are therefore not "shapes without priors"; they hold their own
hyperparameter priors. The baseline is the one prior that is shared across the
whole trajectory and so must be stated once. The rule that enforces this: a
trajectory sum contains at most one distribution (the baseline); two is an error
("two baselines").

### Random walk and breakpoints are one component

In the released model, breakpoints are a Gaussian random walk on the segment
levels (`bp0 = cumulative_sum(bp_effects)`, `bp_effects ~ normal(0, bp_sd)`), and
`rw = 7` and a user `breakpoint` column feed the same path, differing only in knot
placement. So there is no separate `BP()`; `RW()` carries the knots:

- `RW(period = 7)` — regular weekly walk.
- `RW(knots = <dates>)` — irregular breakpoints, anchored to dates so they resolve
  against each region's own date grid; integer indices are an escape hatch.
- a single knot is a one-time level shift.

`knots` given as dates is resolved to indices at data-binding time, the same
late-binding used for distributions, so the number of segment effects is
data-derived (as `bp_n` already is).

## Decisions resolved (implementation status)

Both points below turned out not to need a new decision: `GP(mean =/init =)`
and `RW(mean =/init =)` stay exactly as shipped (single-component sugar, one
baseline and one shape in the same call), and the existing plain-distribution
→ `GP()` deprecation in `rt_opts()` is untouched. A "bare prior" in the sense
below never reaches the model, because that deprecation always wraps it in a
`GP()` first, matching `main`'s default. So:

1. **Default component for a bare prior**: unchanged — resolved by the
   existing deprecation, not by this work.
2. **Default anchor value**: unchanged — `rt_opts()`'s existing `gp_anchor`
   logic is untouched.

## Deprecation (implemented)

`rt_opts(rw = )` and the `breakpoint` column now translate onto the composed
grammar (via `lifecycle::deprecate_warn`/`deprecate_warn`) instead of being
dropped:

- `rt_opts(rw = 7)` composes `+ RW(period = 7)` onto whatever `prior` already
  resolved to (the default `GP()`, an explicit prior, or the user's own
  `GP()`/`RW()`) — in `rt_opts()` itself, since it needs no data.
- the `breakpoint` column composes `+ RW(knots = <positions>)` onto `rt$prior`
  — in `estimate_infections()`, where the data (and hence the knot positions)
  is known. See `resolve_legacy_breakpoints()`.
- if the user's own prior already contains an RW component, both shims skip
  composing (rather than risk a conflicting/second random walk) and warn
  instead that the deprecated input was ignored.

Honouring the `breakpoint` column losslessly required irregular date-anchored
knots on `RW()` (`RW(knots = <Date>|<integer>)`), implemented alongside the
shims rather than deferred: a new Stan `rw_dev_knots` (isolated from the
unchanged, period-based `rw_dev`) and a `rw_knots`/`rw_knots_n`/
`rw_knots_offset` data block. `Date` knots resolve to plain time-indices only
once the data is known (`resolve_state_dates()`), so a spec built before the
data is seen (and reused across `regional_epinow()`'s regions) still works.

The vestigial `bp_n`/`bp_effects`/`bp_sd`/`breakpoints` Stan machinery (already
dead: forced to `bp_n = 0` by the pre-existing `use_breakpoints` deprecation,
so it no longer affected `R`) was removed rather than kept alongside the real
replacement.

## Implementation sketch

Stan:

- `assemble_state(t, n_free, level, dev, link)` is the seam and stays as is.
- Split each generator into a `*_dev` returning the deviation vector (`rw_dev`,
  `gp_dev`); `rw_trajectory`/`gp_trajectory` become thin wrappers so the
  single-component paths, and their byte-identity, are untouched.
- Promote type and hyperparameter references from the state level to a component
  table (`comp_state`, `comp_type`, per-component refs, CSR-indexed by
  `state_comp_offset`/`state_comp_n`). The dispatcher loops the state's
  components, sums their `dev`s, and calls `assemble_state` once.
- The level, link, anchor, free window and future behaviour stay at the state
  level (shared across a state's components).

R:

- One trajectory spec type; a bare `dist_spec` and `Fixed()` lift to zero-component
  specs. `+.dist_spec` and `+.<component>` build and extend the spec.
- `initial()` tags the baseline as init-anchored.
- A plain-English `print()` method: "Baseline: R ~ LogNormal(2, 0.2), average;
  + Gaussian process; + random-walk breakpoints at 2020-03-23, 2020-06-08".
- `create_rt_data`/`create_*` flatten the components into the Stan component table
  and mint one set of hyperparameter ids per component.
- Deprecation shims in `rt_opts`/`create_rt_data` and `obs_opts`.

Tests:

- The `log_prob` byte-identity harness must stay Δlp = 0 for single-component GP
  and RW through the refactor (catches parameter-packing bugs).
- Add a composite test (RW + GP on one trajectory) and an irregular-knots test.

## Future directions

The parameter-agnostic spec leaves room for unification later, none of it in
scope for #1451:

- The standalone top-level `gp`/`rw` arguments become redundant once a GP or RW is
  just a component in a parameter's spec, removing that special-casing.
- Back-calculation infections (a GP on the log scale from an initial value) become
  `initial(...) + GP()`, the same construction rather than a separate path.
- New time-varying parameters accept a trajectory spec with no fresh interface.
- Further component types slot in as new `comp_type`s: `Independent()` (white
  noise; weakly identified against observation overdispersion here, so it wants
  gating), `Input()` (a deterministic user-supplied series, where sign and scale
  are meaningful, unlike the symmetric stochastic components), and a formula
  front-end where `+` is term addition.
- Unify the mechanism and grammar, not the defaults: the sensible lengthscale
  prior, link and identifiability differ by parameter, so parameter-aware defaults
  stay underneath the shared spec.

## Alternatives considered

- **A bare `dist_spec` as the baseline, no wrapper.** The original plan;
  dropped for blurring "a single value" with "a composable trajectory" into
  one type, and it doesn't work mechanically either — `LogNormal(...) + GP()`
  dispatches on `+.dist_spec` (convolution) regardless of the right-hand side
  and errors.
- **Formula, `initial(...) ~ gp() + rw()`.** Elegant but leans on `~`/`~ 1` idioms
  this audience mostly has not internalised, needs the most new machinery (a
  call-tree parser for good errors), and prevents factoring a component out to a
  variable for reuse across regions.
- **Named arguments, `rt_opts(baseline =, anchor =, gp =, rw =)`.** Safest and most
  discoverable, but caps the model at one GP and one RW and reshapes `rt_opts` a
  third time in one release cycle.
- **Pipe, `baseline(...) |> add_gp() |> add_rw()`.** Reads like a recipe and avoids
  `+` entirely, at the cost of more typing and an R ≥ 4.1 floor. A reasonable
  fallback if any overlap with delay convolution is unwanted.
- **`c(dist, GP(), RW())`.** Once the anchor is out of the components this is close
  to the `+` form, but `+` reads better for "layers of one trajectory".
