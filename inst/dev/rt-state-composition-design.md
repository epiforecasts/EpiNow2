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
| `LogNormal(2, 0.2) + GP()` | sampled (prior) | one or more |

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

A bare distribution is the baseline. Components compose onto it with `+`.

```r
# constant Rt (baseline, no components)
rt_opts(prior = LogNormal(2, 0.2))

# single GP, default (mean/stationary) anchor
rt_opts(prior = LogNormal(2, 0.2) + GP())

# weekly random walk
rt_opts(prior = LogNormal(1, 1) + RW(period = 7))

# date-anchored breakpoints
rt_opts(prior = LogNormal(1, 1) +
  RW(knots = as.Date(c("2020-03-23", "2020-06-08"))))

# composed: baseline + GP + breakpoints (the regression this restores)
rt_opts(prior = LogNormal(1, 1) + GP() + RW(knots = bp_dates))

# init-anchored: the prior describes the initial value, not the average
rt_opts(prior = initial(LogNormal(1, 1)) + GP())

# the same grammar on another parameter
obs_opts(scale = LogNormal(0, 0.2) + GP())
```

This reads as "Rt is a baseline plus a GP plus breakpoints", which is the
sentence a modeller says out loud describing the model.

### The `+` operator

`+` is already convolution on `dist_spec` (`+.dist_spec <- function(e1, e2)
c(e1, e2)`), used for delays. It is reused here without a clash by making it
polymorphic on operand type:

- `dist + dist` → convolution, yielding a distribution (unchanged; delay path
  untouched).
- `dist + component` → a trajectory (baseline + component).
- `component + component` / `trajectory + component` → add the component.

The two meanings never overlap: convolving a distribution with a GP has no
interpretation, so the right-hand type disambiguates completely. The composition
logic lives in `+.dist_spec` (baseline on the left) and `+.<component>` (component
on the left) so either order works. This is a deliberate semantic overload of `+`;
the alternative (a dedicated `baseline()` object on its own class) keeps
convolution and composition strictly separate but adds a wrapper the bare-dist
form does not need.

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

- **bare distribution = the default anchor** (stationary/mean), so the common
  case needs no wrapper.
- **`initial(dist)` = init-anchored.** `initial` is chosen because it describes
  the meaning and shadows no base function (unlike `mean`).

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

## Decisions still open

1. **Default component set for a bare prior.** On `main`, `rt_opts(prior = X)`
   with the default GP gave a GP. Under the clean reading, a bare prior with no
   components is constant. These are different models, so the default has to be
   chosen explicitly. Leaning: keep GP as the implicit default component (bare
   prior stays time-varying, matching `main`), with `Fixed()`/no components the
   explicit way to say constant. This interacts with the shipped
   plain-distribution → `GP()` deprecation.
2. **Default anchor value.** Match the current sensible default rather than change
   behaviour silently.

## Deprecation

Deprecate, do not remove. The old inputs keep working for a release, translated
onto the new grammar with `lifecycle::deprecate_warn`:

- `rt_opts(rw = 7)` → `LogNormal(<default>) + RW(period = 7)`
- user `breakpoint` column → `... + RW(knots = <dates where column == 1>)`
- `rt_opts(rw = 7)` with the default GP → `... + GP() + RW(period = 7)` (the
  composed case)

Honouring the `breakpoint` column losslessly requires irregular date-anchored
knots on `RW()`, so knots are parity work, not a later addition.

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

- **`baseline(prior, anchor = )` object.** Keeps `+` strictly one-meaning-per-class
  and is self-documenting, but adds a wrapper the bare distribution makes
  unnecessary once a `dist_spec` is treated as a zero-component baseline.
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
