# Time-varying parameter composition: interface design

Design note for #1451's time-varying parameter interface. Not user-facing
documentation.

## Time-varying parameters

Any model parameter (e.g. the reproduction number, Rt) can be:

- a **`dist_spec`** (e.g. `Fixed(1)`, `LogNormal(2, 0.2)`): a single value,
  known or with a prior, constant over time; or
- a **state**, built with **`GP()`/`RW()`**: the parameter follows a Gaussian
  process or a random walk around a baseline. `GP(mean = LogNormal(2, 0.2))`
  says the parameter follows a Gaussian process whose long-run average has
  that prior.

A parameter given a `GP()`/`RW()` becomes a **`state_spec`** object rather
than a plain `dist_spec`; `is_state_spec()` tells the two apart. In Stan,
`get_state_trajectory()` builds the actual value-over-time vector for a
parameter, whichever of the above it is.

`get_state_trajectory()` dispatches on a single scalar `state_type`: a state
is exactly one GP *or* one RW, never both.

## Composing components

The released model, `main`, already combines a random walk (breakpoints)
*and* a Gaussian process on the same Rt trajectory: its `update_Rt` builds
`logR = log(R0) + bp + gp`, adding both terms when both are present, and this
is the default when a weekly random walk is requested with the GP left on.
The single-`state_type` design above cannot express this: a state is one
component, not several. This section designs composition, so a state can be
built from any number of GP/RW components, generalised to any parameter (not
just Rt).

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

Because `get_state_trajectory` returns a `vector[t]` for every parameter,
"fixed", "constant but uncertain" and "time-varying" are the same structure with
different fillings:

| specification | level | components |
| --- | --- | --- |
| `Fixed(1)` | known number | none |
| `LogNormal(2, 0.2)` | sampled (prior) | none |
| `constant(LogNormal(2, 0.2)) + GP()` | sampled (prior) | one or more |

Fixed and constant collapse to the same thing (zero components); they differ
only in whether the level is known or sampled. Representing every parameter
this way — as a `state_spec`, just with zero components for the constant case
— removes a distinction R code would otherwise have to make explicitly
between a `dist_spec` and a `state_spec` to tell "does this parameter vary"
(e.g. `is_state_spec(x) || x != Fixed(1)`).

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

# composed: baseline + GP + breakpoints (matches main's combined behaviour)
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
A single `baseline(x, anchor = )` constructor covering both anchors was
considered and rejected in favour of the `constant()`/`initial()` pair, which
reads better at the call site (no `anchor = "mean"/"init"` string to get
right).

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

In `main`, breakpoints are a Gaussian random walk on the segment levels
(`bp0 = cumulative_sum(bp_effects)`, `bp_effects ~ normal(0, bp_sd)`), and a
weekly random walk and a user `breakpoint` column feed the same path,
differing only in knot placement. So there is no separate `BP()`; `RW()`
carries the knots:

- `RW(period = 7)` — regular weekly walk.
- `RW(knots = <dates>)` — irregular breakpoints, anchored to dates so they resolve
  against each region's own date grid; integer indices are an escape hatch.
- a single knot is a one-time level shift.

`knots` given as dates is resolved to indices at data-binding time, the same
late-binding used for distributions, so the number of segment effects is
data-derived (as it is in `main`).

## A bare prior defaults to a GP

A parameter given a plain distribution rather than a `GP()`/`RW()`/`constant()`/
`initial()` (e.g. `rt_opts(prior = LogNormal(2, 0.2))`) is deprecated and
auto-wrapped in a `GP()`, matching `main`'s default; the anchor it gets
(mean- or init-reverting) matches `rt_opts()`'s existing default-anchor
setting for that case (`gp_anchor`, set from the deprecated `gp_on` argument
when supplied). Composition does not change this default.

## Deprecation

`rt_opts(rw = )` and the `breakpoint` column translate onto the composed
grammar (via `lifecycle::deprecate_warn`) instead of being dropped:

- `rt_opts(rw = 7)` composes `+ RW(period = 7)` onto whatever `prior` already
  resolved to (the default `GP()`, an explicit prior, or the user's own
  `GP()`/`RW()`) — inside `rt_opts()` itself, since it needs no data. See
  `compose_deprecated_rw()`.
- the `breakpoint` column composes `+ RW(knots = <positions>)` onto `rt$prior`
  — inside `estimate_infections()`, where the data (and hence the knot
  positions) is known. See `resolve_legacy_breakpoints()`.
- if the user's own prior already contains an RW component, both shims skip
  composing (rather than risk a conflicting/second random walk) and warn
  instead that the deprecated input was ignored.

Honouring the `breakpoint` column losslessly requires irregular date-anchored
knots on `RW()` (`RW(knots = <Date>|<integer>)`), built alongside the shims
rather than deferred (see Implementation).

`main`'s `bp_n`/`bp_effects`/`bp_sd`/`breakpoints` Stan machinery is retired
rather than kept alongside the composed replacement.

## Implementation

Stan (`inst/stan/functions/state.stan`, `inst/stan/data/states.stan`,
`inst/stan/data/random_walk.stan`):

- `assemble_state(t, n_free, level, dev, link)` combines a baseline with a
  link-scale deviation `dev` (length `n_free`, the window over which the state
  varies) and holds the last value through the forecast horizon.
- Each generator is a `*_dev` function returning just the deviation vector
  (`rw_dev`, `gp_dev`, and `rw_dev_knots` for irregular knots); the
  single-component builders (`rw_trajectory`, `gp_trajectory`) are thin
  wrappers around `assemble_state`, verified byte-identical to `main`'s
  target density via `log_prob` (see Tests).
- A state indexes a contiguous block of a **component table**
  (`state_comp_offset`, `state_comp_n`), each entry with its own
  `comp_type`/`comp_pos` and hyperparameter references. `get_state_trajectory()`
  loops a state's components, sums their `*_dev` deviations, and calls
  `assemble_state` once.
- The level, link, anchor, free window and future behaviour stay at the state
  level (shared across a state's components).

R (`R/state.R`, `R/create.R`, `R/opts.R`):

- `GP()`/`RW()` build a "bare" one-component `state_spec` (`new_component_spec()`).
  `constant()`/`initial()` build a zero-component `state_spec` carrying just
  the baseline. Supplying `mean =`/`init =` to `GP()`/`RW()` is sugar that
  composes one of these onto the bare component (`with_optional_anchor()`),
  so there is exactly one internal representation regardless of how a spec
  was written.
- `+.state_spec` combines two specs, enforcing exactly one baseline (see
  The `+` operator).
- `create_state_data()` flattens a parameter's components into the Stan
  component table and mints one set of hyperparameter ids per component
  (`resolve_rw_component()`/`resolve_gp_component()`).
- Deprecation shims: `compose_deprecated_rw()` in `rt_opts()`;
  `resolve_legacy_breakpoints()` and `resolve_state_dates()` (which resolves
  `Date` knots to indices once the data is known) in `estimate_infections()`.

Tests:

- A `log_prob` byte-identity check holds Δlp = 0 for single-component GP and
  RW against `main`'s target density (catches parameter-packing bugs).
- Composite tests (RW + GP on one trajectory) and an irregular-knots test.

## Future directions

The parameter-agnostic spec leaves room for unification later, none of it in
scope for #1451:

- The standalone top-level `gp`/`rw` arguments become redundant once a GP or RW is
  just a component in a parameter's spec, removing that special-casing.
- Back-calculation infections (a GP on the log scale from an initial value) become
  `initial(...) + GP()`, the same construction rather than a separate path.
- New time-varying parameters accept a trajectory spec with no fresh interface.
- Further component types slot in as new `comp_type`s, each needing only its
  own shape constructor (no new anchor-handling code, since `with_optional_anchor()`
  is shared): `Independent()` (white noise; weakly identified against
  observation overdispersion here, so it wants gating), `Input()` (a
  deterministic user-supplied series, where sign and scale are meaningful,
  unlike the symmetric stochastic components), and a formula front-end where
  `+` is term addition.
- Unify the mechanism and grammar, not the defaults: the sensible lengthscale
  prior, link and identifiability differ by parameter, so parameter-aware defaults
  stay underneath the shared spec.

## Alternatives considered

- **A bare `dist_spec` as the baseline, no wrapper.** Considered first, and
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
