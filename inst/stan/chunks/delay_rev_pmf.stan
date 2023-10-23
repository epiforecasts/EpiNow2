vector[delay_type_max[delay_id] + 1] delay_rev_pmf = get_delay_rev_pmf(
  delay_id, delay_type_max[delay_id] + 1, delay_types_p, delay_types_id,
  delay_types_groups, delay_max, delay_np_pmf,
  delay_np_pmf_groups, delay_mean, delay_sd, delay_dist,
  0, 1, 0
);
