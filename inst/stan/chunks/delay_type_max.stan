array[delay_types] int delay_type_max;
for (i in 1:delay_types) {
  delay_type_max[i] = 0;
  for (j in delay_types_groups[i]:(delay_types_groups[i + 1] - 1)) {
    if (delay_types_p[j]) { // parametric
      delay_type_max[i] += delay_max[delay_types_id[j]];
    } else { // nonparametric
      delay_type_max[i] += delay_np_pmf_groups[delay_types_id[j] + 1] -
        delay_np_pmf_groups[delay_types_id[j]] - 1;
    }
  }
}
