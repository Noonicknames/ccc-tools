# Input Data

Input data refers to the file containing data for cross section, incident energy data points.
This file should be organised as a two column file with comments starting with #.
For example,

```text
# ICS+ for s2P <-- s2S, in units of a0^2
9.306999999999999 0E0
12 1.36352E1
20 1.36506E1
24 1.28318E1
25.142 1.28796E1
30 1.20183E1
50 1.05025E1
70 9.04136E0
90 7.93693E0
100 7.48858E0
200 4.8727E0
```

Comma separated values are also supported,

```text
# ICS+ for s2P <-- s2S, in units of a0^2
9.306999999999999, 0E0
12, 1.36352E1
20, 1.36506E1
24, 1.28318E1
25.142, 1.28796E1
30, 1.20183E1
50, 1.05025E1
70, 9.04136E0
90, 7.93693E0
100, 7.48858E0
200, 4.8727E0
```

and semicolon separated,

```text
# ICS+ for s2P <-- s2S, in units of a0^2
9.306999999999999; 0E0
12; 1.36352E1
20; 1.36506E1
24; 1.28318E1
25.142; 1.28796E1
30; 1.20183E1
50; 1.05025E1
70; 9.04136E0
90; 7.93693E0
100; 7.48858E0
200; 4.8727E0
```

## Units Inference
If the units for a result set are set to [EnergyUnitsOrAuto::Auto](https://noonicknames.github.io/ccc-tools/doc/calc_rates/config/enum.EnergyUnitsOrAuto.html) or [CsUnits::Auto](https://noonicknames.github.io/ccc-tools/doc/calc_rates/config/enum.CsUnitsOrAuto.html), the units will be inferred from the first line of the input data file.
If the units cannot be inferred, the units will default to electron volts for energy and atomic units for the cross section.

Units are inferred by searching for matches of 