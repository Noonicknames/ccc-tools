# Checking Integrands

`calc-rates` at its heart just calculates an integral.
It may be useful to observe the integrand or assess the performance of the monotone cubic spline or the natural cubic spline interpolation.

`calc-rates` may be configured to output the integrand through changing/adding the following field to `calc-rates.ron`.

File: calc-rates.ron
```
(
    ...
    output_integrands: true, // or false to turn off
)
```

From there integrands will be outputted to `${OUTPUT_FOLDER}/${RESULT_SET_NAME}.integrands/`,

```
my_project/
├── calc-rates-out/
|   ├── monotone_cubic_spline.integrands/
|   |   ├── T=1eV // Integrand for each temperature specified
|   |   └── T=2eV
|   ├── natural_cubic_spline.integrands/
|   |   ├── T=1eV
|   |   └── T=2eV
|   ├── monotone_cubic_spline
|   └── natural_cubic_spline
├── results/
|   └── ics.s2P.s2S
└── calc-rates.ron
```

## Rationale
The program essentially computes the integral below,

$$
    R_{p}(T) = \frac{1}{\sqrt{m_e \pi}} \left(\frac{2}{k_{B}T}\right) \int_{0}^{\infty}\sigma_{p}(E)E e^{-E/k_{B}T},
$$

where $ \sigma_{p} $ is the cross section for scattering process $ p $, $ E $ is the incident electron energy, $ k_B $ is the Boltzmann constant, $ m_{e} $ is the electron mass and $ T $ is the electron temperature.

An integral rule for integrating to infinity is not used, instead it is simply assumed that the exponential decay factor takes effect sufficiently fast and the integrand decays to zero before the end of all data points.
However, this may become an issue if data points do not extend to a high enough energy.
For this reason, `calc-rates` supports outputting the integrand it uses to integrate.

