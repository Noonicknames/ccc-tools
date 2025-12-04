# calc-rates

`calc-rates` is a tool for calculating effective collision rates against temperature from incident energy cross sections. Essentially, it calculates the following integral,

$$
    R_{p}(T) = \frac{1}{\sqrt{m_e \pi}} \left(\frac{2}{k_{B}T}\right) \int_{0}^{\infty}\sigma_{p}(E)E e^{-E/k_{B}T},
$$

where $ \sigma_{p} $ is the cross section for scattering process $ p $, $ E $ is the incident electron energy, $ k_B $ is the Boltzmann constant, $ m_{e} $ is the electron mass and $ T $ is the electron temperature.