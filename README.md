# Standard Deviation Estimators
Simulate some estimators of the standard deviation presented at the [Wikipedia](https://en.wikipedia.org/wiki/Standard_deviation), as [discussed](https://fortran-lang.discourse.group/t/simulating-estimators-for-the-standard-deviation/2317) at Fortran Discourse.
<br>Compile with ```gfortran kind.f90 stdev.f90 ziggurat.f90 xstdev.f90```.

Truncated output:

```OVERALL
        method        avg_sd          rmse     avg_error
             1  0.9924683782  0.0002245609  0.0567451915
             2  0.9974682505  0.0002245620  0.0566951266
             3  0.9999966912  0.0002249881  0.0567783850
             4  0.9999902820  0.0002249867  0.0567780829
             5  0.9949588924  0.0002244215  0.0566846522
```

Besides method 2, which divides the sum of squared deviations by `(n-1)`, method 5, which divides by `(n-0.5)`, also looks good, having lower bias and RMSE than method 1, which divides by `n`.
