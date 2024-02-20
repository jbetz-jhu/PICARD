PICARD: Planning Information-monitored Covariate Adjusted RCT Designs
================
Josh Betz (<jbetz@jhu.edu>)
2024-02-20

- [What are Precision-Adaptive
  Designs?](#what-are-precision-adaptive-designs)
- [What is PICARD?](#what-is-picard)
- [References](#references)

# What are Precision-Adaptive Designs?

Investigators are faced with many challenges in designing efficient,
ethical randomized trials due to many competing demands. A trial must
collect enough information to identify meaningful benefits or harms with
a desired probability, while also minimizing potential harm and
suboptimal treatment of participants. Satisfying these competing demands
is further complicated by the limited and imprecise information
available during the design of a study.

Rather than planning analyses around sample sizes, investigators can
plan analyses which occur when a specified level of precision is
reached: this is known as information monitoring (Mehta and Tsiatis
2001). A precision-adaptive design can reduce the risk of under- or
overpowered trials by collecting data until the precision is sufficient
to conduct analyses. The precision of an estimator is simply the
reciprocal of the variance of the estimator. Since this is unknown in
practice, the variance is estimated using the square of the standard
error of the estimator.

# What is PICARD?

PICARD is an app for helping investigators plan precision-adaptive
(i.e. information monitored) study designs. Users can get an idea of
what sample size might be required to achieve a desired level of power
based on preliminary estimates of population parameters. Planning can be
done for continuous, binary, and ordinal outcomes.

PICARD can be used for both fixed sample size design with one analysis
and study designs with pre-planned interim analyses for early stopping
(Jennison and Turnbull 1999). Plots show estimates of the information
expected to accrue for a given sample size, and when interim analyses
may be conducted. Actual information levels will vary, and must be
monitored during an ongoing study.

Rather than performing analyses based on accrued outcomes, investigators
compare the standard error of their estimate, and perform their analysis
when sufficient precision is reached. This avoids wasting resources when
sufficient information on potential benefits or harms has already been
accrued.

Information monitoring can integrate both interim analyses and covariate
adjustment using a broad class of methods (Van Lancker, Betz, and
Rosenblum 2022). For more information, see the ‘Background’ tab in the
app.

[PICARD can be run on
ShinyApps.io](https://josh-betz.shinyapps.io/PICARD/) or downloaded for
local use.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Jennison1999" class="csl-entry">

Jennison, Christopher, and Bruce W. Turnbull. 1999. *Group Sequential
Methods with Applications to Clinical Trials*. Chapman; Hall/CRC.
<https://doi.org/10.1201/9780367805326>.

</div>

<div id="ref-Mehta2001" class="csl-entry">

Mehta, Cyrus R., and Anastasios A. Tsiatis. 2001. “Flexible Sample Size
Considerations Using Information-Based Interim Monitoring.” *Drug
Information Journal* 35 (4): 1095–1112.
<https://doi.org/10.1177/009286150103500407>.

</div>

<div id="ref-VanLancker2022" class="csl-entry">

Van Lancker, Kelly, Joshua Betz, and Michael Rosenblum. 2022. “Combining
Covariate Adjustment with Group Sequential, Information Adaptive Designs
to Improve Randomized Trial Efficiency.” *arXiv Preprint
arXiv:1409.0473*. <https://doi.org/10.48550/ARXIV.2201.12921>.

</div>

</div>
