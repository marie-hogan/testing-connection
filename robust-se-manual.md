---
editor_options: 
  markdown: 
    wrap: 72
---

TEST

# Recreating White's Robust Standard Errors

Marie Hogan 7/18/2023

I recreate the robust standard errors provided by R package **car**,
using the *Chirot* dataset preloaded on the companion **carData**
package. I use White's formula for robust standard errors.

``` {.r}
#installing necessary packages
#requires package car, lmtest
library(car)
library(lmtest)
library(carData)

#sets up tester dataset, part of carData/car package preloaded data
df <- Chirot
```

**Regression Model**

This model is for demonstration purposes only and I do not offer any
interpretation. Its unit of observation is each county in Romania. There
are 32 observations. Outcome variable intensity describes conflict
intensity during Peasant Rebellion. For more information on the data,
search "Chirot" in the R Help view after installing the package carData.
$$ Conflict Intensity = \alpha + \beta_1 Commerce + \beta_2 Tradition + \beta_3Peasantry + \beta_4Inequality$$

``` {.r}
#manual coefficients
#define x, y
x <- as.matrix(cbind(1,df[2:5]))
y <- as.matrix(df[1])

#estimate coefficients
beta_hat <- solve(crossprod(x))%*%t(x)%*%y
labels = labels(beta_hat)[[1]]
as.data.frame(beta_hat, row.names = c("intercept", labels[2:5]))
```

                   intensity
    intercept  -12.919017967
    commerce     0.091140456
    tradition    0.116786574
    midpeasant  -0.003341668
    inequality   1.137969619

**Robust Standard Errors**

Next, I follow White's formula for heteroskedasticity robust standard
errors to find standard errors for the estimated coefficients. The
formula I follow is below:

$$ (X'X)^{-1}Xdiag(e_i^2)X'(X'X)^{-1}$$

``` {.r}
manual_se = sqrt(diag(solve(crossprod(x))%*%(t(x)%*%(diag(as.vector((y - x%*%beta_hat)^2)))%*%x)%*%(solve(crossprod(x)))))

results = data.frame(beta_hat,manual_se, row.names = c("intercept", labels[2:5]))
colnames(results) = c("manual_coef", "manual_SE")
results
```

                 manual_coef  manual_SE
    intercept  -12.919017967 4.73673159
    commerce     0.091140456 0.01793594
    tradition    0.116786574 0.05416464
    midpeasant  -0.003341668 0.01044912
    inequality   1.137969619 2.00779512

**Comparing Against R's Packages**

Using the strategy recommended in [Principles of Econometrics with
R](https://bookdown.org/ccolonescu/RPoE4/heteroskedasticity.html#heteroskedasticity-consistent-standard-errors)
I calculate coefficient estimates and robust standard errors using
pre-built functions in the **car** and **lmtest** packages.

``` {.r}
#built in to check
model = lm(intensity ~ commerce + tradition + midpeasant + inequality, df)
vcov_mat = hccm(model, type = "hc0")
built_in = coeftest(model, vcov = vcov_mat)
```

**Comparing Results**

``` {.r}
#packages the outputs together to view and compare
results$built_in_SE = built_in[,2]
results$built_in_coef = built_in[,1]
results[,c(1,4,2,3)]
```

                 manual_coef built_in_coef  manual_SE built_in_SE
    intercept  -12.919017967 -12.919017967 4.73673159  4.73673159
    commerce     0.091140456   0.091140456 0.01793594  0.01793594
    tradition    0.116786574   0.116786574 0.05416464  0.05416464
    midpeasant  -0.003341668  -0.003341668 0.01044912  0.01044912
    inequality   1.137969619   1.137969619 2.00779512  2.00779512
