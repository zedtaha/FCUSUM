# fcumtest: Fourier CUSUM Cointegration Test

## Overview

The `FCUSUM` package implements the Fourier CUSUM cointegration test for 
detecting cointegration relationships in time series data with structural 
breaks. The test combines:

- **Fourier approximations** to capture smooth structural changes
- **CUSUM statistics** to test for cointegration stability

## Installation

```r
# Install from GitHub (replace with actual repo)
# devtools::install_github("zedtaha/FCUSUM")

# Or install from source
# install.packages("FCUSUM_1.0.0.tar.gz", repos = NULL, type = "source")
```

## Usage

```r
library(FCUSUM)

# Generate sample data
set.seed(123)
n <- 100
x <- cumsum(rnorm(n))
y <- 2 + 1.5 * x + rnorm(n)

# Run the Fourier CUSUM test
result <- fcum(y, x, kstar = 3)

# View results
print(result)
summary(result)
```

## Key Features

- Automatic frequency selection using AICc criterion
- Critical values for different model specifications
- S3 methods for clean output formatting
- Comprehensive documentation and examples

## References

Zaghdoudi, T. (2025). Testing the Long-Run Relationship Between Oil Price and Inflation in Tunisia: A Fourier CUSUM Cointegration Test. Energy RESEARCH LETTERS, 6(Early View).

## License

GPL-3

---
