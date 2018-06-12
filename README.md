# reverse-engineer-pe-fees
reverse engineering private equity fees from performance data

The accompanying Rmd file provides a function to reverse engineer private equity incentive fees from cash flow and valuation information.  Returns estimates of fees on both cash and accrual basis.

Assumes a european style waterfall since deal level cash flows and valuations are rarely available.  The european waterfall assumption provides a lower bound for the estimate of fees accrued or paid.  