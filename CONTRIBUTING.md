We welcome contributions to `BiomeBGCR`, both as bug reports or package enhancements.

## Report a bug

1. Use the [issue tracker](https://github.com/PredictiveEcology/BiomeBGCR/issues) to report a bug.

2. Please include a minimum [reproducible example](https://stackoverflow.com/q/5963269/1380598) that triggers the bug.

3. Please include the output of `devtools::session_info()`.

## Submit an enhancement

This Git repository uses the [Git Flow](https://nvie.com/posts/a-successful-git-branching-model/) branching model (the [`git flow`](https://github.com/petervanderdoes/gitflow-avh) extension is useful for this).
The [`development`](https://github.com/PredictiveEcology/BiomeBGCR/tree/development) branch contains the latest contributions and other code that will appear in the next release, and the [`master`](https://github.com/PredictiveEcology/SpaDES.core) branch contains the code of the latest release, which is exactly what is currently on [CRAN](https://cran.r-project.org/package=SpaDES.core).

To make a contribution to the package, just send a [pull request](https://doc.github.com/articles/using-pull-requests/). 
When you send your PR, make sure `development` is the destination branch on the [SpaDES.core repository](https://github.com/PredictiveEcology/BiomeBGCR).
Your PR should pass `R CMD check --as-cran`, which will also be checked when the PR is submitted.

We'll try to review your pull request and provide feedback / merge improvements as quickly as possible.

Thank you!
