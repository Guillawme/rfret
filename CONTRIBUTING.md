# Contributing to rfret

## Bug reports

Bugs should be reported [here][bugs]. Before reporting any bug, please make sure
it is not already listed as an [open issue][open-issues] or
[already closed issue][closed-issues].

## Code contributions

Contributors, please follow this standard workflow:

1. Fork this repository to your GitHub account.
2. Make your modifications, ideally in a branch named after the particular
   feature or modification you are contributing.
3. Send a pull request to my `dev` branch.
4. Be patient, and don't be upset if I take days to get back to you: I am
   maintaining this package on my free time.

Also please follow these additional guidelines:

- Follow these [package development guidelines][r-pkg].
- Follow this [style guide][style-guide] (except, use 4 spaces for indentation
  instead of 2).
- Before submitting a pull request, make sure that:
    * you updated the documentation (use `devtools::document()`);
    * vignettes build without errors (use `devtools::build_vignettes()`);
    * you rebuilt the documentation website (use `pkgdown::build_site()`);
    * all unit tests pass (use `devtools::test()`);
    * in case you modified an interface, you updated the documentation and the 
      corresponding unit test, or added an appropriate unit test to cover this 
      new case;
    * `devtools::check()` finishes with no errors and no warnings (notes are OK,
      but please explain what caused them in your PR if you get more notes than
      my `master` branch);
    * `devtools::install()` finishes with no errors and no warnings.


[r-pkg]: http://r-pkgs.had.co.nz
[bugs]: https://github.com/Guilz/rfret/issues/new
[open-issues]: https://github.com/Guilz/rfret/issues
[closed-issues]: https://github.com/Guilz/rfret/issues?q=is%3Aissue+is%3Aclosed
[style-guide]: http://adv-r.had.co.nz/Style.html
