# Contributing to rfret

## Bug reports

Bugs should be reported [here][bugs]. Before reporting any bug, please make sure
it is not already listed as an [open issue][open-issues] or
[already closed issue][closed-issues].

## Code contributions

Contributors, please follow this standard workflow:

1. Fork this repository to your GitHub account.
2. Use RStudio and open the project through the `rfret.Rproj` file: this will
   ensure project-specific settings are applied (indentation, etc.).
2. Make your modifications starting from my `dev` branch, and ideally in a
   branch named after the particular feature or modification you are
   contributing.
3. Send a pull request to my `dev` branch.
4. Be patient, and don't get upset if I take days to get back to you: I am
   maintaining this package on my free time.

Also, before submitting a pull request, please check that:

- in case you modified an interface, you updated its documentation and its unit
  test, or added an appropriate unit test to cover a new case (`covr::report()`
  will indicate what code lacks unit tests);
- all unit tests pass (use `devtools::test()`);
- vignettes build without errors (use `devtools::build_vignettes()`);
- you updated the documentation (use `devtools::document()`);
- you rebuilt the documentation website (use `pkgdown::build_site()`);
- `devtools::check()` finishes with no errors and no warnings (notes are OK,
  but please explain what caused them in your PR if you get more notes than
  my `dev` branch);
- `devtools::install()` finishes with no errors and no warnings.


[r-pkg]: http://r-pkgs.had.co.nz
[bugs]: https://github.com/Guilz/rfret/issues/new
[open-issues]: https://github.com/Guilz/rfret/issues
[closed-issues]: https://github.com/Guilz/rfret/issues?q=is%3Aissue+is%3Aclosed
[style-guide]: http://adv-r.had.co.nz/Style.html
