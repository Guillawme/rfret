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
3. Send a pull request.
4. Be patient, and don't be upset if I take days to get back to you: I am
   maintaining this package in my free time.

Also please follow these additional guidelines:

- Follow these [package development guidelines][r-pkg].
- Before submitting a pull request, make sure that:
    * you updated the documentation (for example, using `devtools::document()`);
    * `devtools::check()` finishes with no errors and no warnings (notes are OK,
      but please explain what caused them in your PR);
    * `devtools::install()` finishes with no errors and no warnings.


[r-pkg]: http://r-pkgs.had.co.nz
[bugs]: https://github.com/Guilz/rfret/issues/new
[open-issues]: https://github.com/Guilz/rfret/issues
[closed-issues]: https://github.com/Guilz/rfret/issues?q=is%3Aissue+is%3Aclosed
