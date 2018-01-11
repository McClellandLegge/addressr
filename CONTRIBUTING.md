## Submitting a bug

If you notice something strange, please [submit an issue on GitHub](https://github.com/McClellandLegge/addressr/issues). In the issue, please try to achieve the following:

  * Describe what you did
* Describe what happened when you did it
* Describe what you think should happen
* If possible, describe where you think the error is occuring

If you have multiple issues, please submit multiple requests. Once you submit your report, we'll often engage in a conversation or give it a label to be fixed.

## Making Changes

When you want to make a change, either to fix a bug or introduce a new feature, please follow the instructions below

* Create a branch or fork of the project based off of the `dev` branch.
* Make commits of logical units
* Add unit tests for any new features
* Iterate either version or build number in the `DESCRIPTION` file:
* The version number follows the `x.y.z-build` format and increments based on [semantic versioning 2.0.0](http://semver.org/spec/v2.0.0.html). Please update versions corresponding to those guidelines.
* If your contribution takes several commits, please increment the build number (e.g., x.y.z-build) so there is a unique relationship of the version-build number to each commit.
* Run all tests in `tests/testthat/`
* Create a pull request with a robust description or [reference the issue number](https://github.com/McClellandLegge/addressr/issues)
