# camtraptor 1.0.0

This is a major new release with a number of breaking changes. 

* New functions `deployments()`, `observations()` and `media()` make it easier to retrieve table of same name from camtrap-dp object (#232).
* The `datapkg` argument is now retired and no longer available (#259). Please use the `package` argument instead.
* The `media` argument is now retired and no longer available (#255). This means that media are always read.
