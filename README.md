# (Rust-)TypeInformation
**This is a framework to get runtime information about rust types**

** THIS IS CURRENTLY NOT IN A USABLE STATE **


[![Build Status](https://travis-ci.org/Tomok/serde-meta.svg?branch=master)](https://travis-ci.org/Tomok/serde-meta)

This library can be used to extend structures with meta information allowing
to query them for their fields at runtime.

*Note:* This provides read only access only.

The primary idea for this library is to allow the implementation of interfaces,
that can be queried for the format of data to be send to/received from them.
The real interfaces can than be implemented with serde or a similar framework.

### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in Serde by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
</sub>
