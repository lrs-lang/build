// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

pub const ARM:     &'static str = "arm-unknown-linux-gnueabi";
pub const AARCH64: &'static str = "aarch64-unknown-linux-gnu";
pub const I686:    &'static str = "i686-unknown-linux-gnu";
pub const X86_64:  &'static str = "x86_64-unknown-linux-gnu";

#[cfg(target_arch = "arm")]
pub const DEFAULT: &'static str = ARM;

#[cfg(target_arch = "aarch64")]
pub const DEFAULT: &'static str = AARCH64;

#[cfg(target_arch = "i686")]
pub const DEFAULT: &'static str = I686;

#[cfg(target_arch = "x86_64")]
pub const DEFAULT: &'static str = X86_64;
