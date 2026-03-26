#![allow(unused_qualifications)]
#![allow(clippy::to_string_trait_impl)]

use validator::Validate;

use crate::models;
#[cfg(any(feature = "client", feature = "server"))]
use crate::header;
