#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;

use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;
use std::string::ParseError;

