#![allow(dead_code, unused_imports, unused_variables)]

#[macro_use]
extern crate quick_error;
#[macro_use]
extern crate lazy_static;

pub mod cpu;
pub mod memory;
pub mod address;
pub mod types;
pub mod rom;

use cpu::*;
use types::*;
use address::*;
use memory::*;
use std::num::Wrapping;
use std::cmp::Ord;

#[cfg(test)]
mod tests;
