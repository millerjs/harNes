extern crate harness;
extern crate clap;

use clap::{Arg, App};
use harness::cartridge::Cartridge;

fn main() {
    let matches = App::new("read_rom")
        .about("Parse iNES rom")
        .arg(Arg::with_name("PATH")
             .short("p")
             .long("path")
             .takes_value(true)
             .required(true))
        .get_matches();
    let path = matches.value_of("PATH").unwrap();
    let rom = Cartridge::load_file(&path).unwrap();
    println!("Loaded rom: {}", rom);
}
