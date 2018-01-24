extern crate harness;
extern crate clap;
extern crate env_logger;

use clap::{Arg, App};
use harness::cartridge::Cartridge;
use harness::memory::MappedMemory;
use harness::cpu::Cpu;

fn main() {
    env_logger::init();
    let matches = App::new("read_rom")
        .about("Parse iNES rom")
        .arg(Arg::with_name("PATH")
             .short("p")
             .long("path")
             .takes_value(true)
             .required(true))
        .get_matches();

    let path = matches.value_of("PATH").unwrap();
    let cartridge = Cartridge::load_file(&path).unwrap();
    let memory = MappedMemory::default().cartridge(cartridge);
    let cpu = Cpu::default().memory(memory);
}
