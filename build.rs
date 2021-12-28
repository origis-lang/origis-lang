use std::fs::File;
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::path::Path;

use itertools::Itertools;

fn main() {
    builtin_std();
}

fn builtin_std() {
    let path = Path::new(&std::env::var("OUT_DIR").unwrap())
        .join("builtin_std.rs");
    let mut file = File::create(&path).unwrap();

    let mut map = phf_codegen::Map::new();

    BufReader::new(File::open("src/stdlib/builtin").unwrap())
        .lines()
        .map(|s| s.unwrap())
        .for_each(|line| {
            let mut split = line.split(",").peekable();
            let path = split.next().unwrap();

            map.entry(
                path.to_owned(),
                &format!(
                    "({}, {})",
                    path,
                    if split.peek().filter(|s| **s == ";").is_some() {
                        "Params::Variable".to_owned()
                    } else {
                        format!("Params::from(phf::phf_set!{{{}}})", split.map(|s| format!("\"{}\"", s)).join(","))
                    }
                ),
            );
        });

    file.write_all(map.build().to_string().as_bytes()).unwrap();
    file.sync_data().unwrap();
}
