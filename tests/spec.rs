use std::{
    fs,
    path::{Path, PathBuf},
};

use stylang::lang::parse;

fn files(path: impl AsRef<Path>) -> Vec<PathBuf> {
    let mut dirs = vec![Path::new(env!("CARGO_MANIFEST_DIR")).join(path)];
    let mut files = vec![];

    while let Some(dir) = dirs.pop() {
        for entry in fs::read_dir(&dir).unwrap() {
            let entry = entry.unwrap();

            if entry.file_type().unwrap().is_dir() {
                dirs.push(dir.join(entry.path()));

                continue;
            }

            if let Some(extension) = entry.path().extension() {
                if extension == "sl" {
                    files.push(dir.join(entry.path()));
                }
            }
        }
    }

    files
}

#[test]
fn parse_core_lib() {
    for path in files("core") {
        print!("parse {:?}", path);

        let source = fs::read_to_string(path).unwrap();

        let stmts = parse(&source).unwrap();

        println!(" ... ok({})", stmts.len());
    }
}

// #[test]
// fn parse_spec() {
//     for path in files("spec") {
//         print!("parse {:?}", path);

//         let source = fs::read_to_string(path).unwrap();

//         let stmts = parse(&source).unwrap();

//         println!(" ... ok({})", stmts.len());
//     }
// }
