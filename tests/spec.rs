use std::{
    fs,
    path::{Path, PathBuf},
};

use stylang::lang::{Item, TokenStream, parse};

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

fn test_script(mut path: PathBuf) {
    print!("parse {:?}", path);

    let source = fs::read_to_string(&path).unwrap();

    let script = parse(&source).unwrap();

    println!(" ... ok({})", script.0.len());

    path.set_extension("b");

    let data = bendy::serde::ser::to_bytes(&script).unwrap();

    std::fs::write(path, &data).unwrap();

    let _: Vec<Item<TokenStream<'_>>> = bendy::serde::de::from_bytes(&data).unwrap();
}

#[test]
fn test_specs() {
    for path in files("core") {
        test_script(path);
    }

    for path in files("spec") {
        test_script(path);
    }
}
