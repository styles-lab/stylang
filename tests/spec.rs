// use std::{
//     fs,
//     path::{Path, PathBuf},
// };

// use stylang::lang::{file::File, input::TokenStream};

// fn files(path: impl AsRef<Path>) -> Vec<PathBuf> {
//     let mut dirs = vec![Path::new(env!("CARGO_MANIFEST_DIR")).join(path)];
//     let mut files = vec![];

//     while let Some(dir) = dirs.pop() {
//         for entry in fs::read_dir(&dir).unwrap() {
//             let entry = entry.unwrap();

//             if entry.file_type().unwrap().is_dir() {
//                 dirs.push(dir.join(entry.path()));

//                 continue;
//             }

//             if let Some(extension) = entry.path().extension() {
//                 if extension == "sl" {
//                     files.push(dir.join(entry.path()));
//                 }
//             }
//         }
//     }

//     files
// }

// fn test_script(mut path: PathBuf) {
//     print!("parse {:?}", path);

//     let source = fs::read_to_string(&path).unwrap();

//     let file = File::try_from(&source).unwrap();

//     path.set_extension("b");

//     // let data = bendy::serde::ser::to_bytes(&script).unwrap();
//     let data = flexbuffers::to_vec(&file).unwrap();
//     // let data = serde_lexpr::to_vec(&script).unwrap();

//     std::fs::write(&path, &data).unwrap();

//     let _: File<TokenStream<'_>> = flexbuffers::from_slice(&data).unwrap();

//     // let _: Script<TokenStream<'_>> = bendy::serde::de::from_bytes(&data).unwrap();
//     // let _: Vec<Item<TokenStream<'static>>> = serde_lexpr::from_slice(s);

//     println!(" ... ok({},{})", file.len(), data.len());

//     path.set_extension("json");

//     std::fs::write(&path, serde_json::to_string_pretty(&file).unwrap()).unwrap();
// }

// #[test]
// fn test_specs() {
//     for path in files("stylang/std") {
//         test_script(path);
//     }

//     for path in files("stylang/spec") {
//         test_script(path);
//     }
// }
