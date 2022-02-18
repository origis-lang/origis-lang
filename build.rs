use includedir_codegen::Compression;
use walkdir::WalkDir;

fn main() {
    let mut incl = includedir_codegen::start("STD");
    for entry in
        WalkDir::new("src/std").follow_links(true).into_iter()
    {
        match entry {
            Ok(ref e)
                if !e.file_type().is_dir()
                    && e.file_name()
                        .to_string_lossy()
                        .ends_with(".gs") =>
            {
                incl.add_file(e.path(), Compression::Gzip).unwrap();
            }
            _ => (),
        }
    }
    incl.build("builtin_std.rs").unwrap();
}
