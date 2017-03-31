use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Write};
use std::io::Result as IoResult;
use tempfile;

struct SourceFile {
    pub path: String,
    pub body: Vec<u8>,
}

impl SourceFile {

    pub fn load(path: &str) -> IoResult<SourceFile> {
        File::open(path).and_then(|mut f| {
            let mut source_file = SourceFile::new(path);
            f.read_to_end(&mut source_file.body)
                .and(Ok(source_file))
        })
    }

    fn new(path: &str) -> SourceFile {
        SourceFile {
            path: path.to_string(),
            body: Vec::new(),
        }
    }
}

#[test]
fn test_load_source_file() {
    let mut tmpfile = tempfile::NamedTempFile::new().unwrap();
    write!(tmpfile, "Hello World!").unwrap();
    tmpfile.seek(SeekFrom::Start(0)).unwrap();

    let sf = SourceFile::load(tmpfile.path().to_str().unwrap()).unwrap();
    assert_eq!(sf.path, tmpfile.path().to_str().unwrap());
    assert_eq!(sf.body, "Hello World!".as_bytes());
}
