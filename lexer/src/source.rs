use std::fs::{self, File};
use std::io::{self, Read, Write};
pub trait Source {
    fn get_char(&mut self) -> char;
}

pub struct FileSource {
    file_handle: File,
}

impl FileSource {
    pub fn new(path_to_file: String) -> io::Result<FileSource> {
        let file = File::open(path_to_file)?;
        Ok(FileSource { file_handle: file })
    }
}

impl Source for FileSource {
    fn get_char(&mut self) -> char {
        let mut buf = [0; 1];
        match self.file_handle.read_exact(&mut buf) {
            Err(e) => {
                if e.kind() == io::ErrorKind::UnexpectedEof {
                    '$'
                } else {
                    panic!("Something went wrong reading the file")
                }
            }
            Ok(()) => {
                if std::str::from_utf8(&buf).is_ok() {
                    buf[0] as char
                } else {
                    panic!("Not UTF-8 Encoded character");
                }
            }
        }
    }
}

pub struct StringSource {
    string: String,
}

impl StringSource {
    pub fn new(string: String) -> StringSource {
        StringSource {
            // storing as reversed for easy popping
            string: string.chars().rev().collect(),
        }
    }
}

impl Source for StringSource {
    fn get_char(&mut self) -> char {
        match self.string.pop() {
            Some(c) => c,
            None => '$',
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_from_file_source() {
        let mut file = File::create("test_file.txt").unwrap();
        let test_file_contents = b"let x = 5";
        file.write_all(b"let x = 5").unwrap();
        let mut file_source = FileSource::new(String::from("test_file.txt")).unwrap();
        for &c in test_file_contents {
            assert_eq!(file_source.get_char(), c as char);
        }
        assert_eq!(file_source.get_char(), '$');
        fs::remove_file("test_file.txt").unwrap();
    }
}
