use std::net::{TcpListener, TcpStream};
use std::thread;
use std::io::{BufReader,BufRead,Read};
use std::fs::File;
use std::str;

// path specified in a GET request has to be
// well-formed utf-8 bytes (because I can't
// figure out how Rust's std library enough
// to open a file without first converting it
// to a String)

// copied from docs
fn main() {

let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

for stream in listener.incoming() {
    match stream {
        Ok(stream) => {
            thread::spawn(move|| {
                handle_client(stream)
            });
        }
        Err(_) => { /* connection failed */ }
    }
}

drop(listener);
}

fn handle_client(stream: TcpStream) {
  match parse_request(stream) {
    Some(s) => {
      let fh = File::open(s);
    },
    None => {}
  };
}

fn parse_request<R:Read>(stream : R) -> Option<String> {
  let mut reader = BufReader::new(stream);
  let mut line = Vec::new();
  let prefix = String::from("GET ").into_bytes();
  let prefix_size = prefix.len();
  let suffix = String::from(" HTTP\r\n").into_bytes();
  let suffix_size = suffix.len();
  match reader.read_until(b'\n', &mut line) {
    Ok(n) => {
     if n >= prefix.len() + suffix.len() &&
        &line[0..prefix_size] == &prefix[0..prefix_size] &&
        &line[line.len()-suffix_size..line.len()] == &suffix[0..suffix_size] {
     match str::from_utf8(&line[prefix_size..line.len()-suffix_size]) {
       Ok(s) => Some(s.to_owned()),
       Err(_) => None
     }
     } else { None }},
   Err(_) => None
  }
}

#[cfg(test)]
mod graph_tests {
  use std::io::{Result,Read};


  #[test]
  fn parse_request_test() {
    assert_eq!(super::parse_request(BytesReader::new("")),None);
    assert_eq!(super::parse_request(BytesReader::new("GET HTTP\r\n")),None);
    assert_eq!(super::parse_request(BytesReader::new("GET  HTTP\r\n")),
               Some("".to_owned()));
    assert_eq!(super::parse_request(BytesReader::new("GET /a/b/c HTTP\r\n")),
               Some("/a/b/c".to_owned()));
    assert_eq!(super::parse_request(BytesReader::new("GET /a/b b/c HTTP\r\n")),
               Some("/a/b b/c".to_owned()));

    let prefix="GET ".to_owned().into_bytes();
    let suffix=" HTTP\r\n".to_owned().into_bytes();

    let mut blank=Vec::new();
    blank.append(&mut prefix.to_owned());
    blank.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(BytesReader{contents:blank,position:0}),
               Some("".to_owned()));

    let mut one_char=Vec::new();
    one_char.append(&mut prefix.to_owned());
    one_char.append(& mut(vec![97]));
    one_char.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(BytesReader{contents:one_char,position:0}),
               Some("a".to_owned()));


    let mut not_utf8=Vec::new();
    not_utf8.append(&mut prefix.to_owned());
    not_utf8.append(& mut(vec![255,255]));
    not_utf8.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(BytesReader{contents:not_utf8,position:0}),
               None)

  }

    struct BytesReader {
        contents: Vec<u8>,
        position: usize,
    }

    impl BytesReader {
        fn new(s: &str) -> Self {
            BytesReader {
                contents: s.to_owned().into_bytes(),
                position: 0,
            }
        }
    }

    impl Read for BytesReader {
        fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
            let mut count = 0;
            
            while self.position < self.contents.len() && count < buf.len() {
                buf[count] = self.contents[self.position];
                count += 1;
                self.position += 1;
            }
            
            return Ok(count);
        }
    }
}