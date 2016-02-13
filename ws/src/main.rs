use std::net::{TcpListener, TcpStream};
use std::thread;
use std::io::{BufReader,BufRead,Read,Write,ErrorKind};
use std::fs::File;
use std::str;

// path specified in a GET request has to be
// well-formed utf-8 bytes

// if a GET request doesn't have the leading / in a path,
// it gets a 400, not a 404 or 403

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
}

#[derive(Debug)]
enum Response {
  R200{fh: File},
  R400,
  R403,
  R404
}

impl PartialEq for Response {
  fn eq(&self, that: &Response) -> bool {
    match self {
      &Response::R200{ref fh} => match *that {Response::R200{ref fh} => true, _ => false},
      &Response::R400 => match *that {Response::R400 => true, _ => false},
      &Response::R403 => match *that {Response::R403 => true, _ => false},
      &Response::R404 => match *that {Response::R404 => true, _ => false},
    }
  }
}

fn determine_response<R:Read>(mut reader : &mut BufReader<R>) -> Response {
  let whatever = parse_request(&mut reader);
  match whatever {
    Some(s) => 
      match File::open(s) {
        Ok(fh) => Response::R200{fh:fh},
	Err(x) =>
	match x.kind() {
	  ErrorKind::NotFound => Response::R404,
	  _ => Response::R403}
      },
    None => Response::R400
  }
}

fn handle_client(stream: TcpStream) {
  let mut reader = BufReader::new(stream);
  let whatever = determine_response(&mut reader);
  let stream = reader.into_inner();
  match whatever {
    Response::R200{fh} => send200(stream,fh),
    Response::R400 => send400(stream),
    Response::R403 => send403(stream),
    Response::R404 => send404(stream),
  }
}

fn send200(mut stream: TcpStream, mut fh: File) {
  response(&stream, 200);
  let mut buf = vec![0;1024];
  loop {
    match fh.read(&mut buf) {
      Ok(n) => {
      if n==0 {break};
      match stream.write_all(&buf[0..n]) {
        Ok(_) => {}
        Err(_) => {break}
       }
     },
    Err(_) => break
    }
  }
}

fn send400(mut stream: TcpStream) {response(&stream,400)}
fn send403(mut stream: TcpStream) {response(&stream,404)}
fn send404(mut stream: TcpStream) {response(&stream,404)}
fn send405(mut stream: TcpStream) {response(&stream,405)}

fn response(mut stream: &TcpStream, code: u16) {
  let s = String::from("whatever").into_bytes();
  stream.write_all(&s[0..s.len()]);
}

fn parse_request<R:Read>(reader : &mut BufReader<R>) -> Option<String> {
  let mut line = Vec::new();
  let prefix = String::from("GET /").into_bytes();
  let prefix_size = prefix.len();
  match reader.read_until(b'\n', &mut line) {
    Ok(n) => {
      if n >= prefix.len() &&
          &line[0..prefix_size] == &prefix[0..prefix_size] {
        match valid_suffix(&line) {
          Some(suffix_size) => {
            match str::from_utf8(&line[prefix_size..line.len()-suffix_size]) {
              Ok(s) => Some(s.to_owned()),
              Err(_) => None
            }},
	  None => None
        }
      } else { None }},
    Err(_) => None
  }
}

// this is a little finite state machine that
// recognizes the regexp #rx" HTTP(/[0-9]+[.][0-9]+)?\r\n"
fn valid_suffix(s : &Vec<u8>) -> Option<usize> {
  if s.len() <= 2 {return None};
  if s[s.len()-1] != '\n' as u8 {return None};
  if s[s.len()-2] != '\r' as u8 {return None};
  let mut i=s.len()-3;
  if i==0 {return None};
  if s[i]=='P' as u8 {
    check_http(s,i)
  } else {
    loop {
      if i==0 {return None}
      if s[i] == '.' as u8 { i-=1 ; break }
      if s[i] <= '9' as u8 || s[i] >= '0' as u8 {
        i-=1
      } else {
        return None
      }
    }
    loop {
      if s[i] == '/' as u8 { break }
      if i==0 {return None}
      if s[i] <= '9' as u8 || s[i] >= '0' as u8 {
        i-=1
      } else {
        return None
      }
    }
    check_http(s,i-1)
  }
}

fn check_http(s : &
Vec<u8>,i : usize) -> Option<usize> {
    if i < 4  {return None};
    if s[i-4] != ' ' as u8 {return None};
    if s[i-3] != 'H' as u8 {return None};
    if s[i-2] != 'T' as u8 {return None};
    if s[i-1] != 'T' as u8 {return None};
    if s[i]   != 'P' as u8 {return None};
    return Some(s.len()-(i-4))
}

#[cfg(test)]
mod graph_tests {
  use std::io::{Result,Read,BufReader};
  use super::Response;
  use super::valid_suffix;

  #[test]
  fn valid_suffix_test() {
    assert_eq!(valid_suffix(&String::from("").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from("\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from("\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from("HTTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTTP\r\n").into_bytes()),Some(7));
    assert_eq!(valid_suffix(&String::from(" HxTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" THTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTTP/1.0\r\n").into_bytes()),Some(11));
    assert_eq!(valid_suffix(&String::from(" HTTP/14.99\r\n").into_bytes()),Some(13));
    assert_eq!(valid_suffix(&String::from(" HTT_/14.99\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTTP/13399\r\n").into_bytes()),None);
  }


  #[test]
  fn parse_request_test() {
    assert_eq!(super::parse_request(&mut stor("")),None);
    assert_eq!(super::parse_request(&mut stor("GET HTTP\r\n")),None);
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/1.0\r\n")),
               Some("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP\r\n")),
               Some("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/0.9\r\n")),
               Some("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/123.3542\r\n")),
               Some("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/1233542\r\n")),
               None);
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/xyzpqr\r\n")),
               None);
    assert_eq!(super::parse_request(&mut stor("GET /a/b/c HTTP/1.0\r\n")),
               Some("a/b/c".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET /a/b b/c HTTP/1.0\r\n")),
               Some("a/b b/c".to_owned()));

    let prefix="GET /".to_owned().into_bytes();
    let suffix=" HTTP/1.0\r\n".to_owned().into_bytes();

    let mut blank=Vec::new();
    blank.append(&mut prefix.to_owned());
    blank.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(&mut BufReader::new(BytesReader{contents:blank,position:0})),
               Some("".to_owned()));

    let mut one_char=Vec::new();
    one_char.append(&mut prefix.to_owned());
    one_char.append(& mut(vec![97]));
    one_char.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(&mut BufReader::new(BytesReader{contents:one_char,position:0})),
               Some("a".to_owned()));


    let mut not_utf8=Vec::new();
    not_utf8.append(&mut prefix.to_owned());
    not_utf8.append(& mut(vec![255,255]));
    not_utf8.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(&mut BufReader::new(BytesReader{contents:not_utf8,position:0})),
               None)

  }

  // note: this test depends on `cargo run` happening
  // in the `ws` directory
  #[test]
  fn correct_response_test() {
      assert_eq!(super::determine_response(&mut stor("GET /dne HTTP\r\n")),
                 Response::R404);
  }

   fn stor(s:&'static str) -> BufReader<BytesReader> {BufReader::new(BytesReader::new(s))}
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