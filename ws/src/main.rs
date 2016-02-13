use std::net::{TcpListener, TcpStream};
use std::thread;
use std::io::{BufReader,BufRead,Read,Write,ErrorKind};
use std::fs::{self,File,OpenOptions};
use std::str;
use std::result;

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
  R200{fh: File, file:String},
  R400{request:Vec<u8>},
  R403{file:String},
  R404{file:String},
}

impl PartialEq for Response {
  fn eq(&self, that: &Response) -> bool {
    match self {
      &Response::R200{ref fh, ref file} =>
        match *that {Response::R200{ref fh, ref file} => true, _ => false},
      &Response::R400{request: ref request1} =>
        match *that {Response::R400{request: ref request2} => request1==request2, _ => false},
      &Response::R403{file: ref file1} =>
        match *that {Response::R403{file: ref file2} => file1==file2, _ => false},
      &Response::R404{file: ref file1} =>
        match *that {Response::R404{file: ref file2} => file1==file2, _ => false},
    }
  }
}

fn determine_response<R:Read>(mut reader : &mut BufReader<R>) -> Response {
  let parsed_request = parse_request(&mut reader);
  match parsed_request {
  Ok(s) => {
    match fs::metadata(&s) {
    Ok (md) => 
      if md.is_dir() {
        try_to_open_file(&(s.to_owned() + "/index.html"),
        |x,_| try_to_open_file(&(s.to_owned() + "/index.shtml"),
        |x,_| try_to_open_file(&(s.to_owned() + "/index.txt"),
        |x,_| give_up(x,&s))))
      } else if md.is_file() {
        try_to_open_file(&s,give_up)
      } else {Response::R404{file:s.to_owned()}},
    Err(x) =>
      match x.kind() {
      ErrorKind::NotFound => Response::R404{file:s.to_owned()},
      _ => Response::R403{file:s.to_owned()}
      }
    }},
  Err(s) => Response::R400{request:s}
}}

fn try_to_open_file<K>(s : &String, k: K) -> Response
  where K : Fn(std::io::Error,&String) -> Response {
  match File::open(s) {
  Ok(fh) => Response::R200{fh:fh,file:s.to_owned()},
  Err(x) => k(x,s)
  }
}

fn give_up(x:std::io::Error, s: &String) -> Response {
  match x.kind() {
  ErrorKind::NotFound => Response::R404{file:s.to_owned()},
  _ => Response::R403{file:s.to_owned()}}
}

fn handle_client(stream: TcpStream) {
  let mut reader = BufReader::new(stream);
  let response = determine_response(&mut reader);
  atomic_log_response(&response);
  handle_response(response, reader.into_inner());
}

// must not be called concurrently
fn atomic_log_response(response:&Response) {
  let mut log_file=OpenOptions::new()
     .write(true).create(true).append(true)
     .open("log.txt").unwrap();
  match response {
  &Response::R200{ref fh,ref file} => {
    log_line(&log_file,String::from("200"),&(file.to_owned().into_bytes()));
  },
  &Response::R400{ref request} => {
    log_line(&log_file,String::from("400"),request);
  }
  &Response::R403{ref file} => {
    log_line(&log_file,String::from("403"),&(file.to_owned().into_bytes()));
  }
  &Response::R404{ref file} => {
    log_line(&log_file,String::from("404"),&(file.to_owned().into_bytes()));
  }
  }
}

fn log_line(mut log_file : &File, code:String,content:&Vec<u8>) {
    log_file.write(&(code.into_bytes()[0..]));
    log_file.write(&(String::from(" ").into_bytes()[0..]));
    log_file.write(&(content[0..content.len()]));
    log_file.write(&(String::from("\n").into_bytes()[0..]));
}

fn handle_response(response : Response, mut stream : TcpStream) {
  match response {
  Response::R200{fh,file} => send200(stream,fh),
  Response::R400{request} => send400(stream),
  Response::R403{file} => send403(stream),
  Response::R404{file} => send404(stream),
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

fn response(mut stream: &TcpStream, code: u16) {
  let s = String::from("whatever").into_bytes();
  stream.write_all(&s[0..s.len()]);
}

fn parse_request<R:Read>(reader : &mut BufReader<R>)
-> std::result::Result<String,Vec<u8>> {
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
              Ok(s) => Ok(s.to_owned()),
              Err(_) => Err(line.to_owned())
            }},
	  None => Err(line.to_owned())
        }
      } else { Err(line.to_owned()) }},
    Err(_) => Err(line.to_owned())
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
  use std::fs::File;

  #[test]
  fn valid_suffix_test() {
    assert_eq!(valid_suffix(&String::from("").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from("\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from("\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from("HTTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTTP\r\n").into_bytes()),Some(7));
    assert_eq!(valid_suffix(&String::from("0123456789 HTTP\r\n").into_bytes()),Some(7));
    assert_eq!(valid_suffix(&String::from(" HxTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" THTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTP\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTTP/1.0\r\n").into_bytes()),Some(11));
    assert_eq!(valid_suffix(&String::from(" HTTP/14.99\r\n").into_bytes()),Some(13));
    assert_eq!(valid_suffix(&String::from(" HTTP/12345.919\r\n").into_bytes()),Some(17));
    assert_eq!(valid_suffix(&String::from(" HTT_/14.99\r\n").into_bytes()),None);
    assert_eq!(valid_suffix(&String::from(" HTTP/13399\r\n").into_bytes()),None);
  }


  #[test]
  fn parse_request_test() {
    assert_eq!(super::parse_request(&mut stor("")),Err(stob("")));
    assert_eq!(super::parse_request(&mut stor("GET HTTP\r\n")),Err(stob("GET HTTP\r\n")));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/1.0\r\n")),Ok("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP\r\n")),Ok("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/0.9\r\n")),Ok("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/123.3542\r\n")),Ok("".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/1233542\r\n")),
               Err(stob("GET / HTTP/1233542\r\n")));
    assert_eq!(super::parse_request(&mut stor("GET / HTTP/xyzpqr\r\n")),
               Err(stob("GET / HTTP/xyzpqr\r\n")));
    assert_eq!(super::parse_request(&mut stor("GET /a/b/c HTTP/1.0\r\n")),
               Ok("a/b/c".to_owned()));
    assert_eq!(super::parse_request(&mut stor("GET /a/b b/c HTTP/1.0\r\n")),
               Ok("a/b b/c".to_owned()));

    let prefix="GET /".to_owned().into_bytes();
    let suffix=" HTTP/1.0\r\n".to_owned().into_bytes();

    let mut blank=Vec::new();
    blank.append(&mut prefix.to_owned());
    blank.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(&mut BufReader::new(BytesReader{contents:blank,position:0})),
               Ok("".to_owned()));

    let mut one_char=Vec::new();
    one_char.append(&mut prefix.to_owned());
    one_char.append(& mut(vec![97]));
    one_char.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(&mut BufReader::new(BytesReader{contents:one_char,position:0})),
               Ok("a".to_owned()));


    let mut not_utf8=Vec::new();
    not_utf8.append(&mut prefix.to_owned());
    not_utf8.append(& mut(vec![255,255]));
    not_utf8.append(&mut suffix.to_owned());
    let mut expected = Vec::new();
    expected.append(&mut prefix.to_owned());
    expected.append(& mut(vec![255,255]));
    expected.append(&mut suffix.to_owned());
    assert_eq!(super::parse_request(&mut BufReader::new(BytesReader{contents:not_utf8,position:0})),
               Err(expected));

  }

  // note: this test depends on `cargo run` happening
  // in the `ws` directory
  #[test]
  fn correct_response_test() {
      let expected_R200 = Response::R200{fh:File::open("x.txt").unwrap(),file:String::new()};
      assert_eq!(super::determine_response(&mut stor("GET /dne HTTP\r\n")),
                 Response::R404{file:"dne".to_owned()});
      assert_eq!(super::determine_response(&mut stor("GET /x.txt HTTP\r\n")),
                 expected_R200);
      assert_eq!(super::determine_response(&mut stor("GET /hasindexhtml HTTP\r\n")),
                 expected_R200);
      assert_eq!(super::determine_response(&mut stor("GET /hasindexshtml HTTP\r\n")),
                 expected_R200);
      assert_eq!(super::determine_response(&mut stor("GET /hasindextxt HTTP\r\n")),
                 expected_R200);
      assert_eq!(super::determine_response(&mut stor("GET /hasnothinguseful HTTP\r\n")),
                 Response::R404{file:"hasnothinguseful".to_owned()}); // suboptimal
      assert_eq!(super::determine_response(&mut stor("GET /hasnothinguseful/whatevs HTTP\r\n")),
                 expected_R200);
      assert_eq!(super::determine_response(&mut stor("junk")),
                 Response::R400{request:"junk".to_owned().into_bytes()});
  }

   fn stor(s:&'static str) -> BufReader<BytesReader> {BufReader::new(BytesReader::new(s))}
   fn stob(s:&'static str) -> Vec<u8> {s.to_owned().into_bytes()}
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