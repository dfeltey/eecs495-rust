use std::collections::{HashMap,HashSet};
use std::io::{BufReader,BufRead,Read,stdin};
use std::env;
use std::vec;
use std::fs::File;

fn main() {
  let graph = parse_args_and_build_graph;
    
  for line in BufReader::new(stdin()).lines() {
      // .. parse a line into two nodes
      // pass arguments to graph.search and print the path
  }
}

// IO functions
fn parse_args_and_build_graph() -> Graph {
    let arguments: Vec<String> = env::args().collect();
    if arguments.len() != 2 {
        panic!("Expected exactly one file passed as a command line argument");
    }
    Graph::new(&arguments[1])
}


#[derive(PartialEq,Debug)]
struct Graph {
    edges: HashMap<String,HashSet<String>>
}

impl Graph {
    fn new(filename: &String) -> Self {
        match File::open(filename) {
            Err(_) => panic!("couldn't open file"),
            Ok(file) => Graph::build_graph(file),
        }
    }

    fn build_graph<R : Read>(reader: R) -> Self {
        let mut edges = HashMap::new();
        // keep track of which nodes have been mentioned as neighbors
        // to make sure they all start a line as well
        let mut neighbors_not_seen = HashSet::new();
        // Ensure each node is seen as the start of only a single line
        let mut seen = HashSet::new();
        for line_result in BufReader::new(reader).lines() {
            let line = line_result.expect("Failed to read");
            let mut words = line.split_whitespace();
            let vertex = words.next().expect("No Vertex to read");
            edges.entry(vertex.to_owned()).or_insert(HashSet::new());
            if seen.contains(vertex) {
                panic!("A vertex appears on multiple lines")
            }
            else {
                seen.insert(vertex.to_owned());
            }
            neighbors_not_seen.remove(vertex);
            for edge in words {
                if !seen.contains(edge){
                    neighbors_not_seen.insert(edge.to_owned());
                }
                edges.get_mut(vertex)
                     .expect("expected vertex is missing")
                     .insert(edge.to_owned());
                edges.entry(edge.to_owned())
                     .or_insert(HashSet::new())
                     .insert(vertex.to_owned());
            }
        }
        if !neighbors_not_seen.is_empty() {
            panic!("a neighbor of some vertex does not appear on its own line");
        }
        return Graph{edges: edges};
    }

    fn print_path(path: Option<Vec<&String>>) {
        match path {
            None => print!("No path exists"),
            Some(path) => {
                for node in path {
                    print!("{} ", node);
                }
            }
        }
        println!("");
    }

    fn search<'a>(& 'a self, source : String, dest : String)
                 -> Option<Vec<& 'a String>>{
      let mut maybe_source_ptr : Option<&String> = None;
      let mut maybe_dest_ptr : Option<&String> = None;
      for src in self.edges.keys() {
        if source == *src { maybe_source_ptr = Some(src) }
        if dest == *src { maybe_dest_ptr = Some(src) }
      }
      let source_ptr = maybe_source_ptr.expect("source node not in graph");
      let dest_ptr = maybe_dest_ptr.expect("source node not in graph");
      Graph::build_path(self.search_ptr(source_ptr, dest_ptr),
                        source_ptr,
                        dest_ptr)
    }

    // this function is a variation of
    // https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
    // that handles disconnected graphs (I think that one doesn't, anyway)
    fn search_ptr<'a>(& 'a self, source: & 'a String, dest : & 'a String)
                     -> HashMap<&String,Option<&String>> {
      let mut dist : HashMap<&String,Option<u32>> = HashMap::new();
      let mut prev : HashMap<&String,Option<&String>> = HashMap::new();
      let mut Q : Vec<&String> = Vec::new();
      for (src,dests) in self.edges.iter() {
        dist.insert(src,None);
        prev.insert(src,None);
        Q.push(src)
      };
      dist.insert(source,Some(0));
      println!("starting loop, source is {}", source);
      loop {
        match Graph::min(&mut Q,&dist) {
          None => break,
          Some(u) => {
            for v in self.edges.get(u).expect("1") {
              let alt = (*(dist.get(u).expect("2"))).map(|m| m+1);
              if Graph::lt(&alt,dist.get(v).expect("3")) {
                dist.insert(v,alt);
                prev.insert(v,Some(u));
              }
            }}
         }
       };
       prev
     }

   fn build_path<'a>(prev : HashMap<&String,Option<& 'a String>>,
                     source : & String,
                     dest : & 'a String)
                    -> Option<Vec<& 'a String>> {
     match *(prev.get(dest).unwrap()) {
       None => if *source == *dest {
                 let mut path = Vec::new();
                 path.push(dest);
		 Some(path)
       	       } else { None },
       Some(_) => {
         let mut path = Vec::new();
         let mut node = dest;
         loop {
           path.push(node);
           if *node == *source {break}
             node = prev.get(node).expect("4").expect("5");
         };
         Some(path)
       }
     }
   }

    fn lt(n1 : &Option<u32>, n2 : &Option<u32>) -> bool {
      match *n1 {
        None => false,
        Some(m1) => match *n2 {
          None => true,
          Some(m2) => m1 < m2
        }
      }
    }

    fn min<'a>(Q: &mut Vec<& 'a String>,
               dist: &HashMap<&String,Option<u32>>)
               -> Option<& 'a String> {
      let mut best_index : Option<usize> = None;
      let mut best_dist : Option<u32> = None;
      for i in 0..Q.len() {
        let v = Q.get(i).expect("6b");
        let v_dist = (dist.get(v)).expect("6");
        if best_dist == None ||
	   (*v_dist != None && best_dist.expect("7") > v_dist.expect("7b"))  {
           best_dist = *v_dist;
           best_index = Some(i);
        }
      };
      match best_index {
        None => None,
	Some(i) => {
	  Some(Q.remove(i))
	}
      }
    }
}


#[cfg(test)]
mod graph_tests {
    use super::Graph;
    use std::io::{BufReader,BufRead,Read};
    use std::io::Result;
    use std::collections::{HashMap,HashSet};


    #[test]
    #[should_panic]
    fn missing_vertex() {
        Graph::build_graph(StringReader::new("a b\n"));
    }

    #[test]
    #[should_panic]
    fn duplicate_vertices() {
        Graph::build_graph(StringReader::new("a b c\nb c\nc\na c"));
    }

    #[test]
    fn single_vertex() {
        let mut edges = HashMap::new();
        edges.insert("a".to_owned(),HashSet::new());
        let expected = Graph{edges: edges};
        let actual = Graph::build_graph(StringReader::new("a"));
        assert_eq!(expected,actual);
    }

    #[test]
    fn example_graph() {
        let mut edges = HashMap::new();
        edges.insert("a".to_owned(),build_edges(vec!("b","d")));
        edges.insert("b".to_owned(),build_edges(vec!("a","d")));
        edges.insert("c".to_owned(),build_edges(vec!("d")));
        edges.insert("d".to_owned(),build_edges(vec!("a","b","c")));
        let expected = Graph{edges: edges};
        let actual = Graph::build_graph(StringReader::new("a b d\nb a d\nc\nd c\n"));
        assert_eq!(expected,actual);
    }

    #[test]
    #[should_panic]
    fn test_badsearch1() {
        let graph = Graph::build_graph(StringReader::new("a b\nb\n"));
        graph.search("a".to_owned(),"c".to_owned());
    }

    #[test]
    #[should_panic]
    fn test_badsearch2() {
        let graph = Graph::build_graph(StringReader::new("a b\nb\n"));
        graph.search("c".to_owned(),"a".to_owned());
    }

    #[test]
    fn test_zero_hops() {
        let graph = Graph::build_graph(StringReader::new("a\n"));
        let path = graph.search("a".to_owned(),"a".to_owned());
	let a = "a".to_owned();
        let mut expected : Vec<& String> = Vec::new();
	expected.push(&a);
        assert_eq!(path,Some(expected));
    }

    #[test]
    fn test_one_hop() {
        let graph = Graph::build_graph(StringReader::new("a b\nb\n"));
        let path = graph.search("a".to_owned(),"b".to_owned());
	let a = "a".to_owned();
	let b = "b".to_owned();
        let mut expected : Vec<& String> = Vec::new();
	expected.push(&b);
	expected.push(&a);
        assert_eq!(path,Some(expected));
    }

    #[test]
    fn test_no_route() {
        let graph = Graph::build_graph(StringReader::new("a\nb\n"));
        let path = graph.search("a".to_owned(),"b".to_owned());
        assert_eq!(path,None)
    }

    #[test]
    fn test_skip_loop() {
        let graph = Graph::build_graph(StringReader::new("a b\nb c d\nc b\nd\n"));
        let path = graph.search("a".to_owned(),"d".to_owned());
	let a = "a".to_owned();
	let b = "b".to_owned();
	let d = "d".to_owned();
        let mut expected : Vec<& String> = Vec::new();
	expected.push(&d);
	expected.push(&b);
	expected.push(&a);
        assert_eq!(path,Some(expected))
    }

    #[test]
    fn test_path_in_disconnected_graph() {
        let graph = Graph::build_graph(StringReader::new("a b\nb c\nc\nd e\ne\n"));
        let path = graph.search("a".to_owned(),"c".to_owned());
	let a = "a".to_owned();
	let b = "b".to_owned();
	let c = "c".to_owned();
        let mut expected : Vec<& String> = Vec::new();
	expected.push(&c);
	expected.push(&b);
	expected.push(&a);
        assert_eq!(path,Some(expected))
    }

    #[test]
    fn test_no_path_in_disconnected_graph() {
        let graph = Graph::build_graph(StringReader::new("a b\nb c\nc\nd e\ne\n"));
        let path = graph.search("a".to_owned(),"e".to_owned());
        assert_eq!(path,None)
    }

    fn build_edges(edges: Vec<&str>) -> HashSet<String> {
        let mut h = HashSet::new();
        for e in edges {
            h.insert(e.to_owned());
        }
        h
    }

    struct StringReader {
        contents: Vec<u8>,
        position: usize,
    }

    impl StringReader {
        fn new(s: &str) -> Self {
            StringReader {
                contents: s.to_owned().into_bytes(),
                position: 0,
            }
        }
    }

    impl Read for StringReader {
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
