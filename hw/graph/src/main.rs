use std::collections::{HashMap,HashSet};
use std::io::{BufReader,BufRead,Read,stdin};
use std::env;
use std::vec;
use std::fs::File;

fn main() {
    Graph::new("foo.txt".to_owned());
}

#[derive(PartialEq,Debug)]
struct Graph {
    edges: HashMap<String,HashSet<String>>
}

impl Graph {
    fn new(filename: String) -> Self {
        match File::open(filename) {
            Err(_) => panic!("couldn't open file"),
            Ok(file) => Graph::build_graph(file),
        }
    }

    fn build_graph<R : Read>(reader: R) -> Self {
        let mut graph = HashMap::new();
        // keep track of which nodes have been mentioned as neighbors
        // to make sure they all start a line as well
        let mut neighbors_not_seen = HashSet::new();
        // Ensure each node is seen as the start of only a single line
        let mut seen = HashSet::new();
        for line_result in BufReader::new(reader).lines() {
            let line = line_result.expect("Failed to read");
            let mut words = line.split_whitespace();
            let vertex = words.next().expect("No Vertex to read");
            graph.entry(vertex.to_owned()).or_insert(HashSet::new());
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
                graph.get_mut(vertex).expect("expected vertex is missing").insert(edge.to_owned());
                graph.entry(edge.to_owned()).or_insert(HashSet::new()).insert(vertex.to_owned());
            }
        }
        if !neighbors_not_seen.is_empty() {
            panic!("a neighbor of some vertex does not appear on its own line");
        }
        return Graph{edges: graph};
    }

    fn neighbors(&self,node: String) -> &HashSet<String> {
        match self.edges.get(&node) {
            Some(hs) => hs,
            None => panic!("No such node {}",node),
        }
    }

    fn search<'a>(& 'a self, source : String, dest : String) -> Option<Vec<& 'a String>>{
      let mut maybe_source_ptr : Option<&String> = None;
      let mut maybe_dest_ptr : Option<&String> = None;
      for (src,dests) in self.edges.iter() {
        if source == *src { maybe_source_ptr = Some(src) }
        if dest == *src { maybe_dest_ptr = Some(src) }
      }
      let source_ptr = maybe_source_ptr.expect("source node not in graph");
      let dest_ptr = maybe_dest_ptr.expect("source node not in graph");
      Graph::build_path(self.search_ptr(source_ptr, dest_ptr),
		        source_ptr,
			dest_ptr)
    }

    // this function is a variation
    // https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
    // that handles disconnected graphs (I think that one doesn't, anyway)
    fn search_ptr<'a>(& 'a self, source: & 'a String, dest : & 'a String) -> HashMap<&String,Option<&String>> {
      let mut dist : HashMap<&String,Option<u32>> = HashMap::new();
      let mut prev : HashMap<&String,Option<&String>> = HashMap::new();
      let mut Q : Vec<&String> = Vec::new();
      for (src,dests) in self.edges.iter() {
        dist.entry(src).or_insert(None);
        prev.entry(src).or_insert(None);
        Q.push(src)
      };
      dist.insert(source,Some(0));
      loop {
        match Graph::min(&mut Q,&dist) {
	  None => break,
	  Some(u) =>
            for v in self.edges.get(u).unwrap() {
              let alt = (*(dist.get(v).unwrap())).map(|m| m+1);
              if Graph::lt(&alt,dist.get(v).unwrap()) {
                dist.insert(v,alt);
                prev.insert(v,Some(u));
              }
            }
         }
       };
       prev
     }

   fn build_path<'a>(prev : HashMap<&String,Option<& 'a String>>, source : & String, dest : & 'a String) -> Option<Vec<& 'a String>> {
     match prev.get(dest) {
       None => None,
       Some(_) => {
         let mut path = Vec::new();
         let mut node = dest;
         loop {
           path.push(node);
           if node == source {break}
             node = prev.get(node).unwrap().unwrap();
         };
         Some(path)
       }
     }
   }

    fn lt(n1 : &Option<u32>, n2 : &Option<u32>) -> bool {
      match *n1 {
        None => false,
        Some(m1) => match *n2 {
          None => false,
          Some(m2) => m1 < m2
        }
      }
    }

    fn min<'a>(Q: &mut Vec<& 'a String>, dist: &HashMap<&String,Option<u32>>) -> Option<& 'a String> {
      let mut best_node : Option<&String>  = None;
      let mut best_dist = None;
      for v in Q.iter() {
        let v_dist = dist.get(v).unwrap();
        if best_dist == None || best_dist.unwrap() > v_dist  {
	   best_dist = Some(v_dist);
           best_node = Some(v);
	}
      }
      None
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
    fn test_one_hop() {
        let graph = Graph::build_graph(StringReader::new("a b\nb\n"));
        let path = graph.search("a".to_owned(),"b".to_owned());
	let expected = Vec::new();
	assert_eq!(path,Some(expected));
    }

    #[test]
    fn test_no_route() {
        let graph = Graph::build_graph(StringReader::new("a\nb\n"));
        let path = graph.search("a".to_owned(),"b".to_owned());
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
