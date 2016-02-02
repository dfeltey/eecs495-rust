use std::collections::{HashMap,HashSet};
use std::io::{BufReader,BufRead,Read,stdin};
use std::env;
use std::fs::File;

fn main() {
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
                neighbors_not_seen.insert(edge.to_owned());
                graph.get_mut(vertex).expect("expected vertex is missing").insert(edge.to_owned());
                graph.entry(edge.to_owned()).or_insert(HashSet::new()).insert(vertex.to_owned());
            }
        }
        if !neighbors_not_seen.is_empty() {
            panic!("a neighbor of some vertex does not appear on its own line");
        }
        return Graph{edges: graph};
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
