use std::collections::{HashMap,HashSet};
use std::io::{BufReader,BufRead,Read,stdin};
use std::env;
use std::fs::File;

fn main() {
    let mut h: HashMap<u8,u8> = HashMap::new();
    h.insert(1,2);
    h.insert(3,4);
    h.insert(5,6);
    {let i = h.entry(7).or_insert(12);};
    {let j = h.entry(1).or_insert(13);};
  
}

struct Graph {
    edges: HashMap<String,HashSet<String>>
}

impl Graph {
    fn new(filename: String) -> Self {
        match File::open(filename) {
            Err(_) => panic!("couldn't open file"),
            Ok(file) => Graph::build_graph_from_file(file),
        }
    }

    fn build_graph_from_file(file: File) -> Self {
        let mut graph = HashMap::new();
        let line_reader = BufReader::new(file);
        // ...
        let lines = line_reader.lines();
        for line_result in lines {
            let line: String = line_result.expect("Failed to read");
            let mut words = line.split_whitespace();
            let vertex = words.next().expect("No Vertex to read");
            for edge in words {
                {
                    let v_edges = graph.entry(vertex.to_owned()).or_insert(HashSet::new());
                    v_edges.insert(edge.to_owned());
                };
                {
                    let e_edges = graph.entry(edge.to_owned()).or_insert(HashSet::new());
                    e_edges.insert(vertex.to_owned());
                };
            }
        }
        return Graph{edges: graph};
    }
}
