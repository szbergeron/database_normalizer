#![feature(map_first_last)]

use std::io;
use std::io::prelude::*;
//use std::collections::HashSet;
//use std::collections::HashMap;
//use std::rc::Rc;
use std::collections::BTreeSet;
//use crate::ImplicationCollection;
use rust_closure_generator::*;

fn main() {
    //let b = Base::new(vec!["a", "b", "c", "d", "e", "f", "g", "h", "i"]);
    let b = Base::new(vec!["a", "b", "c", "d", "e"]);
    //let b = Base::new(vec!['a', 'b']);

    let mut implications = DependencyCollection::new(&b);

    implications.add(b.at(vec!["a"]).fdetermines(vec!["b", "c"]));
    implications.add(b.at(vec!["b"]).fdetermines(vec!["d"]));
    //implications.add(b.at(vec!["b"]).mvdetermines(vec!["b", "g", "h"]));
    implications.add(b.at(vec!["d", "c"]).fdetermines(vec!["a"]));
    //implications.add(b.at(vec!["f", "g", "i"]).fdetermines(vec!["e"]));
    /*
     * a b c d e 
     * ~ ^ ^
     *   ~   ^
     *
     * key is ae
     */
    //implications.add(b.at(vec!['a']).mvdetermines(vec!['b']));

    implications.close();

    println!("Keys: {:?}", implications.keys());

    let imp = implications.clone();

    for d in imp.fds.clone() {
        if !d.uninteresting() {
            //println!("Fd: {:?}", d);
            //println!("Finds {:?} -> {:?}", d.from, d.determines);
        }
    }

    for d in imp.mvds {
        if !d.uninteresting().clone() {
            //println!("Mvd: {:?}", d);
            //println!("Finds {:?} ->> {:?}", d.from, d.mvdetermines);
        }
    }
    println!("Ready >>>>");
    /*println!("Put in your database attributes");

    //let mut implications = ImplicationCollection::new(

    let stdin = io::stdin();
    let schema = stdin.lock().lines().next().unwrap().unwrap();
    let schema: Vec<&str> = schema.split_whitespace().collect();
    let b = Base::new(schema);
    let mut implications = ImplicationCollection::new(&b);
    println!("Add fds and mvds:");

    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let toks: Vec<&str> = line.split_whitespace().collect();

        let mut before: Vec<String> = Vec::new();
        let mut after: Vec<String> = Vec::new();
        let mut before_mid = true;
        //let mut fd = false;

        for token in toks {
            //let before = before.clone();
            //let after = after.clone();

            if token == "end" {
            } else if token == "mvd" {
                let before = before.clone();
                let after = after.clone();
                implications.add(b.owned_at(before).owned_mvdetermines(after));
                //before.clear();
            } else if token == "fd" {
                implications.add(b.owned_at(before).owned_fdetermines(after));
                //implications.add(b.at(before).fdetermines(after));
                //before.clear();
            } else if token == ">" {
                before_mid = false;
            } else if before_mid == true {
                //before.push(token.to_owned());
            } else {
                //after.push(token.to_owned());
            }
            //
        }
    }*/

    let n = Normalizer::new(&implications);
    let normalizations = n.normalize_4nf(vec!["a", "e"]);
    println!("Finished normalizing, culling uninteresting normalizations...");

    //let mut filtered: HashMap<BTreeSet<Rc<AttrCollection>>, Decomposition> = HashMap::new();
    /*for (_, normalization in normalizations {
        filtered.insert(normalization.flatten(), normalization);
        //println!("N: {}", normalization);
    }*/

    for (_flat, _) in normalizations.iter().filter(|(flat, _)| {
        //let mut minimal = true;
        for a in flat.iter() {
            for b in flat.iter() {
                //println!("A: {:?}, B: {:?}", a, b);
                if a.is_subset(b) && a != b { 
                    //println!("sdafhgalkgaiosguiowefwuiojhfiujifoasdj");
                    //println!("Cull non-minimal decomposition {:?}", flat);
                    return false;
                    //minimal = true;
                }
            }
        }

        true
    }){
        //println!("N: {:?} ||| {}", flat, normalization);
        //println!("Normalization: {:?}", flat);
    }
        //println!("Normalizes: 

    let stdin = io::stdin();
    println!("Query for LHSs:");
    for line in stdin.lock().lines() {
        let line: String = line.unwrap();
        let toks: BTreeSet<String> = line.split_whitespace().map(|s: &str| { s.to_owned() }).collect();

        //let imp = implications.clone();

        for d in implications.clone().fds.clone().iter().filter(|fd| *fd.from == toks) {
            if !d.uninteresting() {
                //println!("Fd: {:?}", d);
                println!("{:?} -> {:?}", d.from, d.determines);
            }
        }

        for d in implications.clone().mvds.clone().iter().filter(|fd| *fd.from == toks) {
            if !d.uninteresting() {
                //println!("Mvd: {:?}", d);
                println!("{:?} ->> {:?}", d.from, d.mvdetermines);
            }
        }
        //println!("Line: {}", line.unwrap());
    }
    //let normalizations = n.normalize4NF(to_param_vec(implications.keys().iter().next().unwrap()));
}
