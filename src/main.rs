#![feature(map_first_last)]

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::BTreeSet;
//use std::collections::

fn permute<T>(set: &BTreeSet<T>) -> HashSet<BTreeSet<T>>
    where T: Clone + Ord + std::hash::Hash + std::fmt::Debug
{

    if set.is_empty() {
        let mut h = HashSet::new();
        h.insert(BTreeSet::new());
        return h;
    }

    let mut passed = set.clone();
    let letter = passed.pop_first().unwrap();

    //println!("Explore: {:?}", letter);

    let mut sets: HashSet<BTreeSet<T>> = permute(&passed);
    //println!("Sets: {:?}", sets);

    //let sum = BTreeSet::new();
    let mut prefix = BTreeSet::new();
    prefix.insert(letter);
    //println!("Prefix: {:?}", prefix);
    //let prefixed = sets.clone().iter().map(|set| { set.union(&prefix) });
    let mut prefixed = sets.clone().iter().map(|set| {
        //println!("Set cur: {:?}", set);
        set.union(&prefix).cloned().collect()
    }).collect();
    //println!("Prefixed: {:?}", prefixed);
    //let mut prefixed = prefixed.collect();
    let normal = sets.clone();

    normal.union(&prefixed).cloned().collect()
    /*for prefix in vec![true, false].iter() {
        let s = match prefix {
            false => sets.clone(),
            true => {
                let mut s = sets.clone();
                //
                s
            },
        };
    }*/

    //let mut working: BTreeSet<T> = BTreeSet::new();
    /*let with = {
        let mut working: BTreeSet<T> = set.clone();
        let attr = working.pop_first();
    }*/
}

fn main() {
    let b = Base::new(vec!['a', 'b', 'c', 'd', 'e']);

    /*let mut implications = ImplicationCollection::new(b);

    implications.add(b.at(vec!['a']).fdetermines(vec!['b', 'c']));
    implications.add(b.at(vec!['b']).fdetermines(vec!['d']));
    implications.add(b.at(vec!['b']).mvdetermines(vec!['c', 'd']));

    implications.close();*/

    println!("Permute:");
    let mut p = BTreeSet::new();
    p.insert('a');
    p.insert('b');
    p.insert('c');
    p.insert('d');

    println!("output: {:?}", permute(&p));

    //let mut n = Normalizer::new(implications);
    //let normalizations = n.normalize4();
}

fn close(m: HashMap<char, Determines>) {
    let mut unchanged = false;
    let mut det_set: HashSet<HashMap<char, Determines>> = HashSet::new();
    while !unchanged {
        unchanged = true;
        //
    }
}

enum Determines {
    None,
    MVDSource,
    FDSource,
    MVDDeterminant,
    FDDeterminant,
}

#[derive(Hash, Eq, PartialEq, Debug)]
struct Base {
    attributes: Vec<char>,
}

struct ImplicationBuilder<'a> {
    attribute: char,
    base: &'a Base,
}

impl Base {
    pub fn new(attrs: Vec<char>) -> Base {
        Base { attributes: attrs }
    }

    pub fn at(&self, attr: char) -> ImplicationBuilder {
        ImplicationBuilder {
            attribute: attr,
            base: self
        }
    }
}

#[derive(Hash, Eq, PartialEq, Debug)]
struct FunctionalDependency<'a> {
    base: &'a Base,
    from: BTreeSet<char>,
    determines: BTreeSet<char>,
}

#[derive(Hash, Eq, PartialEq, Debug)]
struct MultivaluedDependency<'a> {
    base: &'a Base,
    from: BTreeSet<char>,
    mvdetermines: BTreeSet<char>,
}

impl<'a> MultivaluedDependency<'a> {
    pub fn trivial(&self) -> bool {
        self.mvdetermines.is_subset(&self.from) || self.from.union(&self.mvdetermines).count() == self.base.attributes.len()
    }

    pub fn complementation_closure(&self) -> HashSet<MultivaluedDependency<'a>> {
        let all_elems: HashSet<char> = self.base.attributes.clone().into_iter().collect();
        let complement = all_elems
            .difference( &self.mvdetermines.clone().into_iter().collect() )
            .cloned()
            .collect::<HashSet<char>>()
            .difference( &self.from.clone().into_iter().collect() )
            .cloned()
            .collect::<HashSet<char>>();

        let mut h = HashSet::new();
        h.insert(MultivaluedDependency {
            base: self.base.clone(),
            from: self.from.clone(),
            mvdetermines: complement.into_iter().collect(),
        });

        h
    }

    pub fn multivalued_augmentation_closure(&self) -> HashSet<MultivaluedDependency<'a>> {
        let mut r = HashSet::new();
        for gamma in permute(&self.base.attributes.clone().into_iter().collect()) {
            for delta in permute(&gamma) {
                let mvd = MultivaluedDependency {
                    from: self.from.union(&gamma).cloned().collect(),
                    mvdetermines: self.mvdetermines.union(&delta).cloned().collect(),
                    base: self.base.clone(),
                };
                r.insert(mvd);
            }
        }

        r
    }

    /*pub fn inversion(&self) -> Option<MultivaluedDependency<'a>> { // rule 4
        let mut mvdetermines: BTreeSet<char> = BTreeSet::new();

        for attr in self.base.attributes.iter() {
            if self.mvdetermines.contains(attr) {
                // don't include
            } else {
                mvdetermines.insert(*attr);
            }
        }

        Some(MultivaluedDependency {
            mvdetermines,
            from: self.from.clone(),
            base: self.base,
        })
    }

    pub fn transitive(&self, other: &MultivaluedDependency<'a>) -> Option<MultivaluedDependency<'a>> {
        if self.mvdetermines == other.from {
            Some(MultivaluedDependency {
                base: self.base,
                mvdetermines: other.mvdetermines.clone(),
                from: self.from.clone(),
            })
        } else {
            None
        }
    }

    pub fn subtractive_combinatory(&self, other: &MultivaluedDependency<'a>) -> Option<MultivaluedDependency<'a>> {
        if self.from == other.from {
            Some(MultivaluedDependency {
                base: self.base,
                mvdetermines: self.mvdetermines.clone(),
                from: self.from.clone(), // TODO: fix these, not right
            })
        } else {
            None
        }
    }*/
}

impl<'a> FunctionalDependency<'a> {
    pub fn trivial(&self) -> bool {
        self.determines.is_subset(&self.from)
    }

    pub fn reflexive_closure(&self) -> HashSet<FunctionalDependency<'a>> {
        let mut r = HashSet::new();
        for s in permute(&self.from).into_iter() {
            let fd = FunctionalDependency {
                from: self.from.clone(),
                determines: s,
                base: self.base.clone(),
            };
            r.insert(fd);
        }

        r
    }

    pub fn autmentation_closure(&self) -> HashSet<FunctionalDependency<'a>> {
        let mut r = HashSet::new();
        for s in permute(&self.base.attributes.clone().into_iter().collect()) {
            let fd = FunctionalDependency {
                from: self.from.union(&s).cloned().collect(),
                determines: self.determines.union(&s).cloned().collect(),
                base: self.base.clone(),
            };
            r.insert(fd);
        }

        r
    }

    pub fn transitive_closure(&self, other: &FunctionalDependency<'a>) -> HashSet<FunctionalDependency<'a>> {
        let mut r = HashSet::new();
        if self.determines == other.from {
            let fd = FunctionalDependency {
                from: self.from.clone(),
                determines: other.determines.clone(),
                base: self.base.clone(),
            };
            r.insert(fd);
        }

        r
    }

    pub fn replication_closure(&self) -> HashSet<MultivaluedDependency<'a>> {
        let mut r = HashSet::new();
    }
}

enum Implication<'a> {
    Functional(FunctionalDependency<'a>),
    Multivalued(MultivaluedDependency<'a>),
}

/*struct Implication<'a> {
    pub m: HashMap<char, Determines>,
}*/

/*impl<'a> Implication<'a> {
    pub fn new() -> Implication<'a> {
        Implication {
            m: HashMap::new()
        }
    }

    pub fn newFromMap(m: HashMap<char, Determines>) -> Implication {
        Implication {
            m
        }
    }
}*/


struct ImplicationCollection<'a> {
    implications: HashSet<Implication<'a>>,
}

impl<'a> ImplicationBuilder<'a> {
}
