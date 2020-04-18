#![feature(map_first_last)]

use std::io;
use std::io::prelude::*;
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

    let sets: HashSet<BTreeSet<T>> = permute(&passed);
    //println!("Sets: {:?}", sets);

    //let sum = BTreeSet::new();
    let mut prefix = BTreeSet::new();
    prefix.insert(letter);
    //println!("Prefix: {:?}", prefix);
    //let prefixed = sets.clone().iter().map(|set| { set.union(&prefix) });
    let prefixed = sets.clone().iter().map(|set| {
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

type AttrCollection = BTreeSet<String>;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Base {
    pub attributes: AttrCollection,
}

pub struct ImplicationBuilder<'a> {
    attribute: Vec<String>,
    base: &'a Base,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct FunctionalDependency<'a> {
    base: &'a Base,
    pub from: AttrCollection,
    pub determines: AttrCollection,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct MultivaluedDependency<'a> {
    base: &'a Base,
    pub from: AttrCollection,
    pub mvdetermines: AttrCollection,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum Implication<'a> {
    Functional(FunctionalDependency<'a>),
    Multivalued(MultivaluedDependency<'a>),
}


#[derive(Clone, Debug)]
pub struct ImplicationCollection<'a> {
    pub fds: HashSet<FunctionalDependency<'a>>,
    pub mvds: HashSet<MultivaluedDependency<'a>>,
    base: &'a Base,
}

impl<'a> ImplicationBuilder<'a> {
    pub fn fdetermines(&self, attrs: Vec<&str>) -> Implication<'a> {
        let attrs = attrs.into_iter().map(|s: &str| s.to_owned()).collect();
        self.owned_fdetermines(attrs)
    }

    pub fn owned_fdetermines(&self, attrs: Vec<String>) -> Implication<'a> {
        let attrs = attrs.into_iter().collect();
        Implication::Functional(FunctionalDependency {
            base: self.base,
            from: self.attribute.clone().into_iter().collect(),
            determines: attrs,
        })
    }

    pub fn mvdetermines(&self, attrs: Vec<&str>) -> Implication<'a> {
        let attrs = attrs.into_iter().map(|s: &str| s.to_owned()).collect();
        self.owned_mvdetermines(attrs)
    }

    pub fn owned_mvdetermines(&self, attrs: Vec<String>) -> Implication<'a> {
        let attrs = attrs.into_iter().collect();
        Implication::Multivalued(MultivaluedDependency {
            base: self.base,
            from: self.attribute.clone().into_iter().collect(),
            mvdetermines: attrs,
        })
    }
}

impl Base {
    pub fn new(attrs: Vec<&str>) -> Base {
        Base { attributes: attrs.into_iter().map(|s: &str| s.to_owned()).collect() }
    }

    pub fn at(&self, attr: Vec<&str>) -> ImplicationBuilder {
        let attr = attr.into_iter().map(|s: &str| s.to_owned()).collect();
        ImplicationBuilder {
            attribute: attr,
            base: self
        }
    }

    #[allow(dead_code)]
    pub fn owned_at(&self, attr: Vec<String>) -> ImplicationBuilder {
        ImplicationBuilder {
            attribute: attr,
            base: self 
        }
    }
}

impl<'a> MultivaluedDependency<'a> {
    pub fn decompositionally_useful(&self) -> bool {
        self.base.attributes.len() > self.attr_count()
    }

    pub fn attr_count(&self) -> usize {
        self.from.len() + self.mvdetermines.len()
    }

    pub fn uninteresting(&self) -> bool {
        self.trivial() || self.mvdetermines.len() == 0 || !self.from.is_disjoint(&self.mvdetermines)
    }
    pub fn trivial(&self) -> bool {
        self.mvdetermines.is_subset(&self.from) || self.from.union(&self.mvdetermines).count() == self.base.attributes.len()
    }

    pub fn complementation_closure(&self) -> HashSet<MultivaluedDependency<'a>> {
        let all_elems: HashSet<String> = self.base.attributes.clone().into_iter().collect();
        let complement = all_elems
            .difference( &self.mvdetermines.clone().into_iter().collect() )
            .cloned()
            .collect::<HashSet<String>>()
            .difference( &self.from.clone().into_iter().collect() )
            .cloned()
            .collect::<HashSet<String>>();

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

    pub fn coalescence_closure(&self, other: &FunctionalDependency<'a>) -> HashSet<FunctionalDependency<'a>> {
        let mut r = HashSet::new();
        
        // if alpha ->> beta
        // gamma subset beta
        // delta within R
        // delta disjoint beta
        // delta -> gamma
        //
        // then
        // alpha -> gamma
        let alpha = &self.from;
        let beta = &self.mvdetermines;
        let delta = &other.from;
        let gamma = &other.determines;
        if gamma.is_subset(beta) {
            if delta.is_disjoint(beta) {
                let fd = FunctionalDependency {
                    from: alpha.clone(),
                    determines: gamma.clone(),
                    base: self.base.clone(),
                };
                r.insert(fd);
            }
        }

        r
    }
}

trait Dependency {
    fn decompositionally_useful(&self) -> bool;
    fn trivial(&self) -> bool;
    fn uninteresting(&self) -> bool;
}

impl<'a> FunctionalDependency<'a> {
    pub fn attr_count(&self) -> usize {
        self.from.len() + self.determines.len()
    }

    pub fn decompositionally_useful(&self) -> bool {
        self.base.attributes.len() > self.attr_count()
    }

    pub fn uninteresting(&self) -> bool {
        self.trivial() || self.determines.len() == 0 || !self.from.is_disjoint(&self.determines)
    }

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

    pub fn augmentation_closure(&self) -> HashSet<FunctionalDependency<'a>> {
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
        let mvd = MultivaluedDependency {
            from: self.from.clone(),
            mvdetermines: self.determines.clone(),
            base: self.base.clone(),
        };
        r.insert(mvd);

        r
    }
}

impl<'a> ImplicationCollection<'a> {
    pub fn new(base: &'a Base) -> ImplicationCollection<'a> {
        ImplicationCollection {
            base,
            fds: HashSet::new(),
            mvds: HashSet::new(),
        }
    }

    #[allow(dead_code)]
    pub fn within(&self) -> &'a Base {
        self.base
    }

    // requires already close()ed to return correct results
    pub fn keys(&self) -> BTreeSet<BTreeSet<String>> {
        // find all 
        panic!("not implemented");
    }

    pub fn add(&mut self, implication: Implication<'a>) {
        match implication {
            Implication::Multivalued(mvd) => {
                println!("Given {:?} ->> {:?}", mvd.from, mvd.mvdetermines);
                self.mvds.insert(mvd);
            },
            Implication::Functional(fd) => {
                println!("Given {:?} -> {:?}", fd.from, fd.determines);
                self.fds.insert(fd);
            },
        }
    }

    pub fn close(&mut self) {
        //let mut unchanged = false;
        //let mut det_set: HashSet<HashMap<char, Determines>> = HashSet::new();
        let mut last_size = 0;
        while last_size < (self.mvds.len() + self.fds.len()) {
            last_size = self.mvds.len() + self.fds.len();
            //println!("Inside loop, current size is {}", last_size);
            //unchanged = true;
            for self_mvd in self.mvds.clone() {
                for mvd in self_mvd.complementation_closure() {
                    self.mvds.insert(mvd);
                }
                for mvd in self_mvd.multivalued_augmentation_closure() {
                    self.mvds.insert(mvd);
                }

                for other_fd in self.fds.clone() {
                    for fd in self_mvd.coalescence_closure(&other_fd) {
                        self.fds.insert(fd);
                    }
                }
            }

            for self_fd in self.fds.clone() {
                for fd in self_fd.reflexive_closure() {
                    self.fds.insert(fd);
                }
                for fd in self_fd.augmentation_closure() {
                    self.fds.insert(fd);
                }
                for mvd in self_fd.replication_closure() {
                    self.mvds.insert(mvd);
                }
                for other_fd in self.fds.clone() {
                    for fd in self_fd.transitive_closure(&other_fd) {
                        self.fds.insert(fd);
                    }
                }
            }
        }
    }
}

#[allow(dead_code)]
pub struct Normalizer<'a> {
    ic: &'a ImplicationCollection<'a>,
}

impl<'a> Normalizer<'a> {
    #[allow(dead_code)]
    pub fn new(ic: &'a ImplicationCollection<'a>) -> Normalizer<'a> {
        Normalizer {
            ic,
        }
    }

    #[allow(dead_code, unused_variables, unused_mut)]
    pub fn normalize4NF(&self, key: Vec<&str>) -> BTreeSet<BTreeSet<AttrCollection>> {
        let key: AttrCollection = key.into_iter().map(|s| s.to_owned()).collect();

        let mut decompositions: HashSet<HashSet<Vec<char>>> = HashSet::new();

        //let implications: Vec<Implication<'a>> = Vec::new();

        //self.ic.fds
        //self.ic.fds
        let ic: &ImplicationCollection<'a> = &self.ic;
        let dependencies: Vec<Implication<'a>> = ic
                .fds
                .iter()
                .filter(|fd| {
                    !fd.trivial()
                    &&
                    fd.decompositionally_useful()
                    &&
                    !key.is_subset(&fd.from)
                    &&
                    !fd.uninteresting()
                })
                .map(|fd| {
                    Implication::Functional(fd.clone())
                })
            .chain(ic
                .mvds
                .iter()
                .filter(|mvd| {
                    !mvd.trivial()
                    &&
                    mvd.decompositionally_useful()
                    &&
                    !key.is_subset(&mvd.from)
                    &&
                    !mvd.uninteresting()
                })
                .map(|mvd| {
                    Implication::Multivalued(mvd.clone())
                }))
            .collect::<Vec<Implication<'a>>>();


        let mut r = BTreeSet::new();
        if dependencies.len() == 0 {
            //r.insert(from_param_vec(self.ic.base.attributes));
            let mut r_inner = BTreeSet::new();
            r_inner.insert(self.ic.base.attributes.clone());
            r.insert(r_inner);
        }

        for dependency in dependencies.iter() {
            //println!("Decomp tries dependency {:?}", dependency);
            //all the different ways we can split up the table
            let (from, to, base, key_a) = match dependency {
                Implication::Functional(fd) => (fd.from.clone(), fd.determines.clone(), fd.base, fd.from.clone()),
                Implication::Multivalued(mvd) => (
                    mvd.from.clone(),
                    mvd.mvdetermines.clone(),
                    mvd.base,
                    mvd.from.union(&(mvd.mvdetermines)).cloned().collect()),
            };


            let set_a: AttrCollection = from.union(&to).cloned().collect();
            let base_a = Base { attributes: set_a.clone() };
            //let dependencies_a = dependencies.iter().filter(|dep|
            let set_b: AttrCollection = base.attributes.difference(&to).cloned().collect();
            let base_b = Base { attributes: set_b.clone() };

            let fds: Vec<HashSet<FunctionalDependency>> = vec![&set_a, &set_b].iter().map(|attrs| {
                ic.fds.clone().into_iter().filter(|fd| {
                    fd.from.is_subset(attrs) && fd.determines.is_subset(&set_a)
                }).map(|fd| {
                    FunctionalDependency { base: &base_a, from: fd.from, determines: fd.determines }
                }).collect::<HashSet<FunctionalDependency>>()
            }).collect();

            let mvds: Vec<HashSet<MultivaluedDependency>> = vec![&set_a, &set_b].iter().map(|attrs| {
                ic.mvds.clone().into_iter().filter(|mvd| {
                    mvd.from.is_subset(attrs) && mvd.mvdetermines.is_subset(&set_a)
                }).map(|mvd| {
                    MultivaluedDependency { base: &base_a, from: mvd.from, mvdetermines: mvd.mvdetermines }
                }).collect::<HashSet<MultivaluedDependency>>()
            }).collect();

            let relation_a = ImplicationCollection {
                fds: fds[0].clone(),
                mvds: mvds[0].clone(),
                base: &base_a,
            };

            let normalized_a = Normalizer::new(&relation_a);

            let relation_b = ImplicationCollection {
                fds: fds[0].clone(),
                mvds: mvds[0].clone(),
                base: &base_b,
            };

            let normalized_b = Normalizer::new(&relation_b);

            //println!("rel a is {:?}", relation_a.base);
            //println!("rel b is {:?}", relation_b.base);

            let key_b: AttrCollection = key.difference(&to).cloned().collect();
            let decomps_a = normalized_a.normalize4NF(to_param_vec(&key_a));

            let decomps_b = normalized_b.normalize4NF(to_param_vec(&key_b));

            //println!("Decomps a: {:?}", decomps_a);
            //println!("Decomps b: {:?}", decomps_b);
            /*if decomps_b.len() == 0 || decomps_a.len() == 0 {
                panic!();
            }*/

            //let mut r_inner = BTreeSet::new();
            for decomp_a in decomps_a.iter() {
                for decomp_b in decomps_b.iter() {
                    let mut pairings: BTreeSet<AttrCollection> = BTreeSet::new();
                    for relation_a in decomp_a.clone() {
                        pairings.insert(relation_a);
                    }
                    for relation_b in decomp_b.clone() {
                        pairings.insert(relation_b);
                    }
                    r.insert(pairings);
                }
            }
            //r_inner.insert(decomps_a);
            //r_inner.insert(decomps_b);
            //r.insert(r_inner);
            //normalized_a.normalize4NF(
        }

        r
    }

    pub fn normalizeBCNF(&mut self, key: Vec<&str>) {
    }

    pub fn normalize3NF(&mut self, key: Vec<&str>) {
    }

    pub fn normalize2NF(&mut self, key: Vec<&str>) {
    }
}

pub fn to_param_vec<'a>(owned_collection: &'a AttrCollection) -> Vec<&'a str> {
    owned_collection.iter().map(|s: &String| &s[..]).collect()
}

pub fn from_param_vec<'a>(ref_collection: &Vec<&'a str>) -> AttrCollection {
    ref_collection.clone().into_iter().map(|s: &str| s.to_owned()).collect()
}
