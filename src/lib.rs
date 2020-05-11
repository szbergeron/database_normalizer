#![feature(map_first_last)]

//use std::io;
//use std::io::prelude::*;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
//use rayon::prelude::*;
//use rayon::prelude::*;
use std::rc::Rc;
//use std::collections::

static_assertions::assert_impl_all!(Dependency: std::marker::Sized);

fn permute<T>(set: &BTreeSet<T>) -> HashSet<BTreeSet<T>>
where
    T: Clone + Ord + std::hash::Hash + std::fmt::Debug,
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
    let prefixed = sets
        .clone()
        .iter()
        .map(|set| {
            //println!("Set cur: {:?}", set);
            set.union(&prefix).cloned().collect()
        })
        .collect();
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

pub type AttrCollection = BTreeSet<String>;

#[derive(Hash, Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
pub struct Base {
    pub attributes: Rc<AttrCollection>,
}

pub struct DependencyBuilder {
    attribute: Vec<String>,
    base: Rc<Base>,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
pub struct Key {
    pub attributes: Rc<AttrCollection>,
    pub derivation: Rc<BTreeSet<FunctionalDependency>>,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
pub struct FunctionalDependency {
    base: Rc<Base>,
    pub from: Rc<AttrCollection>,
    pub determines: Rc<AttrCollection>,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
pub struct MultivaluedDependency {
    base: Rc<Base>,
    pub from: Rc<AttrCollection>,
    pub mvdetermines: Rc<AttrCollection>,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
pub enum Dependency {
    Functional(FunctionalDependency),
    Multivalued(MultivaluedDependency),
}

impl Dependency {
    pub fn base(&self) -> Rc<Base> {
        match self {
            Dependency::Functional(fd) => fd.base.clone(),
            Dependency::Multivalued(mvd) => mvd.base.clone(),
        }
    }
}

impl<'a> fmt::Display for Dependency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = match self {
            Dependency::Functional(fd) => write!(f, "{}", fd),
            Dependency::Multivalued(mvd) => write!(f, "{}", mvd),
        };

        Ok(())
    }
}

fn join_attr_list(attribute_list: &AttrCollection) -> String {
    attribute_list
        .iter()
        .fold(String::new(), |push, token| push + token)
}

impl<'a> fmt::Display for MultivaluedDependency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ->> {} within {}",
            join_attr_list(&self.from),
            join_attr_list(&self.mvdetermines),
            self.base
        )
    }
}

//use itertools::free::join;
impl<'a> fmt::Display for FunctionalDependency {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //let join = |attribute_list: AttrCollection| -> String { attribute_list.iter().fold(String::new(), |push, token| push + token) };
        //let first = self.from.iter().fold(String::new(), |push, token| push + token);
        //let second = self.determines.iter().fold(String::new(), |push, token|
        write!(
            f,
            "{} -> {} within {}",
            join_attr_list(&self.from),
            join_attr_list(&self.determines),
            self.base
        )
    }
}

impl fmt::Display for Base {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", join_attr_list(&self.attributes))
    }
}

#[derive(Clone, Debug)]
pub struct DependencyCollection<'a> {
    pub fds: HashSet<FunctionalDependency>,
    pub mvds: HashSet<MultivaluedDependency>,
    base: &'a Base,
}

impl<'a> DependencyBuilder {
    pub fn fdetermines(&self, attrs: Vec<&str>) -> Dependency {
        let attrs = attrs.into_iter().map(|s: &str| s.to_owned()).collect();
        self.owned_fdetermines(attrs)
    }

    pub fn owned_fdetermines(&self, attrs: Vec<String>) -> Dependency {
        let attrs = Rc::new(attrs.into_iter().collect());
        Dependency::Functional(FunctionalDependency {
            base: self.base.clone(),
            from: Rc::new(self.attribute.clone().into_iter().collect()),
            determines: attrs,
        })
    }

    pub fn mvdetermines(&self, attrs: Vec<&str>) -> Dependency {
        let attrs = attrs.into_iter().map(|s: &str| s.to_owned()).collect();
        self.owned_mvdetermines(attrs)
    }

    pub fn owned_mvdetermines(&self, attrs: Vec<String>) -> Dependency {
        let attrs = Rc::new(attrs.into_iter().collect());
        Dependency::Multivalued(MultivaluedDependency {
            base: self.base.clone(),
            from: Rc::new(self.attribute.clone().into_iter().collect()),
            mvdetermines: attrs,
        })
    }
}

impl Base {
    pub fn new(attrs: Vec<&str>) -> Base {
        Base {
            attributes: Rc::new(attrs.into_iter().map(|s: &str| s.to_owned()).collect()),
        }
    }

    pub fn at(&self, attr: Vec<&str>) -> DependencyBuilder {
        let attr = attr.into_iter().map(|s: &str| s.to_owned()).collect();
        DependencyBuilder {
            attribute: attr,
            base: Rc::new(self.clone()),
        }
    }

    #[allow(dead_code)]
    pub fn owned_at(&self, attr: Vec<String>) -> DependencyBuilder {
        DependencyBuilder {
            attribute: attr,
            base: Rc::new(self.clone()),
        }
    }
}

impl<'a> MultivaluedDependency {
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
        self.mvdetermines.is_subset(&self.from)
            || self.from.union(&self.mvdetermines).count() == self.base.attributes.len()
    }

    pub fn complementation_closure(&self) -> HashSet<MultivaluedDependency> {
        //let all_elems: BTreeSet<String> = self.base.attributes.clone().into_iter().collect();
        let all_elems = self.base.attributes.clone();
        /*let complement = all_elems
        .difference( &self.mvdetermines.clone().into_iter().collect() )
        //.difference( self.mvdetermines.iter()
        .cloned()
        .collect::<HashSet<String>>()
        .difference( &self.from.clone().into_iter().collect() )
        .cloned()
        .collect::<HashSet<String>>();*/
        let complement = all_elems
            .difference(&self.mvdetermines)
            .cloned()
            .collect::<AttrCollection>()
            .difference(&self.from)
            .cloned()
            .collect();

        let mut h = HashSet::new();
        h.insert(MultivaluedDependency {
            base: self.base.clone(),
            from: self.from.clone(),
            //mvdetermines: Rc::new(complement.into_iter().collect()),
            mvdetermines: Rc::new(complement),
        });

        h
    }

    pub fn multivalued_augmentation_closure(&self) -> HashSet<MultivaluedDependency> {
        let mut r = HashSet::new();
        //for gamma in permute(&self.base.attributes.clone().into_iter().collect()) {
        for gamma in permute(&self.base.attributes) {
            for delta in permute(&gamma) {
                let mvd = MultivaluedDependency {
                    from: Rc::new(self.from.union(&gamma).cloned().collect()),
                    mvdetermines: Rc::new(self.mvdetermines.union(&delta).cloned().collect()),
                    base: self.base.clone(),
                };
                r.insert(mvd);
            }
        }

        r
    }

    pub fn coalescence_closure(
        &self,
        other: &FunctionalDependency,
    ) -> HashSet<FunctionalDependency> {
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

/*trait Dependency {
    fn decompositionally_useful(&self) -> bool;
    fn trivial(&self) -> bool;
    fn uninteresting(&self) -> bool;
}*/

impl<'a> FunctionalDependency {
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

    pub fn reflexive_closure(&self) -> HashSet<FunctionalDependency> {
        let mut r = HashSet::new();
        for s in permute(&self.from).into_iter() {
            let fd = FunctionalDependency {
                from: self.from.clone(),
                determines: Rc::new(s),
                base: self.base.clone(),
            };
            r.insert(fd);
        }

        r
    }

    pub fn partially_functionally_dependent(&self, key: &AttrCollection) -> bool {
        self.from.is_subset(key) && key.len() > self.from.len()
    }

    pub fn fully_functionally_dependent(&self, key: &AttrCollection) -> bool {
        *self.from == *key
    }

    pub fn augmentation_closure(&self) -> HashSet<FunctionalDependency> {
        let mut r = HashSet::new();
        for s in permute(&(*self.base.attributes).clone().into_iter().collect()) {
            let fd = FunctionalDependency {
                from: Rc::new(self.from.union(&s).cloned().collect()),
                determines: Rc::new(self.determines.union(&s).cloned().collect()),
                base: self.base.clone(),
            };
            r.insert(fd);
        }

        r
    }

    pub fn transitive_closure(
        &self,
        other: &FunctionalDependency,
    ) -> HashSet<FunctionalDependency> {
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

    pub fn replication_closure(&self) -> HashSet<MultivaluedDependency> {
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

impl<'a> DependencyCollection<'a> {
    pub fn new(base: &'a Base) -> DependencyCollection<'a> {
        DependencyCollection {
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
    pub fn keys(&self) -> BTreeSet<Key> {
        // find all
        //panic!("not implemented");
        //println!("In keys");
        let v: Vec<Key> = permute(&(*self.base.attributes))
            .into_iter()
            .filter_map(|schema| {
                //println!("was given schema {:?}", schema);
                let mut reachable: BTreeSet<String> = schema.clone();
                let mut changing = true;

                let mut derivation: BTreeSet<FunctionalDependency> = BTreeSet::new();
                while changing {
                    changing = false;
                    for fd in self.fds.iter() {
                        if fd.from.is_subset(&reachable) {
                            //reachable.append(&mut (*fd.determines).clone());
                            let size_before = reachable.len();
                            //changing = true;
                            fd.determines.iter().for_each(|elem| {
                                reachable.insert(elem.clone());
                            });

                            let size_after = reachable.len();

                            changing = size_after != size_before;
                            if changing {
                                derivation.insert(fd.clone());
                            }
                        }
                    }
                }
                //println!("{:?} is reachable from key candidate {:?}", reachable, schema);

                if reachable.is_superset(&(*self.base.attributes)) {
                    Some(Key {
                        derivation: Rc::new(derivation),
                        attributes: Rc::new(schema),
                    })
                } else {
                    None
                }
            })
            .collect();

        v.clone()
            .into_iter()
            .filter(|superkey| {
                // only allow through if not superset of any other key
                v.iter()
                    .filter(|otherkey| {
                        superkey.attributes.is_superset(&otherkey.attributes)
                            && superkey.attributes != otherkey.attributes
                    })
                    .count()
                    == 0
            })
            .collect()
    }

    pub fn add(&mut self, implication: Dependency) {
        match implication {
            Dependency::Multivalued(mvd) => {
                println!("Given {:?} ->> {:?}", mvd.from, mvd.mvdetermines);
                self.mvds.insert(mvd);
            }
            Dependency::Functional(fd) => {
                println!("Given {:?} -> {:?}", fd.from, fd.determines);
                self.fds.insert(fd);
            }
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
    ic: &'a DependencyCollection<'a>,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
pub enum Decomposition {
    Split(Split),
    Leaf(Rc<AttrCollection>),
}

impl Decomposition {
    pub fn flatten(&self) -> BTreeSet<Rc<AttrCollection>> {
        match self {
            Decomposition::Leaf(collection) => {
                let mut s = BTreeSet::new();
                s.insert(collection.clone());
                s
            }
            Decomposition::Split(split) => split
                .left
                .flatten()
                .union(&split.right.flatten())
                .cloned()
                .collect(),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Ord, PartialOrd)]
pub struct Split {
    pub left: Rc<Decomposition>,
    pub right: Rc<Decomposition>,
    by: Dependency,
}

use std::fmt;
impl<'a, 'b> std::fmt::Display for Decomposition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decomposition::Split(split) => write!(
                f,
                "split ({}), ({}) by applying ({})",
                split.left, split.right, split.by
            ),
            Decomposition::Leaf(attributes) => write!(f, "{}", join_attr_list(attributes)),
        }

        //Ok(())
    }
}

impl<'a> Normalizer<'a> {
    #[allow(dead_code)]
    pub fn new(ic: &'a DependencyCollection<'a>) -> Normalizer<'a> {
        Normalizer { ic }
    }

    #[allow(dead_code, unused_variables, unused_mut)]
    pub fn normalize_4nf(
        &self,
        key: Vec<&str>,
    ) -> HashMap<BTreeSet<Rc<AttrCollection>>, Decomposition> {
        let key: AttrCollection = key.into_iter().map(|s| s.to_owned()).collect();

        let mut decompositions: HashSet<HashSet<Vec<char>>> = HashSet::new();

        //let implications: Vec<Implication<'a>> = Vec::new();

        //self.ic.fds
        //self.ic.fds
        let ic: &DependencyCollection<'a> = &self.ic;
        let dependencies = self.useful_dependencies_from(&key);
        /*let ic: &ImplicationCollection<'a> = &self.ic;
        let dependencies: Vec<Implication> = ic
            .fds
            .iter()
            .filter(|fd| {
                !fd.trivial()
                    && fd.decompositionally_useful()
                    && !key.is_subset(&fd.from)
                    && !fd.uninteresting()
            })
            .map(|fd| Implication::Functional(fd.clone()))
            .chain(
                ic.mvds
                    .iter()
                    .filter(|mvd| {
                        !mvd.trivial()
                            && mvd.decompositionally_useful()
                            && !key.is_subset(&mvd.from)
                            && !mvd.uninteresting()
                    })
                    .map(|mvd| Implication::Multivalued(mvd.clone())),
            )
            .collect::<Vec<Implication>>();*/

        let mut r = HashMap::new();
        if dependencies.len() == 0 {
            //r.insert(from_param_vec(self.ic.base.attributes));
            //let mut r_inner = BTreeSet::new();
            //r_inner.insert(self.ic.base.attributes.clone());
            let decomp = Decomposition::Leaf(self.ic.base.attributes.clone());
            r.insert(decomp.flatten(), decomp);
            //r.insert(r_inner);
        }

        //dependencies.clone().into_par_iter();

        for dependency in dependencies.iter() {
            //println!("Decomp tries dependency {:?}", dependency);
            //all the different ways we can split up the table
            let (from, to, base, key_a) = match dependency {
                Dependency::Functional(fd) => (
                    fd.from.clone(),
                    fd.determines.clone(),
                    fd.base.clone(),
                    fd.from.clone(),
                ),
                Dependency::Multivalued(mvd) => (
                    mvd.from.clone(),
                    mvd.mvdetermines.clone(),
                    mvd.base.clone(),
                    Rc::new(mvd.from.union(&(mvd.mvdetermines)).cloned().collect()),
                ),
            };

            let set_a: Rc<AttrCollection> = Rc::new(from.union(&to).cloned().collect());
            let base_a = Rc::new(Base {
                attributes: set_a.clone(),
            });
            //let dependencies_a = dependencies.iter().filter(|dep|
            let set_b: Rc<AttrCollection> =
                Rc::new(base.attributes.difference(&to).cloned().collect());
            let base_b = Rc::new(Base {
                attributes: set_b.clone(),
            });

            //let fds: Vec<HashSet<FunctionalDependency>> = vec![&set_a, &set_b].iter().map(|attrs| {
            let fds: Vec<HashSet<FunctionalDependency>> =
                vec![(&set_a, base_a.clone()), (&set_b, base_b.clone())]
                    .iter()
                    .map(|(set, base)| {
                        ic.fds
                            .clone()
                            .into_iter()
                            .filter(|fd: &FunctionalDependency| {
                                fd.from.is_subset(&set) && fd.determines.is_subset(&set)
                            })
                            .map(|fd| FunctionalDependency {
                                base: base.clone(),
                                from: fd.from,
                                determines: fd.determines,
                            })
                            .collect::<HashSet<FunctionalDependency>>()
                    })
                    .collect();

            let mvds: Vec<HashSet<MultivaluedDependency>> =
                vec![(&set_a, base_a.clone()), (&set_b, base_b.clone())]
                    .iter()
                    .map(|(set, base)| {
                        ic.mvds
                            .clone()
                            .into_iter()
                            .filter(|mvd| {
                                mvd.from.is_subset(&set) && mvd.mvdetermines.is_subset(&set)
                            })
                            .map(|mvd| MultivaluedDependency {
                                base: base.clone(),
                                from: mvd.from,
                                mvdetermines: mvd.mvdetermines,
                            })
                            .collect::<HashSet<MultivaluedDependency>>()
                    })
                    .collect();

            /*if base_a.attributes.union(&base_b.attributes).count() < dependency.base().attributes.len() {
                panic!("Decomposition lost some attrs");
            }*/

            let relation_a = DependencyCollection {
                fds: fds[0].clone(),
                mvds: mvds[0].clone(),
                base: &base_a,
            };

            let normalized_a = Normalizer::new(&relation_a);

            let relation_b = DependencyCollection {
                fds: fds[1].clone(),
                mvds: mvds[1].clone(),
                base: &base_b,
            };

            let normalized_b = Normalizer::new(&relation_b);

            //println!("rel a is {:?}", relation_a.base);
            //println!("rel b is {:?}", relation_b.base);

            let key_b: AttrCollection = key.difference(&to).cloned().collect();
            let decomps_a = normalized_a.normalize_4nf(to_param_vec(&key_a));

            let decomps_b = normalized_b.normalize_4nf(to_param_vec(&key_b));

            //println!("Decomps a: {:?}", decomps_a);
            //println!("Decomps b: {:?}", decomps_b);
            /*if decomps_b.len() == 0 || decomps_a.len() == 0 {
                panic!();
            }*/

            //let mut r_inner = BTreeSet::new();
            for (flat_a, decomp_a) in decomps_a.iter() {
                for (flat_b, decomp_b) in decomps_b.iter() {
                    if false {
                        let mut maybe_all = AttrCollection::new();
                        for set in vec![flat_a, flat_b].iter() {
                            for attrset in set.iter() {
                                //maybe_all.append(*attrset);
                                for attr in attrset.iter() {
                                    maybe_all.insert(attr.clone());
                                }
                                //maybe_all.extend(attrset.iter().map(|item: String| item));
                            }
                        }

                        //if maybe_all.len() != dependency.base().attributes.len() {
                        if maybe_all != *dependency.base().attributes {
                            println!("Got differently lengthd current base and relation bases.");
                            println!("Current base is {}", dependency.base());
                            println!("Base a is {}", base_a);
                            println!("Base b is {}", base_b);
                            println!("Relation a decomps to {}", decomp_a);
                            println!("Relation b decomps to {}", decomp_b);
                            println!("Dependency was {}", dependency);
                            println!("set_a was {:?}", set_a);
                            println!("set_b was {:?}", set_b);
                            panic!();
                        }
                    }
                    /*assert!(maybe_all.len() == dependency.base().attributes.len(),
                    " maybe_all was {:?} while dep was {}", maybe_all, dependency);*/
                    //let mut pairings = BTreeSet
                    let decomp = Decomposition::Split(Split {
                        left: Rc::new(decomp_a.clone()),
                        right: Rc::new(decomp_b.clone()),
                        by: dependency.clone(),
                    });
                    r.insert(decomp.flatten(), decomp);
                    /*r.insert(Decomposition::Split(
                    Split{
                        left: Rc::new(decomp_a.clone()),
                        right: Rc::new(decomp_b.clone()),
                        by: dependency.clone()}));*/
                    /*let mut pairings: BTreeSet<AttrCollection> = BTreeSet::new();
                    for relation_a in decomp_a.clone() {
                        pairings.insert(relation_a);
                    }
                    for relation_b in decomp_b.clone() {
                        pairings.insert(relation_b);
                    }
                    r.insert(pairings);*/
                }
            }
            //r_inner.insert(decomps_a);
            //r_inner.insert(decomps_b);
            //r.insert(r_inner);
            //normalized_a.normalize4NF(
        }

        r
    }

    fn useful_dependencies_from(&self, key: &AttrCollection) -> Vec<Dependency> {
        let ic: &DependencyCollection<'a> = &self.ic;
        let dependencies: Vec<Dependency> = ic
            .fds
            .iter()
            .filter(|fd| {
                !fd.trivial()
                    && fd.decompositionally_useful()
                    && !key.is_subset(&fd.from)
                    && !fd.uninteresting()
            })
            .map(|fd| Dependency::Functional(fd.clone()))
            .chain(
                ic.mvds
                    .iter()
                    .filter(|mvd| {
                        !mvd.trivial()
                            && mvd.decompositionally_useful()
                            && !key.is_subset(&mvd.from)
                            && !mvd.uninteresting()
                    })
                    .map(|mvd| Dependency::Multivalued(mvd.clone())),
            )
            .collect::<Vec<Dependency>>();

        dependencies
    }


    pub fn normalize_bcnf(&mut self, _key: Vec<&str>) {}

    pub fn normalize_3nf(&mut self, _key: Vec<&str>) {}

    pub fn normalize_2nf(&mut self, key: Vec<&str>) -> HashMap<BTreeSet<Rc<AttrCollection>>, Decomposition> {
        let key: AttrCollection = key.into_iter().map(|s| s.to_owned()).collect();
        //let _ic: &DependencyCollection<'a> = &self.ic;
        let dependencies = self.useful_dependencies_from(&key);
        let fds = dependencies
            .iter()
            .filter_map(|dep|
                    match dep {
                        Dependency::Multivalued(_) => None,
                        Dependency::Functional(fd) => Some(fd)
                    }
            )
            .filter(|fd| {
                fd.partially_functionally_dependent(&key)
            });

        for _fd in fds {
            //
        }

        panic!()
    }
}

pub fn to_param_vec<'a>(owned_collection: &'a AttrCollection) -> Vec<&'a str> {
    owned_collection.iter().map(|s: &String| &s[..]).collect()
}

pub fn from_param_vec<'a>(ref_collection: &Vec<&'a str>) -> AttrCollection {
    ref_collection
        .clone()
        .into_iter()
        .map(|s: &str| s.to_owned())
        .collect()
}
