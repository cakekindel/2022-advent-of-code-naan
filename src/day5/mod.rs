//! The expedition can depart as soon as the final supplies have been unloaded from the ships.
//! Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates,
//! the crates need to be rearranged.
//!
//! The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over,
//! the crane operator will rearrange them in a series of carefully-planned steps.
//! After the crates are rearranged, the desired crates will be at the top of each stack.
//!
//! The Elves don't want to interrupt the crane operator during this delicate procedure,
//! but they forgot to ask her which crate will end up where, and they want to be ready to unload
//! them as soon as possible so they can embark.
//!
//! They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure.
use naan::prelude::*;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum CraneBehavior {
  MoveOneCrateAtOnce,
  MoveManyCratesAtOnce,
}

pub trait SplitVec<T>
  where Self: Sized,
        T: Clone
{
  fn len(&self) -> usize;

  fn split_vec(self, ix: usize) -> Option<(Vec<T>, Vec<T>)>;

  fn init_and_last(self) -> Option<(Vec<T>, T)>;
}

impl<T> SplitVec<T> for Vec<T> where T: Clone
{
  fn len(&self) -> usize {
    self.len()
  }

  fn split_vec(mut self, ix: usize) -> Option<(Vec<T>, Vec<T>)> {
    match (self.len(), ix) {
      | (0, _) => None,
      | (1, 0) => Some((self, vec![])),
      | (1, 1) => Some((vec![], self)),
      | (1, _) => None,
      | _ => {
        let init = self.drain(0..ix).collect();
        Some((init, self))
      },
    }
  }

  fn init_and_last(self) -> Option<(Vec<T>, T)> {
    let len = self.len();
    match len {
      | 0 => None,
      | 1 => Some((vec![], self.into_iter().next().unwrap())),
      | _ => self.split_vec(len - 1)
                 .map(|(init, last)| (init, last.into_iter().next().unwrap())),
    }
  }
}

fn usize_dec<S>(s: S) -> usize
  where S: AsRef<str>
{
  usize::from_str_radix(s.as_ref(), 10).unwrap()
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Crate(String);

/// Using Vec as upside-down stack;
/// push == shift, pop == unshift
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Stack(Vec<Crate>);
impl Stack {
  pub fn new() -> Self {
    Stack(vec![])
  }

  /// Take `n` crates from this stack, yielding (<this stack>, <stack with taken crates>)
  pub fn take(mut self, cb: CraneBehavior, n: usize) -> (Self, Self) {
    let taken = Self(self.0.drain(0..n).collect());
    match cb {
      | CraneBehavior::MoveOneCrateAtOnce => (self, taken.reverse()),
      | CraneBehavior::MoveManyCratesAtOnce => (self, taken),
    }
  }

  pub fn add(self, c: Crate) -> Self {
    Self(vec![c].append(self.0))
  }

  pub fn top(&self) -> &Crate {
    self.0.get(0).unwrap()
  }

  pub fn reverse(mut self) -> Self {
    self.0.reverse();
    self
  }
}

impl Semigroup for Stack {
  fn append(self, b: Self) -> Self {
    Self(b.0.append(self.0))
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Manifest(Vec<Stack>);
impl Manifest {
  pub fn new() -> Self {
    Manifest(vec![])
  }

  pub fn add(self, s: Stack) -> Self {
    Self(self.0.append_one(s))
  }

  pub fn modify_all<F>(self, f: F) -> Self
    where F: F1<Stack, Stack>
  {
    Self(self.0.into_iter().map(|s| f.call(s)).collect())
  }

  pub fn modify<F>(self, ix: usize, f: F) -> Self
    where F: F1Once<Stack, Stack>
  {
    self.modify_with(ix, |s| (f.call1(s), ())).0
  }

  pub fn modify_with<F, R>(self, ix: usize, f: F) -> (Self, R)
    where F: F1Once<Stack, (Stack, R)>
  {
    let Self(vec) = self;
    let mut vec = vec.fmap(Option::pure);
    let slot = vec.get_mut(ix).unwrap();
    let taken_from_slot = slot.take().unwrap();
    let (new, r) = f.call1(taken_from_slot);
    *slot = Some(new);

    (Self(vec.fmap(Option::unwrap)), r)
  }

  pub fn perform(self, cb: CraneBehavior, instr: Instr) -> Self {
    let (man, taken) = self.modify_with(instr.from, |s: Stack| s.take(cb, instr.count));
    man.modify(instr.to, |s: Stack| s.append(taken))
  }
}

impl Semigroup for Manifest {
  fn append(self, b: Self) -> Self {
    Self(self.0.append(b.0))
  }
}

impl<S> From<S> for Manifest where S: ToString
{
  fn from(s: S) -> Self {
    s.to_string()
     .lines()
     .fold(Manifest::new().add(Stack::new()), |manifest, line| {
       line.split("")
           .filter(|s| !s.is_empty())
           .fold((vec![vec![]], false), |(chars, skipped_one), c| {
             let last_is_len_3 = chars.last().map(|v| v.len()).get_or(0) == 3;
             if !skipped_one && last_is_len_3 {
               (chars, true)
             } else if last_is_len_3 {
               (chars.append_one(vec![c]), false)
             } else {
               let (init, last) = chars.init_and_last().get_or((vec![], vec![]));
               (init.append_one(last.append_one(c)), false)
             }
           })
           .0
           .into_iter()
           .filter(|v| !v.is_empty())
           .map(|v| match v.as_slice() {
             | ["[", c, "]"] => Some(*c),
             | [" ", " ", " "] => None,
             | _ => panic!("{v:?}"),
           })
           .enumerate()
           .fold(manifest, |manifest, (ix, c)| {
             let manifest = if ix > manifest.clone().0.len() - 1 {
               manifest.clone().append(Manifest::new().add(Stack::new()))
             } else {
               manifest
             };

             c.map(|c| {
                manifest.clone()
                        .modify(ix, |s: Stack| s.add(Crate(c.into())))
              })
              .get_or(manifest)
           })
     })
     .modify_all(Stack::reverse)
  }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Instr {
  pub count: usize,
  pub from: usize,
  pub to: usize,
}

impl Instr {
  pub fn parse_many<S>(s: S) -> Vec<Self>
    where S: ToString
  {
    s.to_string()
     .trim()
     .lines()
     .fold(Vec::<Self>::new(), |v, l| {
       match l.trim().split(" ").collect::<Vec<_>>().as_slice() {
         | ["move", count, "from", from, "to", to] => {
           v.append_one(Self { count: usize_dec(count),
                               from: usize_dec(from) - 1,
                               to: usize_dec(to) - 1 })
         },
         | other => panic!("{other:?}"),
       }
     })
  }
}

pub struct Input(Manifest, Vec<Instr>);
impl Input {
  pub fn perform_all(self, cb: CraneBehavior) -> Manifest {
    self.1
        .foldl(|man: Manifest, instr: Instr| man.perform(cb, instr), self.0)
  }
}

impl<S> From<S> for Input where S: ToString
{
  fn from(s: S) -> Self {
    let s = s.to_string();
    let lines = s.lines()
                 .filter(|l| !l.trim().is_empty() && !l.trim().starts_with("1"));
    let is_manifest_section = |l: &&str| !l.trim().starts_with("move");
    let join_lines = |acc: String, s: &str| acc.append("\n".into()).append(s.into());
    let manifest = Manifest::from(lines.clone()
                                       .take_while(is_manifest_section)
                                       .fold(String::new(), join_lines));
    let instrs = Instr::parse_many(lines.skip_while(is_manifest_section)
                                        .fold(String::new(), join_lines));
    Self(manifest, instrs)
  }
}

/// The Elves just need to know which crate will end up on top of each stack;
/// in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CM
pub fn get_top_crates<S>(s: S, cb: CraneBehavior) -> String
  where S: ToString
{
  let man = Input::from(s).perform_all(cb);
  man.0.iter().map(Stack::top).map(|c| c.0.as_str()).collect()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn init_and_last() {
    assert_eq!(Vec::<u32>::new().init_and_last(), None);
    assert_eq!(vec![1u32].init_and_last(), Some((vec![], 1u32)));
    assert_eq!(vec![1u32, 2].init_and_last(), Some((vec![1], 2u32)));
  }

  #[test]
  fn part1_sample() {
    assert_eq!(get_top_crates(include_str!("./input.sample"),
                              CraneBehavior::MoveOneCrateAtOnce),
               "CMZ".to_string())
  }

  #[test]
  fn part1() {
    assert_eq!(get_top_crates(include_str!("./input"), CraneBehavior::MoveOneCrateAtOnce),
               "WSFTMRHPP".to_string())
  }

  #[test]
  fn part2_sample() {
    assert_eq!(get_top_crates(include_str!("./input.sample"),
                              CraneBehavior::MoveManyCratesAtOnce),
               "MCD".to_string())
  }

  #[test]
  fn part2() {
    assert_eq!(get_top_crates(include_str!("./input"), CraneBehavior::MoveManyCratesAtOnce),
               "GSLCMFBRP".to_string())
  }
}
