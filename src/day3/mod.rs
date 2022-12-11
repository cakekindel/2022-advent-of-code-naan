//! One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey.
//! Unfortunately, that Elf didn't quite follow the packing instructions, and so a few items now need to be
//! rearranged.
//!
//! Each rucksack has two large compartments. All items of a given type are meant to go into exactly one of the
//! two compartments. The Elf that did the packing failed to follow this rule for exactly one item type per
//! rucksack.
//!
//! The Elves have made a list of all of the items currently in each rucksack (your puzzle input),
//! but they need your help finding the errors.
//! Every item type is identified by a single lowercase or uppercase letter (that is, a and A refer to different
//! types of items).
//!
//! The list of items for each rucksack is given as characters all on a single line.
//! A given rucksack always has the same number of items in each of its two compartments,
//! so the first half of the characters represent items in the first compartment, while the second half
//! of the characters represent items in the second compartment.

use std::iter::FromIterator;

use naan::prelude::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct Letter(pub u8);
impl Letter {
  pub fn priority(&self) -> u32 {
    match self.0 as u32 {
      // UTF8 capital alphas
      | 0x41..=0x5A => ((self.0 as u32) - 0x41) + 27,
      // UTF8 lowercase alphas
      | 0x61..=0x7A => ((self.0 as u32) - 0x61) + 1,
      | _ => panic!("{} not alpha", core::str::from_utf8(&[self.0]).unwrap()),
    }
  }

  /// Is this letter contained in all of `F<F<Letter>>`? (where `F` is `Foldable)
  pub fn in_all<Hkt, F, G>(&self, f: F) -> bool
    where F: Foldable<Hkt, G>,
          G: Foldable<Hkt, Self>,
          Hkt: HKT1<T<G> = F> + HKT1<T<Self> = G>
  {
    f.all(|fa: &G| fa.contains(self))
  }
}

impl<S> From<S> for Letter where S: AsRef<str>
{
  fn from(s: S) -> Self {
    match s.as_ref().as_bytes() {
      | [b] => Letter(*b),
      | _ => panic!("{} has len > 1", s.as_ref()),
    }
  }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Pocket(pub Vec<Letter>);

impl Pocket {
  pub fn letters(self) -> Vec<Letter> {
    self.0
  }
}

impl<S> FromIterator<S> for Pocket where S: AsRef<str>
{
  fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
    Self(iter.into_iter()
             .map(|s| String::from(s.as_ref()))
             .map(Letter::from)
             .collect())
  }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Rucksack(pub Vec<Pocket>);

impl Rucksack {
  pub fn pockets(self) -> Vec<Pocket> {
    self.0
  }

  pub fn letters(self) -> Vec<Letter> {
    self.pockets().fmap(Pocket::letters).flatten()
  }

  pub fn letter_in_all_pockets(&self) -> Option<Letter> {
    self.0
        .clone()
        .foldl(|shared: Vec<Letter>, p: Pocket| {
                 shared.append(p.0
                                .find(|l: &Letter| {
                                  l.in_all(self.clone().pockets().fmap(Pocket::letters))
                                })
                                .map(Vec::pure)
                                .get_or(vec![]))
               },
               vec![])
        .drain(0..1)
        .next()
  }
}

impl<S> From<S> for Rucksack where S: AsRef<str>
{
  fn from(s: S) -> Self {
    let s = s.as_ref();
    let chars = || s.trim().split("").filter(|s| s != &"");
    let half = s.len() / 2;

    Self(vec![chars().take(half).collect(), chars().skip(half).collect()])
  }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Input(pub Vec<Rucksack>);

impl Input {
  /// To help prioritize item rearrangement, every item type can be converted to a priority:
  ///
  /// * Lowercase item types a through z have priorities 1 through 26.
  /// * Uppercase item types A through Z have priorities 27 through 52.
  ///
  /// In the above example, the priority of the item type that appears in both compartments of each rucksack is
  /// 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157.
  ///
  /// Find the item type that appears in both compartments of each rucksack.
  /// What is the sum of the priorities of those item types?
  pub fn sum_of_all_shared_letter_priorities(&self) -> u32 {
    self.0.clone().foldl(|sum, rucksack: Rucksack| {
                           sum
                           + rucksack.letter_in_all_pockets()
                                     .expect("all rucksacks should share 1 letter")
                                     .priority()
                         },
                         0u32)
  }

  /// As you finish identifying the misplaced items, the Elves come to you with another issue.
  ///
  /// For safety, the Elves are divided into groups of three. Every Elf carries a badge that
  /// identifies their group. For efficiency, within each group of three Elves, the badge
  /// is the only item type carried by all three Elves. That is, if a group's badge is item type B,
  /// then all three Elves will have item type B somewhere in their rucksack, and at most two
  /// of the Elves will be carrying any other item type.
  ///
  /// The problem is that someone forgot to put this year's updated authenticity sticker on the badges.
  /// All of the badges need to be pulled out of the rucksacks so the new authenticity stickers can be attached.
  ///
  /// Additionally, nobody wrote down which item type corresponds to each group's badges.
  /// The only way to tell which item type is the right one is by finding the one item type
  /// that is common between all three Elves in each group.
  ///
  /// Every set of three lines in your list corresponds to a single group, but each group can have
  /// a different badge item type. So, in the above example, the first group's rucksacks are the
  /// first three lines:
  ///
  /// vJrwpWtwJgWrhcsFMMfFFhFp
  /// jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
  /// PmmdzqPrVvPwwTWBwg
  ///
  /// And the second group's rucksacks are the next three lines:
  ///
  /// wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
  /// ttgJtRGJQctTZtZT
  /// CrZsJsPPZsGzwwsLwLmpwMDw
  ///
  /// In the first group, the only item type that appears in all three rucksacks is lowercase r;
  /// this must be their badges. In the second group, their badge item type must be Z.
  ///
  /// Priorities for these items must still be found to organize the sticker attachment efforts: here,
  /// they are 18 (r) for the first group and 52 (Z) for the second group. The sum of these is 70.
  ///
  /// Find the item type that corresponds to the badges of each three-Elf group.
  /// What is the sum of the priorities of those item types?
  pub fn sum_of_all_group_badges(&self) -> u32 {
    self.0
        .chunks_exact(3)
        .into_iter()
        .map(Vec::from)
        .filter_map(|rucks| {
          rucks.clone().find_map(|ruck: Rucksack| {
                         ruck.clone()
                             .letters()
                             .find(|l: &Letter| l.in_all(rucks.clone().fmap(Rucksack::letters)))
                       })
        })
        .fold(0u32, |sum, l| sum + l.priority())
  }
}

impl<S> From<S> for Input where S: AsRef<str>
{
  fn from(s: S) -> Self {
    Self(s.as_ref().trim().lines().map(Rucksack::from).collect())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part_1_sample() {
    let input = Input::from(include_str!("./input.sample"));
    assert_eq!(input.sum_of_all_shared_letter_priorities(), 157)
  }

  #[test]
  fn part_1() {
    let input = Input::from(include_str!("./input"));
    assert_eq!(input.sum_of_all_shared_letter_priorities(), 7903)
  }

  #[test]
  fn part_2_sample() {
    let input = Input::from(include_str!("./input.sample"));
    assert_eq!(input.sum_of_all_group_badges(), 70)
  }

  #[test]
  fn part_2() {
    let input = Input::from(include_str!("./input"));
    assert_eq!(input.sum_of_all_group_badges(), 2548)
  }
}
