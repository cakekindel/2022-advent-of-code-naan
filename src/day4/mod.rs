//! Space needs to be cleared before the last supplies can be unloaded from the ships, and so several
//! Elves have been assigned the job of cleaning up sections of the camp. Every section has a unique
//! ID number, and each Elf is assigned a range of section IDs.
//!
//! However, as some of the Elves compare their section assignments with each other, they've noticed that
//! many of the assignments overlap. To try to quickly find overlaps and reduce duplicated effort, the
//! Elves pair up and make a big list of the section assignments for each pair (your puzzle input).
use core::ops::RangeInclusive;

fn u32_from_str<S>(s: S) -> u32
  where S: AsRef<str>
{
  u32::from_str_radix(s.as_ref(), 10).unwrap()
}

// these could be free functions but I can't be bothered to write type annotations
macro_rules! parse_range {
  ($l:expr) => {
    |r| match r.trim().split("-").collect::<Vec<_>>().as_slice() {
      | [a, b] => [a.to_string(), b.to_string()],
      | _ => panic!("{}", $l),
    }
  };
}

macro_rules! parse_line {
  () => {
    |l| match l.trim()
               .split(",")
               .map(parse_range!(l))
               .collect::<Vec<_>>()
               .as_slice()
    {
      | [[aa, ab], [ba, bb]] => (RangeInclusive::new(u32_from_str(aa), u32_from_str(ab)),
                                 RangeInclusive::new(u32_from_str(ba), u32_from_str(bb))),
      | _ => panic!("{l}"),
    }
  };
}

macro_rules! parse_ranges {
  ($s:expr) => {
    $s.to_string().trim().lines().map(parse_line!())
  };
}

/// Some of the pairs have noticed that one of their assignments fully contains the other.
/// In pairs where one assignment fully contains the other, one Elf in the pair would be exclusively
/// cleaning sections their partner will already be cleaning, so these seem like the most in need of
/// reconsideration.
///
/// In how many assignment pairs does one range fully contain the other?
pub fn count_pairs_where_one_fully_contained_by_other<S>(s: S) -> usize
  where S: ToString
{
  parse_ranges!(s).filter(|(a, b)| {
                    (a.start() <= b.start() && a.end() >= b.end())
                    || (b.start() <= a.start() && b.end() >= a.end())
                  })
                  .count()
}

pub fn count_overlapping_pairs<S>(s: S) -> usize
  where S: ToString
{
  parse_ranges!(s).filter(|(a, b)| {
                    b.clone().into_iter().any(|b_| a.contains(&b_))
                    || a.clone().into_iter().any(|a_| b.contains(&a_))
                  })
                  .count()
}

#[cfg(test)]
mod test {
  use super::{count_overlapping_pairs, count_pairs_where_one_fully_contained_by_other};

  #[test]
  fn part_1_sample() {
    assert_eq!(count_pairs_where_one_fully_contained_by_other(include_str!("./input.sample")),
               2)
  }

  #[test]
  fn part_1() {
    assert_eq!(count_pairs_where_one_fully_contained_by_other(include_str!("./input")),
               483)
  }

  #[test]
  fn part_2_sample() {
    assert_eq!(count_overlapping_pairs(include_str!("./input.sample")), 4)
  }

  #[test]
  fn part_2() {
    assert_eq!(count_overlapping_pairs(include_str!("./input")), 874)
  }
}
