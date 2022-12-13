//! The preparations are finally complete; you and the Elves leave camp on foot and begin to make your way toward
//! the star fruit grove.
//!
//! As you move through the dense undergrowth, one of the Elves gives you a handheld device.
//! He says that it has many fancy features, but the most important one to set up right now is the
//! communication system.
//!
//! However, because he's heard you have significant experience dealing with signal-based systems, he
//! convinced the other Elves that it would be okay to give you their one malfunctioning device - surely
//! you'll have no problem fixing it.
//!
//! As if inspired by comedic timing, the device emits a few colorful sparks.
//!
//! To be able to communicate with the Elves, the device needs to lock on to their signal. The signal is a
//! series of seemingly-random characters that the device receives one at a time.
//!
//! To fix the communication system, you need to add a subroutine to the device that detects a start-of-packet
//! marker in the datastream. In the protocol being used by the Elves, the start of a packet is indicated by a
//! sequence of four characters that are all different.
//!
//! The device will send your subroutine a datastream buffer (your puzzle input); your subroutine needs to
//! identify the first position where the four most recently received characters were all different.
//! Specifically, it needs to report the number of characters from the beginning of the buffer to the
//! end of the first such four-character marker.
use naan::prelude::*;

use super::day5::SplitVec;

pub fn idx_of_first_uniq_sequence<'a, S: IntoIterator<Item = &'a str>>(len: usize, s: S) -> usize {
  let (_, found, _) =
    s.into_iter()
     .fold((0usize, Option::<usize>::empty(), Vec::<&str>::empty()),
           |(ix, found, buf), c| {
             if buf.len() < len {
               (ix + 1, None, buf.append_one(c))
             } else if found.is_some() {
               (ix + 1, found, buf)
             } else {
               let buf = buf.split_vec(1).get_or((vec![], vec![])).1.append_one(c);

               (ix + 1,
                if buf.clone().all(|c: &&str| {
                                buf.clone()
                                   .foldl(|ct, c_| if c_ == *c { ct + 1 } else { ct }, 0)
                                == 1
                              })
                {
                  Some(ix)
                } else {
                  None
                },
                buf)
             }
           });

  found.unwrap()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample() {
    assert_eq!(idx_of_first_uniq_sequence(4,
                                          include_str!("./input.sample").trim()
                                                                        .split("")
                                                                        .filter(|c| {
                                                                          !c.trim().is_empty()
                                                                        })),
               6);
  }

  #[test]
  fn part1() {
    assert_eq!(idx_of_first_uniq_sequence(4,
                                          include_str!("./input").trim()
                                                                 .split("")
                                                                 .filter(|c| !c.trim()
                                                                               .is_empty())),
               1565);
  }

  #[test]
  fn part2_sample() {
    assert_eq!(idx_of_first_uniq_sequence(14,
                                          include_str!("./input.sample").trim()
                                                                        .split("")
                                                                        .filter(|c| {
                                                                          !c.trim().is_empty()
                                                                        })),
               18);
  }

  #[test]
  fn part2() {
    assert_eq!(idx_of_first_uniq_sequence(14,
                                          include_str!("./input").trim()
                                                                 .split("")
                                                                 .filter(|c| !c.trim()
                                                                               .is_empty())),
               2264);
  }
}
