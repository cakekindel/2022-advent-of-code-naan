use std::collections::HashSet;

use naan::prelude::*;

pub mod coord;
pub mod dbg;

use coord::{Coord, Dir};

use self::dbg::{DebugGrid, DebugGridMode};

#[derive(Debug, Clone, Copy)]
pub struct Move(pub Dir, pub usize);
impl Move {
  pub fn parse<S>(s: S) -> Self
    where S: AsRef<str>
  {
    let (dir, n) =
      s.as_ref().split(" ").fold((None, None),
                                 |(dir, n): (Option<Dir>, Option<usize>), chars| match (dir, n) {
                                   | (dir @ Some(_), n @ Some(_)) => (dir, n),
                                   | (dir @ Some(_), None) => {
                                     (dir, Some(usize::from_str_radix(chars, 10).unwrap()))
                                   },
                                   | (None, _) => (Some(Dir::parse(chars)), None),
                                 });
    Self(dir.unwrap(), n.unwrap())
  }

  pub fn parse_many<S>(s: S) -> Vec<Self>
    where S: AsRef<str>
  {
    s.as_ref().trim().split("\n").map(Self::parse).collect()
  }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Visited(pub Coord);
pub struct Head(pub Coord);
pub struct Btwn(pub Vec<Coord>);
pub struct Tail(pub Coord);

pub struct Grid(pub HashSet<Visited>, pub Tail, pub Option<Btwn>, pub Head);
impl Grid {
  pub fn iter(&self) -> impl Iterator<Item = Coord> {
    use core::iter::once;
    once(self.1 .0).chain(self.2.as_ref().fold1(|_, Btwn(v): &Btwn| v.clone(), vec![]))
                   .chain(once(self.3 .0))
  }

  pub fn iter_visited(&self) -> impl Iterator<Item = &Visited> {
    self.0.iter()
  }

  pub fn debug_string(&self, mode: DebugGridMode) -> String {
    let mut dbg = DebugGrid::new(mode, 80, 80);

    match mode {
      | DebugGridMode::Nodes => self.iter().for_each(|c| dbg.touch(c)),
      | DebugGridMode::Visited => self.iter_visited().for_each(|Visited(c)| dbg.touch(*c)),
    };

    dbg.render()
  }

  pub fn modify_head<F>(self, f: F) -> Self
    where F: F1Once<Head, Head>
  {
    Self(self.0, self.1, self.2, f.call1(self.3))
  }

  pub fn new_len(n: usize) -> Self {
    let origin = Coord { x: 0, y: 0 };
    if n == 2 {
      Grid(HashSet::from([Visited(origin)]),
           Tail(origin),
           None,
           Head(origin))
    } else {
      Grid(HashSet::from([Visited(origin)]),
           Tail(origin),
           Some(Btwn(core::iter::repeat(origin).take(n - 2).collect())),
           Head(origin))
    }
  }

  pub fn new() -> Self {
    Self::new_len(2)
  }

  pub fn head_moved(self) -> Self {
    let Grid(mut v, Tail(tail), btwn, Head(head)) = self;

    let btwn = match btwn {
      | Some(Btwn(b)) => Some(Btwn(b.foldr(|cur: Coord, (coords, last): (Vec<Coord>, Coord)| {
                                             (vec![cur.follow(last)].append(coords),
                                              cur.follow(last))
                                           },
                                           (Vec::<Coord>::new(), head))
                                    .0)),
      | None => btwn,
    };

    let tail = tail.follow(btwn.as_ref().and_then(|b| b.0.get(0)).copied().get_or(head));
    v.insert(Visited(tail));

    Grid(v, Tail(tail), btwn, Head(head))
  }

  pub fn mv(self, Move(dir, ct): Move) -> Self {
    fn mv1(g: Grid, dir: Dir, n: usize) -> Grid {
      let g = g.modify_head(|Head(c)| Head(c.mv(dir)));

      if n == 1 {
        g.head_moved()
      } else {
        mv1(g.head_moved(), dir, n - 1)
      }
    }

    mv1(self, dir, ct)
  }
}

#[cfg(test)]
mod tests {
  use super::dbg::{DebugGrid, DebugGridMode};
  use super::*;

  #[test]
  fn part1_sample() {
    let grid = Move::parse_many(include_str!("./input.sample")).foldl(|grid: Grid, mv| grid.mv(mv),
                                                                      Grid::new());
    assert_eq!(grid.iter_visited().count(), 13)
  }

  #[test]
  fn part1() {
    let grid =
      Move::parse_many(include_str!("./input")).foldl(|grid: Grid, mv| grid.mv(mv), Grid::new());
    assert_eq!(grid.iter_visited().count(), 6256)
  }

  #[test]
  fn part2_sample() {
    let grid = Move::parse_many(include_str!("./input.sample")).foldl(|grid: Grid, mv| grid.mv(mv),
                                                                      Grid::new_len(10));
    assert_eq!(grid.iter_visited().count(), 1)
  }

  #[test]
  fn part2_sample2() {
    let grid =
      Move::parse_many(include_str!("./input.sample2")).foldl(|grid: Grid, mv| grid.mv(mv),
                                                              Grid::new_len(10));

    assert_eq!(grid.iter_visited().count(), 36)
  }

  #[test]
  fn part2() {
    let grid = Move::parse_many(include_str!("./input")).foldl(|grid: Grid, mv| grid.mv(mv),
                                                               Grid::new_len(10));
    assert_eq!(grid.iter_visited().count(), 2665)
  }
}
