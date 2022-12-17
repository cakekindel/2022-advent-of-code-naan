//! The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good location for a tree house.
//!
//! First, determine whether there is enough tree cover here to keep a tree house hidden. To do this, you need to count the number of trees that are visible from outside the grid when looking directly along a row or column.
//!
//! The Elves have already launched a quadcopter to generate a map with the height of each tree (your puzzle input). For example:
//!
//! ```text
//! 30373
//! 25512
//! 65332
//! 33549
//! 35390
//! ```
//!
//! Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.
//!
//! A tree is visible if all of the other trees between it and an edge of the grid are shorter than it. Only consider trees in the same row or column; that is, only look up, down, left, or right from any given tree.
//!
//! All of the trees around the edge of the grid are visible - since they are already on the edge, there are no trees to block the view. In this example, that only leaves the interior nine trees to consider:
//!
//! * The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
//! * The top-middle 5 is visible from the top and right.
//! * The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
//! * The left-middle 5 is visible, but only from the right.
//! * The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
//! * The right-middle 3 is visible from the right.
//! * In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
//!
//! With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement.

use std::marker::PhantomPinned;
use std::pin::Pin;
use std::ptr::NonNull;

use naan::prelude::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub struct TreeHeight(pub u8);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct Forest2D(pub Vec<Vec<TreeHeight>>);

impl Forest2D {
  pub fn parse<S: AsRef<str>>(s: S) -> Self {
    Self(s.as_ref()
          .trim()
          .split('\n')
          .filter(|s| !s.is_empty())
          .map(|line| {
            line.split("")
                .filter(|s| !s.is_empty())
                .map(|n| u8::from_str_radix(n, 10).unwrap())
                .map(TreeHeight)
                .collect()
          })
          .collect())
  }

  pub fn width(&self) -> usize {
    self.0[0].len()
  }

  pub fn height(&self) -> usize {
    self.0.len()
  }
}

/// A tree in a [`Forest`]
pub struct Tree {
  height: TreeHeight,
  graph: Option<&'static Vec<Vec<Tree>>>,
  above_xy: Option<(usize, usize)>,
  below_xy: Option<(usize, usize)>,
  left_xy: Option<(usize, usize)>,
  right_xy: Option<(usize, usize)>,
}

impl core::fmt::Debug for Tree {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("Tree")
     .field("height", &self.height)
     .field("right_xy", &self.right_xy)
     .field("left_xy", &self.left_xy)
     .field("below_xy", &self.below_xy)
     .field("above_xy", &self.above_xy)
     .finish_non_exhaustive()
  }
}

impl Tree {
  pub fn height(&self) -> TreeHeight {
    self.height
  }

  pub fn above(&self) -> Option<&Self> {
    self.graph
        .and_then(|nodes| self.above_xy.map(|(x, y)| &nodes[y][x]))
  }

  pub fn below(&self) -> Option<&Self> {
    self.graph
        .and_then(|nodes| self.below_xy.map(|(x, y)| &nodes[y][x]))
  }

  pub fn left(&self) -> Option<&Self> {
    self.graph
        .and_then(|nodes| self.left_xy.map(|(x, y)| &nodes[y][x]))
  }

  pub fn right(&self) -> Option<&Self> {
    self.graph
        .and_then(|nodes| self.right_xy.map(|(x, y)| &nodes[y][x]))
  }

  pub fn travel<F>(&self, f: F) -> Vec<TreeHeight>
    where F: for<'x> F1<&'x Tree, Option<&'x Tree>>
  {
    fn go<F>(f: F, node: &Tree, vec: Vec<TreeHeight>) -> Vec<TreeHeight>
      where F: for<'x> F1<&'x Tree, Option<&'x Tree>>
    {
      match f.call(&node) {
        | Some(node) => go(f, node, vec.append_one(node.height)),
        | None => vec,
      }
    }

    go(f, &self, vec![])
  }
}

impl PartialEq for Tree {
  fn eq(&self, other: &Self) -> bool {
    self.height == other.height
  }
}

impl PartialOrd for Tree {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.height.partial_cmp(&other.height)
  }
}

/// A [complete digraph](https://en.wikipedia.org/wiki/Complete_graph) of trees,
/// where each tree has directed edges pointing in all 4 cardinal directions.
#[derive(PartialEq, PartialOrd, Debug)]
pub struct Forest(Vec<Vec<Tree>>);

impl Forest {
  pub fn new(trees: &Forest2D) -> Pin<Box<Self>> {
    let mut nodes: Vec<Vec<Tree>> = vec![];

    let max_y = trees.height() - 1;
    let max_x = trees.width() - 1;

    for y in 0..trees.height() {
      let mut row = Vec::<Tree>::new();

      for x in 0..trees.width() {
        row.push(Tree { height: trees.0[y][x],
                        graph: None,
                        above_xy: if y > 0 { Some((x, y - 1)) } else { None },
                        below_xy: if y < max_y { Some((x, y + 1)) } else { None },
                        left_xy: if x > 0 { Some((x - 1, y)) } else { None },
                        right_xy: if x < max_x { Some((x + 1, y)) } else { None } });
      }

      nodes.push(row);
    }

    let mut me = Box::pin(Self(nodes));

    // use NonNull to cast &'_ Vec<Vec<Tree>> to &'static Vec<Vec<Tree>>
    let nodes_ptr = NonNull::from(&me.0);

    Pin::get_mut(me.as_mut()).0.iter_mut().for_each(|row| {
                                            row.iter_mut().for_each(|node| {
                                                            // SAFETY:
                                                            // * we use `'static` in place of an imaginary `'self` lifetime;
                                                            //   from the perspective of Tree, the reference may as well be
                                                            //   static since it will live at least as long as the Tree.
                                                            // * nodes_ptr is well-aligned, dereferenceable
                                                            // * reference will not outlive this `Forest`
                                                            //   (Forest owns Tree owns reference, none are `Clone` and reference is not directly accessible)
                                                            // * data pointed to will not be moved out of this `Forest`
                                                            //   (2d vec owned by forest is not directly accessible)
                                                            node.graph =
                                                              Some(unsafe { nodes_ptr.as_ref() })
                                                          })
                                          });

    me
  }

  pub fn top_left(&self) -> &Tree {
    &self.0[0][0]
  }

  pub fn iter<'a>(&'a self) -> ForestIter<'a> {
    ForestIter { forest: self,
                 x: 0,
                 y: 0 }
  }
}

pub struct ForestIter<'a> {
  forest: &'a Forest,
  x: usize,
  y: usize,
}

impl<'a> Iterator for ForestIter<'a> {
  type Item = &'a Tree;

  fn next(&mut self) -> Option<Self::Item> {
    if self.x == self.forest.0[0].len() && self.y < self.forest.0.len() - 1 {
      self.x = 0;
      self.y += 1;
    }

    if self.x < self.forest.0[0].len() {
      let tree = &self.forest.0[self.y][self.x];
      self.x += 1;
      Some(tree)
    } else {
      None
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn parsing() {
    assert_eq!(Forest2D::parse(include_str!("./input.sample")).0[0].clone()
                                                                   .fmap(|TreeHeight(n)| n),
               vec![3, 0, 3, 7, 3]);
  }

  #[test]
  fn forest() {
    let forest = Forest::new(&Forest2D(vec![vec![TreeHeight(1), TreeHeight(2), TreeHeight(3),],
                                            vec![TreeHeight(4), TreeHeight(5), TreeHeight(6)]]));

    let top_left = forest.top_left();
    let bot_left = top_left.below().unwrap();

    assert_eq!(top_left.height(), TreeHeight(1));
    assert_eq!(bot_left.height(), TreeHeight(4));
    assert_eq!(bot_left.travel(Tree::right),
               vec![TreeHeight(5), TreeHeight(6)]);
  }

  fn do_part1<S>(input: S) -> usize
    where S: AsRef<str>
  {
    let forest = Forest::new(&Forest2D::parse(input));

    fn shorter(a: TreeHeight, b: &TreeHeight) -> bool {
      b < &a
    }

    forest.iter()
          .filter(|t| {
            let shorter = shorter.curry().call(t.height());

            vec![Tree::above as fn(&Tree) -> Option<&Tree>,
                 Tree::below,
                 Tree::left,
                 Tree::right].any(|dir| t.travel(dir).all(shorter))
          })
          .count()
  }

  #[test]
  fn part1_sample() {
    assert_eq!(do_part1(include_str!("./input.sample")), 21)
  }

  #[test]
  fn part1() {
    assert_eq!(do_part1(include_str!("./input")), 1787)
  }

  fn do_part2<S>(input: S) -> usize
    where S: AsRef<str>
  {
    let forest = Forest::new(&Forest2D::parse(input));

    fn viewing_distance(t: TreeHeight, v: Vec<TreeHeight>) -> usize {
      v.foldl(|(d, cont): (usize, bool), t_| {
                if !cont {
                  (d, cont)
                } else if t_ >= t {
                  (d + 1, false)
                } else {
                  (d + 1, true)
                }
              },
              (0, true))
       .0
    }

    forest.iter()
          .map(|t| {
            let viewing_distance = viewing_distance.curry().call(t.height());

            vec![Tree::above as fn(&Tree) -> Option<&Tree>,
                 Tree::below,
                 Tree::left,
                 Tree::right].fmap(|dir| t.travel(dir))
                             .fmap(viewing_distance)
                             .foldl(|score, dist| score * dist, 1)
          })
          .fold(Option::<usize>::None, |winner: Option<usize>, cur| {
            Some(winner.filter(|w| w > &cur).unwrap_or(cur))
          })
          .unwrap()
  }

  #[test]
  fn part2_sample() {
    assert_eq!(do_part2(include_str!("./input.sample")), 8)
  }

  #[test]
  fn part2() {
    assert_eq!(do_part2(include_str!("./input")), 440640)
  }
}
