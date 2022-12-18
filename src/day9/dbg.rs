use naan::prelude::*;

use super::Coord;

#[derive(PartialEq, Clone, Copy)]
pub enum DebugGridMode {
  Nodes,
  Visited,
}

pub struct DebugGrid {
  mode: DebugGridMode,
  hgt: usize,
  wdt: usize,
  dots: Vec<Vec<String>>,
  touched: Vec<Coord>,
}

impl DebugGrid {
  pub fn new(mode: DebugGridMode, height: usize, width: usize) -> Self {
    Self { mode,
                          hgt: height,
                          wdt: width,
                          dots:core::iter::repeat(core::iter::repeat("·".to_string()).take(width).collect::<Vec<_>>()).take(height).collect::<Vec<_>>()
                              ,
                          touched: vec![] }
  }

  fn idx(&self, Coord { x, y }: Coord) -> (usize, usize) {
    let (x, y) = (x + (self.wdt as isize / 2), y + (self.hgt as isize / 2));
    (x as usize, self.hgt - y as usize)
  }

  pub fn touch(&mut self, c: Coord) -> () {
    let (x, y) = self.idx(c);
    let spot = &mut self.dots[y][x];

    self.touched.push(c);
    *spot = "#".to_string();
  }

  pub fn render(mut self) -> String {
    if self.mode == DebugGridMode::Nodes {
      self.touched
          .clone()
          .into_iter()
          .enumerate()
          .for_each(|(ix, c)| {
            let dot = if ix + 1 == self.touched.len() {
              "H".to_string()
            } else {
              (self.touched.len() - (ix + 1)).to_string()
            };

            let (x, y) = self.idx(c);
            self.dots[y][x] = dot;
          });
    }

    self.dots
        .fmap(|line: Vec<String>| line.fold())
        .intercalate("\n".to_string())
  }
}
