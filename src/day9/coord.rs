#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Coord {
  pub x: isize,
  pub y: isize,
}

impl Coord {
  pub fn mv(self, dir: Dir) -> Self {
    let Coord { x, y } = self;

    match dir {
      | Dir::Up => Coord { x, y: y + 1 },
      | Dir::Dn => Coord { x, y: y - 1 },
      | Dir::Left => Coord { x: x - 1, y },
      | Dir::Rght => Coord { x: x + 1, y },
    }
  }

  pub fn follow(self, other: Self) -> Self {
    match Gap::between(self, other) {
      | Some(Gap::Linear(d)) => self.mv(d),
      | Some(Gap::Diagonal(a, b)) => self.mv(a).mv(b),
      | None => self,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Dir {
  Up,
  Dn,
  Left,
  Rght,
}
impl Dir {
  pub fn parse<S>(s: S) -> Self
    where S: AsRef<str>
  {
    match &s.as_ref()[0..1] {
      | "U" => Self::Up,
      | "D" => Self::Dn,
      | "L" => Self::Left,
      | "R" => Self::Rght,
      | _ => panic!("{:?} is not a direction", s.as_ref()),
    }
  }
}

pub enum Gap {
  Linear(Dir),
  Diagonal(Dir, Dir),
}

impl Gap {
  pub fn between(from: Coord, to: Coord) -> Option<Self> {
    let dx = from.x.abs_diff(to.x);
    let dy = from.y.abs_diff(to.y);

    let y_dir = if from.y > to.y { Dir::Dn } else { Dir::Up };

    let x_dir = if from.x > to.x { Dir::Left } else { Dir::Rght };

    if dx <= 1 && dy <= 1 {
      None
    } else if dx == 0 {
      Some(Self::Linear(y_dir))
    } else if dy == 0 {
      Some(Self::Linear(x_dir))
    } else {
      Some(Self::Diagonal(x_dir, y_dir))
    }
  }
}
