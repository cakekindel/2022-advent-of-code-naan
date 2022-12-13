//! You can hear birds chirping and raindrops hitting leaves as the expedition proceeds. Occasionally, you can even hear much louder sounds in the distance; how big do the animals get out here, anyway?
//!
//! The device the Elves gave you has problems with more than just its communication system. You try to run a system update:
//!
//! $ system-update --please --pretty-please-with-sugar-on-top
//! Error: No space left on device
//!
//! Perhaps you can delete some files to make space for the update?

use std::path::{Path, PathBuf};

use naan::prelude::*;

use super::day5::SplitVec;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ent {
  Dir(PathBuf, Vec<Ent>),
  File(PathBuf, u32),
}

impl Ent {
  pub fn path(&self) -> &Path {
    match self {
      | Ent::File(p, _) | Ent::Dir(p, _) => &p,
    }
  }

  fn flat_into<P>(prefix: P,
                  es: Vec<Ent>,
                  into: Vec<(PathBuf, Vec<(PathBuf, u32)>)>)
                  -> Vec<(PathBuf, Vec<(PathBuf, u32)>)>
    where P: AsRef<Path>
  {
    let (into, me) =
      es.foldl(|(into, me): (Vec<_>, Vec<(PathBuf, u32)>), e| match e {
                 | Ent::File(p, s) => (into, me.append_one((prefix.as_ref().join(p), s))),
                 | Ent::Dir(p, es) => {
                   let files = Self::flat_into(prefix.as_ref().join(p), es, vec![]);
                   (into.append(files.clone()),
                    files.foldl(|me: Vec<_>, (p, files): (PathBuf, Vec<_>)| {
                                  me.append(files.fmap(|(p_, s)| (p.join(p_), s)))
                                },
                                me))
                 },
               },
               (into, Vec::<(PathBuf, u32)>::empty()));

    into.append_one((prefix.as_ref().into(), me))
  }

  pub fn flat(self) -> Vec<(PathBuf, Vec<(PathBuf, u32)>)> {
    match self {
      | Ent::Dir(p, es) => {
        Self::flat_into(p, es, vec![]).foldl(|dedupt: Vec<_>, (p, fs): (PathBuf, Vec<_>)| {
                 if dedupt.any(|(p_, _): &(PathBuf, _)| p_ == &p) {
                   dedupt
                 } else {
                   let fs = fs.foldl(|dedupt: Vec<_>, (p, s): (PathBuf, u32)| {
                                       if dedupt.any(|(p_, _): &(PathBuf, _)| p_ == &p) {
                                         dedupt
                                       } else {
                                         dedupt.append_one((p, s))
                                       }
                                     },
                                     vec![]);
                   dedupt.append_one((p, fs))
                 }
               },
               vec![])
      },
      | _ => panic!(),
    }
  }

  pub fn parse<S>(s: S) -> Ent
    where S: AsRef<str>
  {
    match s.as_ref().trim().split(" ").collect::<Vec<_>>().as_slice() {
      | ["dir", d] => Ent::Dir(PathBuf::from(d), Vec::empty()),
      | [size, f] => Ent::File(PathBuf::from(f), u32::from_str_radix(size, 10).unwrap()),
      | other => panic!("{other:?}"),
    }
  }

  pub fn modify<F, P>(self, p: P, f: F) -> Self
    where F: F1Once<Self, Self>,
          P: AsRef<Path>
  {
    match self {
      | Ent::Dir(path, ents) => {
        let mut iter = p.as_ref().iter().filter(|s| s.to_str().unwrap() != "/");
        let f = Some(f);
        // TODO(naan): fmap_mut?
        let take_f =
          || unsafe { Option::take((&f as *const _ as *mut Option<F>).as_mut().unwrap()) };

        // recurse until whole path consumed
        // when path is empty, we own the dir we want to modify
        match iter.next() {
          | Some(top) => Ent::Dir(path,
                                  ents.fmap(|e: Ent| {
                                        if e.path() == Path::new(top.clone()) {
                                          e.modify(iter.clone().collect::<PathBuf>(),
                                                   take_f().expect("f should only be called once"))
                                        } else {
                                          e
                                        }
                                      })),
          | None => f.unwrap().call1(Ent::Dir(path, ents)),
        }
      },
      | Ent::File(f, _) => panic!("cannot modify regular file {f:?}"),
    }
  }
}

#[derive(Debug, Clone)]
pub enum Cmd {
  Ls(Vec<Ent>),
  CdUp,
  Cd(PathBuf),
}

impl Cmd {
  pub fn parse<S>(s: S) -> Cmd
    where S: AsRef<str>
  {
    let mut iter = s.as_ref().split("\n").filter(|c| !c.trim().is_empty());
    match iter.next()
              .unwrap()
              .trim()
              .split(" ")
              .collect::<Vec<_>>()
              .as_slice()
    {
      | ["$", "cd", ".."] => Cmd::CdUp,
      | ["$", "cd", p] => Cmd::Cd(PathBuf::from(p)),
      | ["$", "ls"] => Cmd::Ls(iter.map(Ent::parse).collect()),
      | other => panic!("{other:?}"),
    }
  }

  pub fn parse_many<S>(s: S) -> Vec<Cmd>
    where S: AsRef<str>
  {
    s.as_ref()
     .split("\n")
     .filter(|c| !c.trim().is_empty())
     .fold(Vec::<String>::pure("".into()), |cmds, line| {
       let (init, last) = cmds.init_and_last().unwrap();
       if line.trim().starts_with("$") {
         init.append_one(last).append_one(line.into())
       } else {
         init.append_one(last.append("\n".into()).append(line.into()))
       }
     })
     .into_iter()
     .filter(|s| !s.trim().is_empty())
     .map(|s| Cmd::parse(s))
     .collect()
  }
}

#[derive(Debug, Clone)]
pub struct Workdir(pub PathBuf);
#[derive(Debug, Clone)]
pub struct Fs(pub Ent);
#[derive(Debug, Clone)]
pub struct Shell(pub Fs, pub Workdir);
impl Shell {
  pub fn new() -> Self {
    Self(Fs(Ent::Dir("/".into(), vec![])), Workdir("/".into()))
  }

  pub fn fs(&self) -> &Fs {
    &self.0
  }

  pub fn modify_workdir<F>(self, f: F) -> Self
    where F: F1Once<Workdir, Workdir>
  {
    Self(self.0, f.call1(self.1))
  }

  pub fn modify_fs<F>(self, f: F) -> Self
    where F: F2Once<Workdir, Fs, Fs>
  {
    Self(f.call1(self.1.clone(), self.0), self.1)
  }

  pub fn process(self, cmds: Vec<Cmd>) -> Self {
    cmds.foldl(|sh: Self, cmd| sh.process_one(cmd), self)
  }

  pub fn process_one(self, cmd: Cmd) -> Self {
    match cmd {
      | Cmd::CdUp => self.modify_workdir(|mut w: Workdir| {
                           w.0.pop();
                           w
                         }),
      | Cmd::Cd(path) => self.modify_workdir(|mut w: Workdir| {
                               w.0.push(path.clone());
                               w
                             }),
      | Cmd::Ls(ents) => self.modify_fs(|workdir: Workdir, fs: Fs| {
                               Fs(fs.0.modify(workdir.0, |ent: Ent| match ent {
                                        | Ent::Dir(p, _) => Ent::Dir(p, ents),
                                        | _ => panic!("expected {ent:?} to be a dir"),
                                      }))
                             }),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parse_cmds() {
    assert!(matches!(Cmd::parse_many(include_str!("./input.sample")).as_slice(),
                     [Cmd::Cd(_),
                      Cmd::Ls(_),
                      Cmd::Cd(_),
                      Cmd::Ls(_),
                      Cmd::Cd(_),
                      Cmd::Ls(_),
                      Cmd::CdUp,
                      Cmd::CdUp,
                      Cmd::Cd(_),
                      Cmd::Ls(_)]))
  }

  #[test]
  fn process_cmds() {
    let sh = Shell::new();
    let cmds = Cmd::parse_many(include_str!("./input.sample"));
    let sh = sh.process(cmds);
    match sh.0 .0 {
      | Ent::Dir(_, ents) => assert_eq!(ents[0],
                                        Ent::Dir("a".into(),
                                                 vec![Ent::Dir("e".into(),
                                                               vec![Ent::File("i".into(), 584)]),
                                                      Ent::File("f".into(), 29116),
                                                      Ent::File("g".into(), 2557),
                                                      Ent::File("h.lst".into(), 62596)])),
      | _ => panic!(),
    }
  }

  #[test]
  fn flat() {
    let sh = Shell::new();
    let cmds = Cmd::parse_many(include_str!("./input.sample"));
    let sh = sh.process(cmds);
    let mut flat = sh.0 .0.flat().fmap(|(p, mut fs): (_, Vec<_>)| {
                                   fs.sort_by_key(|(p, _): &(PathBuf, _)| p.clone());
                                   (p, fs)
                                 });
    flat.sort_by_key(|(p, _)| p.clone());
    assert_eq!(flat,
               vec![("/".into(),
                     vec![("/a/e/i".into(), 584),
                          ("/a/f".into(), 29116),
                          ("/a/g".into(), 2557),
                          ("/a/h.lst".into(), 62596),
                          ("/b.txt".into(), 14848514),
                          ("/c.dat".into(), 8504156),
                          ("/d/d.ext".into(), 5626152),
                          ("/d/d.log".into(), 8033020),
                          ("/d/j".into(), 4060174),
                          ("/d/k".into(), 7214296)]),
                    ("/a".into(),
                     vec![("/a/e/i".into(), 584),
                          ("/a/f".into(), 29116),
                          ("/a/g".into(), 2557),
                          ("/a/h.lst".into(), 62596)]),
                    ("/a/e".into(), vec![("/a/e/i".into(), 584)]),
                    ("/d".into(),
                     vec![("/d/d.ext".into(), 5626152),
                          ("/d/d.log".into(), 8033020),
                          ("/d/j".into(), 4060174),
                          ("/d/k".into(), 7214296)]),]);
  }

  fn do_part1(s: &str) -> u32 {
    let sh = Shell::new();
    let cmds = Cmd::parse_many(s);
    let sh = sh.process(cmds);
    sh.fs()
      .clone()
      .0
      .flat()
      .fmap(|(_, fs): (_, Vec<(_, u32)>)| fs.foldl(|sum, (_, s)| sum + s, 0))
      .into_iter()
      .filter(|u| *u <= 100000)
      .fold(0, |sum, u| sum + u)
  }

  #[test]
  fn part1_sample() {
    assert_eq!(do_part1(include_str!("./input.sample")), 95437);
  }

  #[test]
  fn part1() {
    assert_eq!(do_part1(include_str!("./input")), 1390824);
  }

  fn do_part2(s: &str) -> u32 {
    let sh = Shell::new();
    let cmds = Cmd::parse_many(s);
    let sh = sh.process(cmds);
    let root = sh.fs()
                 .clone()
                 .0
                 .flat()
                 .find(|(p, _): &(PathBuf, _)| p.to_string_lossy() == "/")
                 .unwrap();

    let free = 70_000_000 - root.1.foldl(|sum, (_, u): (_, u32)| sum + u, 0);
    let needed = 30_000_000 - free;

    sh.fs()
      .0
      .clone()
      .flat()
      .fmap(|(_, fs): (_, Vec<(_, u32)>)| fs.foldl(|sum, (_, s)| sum + s, 0))
      .into_iter()
      .fold(Option::<u32>::empty(), |smallest: Option<u32>, u| {
        if u >= needed {
          Some(smallest.filter(|s| *s < u).get_or(u))
        } else {
          smallest
        }
      })
      .unwrap()
  }

  #[test]
  fn part2_sample() {
    assert_eq!(do_part2(include_str!("./input.sample")), 24933642);
  }

  #[test]
  fn part2() {
    assert_eq!(do_part2(include_str!("./input")), 7490863);
  }
}
