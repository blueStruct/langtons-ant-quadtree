use self::Color::*;
use self::Dir::*;
use self::Quad::*;

use std::cell::RefCell;
use std::rc::{Rc, Weak};

fn main() {
    let mut ant = Ant::new();
    for _ in 1..10 {
        ant.go()
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Dir {
    N,
    E,
    S,
    W,
}

impl Dir {
    fn rev(&self) -> Self {
        match self {
            N => S,
            S => N,
            E => W,
            W => E,
        }
    }
}

impl From<i8> for Dir {
    fn from(a: i8) -> Self {
        match a % 4 {
            0 => N,
            1 => E,
            2 => S,
            3 => W,
            _ => N,
        }
    }
}

#[derive(Clone, Copy, PartialEq, PartialOrd)]
enum Quad {
    NW,
    NE,
    SW,
    SE,
}

impl From<u8> for Quad {
    fn from(a: u8) -> Self {
        match a {
            0 => NW,
            1 => NE,
            2 => SW,
            3 => SE,
            _ => panic!("can't convert number bigger 4 to Quad"),
        }
    }
}

impl Into<usize> for Quad {
    fn into(self) -> usize {
        match self {
            NW => 0,
            NE => 1,
            SW => 2,
            SE => 3,
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum Color {
    Black,
    White,
}

impl Color {
    fn flip(&mut self) {
        match self {
            Black => *self = White,
            White => *self = Black,
        }
    }
}

type Link = Option<Rc<RefCell<Node>>>;
type WeakLink = Option<Weak<RefCell<Node>>>;

struct Node {
    level: u32,
    tile: Option<Color>,
    quad: Option<Quad>,
    parent: WeakLink,
    children: Vec<Link>,
}

struct Ant {
    root: Rc<RefCell<Node>>,
    node: Rc<RefCell<Node>>,
    dir: Dir,
}

impl Ant {
    fn new() -> Self {
        let root = Rc::new(RefCell::new(Node {
            level: 0,
            tile: Some(White),
            quad: None,
            parent: None,
            children: vec![None; 4],
        }));

        let node = root.clone();

        Ant { root, node, dir: N }
    }

    fn try_move(quad: Option<Quad>, dir: Dir) -> Option<Quad> {
        match quad {
            None => None,
            Some(quad) => match (quad, dir) {
                (NW, N) => None,
                (NW, E) => Some(NE),
                (NW, S) => Some(SW),
                (NW, W) => None,

                (NE, N) => None,
                (NE, E) => None,
                (NE, S) => Some(SE),
                (NE, W) => Some(NW),

                (SW, N) => Some(NW),
                (SW, E) => Some(SE),
                (SW, S) => None,
                (SW, W) => None,

                (SE, N) => Some(NE),
                (SE, E) => None,
                (SE, S) => None,
                (SE, W) => Some(SW),
            },
        }
    }

    fn create_parent(&mut self) {
        // if parent already exists, cancel
        if { self.node.borrow().parent.is_some() } {
            return;
        }

        // choose how to expand, TODO: improve
        let expand_as = match self.dir {
            N => SW,
            E => NW,
            S => NE,
            W => SE,
        };

        // new parent node
        let parent = Rc::new(RefCell::new(Node {
            level: { self.node.borrow().level + 1 },
            tile: None,
            quad: None,
            parent: None,
            children: vec![None; 4],
        }));

        // linking the parent and the node
        {
            let mut node = self.node.borrow_mut();
            node.parent = Some(Rc::downgrade(&parent));
            node.quad = Some(expand_as);
        }
        {
            let node = self.node.clone();
            parent.borrow_mut().children[expand_as as usize] = Some(node);
        }

        // set created parent node as Quadtree root
        self.root = parent;
    }

    fn create_child(&mut self, quad: Quad) {
        // if child already exists, cancel
        if { self.node.borrow().children[quad as usize].is_some() } {
            return;
        }

        // new child node
        let level = { self.node.borrow().level - 1 };
        let child = Rc::new(RefCell::new(Node {
            level,
            tile: if level == 0 { Some(White) } else { None },
            quad: Some(quad),
            parent: Some(Rc::downgrade(&self.node)),
            children: vec![None; 4],
        }));

        // linking the parent and the node
        {
            let mut node = self.node.borrow_mut();
            node.children[quad as usize] = Some(child);
        }
    }

    fn level_up(node: &mut Rc<RefCell<Node>>) {
        let parent = node.borrow().parent.clone();
        match parent {
            None => {}
            Some(i) => *node = i.upgrade().unwrap(),
        }
    }

    fn level_down(node: &mut Rc<RefCell<Node>>, quad: Quad) {
        let child = node.borrow().children[quad as usize].clone();
        match child {
            None => {}
            Some(i) => *node = i,
        }
    }

    fn go(&mut self) {
        if { self.node.borrow().level != 0 } {
            panic!("Ant tries to go and is not at level 0");
        }

        let mut quad_stack: Vec<Quad> = Vec::new();

        let mut dir = self.dir;
        let mut quad = self.node.borrow().quad;
        let mut can_move = Ant::try_move(quad, dir).is_some();

        // as long ant hits boundary, go up a level
        while !can_move {
            // if no parent, expand tree into bigger quad
            self.create_parent();

            // add movement history to stack
            quad_stack.push(self.node.borrow().quad.unwrap());

            // go up a level
            Ant::level_up(&mut self.node);

            // check whether move is now possible
            dir = self.dir;
            quad = self.node.borrow().quad;
            can_move = Ant::try_move(quad, dir).is_some();
        }

        // go up again
        quad_stack.push(self.node.borrow().quad.unwrap());

        Ant::level_up(&mut self.node);

        // after going up, go down, mirrored to crossed boundary
        dir = dir.rev();
        quad_stack = quad_stack
            .iter()
            .map(|&x| Ant::try_move(Some(x), dir).unwrap())
            .collect();

        while let Some(quad) = quad_stack.pop() {
            // if child node does not exist, create it
            self.create_child(quad);

            // go down
            Ant::level_down(&mut self.node, quad);
        }

        // turn
        {
            let node = self.node.borrow();
            if let Some(x) = node.tile {
                match x {
                    White => self.dir = Dir::from(self.dir as i8 + 1),
                    Black => self.dir = Dir::from(self.dir as i8 - 1),
                }
            }
        }

        // flip color
        {
            let node = self.node.borrow_mut();
            if let Some(mut x) = node.tile {
                x.flip();
            }
        }
    }

    fn count_black(&mut self) -> u64 {
        let mut n = 0u64;
        let mut node = self.root.clone();

        // go to left-most leaf
        while { node.borrow().level > 0 } {}

        // visit next sibling, when last sibling, go up another level and visit next sibling there

        // TODO
        unimplemented!();
    }
}

impl Drop for Ant {
    fn drop(&mut self) {
        // TODO?
    }
}
