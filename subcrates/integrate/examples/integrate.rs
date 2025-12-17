use std::fmt::Display;

use integrate::integrators::{MonotoneCubicIntegrator, interpolation::Interpolation};


#[derive(Default, Debug)]
pub struct NodeTree<T> {
    data: Vec<Node<T>>,
}

impl<T: Display> Display for NodeTree<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Is left
        let mut depth_direction = Vec::new();
        let mut node = self.root();

        write!(f, "\n")?;

        'outer: loop {
            let value = node.value(self);

            for &dir in depth_direction[0..depth_direction.len().checked_sub(1).unwrap_or(0)].iter()
            {
                if dir {
                    write!(f, "|   ")?;
                } else {
                    write!(f, "    ")?;
                }
            }

            let is_left_child = depth_direction.last().cloned();

            match is_left_child {
                Some(true) => write!(f, "├── {}\n", value)?,
                Some(false) => write!(f, "└── {}\n", value)?,
                None => write!(f, "{}\n", value)?,
            }

            if let Some(left_child) = node.left_child(self) {
                depth_direction.push(true);
                node = left_child;
            } else if let Some(is_left_child) = is_left_child {
                let mut parent = node.parent(self).unwrap();
                if is_left_child {
                    *depth_direction.last_mut().unwrap() = false;
                    node = parent.right_child(self).unwrap();
                } else {
                    loop {
                        if let Some(grandparent) = parent.parent(self) {
                            depth_direction.pop();
                            if grandparent.left_child(self).unwrap() == parent {
                                *depth_direction.last_mut().unwrap() = false;
                                node = grandparent.right_child(self).unwrap();
                                break;
                            } else {
                                parent = grandparent;
                            }
                        } else {
                            break 'outer;
                        }
                    }
                }
            } else {
                break;
            }
        }

        Ok(())
    }
}

impl<T> NodeTree<T> {
    pub fn new(root_value: T) -> Self {
        Self {
            data: vec![Node::new(root_value)],
        }
    }

    pub fn with_capacity(root_value: T, capacity: usize) -> Self {
        Self {
            data: {
                let mut data = Vec::with_capacity(capacity);
                data.push(Node::new(root_value));
                data
            },
        }
    }

    pub fn node_count(&self) -> usize {
        self.data.len()
    }

    pub fn root(&self) -> NodeRef {
        NodeRef { idx: 0 }
    }

    pub fn get(&self, idx: usize) -> Option<NodeRef> {
        if self.data.len() > idx {
            Some(NodeRef { idx })
        } else {
            None
        }
    }

    pub fn leaf_iter(&'_ self) -> NodeTreeLeafIter<'_, T> {
        NodeTreeLeafIter {
            tree: self,
            iter_data: None,
            finished: false,
        }
    }
}

pub struct NodeTreeLeafIter<'a, T> {
    tree: &'a NodeTree<T>,
    iter_data: Option<(bool, NodeRef)>,
    finished: bool,
}

impl<'a, T> Iterator for NodeTreeLeafIter<'a, T> {
    type Item = NodeRef;
    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }
        match self.iter_data {
            None => {
                let mut node = self.tree.root();
                let mut is_left = false;
                while let Some(child) = node.left_child(self.tree) {
                    node = child;
                    is_left = true;
                }
                self.iter_data = Some((is_left, node));
                // Only way this happens is if the tree is just the root
                if is_left == false {
                    self.finished = true;
                }
                Some(node)
            }
            Some((true, node)) => {
                let node = node
                    .parent(self.tree)
                    .unwrap()
                    .right_child(self.tree)
                    .unwrap();

                self.iter_data = Some((false, node));
                Some(node)
            }
            Some((false, mut node)) => {
                // Find next leaf
                loop {
                    let parent = node.parent(self.tree).unwrap();
                    let Some(grandparent) = parent.parent(self.tree) else {
                        self.finished = true;
                        return None;
                    };
                    if grandparent.left_child(self.tree).unwrap() == parent {
                        node = grandparent.right_child(self.tree).unwrap();
                        break;
                    }
                    node = parent;
                }

                let mut is_left = false;
                while let Some(child) = node.left_child(self.tree) {
                    node = child;
                    is_left = true;
                }

                self.iter_data = Some((is_left, node));
                Some(node)
            }
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<'a, T> ExactSizeIterator for NodeTreeLeafIter<'a, T> {
    fn len(&self) -> usize {
        self.tree.data.len() / 2 + 1
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeRef {
    idx: usize,
}

impl NodeRef {
    pub fn inner<'t, T>(&self, tree: &'t NodeTree<T>) -> &'t Node<T> {
        tree.data.get(self.idx).unwrap()
    }
    pub fn inner_mut<'t, T>(&self, tree: &'t mut NodeTree<T>) -> &'t mut Node<T> {
        tree.data.get_mut(self.idx).unwrap()
    }
    pub fn is_left_child<T>(&self, tree: &NodeTree<T>) -> Option<bool> {
        if let Some(parent) = self.parent(tree) {
            Some(parent.left_child(tree).unwrap().eq(self))
        } else {
            None
        }
    }
    pub fn has_parent<T>(&self, tree: &NodeTree<T>) -> bool {
        tree.data[self.idx].parent.is_some()
    }
    pub fn has_children<T>(&self, tree: &NodeTree<T>) -> bool {
        tree.data[self.idx].children.is_some()
    }
    pub fn value<'t, T>(&self, tree: &'t NodeTree<T>) -> &'t T {
        &self.inner(tree).value
    }
    pub fn value_mut<'t, T>(&self, tree: &'t mut NodeTree<T>) -> &'t mut T {
        &mut self.inner_mut(tree).value
    }
    pub fn left_child<T>(&self, tree: &NodeTree<T>) -> Option<NodeRef> {
        self.inner(tree).children.map(|(l, _)| tree.get(l).unwrap())
    }
    pub fn right_child<T>(&self, tree: &NodeTree<T>) -> Option<NodeRef> {
        self.inner(tree).children.map(|(_, r)| tree.get(r).unwrap())
    }
    pub fn parent<T>(&self, tree: &NodeTree<T>) -> Option<NodeRef> {
        self.inner(tree).parent.map(|idx| tree.get(idx).unwrap())
    }
    pub fn create_children<T>(
        &self,
        tree: &mut NodeTree<T>,
        left_value: T,
        right_value: T,
    ) -> Option<(NodeRef, NodeRef)> {
        tree.data
            .push(Node::new_with_parent(self.clone(), left_value));
        tree.data
            .push(Node::new_with_parent(self.clone(), right_value));
        let new_val = (tree.data.len() - 2, tree.data.len() - 1);
        self.inner_mut(tree)
            .children
            .replace(new_val)
            .map(|(l, r)| (NodeRef { idx: l }, NodeRef { idx: r }))
    }
    pub fn children<T>(&self, tree: &NodeTree<T>) -> Option<(NodeRef, NodeRef)> {
        self.inner(tree)
            .children
            .map(|(l, r)| (tree.get(l).unwrap(), tree.get(r).unwrap()))
    }
}

#[derive(Default, Debug)]
pub struct Node<T> {
    value: T,
    parent: Option<usize>,
    children: Option<(usize, usize)>,
}

impl<T> Node<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            parent: None,
            children: None,
        }
    }
    pub fn new_with_parent(parent: NodeRef, value: T) -> Self {
        Self {
            value,
            parent: Some(parent.idx),
            children: None,
        }
    }
}

fn main() {
    #[derive(Default, Debug)]
    pub struct NodeData {
        range: (f64, f64),
        contained_points: (usize, usize),
    }

    impl Display for NodeData {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} to {}", self.range.0, self.range.1)
        }
    }

    impl NodeData {
        pub fn contained_points_len(&self) -> usize {
            self.contained_points.1 - self.contained_points.0
        }
        pub fn split(&self, interest_points: &[f64]) -> (NodeData, NodeData) {
            let split_point = (self.range.0 + self.range.1) / 2.0;
            let split_idx = &interest_points[self.contained_points.0..self.contained_points.1]
                .iter()
                .position(|val| *val >= split_point)
                .unwrap_or(self.contained_points_len() - 1)
                + self.contained_points.0;

            let child_1 = NodeData {
                range: (self.range.0, split_point),
                contained_points: (self.contained_points.0, split_idx),
            };
            let child_2 = NodeData {
                range: (split_point, self.range.1),
                contained_points: (split_idx, self.contained_points.1),
            };

            // println!("Split Result: {}..{}, {:?}, split at {}={}", self.range.0, self.range.1, &interest_points[self.contained_points.0..self.contained_points.1], split_point, split_idx - self.contained_points.0);
            (child_1, child_2)
        }
    }

    let f = 20.0;
    let w = f * 2.0 * std::f64::consts::PI;
    let xs = Vec::from_iter(
        (0..10)
            .map(|i| i as f64 / 20.0)
            .chain((0..=2).map(|i| i as f64 / 4.0 + 0.5)),
    );
    let ys = Vec::from_iter(xs.iter().map(|&x| {
        if x < 0.5 {
            (w * x).sin()
        } else {
            w * (x - 0.5) * f64::exp((-w / std::f64::consts::E) * (x - 0.5))
        }
    }));

    let integrator = MonotoneCubicIntegrator::new(&xs);
    let interpolation = integrator.interpolation(&ys);

    let interest_points = xs.as_slice();

    let interp_range = interpolation.interpolate_range();

    let mut tree: NodeTree<NodeData> = NodeTree::new(NodeData {
        range: (*interp_range.start(), *interp_range.end()),
        contained_points: (
            interest_points
                .iter()
                .position(|val| val >= interp_range.start())
                .unwrap_or(xs.len() - 1),
            interest_points
                .iter()
                .position(|val| val > interp_range.end())
                .unwrap_or(xs.len() - 1),
        ),
    });
    let root = tree.root();

    println!("Size of node: {}", size_of_val(root.inner(&tree)));

    let mut node = root;

    'outer: loop {
        let value = node.value(&tree);
        assert!(!node.has_children(&tree));
        if value.contained_points_len() > 1 {
            // println!("{}..{} = {}..{}", value.range.0, value.range.1, value.contained_points.0, value.contained_points.1);
            let (l_child, r_child) = value.split(interest_points);
            node.create_children(&mut tree, l_child, r_child);
            node = node.left_child(&tree).unwrap();
        } else {
            // Get next leaf
            loop {
                let Some(parent) = node.parent(&tree) else {
                    break 'outer;
                };
                // std::thread::sleep(Duration::from_millis(200));
                let right_child = parent.right_child(&tree).unwrap();
                if node == right_child {
                    node = parent;
                } else {
                    node = right_child;
                    break;
                }
            }
        }
    }

    println!("interest points: {:?}", interest_points);
    println!("Tree[{}]: {}", tree.node_count(), tree); 
    for leaf in tree.leaf_iter() {
        println!("{:?}", leaf.value(&tree));
    }
}
