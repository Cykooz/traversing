use std::any::{Any, TypeId};
use std::cell::{BorrowMutError, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

pub trait EntityTrait: Any {
    fn as_any(&self) -> &dyn Any;

    fn child(&self, name: &str) -> Option<Rc<dyn EntityTrait>> {
        None
    }
}

pub type Entity = Rc<dyn EntityTrait>;

#[derive(Clone)]
struct InnerNode {
    name: Rc<String>,
    parent: Option<Node>,
    entity: Entity,
}

#[derive(Clone)]
pub struct Node {
    inner: Rc<InnerNode>,
}

impl Node {
    pub fn new(name: &str, parent: Option<Node>, entity: Entity) -> Self {
        Self {
            inner: Rc::new(InnerNode {
                name: Rc::new(name.to_string()),
                parent,
                entity,
            }),
        }
    }

    pub fn name(&self) -> &str {
        self.inner.name.as_str()
    }

    pub fn parent(&self) -> Option<Node> {
        self.inner.parent.clone()
    }

    pub fn entity(&self) -> Entity {
        self.inner.entity.clone()
    }

    pub fn entity_type_id(&self) -> TypeId {
        self.entity().as_any().type_id()
    }

    pub fn path(&self) -> String {
        let mut names = vec![self.name().to_string()];
        let mut parent = self.parent();
        while let Some(p) = parent {
            names.push(p.name().to_string());
            parent = p.parent();
        }
        if names.len() == 1 {
            names.push("".to_string());
        }
        names.reverse();
        names.join("/")
    }

    pub fn child(&self, name: &str) -> Option<Node> {
        let child = self.inner.entity.child(name);
        child.map(|resource| Node::new(name, Some(self.clone()), resource))
    }
}

trait Apply: EntityTrait {
    fn apply<F, R>(node: &Node, f: F) -> Option<R>
        where
            F: FnOnce(&Self) -> R;

    fn inside_node(node: &Node) -> bool {
        node.entity_type_id() == TypeId::of::<Self>()
    }
}

impl<T: EntityTrait> Apply for T {
    fn apply<F, R>(node: &Node, f: F) -> Option<R>
        where
            F: FnOnce(&Self) -> R,
    {
        let entity = node.entity();
        entity.as_any().downcast_ref::<T>().map(f)
    }
}

#[derive(Default)]
pub struct SimpleContainer {
    pub children: RefCell<HashMap<String, Entity>>,
}

impl SimpleContainer {
    pub fn set_child<T: EntityTrait + 'static>(
        &self,
        name: &str,
        child: T,
    ) -> Result<Rc<T>, BorrowMutError> {
        let child_rc = Rc::new(child);
        self.children
            .try_borrow_mut()?
            .insert(name.to_string(), child_rc.clone());
        Ok(child_rc)
    }
}

impl EntityTrait for SimpleContainer {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn child(&self, name: &str) -> Option<Entity> {
        self.children.borrow().get(name).cloned()
    }
}

/// Finds a node with given path by traversing a node hierarchy.
pub fn traverse<'p>(root_node: &Node, mut path: &'p str) -> (Node, &'p str) {
    let mut result_node = root_node.clone();
    path = path.trim_matches('/');
    while !path.is_empty() {
         match path.split_once('/') {
            Some((name, tail)) => {
                result_node = match result_node.child(name) {
                    Some(node) => node,
                    None => break
                };
                path = tail;
            },
            None =>  {
                if let Some(node) =  result_node.child(path) {
                    result_node = node;
                    path = "";
                }
                break
            }
        }
    }
    (result_node, path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let root = SimpleContainer::default();
        let child1 = root
            .set_child("child1", SimpleContainer::default())
            .unwrap();
        child1
            .set_child("sub_child1", SimpleContainer::default())
            .unwrap();
        root.set_child("child2", SimpleContainer::default())
            .unwrap();

        let root_node = Node::new("", None, Rc::new(root));
        let child1 = root_node.child("child1").unwrap();
        let _child2 = root_node.child("child2").unwrap();
        let sub_child1 = child1.child("sub_child1").unwrap();

        assert_eq!(sub_child1.path(), "/child1/sub_child1");

        if SimpleContainer::inside_node(&sub_child1) {
            SimpleContainer::apply(&sub_child1, |container| {
                container
                    .set_child("sub_sub_child", SimpleContainer::default())
                    .unwrap();
            });
        }

        let sub_sub_child = sub_child1.child("sub_sub_child").unwrap();
        assert_eq!(sub_sub_child.path(), "/child1/sub_child1/sub_sub_child");

        let (node, path_tail) = traverse(&root_node, "/child1/sub_child1/sub_sub_child");
        assert_eq!(path_tail, "");
        assert_eq!(node.path(), "/child1/sub_child1/sub_sub_child");

        let (node, path_tail) = traverse(&root_node, "/child1/unknown/child");
        assert_eq!(node.path(), "/child1");
        assert_eq!(path_tail, "unknown/child");

        let (node, path_tail) = traverse(&root_node, "/");
        assert_eq!(node.path(), "/");
        assert_eq!(path_tail, "");
    }
}
