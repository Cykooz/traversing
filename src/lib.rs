use std::any::{Any, TypeId};
use std::cell::{BorrowMutError, RefCell};
use std::collections::HashMap;
use std::rc::Rc;

pub trait ResourceTrait: Any {
    fn as_any(&self) -> &dyn Any;

    fn child(&self, name: &str) -> Option<Rc<dyn ResourceTrait>> {
        None
    }
}

pub type Resource = Rc<dyn ResourceTrait>;

#[derive(Clone)]
struct InnerNode {
    name: Rc<String>,
    parent: Option<Node>,
    resource: Resource,
}

#[derive(Clone)]
pub struct Node {
    inner: Rc<InnerNode>,
}

impl Node {
    pub fn new(name: &str, parent: Option<Node>, resource: Resource) -> Self {
        Self {
            inner: Rc::new(InnerNode {
                name: Rc::new(name.to_string()),
                parent,
                resource,
            }),
        }
    }

    pub fn name(&self) -> &str {
        self.inner.name.as_str()
    }

    pub fn parent(&self) -> Option<Node> {
        self.inner.parent.clone()
    }

    pub fn resource(&self) -> Resource {
        self.inner.resource.clone()
    }

    pub fn resource_type_id(&self) -> TypeId {
        self.resource().as_any().type_id()
    }

    pub fn path(&self) -> String {
        let mut names = vec![self.name().to_string()];
        let mut parent = self.parent();
        while let Some(p) = parent {
            names.push(p.name().to_string());
            parent = p.parent();
        }
        names.reverse();
        names.join("/")
    }

    pub fn child(&self, name: &str) -> Option<Node> {
        let child = self.inner.resource.child(name);
        child.map(|resource| Node::new(name, Some(self.clone()), resource))
    }
}

trait Apply: ResourceTrait {
    fn apply<F, R>(node: &Node, f: F) -> Option<R>
    where
        F: FnOnce(&Self) -> R;

    fn inside_node(node: &Node) -> bool {
        node.resource_type_id() == TypeId::of::<Self>()
    }
}

impl<T: ResourceTrait> Apply for T {
    fn apply<F, R>(node: &Node, f: F) -> Option<R>
    where
        F: FnOnce(&Self) -> R,
    {
        let resource = node.resource();
        resource.as_any().downcast_ref::<T>().map(f)
    }
}

#[derive(Default)]
pub struct SimpleContainer {
    pub children: RefCell<HashMap<String, Resource>>,
}

impl SimpleContainer {
    pub fn set_child<T: ResourceTrait + 'static>(
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

impl ResourceTrait for SimpleContainer {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn child(&self, name: &str) -> Option<Resource> {
        self.children.borrow().get(name).cloned()
    }
}

pub fn traverse(node: &Node, path: &str) -> Result<Node, String> {
    let mut result_node = node.clone();
    for name in path.split('/').filter(|n| !n.is_empty()) {
        result_node = match result_node.child(name) {
            Some(node) => node,
            None => {
                return Err("not found".to_string());
            }
        }
    }
    Ok(result_node)
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

        let sub_sub_child = traverse(&root_node, "/child1/sub_child1/sub_sub_child");
        assert!(sub_sub_child.is_ok());
        if let Ok(node) = sub_sub_child {
            assert_eq!(node.path(), "/child1/sub_child1/sub_sub_child");
        }
    }
}
