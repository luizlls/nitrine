use std::collections::HashMap;


struct Context<'ctx> {
    parent: Option<&'ctx Self>,
    items: HashMap<String, ()>
}

impl<'ctx> Context<'ctx> {
    fn new() -> Context<'ctx> {
        Context {
            parent: None,
            items: HashMap::new()
        }
    }

    fn nested(parent: &'ctx Context) -> Context<'ctx> {
        Context {
            parent: Some(parent),
            items: HashMap::new()
        }
    }

    fn insert(&mut self, name: String) {
        self.items.insert(name, ());
    }

    fn find(&self, name: &str) -> bool {
        if self.items.get(name).is_some() {
            true
        } else if let Some(parent) = self.parent {
            parent.find(name)
        } else {
            false
        }
    }
}

struct Analyzer {
}
