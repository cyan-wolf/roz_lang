
use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Effect {
    InFunction,
    // TODO: Use this variant to add `break`/`continue` statements.
    InLoop,
}

/// Used for tracking any "effects" during the resolution pass.
pub(super) struct Context(HashSet<Effect>);

impl Context {
    pub(super) fn new() -> Self {
        Self(HashSet::new())
    }

    pub(super) fn add_effect(&mut self, effect: Effect) {
        self.0.insert(effect);
    }

    pub(super) fn remove_effect(&mut self, effect: &Effect) {
        self.0.remove(effect);
    }

    pub(super) fn has_effect(&self, effect: &Effect) -> bool {
        self.0.contains(effect)
    }
}