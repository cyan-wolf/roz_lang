
use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Effect {
    /// Used for restricting the use of `return`. 
    InFunction,
    /// Used for restricting the use of `this` and `super`.
    InMethod,
    /// Used for restricting the use of `super` outside of subclasses.
    InSubclass,
    /// Used for restricting the use of `break` and `continue`.
    InLoop,
}

/// Used for tracking any "effects" during the resolution pass.
#[derive(Debug, Clone)]
pub(super) struct Context(HashSet<Effect>);

impl Context {
    pub(super) fn new() -> Self {
        Self(HashSet::new())
    }

    pub (super) fn with_effect(mut self, effect: Effect) -> Self {
        self.0.insert(effect);
        self
    }

    pub (super) fn without_effect(mut self, effect: &Effect) -> Self {
        self.0.remove(effect);
        self
    }

    pub(super) fn has_effect(&self, effect: &Effect) -> bool {
        self.0.contains(effect)
    }
}