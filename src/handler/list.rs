use super::{HandlerInfoPtr, HandlerPriority};

#[derive(Debug, Default)]
pub(crate) struct HandlerList {
    before: u32,
    after: u32,
    entries: Vec<HandlerInfoPtr>,
}

unsafe impl Sync for HandlerList {}

impl HandlerList {
    pub(crate) const fn new() -> HandlerList {
        Self {
            before: 0,
            after: 0,
            entries: vec![],
        }
    }

    pub(crate) fn insert(&mut self, ptr: HandlerInfoPtr, priority: HandlerPriority) {
        assert!(self.entries.len() < u32::MAX as usize);

        match priority {
            HandlerPriority::High => {
                self.entries.insert(self.before as usize, ptr);
                self.before += 1;
                self.after += 1;
            }
            HandlerPriority::Medium => {
                self.entries.insert(self.after as usize, ptr);
                self.after += 1;
            }
            HandlerPriority::Low => {
                self.entries.push(ptr);
            }
        }
    }

    pub(crate) fn remove(&mut self, ptr: HandlerInfoPtr) -> bool {
        if let Some(idx) = self.entries.iter().position(|&p| p == ptr) {
            self.entries.remove(idx);

            let idx = idx as u32;

            if idx < self.after {
                self.after -= 1;

                if idx < self.before {
                    self.before -= 1;
                }
            }

            true
        } else {
            false
        }
    }

    pub(crate) fn handlers(&self) -> &[HandlerInfoPtr] {
        &self.entries
    }
}
