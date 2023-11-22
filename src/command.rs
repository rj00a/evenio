pub(crate) struct CommandQueue {
    commands: Vec<Command>,
}

pub enum Command {
    Insert {},
    Remove {},
}
