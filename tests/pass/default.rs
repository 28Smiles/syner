use syner::Syner;

#[derive(Syner)]
struct Test {
    #[syner(default)]
    pub is_default: bool,
}

fn main() {
}