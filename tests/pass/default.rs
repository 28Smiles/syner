use syner::Syner;

#[derive(Syner)]
struct Test {
    #[syner(default)]
    pub is_default: bool,
    #[syner(default = "String::from(\"default\")")]
    pub default: String,
}

fn main() {
}