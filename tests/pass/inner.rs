use syner::Syner;

#[derive(Syner)]
struct Test {
    pub inner: Inner,
    pub inner_list: Vec<Inner>,
    pub inner_maybe: Option<Inner>,
}

#[derive(Syner)]
struct Inner {
    pub some: String,
    pub is_default: bool,
}

fn main() {
}