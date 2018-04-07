extern crate rox;

fn main() {
    match rox::run() {
        Ok(_) => (),
        Err(e) => eprintln!("{}", e),
    }
}