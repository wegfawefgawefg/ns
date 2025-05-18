use ns_core::get_ns_engine_greeting;

fn main() {
    println!("ns (NotScheme) CLI starting...");
    let greeting = get_ns_engine_greeting();
    println!("{}", greeting);
}
