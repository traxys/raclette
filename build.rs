fn main() {
    lalrpop::Configuration::default()
        .process_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/src"))
        .unwrap();
}
