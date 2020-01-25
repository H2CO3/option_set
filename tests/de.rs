#[macro_use]
extern crate option_set;
#[macro_use]
extern crate bitflags;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

use serde::{Serialize, Deserialize};

option_set! {
    pub struct Flags: Identity + u32 {
        const MY_FLAG = 1 << 0;
    }
}

#[derive(Serialize, Deserialize)]
pub struct A {
    pub vec: Vec<B>
}
#[derive(Serialize, Deserialize)]
pub struct B {
    pub flags: Flags
}

#[test]
fn deserialize_json() {
    let data = "{\"vec\":[{\"flags\":[\"MY_FLAG\"]}]}";
    let value: A = serde_json::from_str(data).unwrap();
    assert!(value.vec[0].flags.contains(Flags::MY_FLAG));
}

#[test]
fn deserialize_yaml() {
    let data = "
    vec:
      - flags: [MY_FLAG]    
    ";
    let value: A = serde_yaml::from_str(data).unwrap();
    assert!(value.vec[0].flags.contains(Flags::MY_FLAG));
}