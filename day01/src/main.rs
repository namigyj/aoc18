use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use core::iter::Cycle;
use std::collections::HashSet;

fn parse_pure(s: String) -> i32 {
    let a = s.parse::<i32>();
    if !a.is_ok() {
        panic!("error on parsing: {:?}", s);
    }
    return a.unwrap();
}

fn day2<I>(xs : Cycle<I<i32>>) -> i32 {
    let mut s = HashSet::new();
    let mut r : i32 = 0;
    for x in xs {
        r += x;
        if s.contains(&r) {
            return r;
        }
        s.insert(r);
    }
    return -1;
}

fn main() {
    use std::io::{Error, ErrorKind};

    let usage = std::format!("usage: {} INPUTFILE", std::env::args().nth(0).unwrap());

    let input =
        std::env::args().nth(1)
        .ok_or(Error::new(ErrorKind::InvalidInput, usage))
        .and_then(File::open)
        .map(BufReader::new);

    if input.is_err() {
        println!("{:?}", input.unwrap_err());
        std::process::exit(1);
    }
    let input : BufReader<File> = input.unwrap();

    let input =
        input.lines()
        .map(|l| parse_pure(l.unwrap()))
        .collect::<Vec<i32>>();

    let d1 : i32 = input.iter().sum();
    println!("day1: {}", d1);
    let d2 : i32 = day2(input.iter().cycle());
    println!("day2: {}", d2);
}

#[test]
fn test_d2() {
    let xs = [1, -2, 3, 1];
    assert_eq!(day2(xs.iter().cycle()), 2);
}
