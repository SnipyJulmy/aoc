use std::collections::HashMap;
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    let filepath: &String = &args[1];
    let input: String = fs::read_to_string(filepath).unwrap();
    let location_ids: Vec<Vec<i32>> = input
        .split("\n")
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split(char::is_whitespace)
                .filter(|str| !str.is_empty())
                .map(|str| str.parse::<i32>().unwrap())
                .collect()
        })
        .collect();

    let mut ids1: Vec<i32> = vec![];
    let mut ids2: Vec<i32> = vec![];

    for i in 0..location_ids.len() {
        ids1.push(location_ids[i][0]);
        ids2.push(location_ids[i][1]);
    }

    ids1.sort();
    ids2.sort();
    assert!(ids1.len() == ids2.len());

    let mut score1 = 0;

    for i in 0..ids1.len() {
        let diff = (ids1[i] - ids2[i]).abs();
        score1 += diff;
    }

    let mut similarity_score: HashMap<i32, i32> = HashMap::new();
    for n in ids2 {
        match similarity_score.get(&n) {
            Some(v) => similarity_score.insert(n, *v + 1),
            None => similarity_score.insert(n, 1),
        };
    }

    let mut score2 = 0;

    for n in ids1 {
        match similarity_score.get(&n) {
            Some(v) => score2 += n * *v,
            None => (),
        }
    }

    println!("Part One: {}", score1);
    println!("Part Two: {}", score2);
}
