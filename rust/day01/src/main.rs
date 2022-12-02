use std::{cmp::Reverse, env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    let filepath: &String = &args[1];
    let input: String = fs::read_to_string(filepath).unwrap();
    let mut calories_by_elf: Vec<i32> = input
        .split("\n\n")
        .filter(|carried| !carried.is_empty())
        .map(|carried| {
            carried
                .split('\n')
                .filter(|line| !line.is_empty())
                .map(|line| line.parse::<i32>().unwrap())
                .reduce(|a, b| a + b)
                .unwrap()
        })
        .collect();

    calories_by_elf.sort_by_key(|x| Reverse(*x));

    println!("Part One : {}", calories_by_elf[0]);

    let total_calories_top3 = calories_by_elf[0] + calories_by_elf[1] + calories_by_elf[2];

    println!("Part Two : {}", total_calories_top3);
}
