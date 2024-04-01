use std::io;
use std::io::Write;
use md5;

fn main() {
    for root in ["abc", "ugkcyxxp"] {
        let mut index= 0;
        let mut part1_counter = 0;
        let mut part2_counter = 0;
        let mut found_part2: [Option<char>; 8] = Default::default();
        println!("Searching for '{}' :", root);
        while part1_counter < 8 || part2_counter < 8 {
            let digest = md5::compute(format!("{}{}", root, index));
            if digest[0] == 0 && digest[1] == 0 && digest[2] < 16 {
                let sixth: usize = digest[2] as usize;
                if digest[2] < 8 && found_part2[sixth] == None {
                    found_part2[sixth] = Some(format!("{:#04x}", digest[3]).chars().nth(2).unwrap());
                    part2_counter += 1
                }
                if part1_counter < 8 {
                    print!("{:x}", digest[2]);
                    io::stdout().flush().unwrap();
                    part1_counter += 1
                }
            }
            index += 1
        }
        println!();
        for part2 in found_part2 {
            print!("{}", part2.unwrap_or("_".parse().unwrap()));
        }
        println!();

    }
}
