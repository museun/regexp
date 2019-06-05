use super::*;

use std::cmp::max;
use std::io::Write;

#[derive(Debug, Default)]
pub struct Program {
    pub insts: Vec<Instruction>,
    pub names: Vec<GroupType>,
}

impl Program {
    pub fn dump<W: Write>(&self, w: &mut W) {
        let mut labels = vec![];
        for inst in self.insts.iter() {
            match *inst {
                Instruction::Split(x, y) => labels.extend_from_slice(&[x, y]),
                Instruction::Jump(n) => labels.push(n),
                _ => continue,
            }
        }
        labels.sort_unstable();
        labels.dedup_by(|a, b| *a > 0 && a == b);

        let max = self
            .insts
            .iter()
            .map(|i| i.columns())
            .fold(vec![0; 3], |mut a, b| {
                for (i, n) in b.iter().enumerate() {
                    a[i] = max(a[i], *n)
                }
                a
            });

        let label_max = count_digits(*labels.iter().max_by(|x, y| x.cmp(y)).unwrap()) + 1; // for the colon
        let label_pad = str::repeat(" ", label_max);

        for (i, inst) in self.insts.iter().enumerate() {
            let label = if labels.contains(&(i as u32)) {
                format!("{:^1$}", format!("{}:", i), label_max)
            } else {
                label_pad.clone() // TODO don't do this
            };

            let name = format!("{: >1$}", format!("{}", inst), max[0]);
            let args = match inst {
                Instruction::Split(x, y) => format!("{:<2$} or {:>3$}", x, y, max[1], max[2]),
                Instruction::Jump(n) => format!("{}", n),
                Instruction::Start(n) => format!("#{}", n),
                Instruction::End(n) => format!("#{}", n),
                Instruction::Char(Char::Char(ch)) => format!("{}", ch),
                Instruction::CharSet(cs) => format!("{}", cs),
                _ => "".into(),
            };
            writeln!(w, "{} {} | {}", label, name, args).unwrap()
        }
    }
}
