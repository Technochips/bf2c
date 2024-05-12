use std::collections::HashMap;
use std::env::args;
use std::fs::File;
use std::io::Write;
use std::io::{BufReader, BufWriter, Read};
use std::num::{NonZeroI16, NonZeroI8, NonZeroUsize};

use itertools::Itertools;

fn increment_i8(value: NonZeroI8) -> String {
    if value.get() < -1 {
        format!(" -= {}", value.abs())
    } else if value.get() == -1 {
        "--".to_string()
    } else if value.get() == 1 {
        "++".to_string()
    } else {
        format!(" += {}", value)
    }
}
fn increment_i16(value: NonZeroI16) -> String {
    if value.get() < -1 {
        format!(" -= {}", value.abs())
    } else if value.get() == -1 {
        "--".to_string()
    } else if value.get() == 1 {
        "++".to_string()
    } else {
        format!(" += {}", value)
    }
}
fn add_i16(value: i16, absolute: bool) -> String {
    if absolute {
        (value as u16).to_string()
    } else {
        match value {
            i16::MIN..=-1 => format!("(uint16_t)(ptr - {})", value.abs()),
            0 => "ptr".to_string(),
            1..=i16::MAX => format!("(uint16_t)(ptr + {})", value),
        }
    }
}

#[derive(Debug)]
enum Instruction {
    Change {
        changes: HashMap<i16, NonZeroI8>,
        movement: i16,
        absolute: bool,
    },
    Write {
        times: NonZeroUsize,
        offset: i16,
        absolute: bool,
    },
    Read {
        times: NonZeroUsize,
        offset: i16,
        absolute: bool,
    },
    Loop {
        block: Vec<Instruction>,
        offset: i16,
        movement: i16,
        absolute: bool,
    },
}
impl Instruction {
    fn write_c_block(
        write: &mut impl Write,
        block: &Vec<Instruction>,
        indent: usize,
    ) -> Result<(), std::io::Error> {
        match block.iter().map(|x| x.line_count()).sum() {
            0 => writeln!(write, ";"),
            1 => {
                write!(write, " ")?;
                block[0].write_c(write, 0)
            }
            _ => {
                writeln!(write, " {{")?;
                for instruction in block {
                    instruction.write_c(write, indent + 1)?;
                }
                writeln!(write, "{}}}", "\t".repeat(indent))
            }
        }
    }
    fn write_c(&self, write: &mut impl Write, indent: usize) -> Result<(), std::io::Error> {
        let tab = "\t".repeat(indent);
        match self {
            Instruction::Change {
                changes,
                movement,
                absolute,
            } => {
                for (k, v) in changes.iter().sorted() {
                    writeln!(
                        write,
                        "{}mem[{}]{};",
                        tab,
                        add_i16(*k, *absolute),
                        increment_i8(*v)
                    )?;
                }
                if let Some(movement) = NonZeroI16::new(*movement) {
                    writeln!(write, "{}ptr{};", tab, increment_i16(movement))?;
                }
            }
            Instruction::Write {
                times,
                offset,
                absolute,
            } => {
                if times.get() > 1 {
                    write!(write, "{}for(int i = 0; i < {}; i++) ", tab, times)?;
                } else {
                    write!(write, "{}", tab)?;
                }
                writeln!(write, "putchar(mem[{}]);", add_i16(*offset, *absolute))?;
            }
            Instruction::Read {
                times,
                offset,
                absolute,
            } => {
                if times.get() > 1 {
                    writeln!(
                        write,
                        "{}for(int i = 0; i < {}; i++) getchar();",
                        tab,
                        times.get() - 1
                    )?;
                }
                writeln!(
                    write,
                    "{}mem[{}] = getchar();",
                    tab,
                    add_i16(*offset, *absolute)
                )?;
            }
            Instruction::Loop {
                block,
                offset,
                absolute,
                movement,
            } => {
                if let Some(movement) = NonZeroI16::new(*movement) {
                    write!(
                        write,
                        "{}for(; mem[{}]; ptr{})",
                        tab,
                        add_i16(*offset, *absolute),
                        increment_i16(movement)
                    )?;
                } else {
                    write!(write, "{}while(mem[{}])", tab, add_i16(*offset, *absolute))?;
                }
                Self::write_c_block(write, block, indent)?;
            }
        }
        Ok(())
    }
    fn line_count(&self) -> usize {
        match self {
            Instruction::Change {
                changes, movement, ..
            } => changes.len() + (*movement != 0) as usize,
            Instruction::Write { .. } => 1,
            Instruction::Read { .. } => 2,
            Instruction::Loop { block, .. } => match block.iter().map(|x| x.line_count()).sum() {
                0..=1 => 1,
                i => i + 2,
            },
        }
    }
}
fn write_c(write: &mut impl Write, program: &Vec<Instruction>) -> Result<(), std::io::Error> {
    writeln!(
        write,
        "#include <stdint.h>\n\
        #include <stdio.h>\n\n\
        int main() {{\n\
        \tuint8_t mem[UINT16_MAX + 1] = {{ 0 }};\n\
        \tuint16_t ptr = 0;"
    )?;
    for instruction in program {
        instruction.write_c(write, 1)?;
    }
    writeln!(
        write,
        "\treturn 0;\n\
    }}"
    )?;
    Ok(())
}

fn process<T: Iterator<Item = u8>>(
    program: &mut T,
    inside_loop: bool,
) -> Result<Vec<Instruction>, &'static str> {
    let mut instructions = vec![];
    loop {
        let Some(instruction) = program.next() else {
            break if inside_loop {
                Err("loop start with no matching end")
            } else {
                Ok(instructions)
            };
        };

        match instruction {
            b'>' => instructions.push(Instruction::Change {
                changes: HashMap::new(),
                movement: 1,
                absolute: false,
            }),
            b'<' => instructions.push(Instruction::Change {
                changes: HashMap::new(),
                movement: -1,
                absolute: false,
            }),
            b'+' => instructions.push(Instruction::Change {
                changes: HashMap::from([(0, 1.try_into().unwrap())]),
                movement: 0,
                absolute: false,
            }),
            b'-' => instructions.push(Instruction::Change {
                changes: HashMap::from([(0, (-1).try_into().unwrap())]),
                movement: 0,
                absolute: false,
            }),
            b'.' => instructions.push(Instruction::Write {
                times: 1.try_into().unwrap(),
                offset: 0,
                absolute: false,
            }),
            b',' => instructions.push(Instruction::Read {
                times: 1.try_into().unwrap(),
                offset: 0,
                absolute: false,
            }),
            b'[' => instructions.push(Instruction::Loop {
                block: process(program, true)?,
                offset: 0,
                absolute: false,
                movement: 0,
            }),
            b']' => {
                break if inside_loop {
                    Ok(instructions)
                } else {
                    Err("loop end with no matching start")
                }
            }
            _ => (),
        }
    }
}
fn optimize_combine(program: &mut Vec<Instruction>) {
    let mut current: Option<&mut Instruction> = None;
    let mut slice = program.as_mut_slice();

    let mut i = 0;
    let mut j = 0;
    while let Some(mut instruction) = slice.get_mut(i - j) {
        i += 1;
        if current.as_deref().is_some_and(|current| {
            std::mem::discriminant(current) != std::mem::discriminant(instruction)
        }) {
            program.drain(j..i - 1);
            i = j + 1;
            j = 0;
            slice = program.as_mut_slice();
            instruction = slice.get_mut(i - 1).unwrap();
            current = None;
        }
        match (instruction, &mut current) {
            // unoptimizable blocks are at the top
            (Instruction::Loop { block, .. }, ..) => optimize_combine(block),
            (.., None) => {
                let (a, b) = slice.split_at_mut(i);
                slice = b;
                current = Some(&mut a[i - 1]);
                j = i;
            }
            // do the optimization bit
            (
                Instruction::Change {
                    changes, movement, ..
                },
                Some(Instruction::Change {
                    changes: changes_t,
                    movement: movement_t,
                    ..
                }),
            ) => {
                *movement_t += *movement;
                for (k, v) in changes {
                    let k = *k + *movement_t;
                    let v = match changes_t.get(&k) {
                        Some(x) => NonZeroI8::new(x.get().wrapping_add(v.get())),
                        None => Some(*v),
                    };
                    if let Some(v) = v {
                        changes_t.insert(k, v);
                    } else {
                        changes_t.remove(&k);
                    }
                }
            }
            (Instruction::Read { times, .. }, Some(Instruction::Read { times: times_t, .. })) => {
                *times_t = times_t.checked_add(times.get()).unwrap();
            }
            (Instruction::Write { times, .. }, Some(Instruction::Write { times: times_t, .. })) => {
                *times_t = times_t.checked_add(times.get()).unwrap();
            }
            _ => (),
        }
    }
    if current.is_some() {
        program.drain(j..);
    }
}
fn relativize(block: &mut [Instruction]) {
    for instruction in block {
        match instruction {
            Instruction::Change { absolute, .. } => *absolute = false,
            Instruction::Write { absolute, .. } => *absolute = false,
            Instruction::Read { absolute, .. } => *absolute = false,
            Instruction::Loop {
                block, absolute, ..
            } => {
                *absolute = false;
                relativize(block);
            }
        }
    }
}
fn optimize_absolute_block(
    block: &mut Vec<Instruction>,
    a_absolute: &mut bool,
    mut a_offset: i16,
    in_loop: bool,
) -> i16 {
    let original_offset = a_offset;
    let mut i = 0;
    while let Some(instruction) = block.get_mut(i) {
        i += 1;
        match instruction {
            Instruction::Change {
                changes,
                movement,
                absolute,
            } => {
                if a_offset != 0 {
                    *changes = changes.drain().map(|(k, v)| (k + a_offset, v)).collect();
                }
                a_offset += *movement;
                if changes.is_empty() {
                    i -= 1;
                    block.remove(i);
                } else {
                    *movement = 0;
                    *absolute = *a_absolute;
                }
            }
            Instruction::Write {
                offset, absolute, ..
            } => {
                *absolute = *a_absolute;
                *offset += a_offset;
            }
            Instruction::Read {
                offset, absolute, ..
            } => {
                *absolute = *a_absolute;
                *offset += a_offset;
            }
            Instruction::Loop {
                block: l_block,
                offset,
                absolute,
                movement,
            } => {
                let old = *a_absolute;
                *movement += optimize_absolute_block(l_block, a_absolute, a_offset, true);
                *offset += a_offset;
                *absolute = *a_absolute;
                if in_loop && old != *a_absolute {
                    relativize(&mut block[..i]);
                }
            }
        }
    }
    if in_loop && a_offset != original_offset {
        *a_absolute = false;
    }
    a_offset - original_offset
}
fn optimize_absolute(program: &mut Vec<Instruction>) {
    optimize_absolute_block(program, &mut true, 0, false);
}
fn load<T: Iterator<Item = u8>>(program: &mut T) -> Result<Vec<Instruction>, &'static str> {
    process(program, false)
}
fn main() {
    let mut args = args().skip(1);
    let in_file = File::open(args.next().expect("input expected")).unwrap();
    let out_file = File::create(args.next().expect("output expected")).unwrap();
    let mut in_buf = BufReader::new(in_file).bytes().flatten();
    let mut out_buf = BufWriter::new(out_file);

    let mut program = load(&mut in_buf).unwrap();
    optimize_combine(&mut program);
    optimize_absolute(&mut program);

    write_c(&mut out_buf, &program).unwrap();
}
