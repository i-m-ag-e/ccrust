#! /usr/bin/python3
import pathlib
import termcolor

TESTS_DIR = pathlib.Path("tests/")

TEMPLATE = """\
use std::fs;
use assert_cmd::Command;
use std::process;
use std::path::PathBuf;

#[test]
fn test_return() {{
    let file = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("{0}");
    let mut command = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();

    command
        .arg(&file)
        .args(["-o", "{1}"]);
    println!("{{command:?}}");

    let output = command.output().unwrap();
    println!(
        "STDOUT: {{:?}}, STDERR: {{:?}}",
        String::from_utf8_lossy(&output.stderr),
        String::from_utf8_lossy(&output.stderr)
    );
    let status = output.status;
    assert!(status.success());

    let mut gcc_compile = process::Command::new("gcc");
    let gcc_compile_status = gcc_compile
        .arg(&file)
        .args([
            "-o",
            "{1}_gcc",
        ])
        .status()
        .unwrap();
    assert!(gcc_compile_status.success());

    let expected_ec = process::Command::new("{1}_gcc")
        .status()
        .unwrap()
        .code()
        .unwrap();

    Command::new("{1}")
        .assert()
        .code(expected_ec);
    fs::remove_file("{2}").unwrap();
}}
"""

def generate_template(c_file: pathlib.PosixPath) -> pathlib.PosixPath:
    out_file = c_file.parent / "output" / c_file.with_suffix("").name
    return TEMPLATE.format(c_file, out_file, c_file.with_suffix(".s"))

if __name__ == "__main__":
    print("Generating...")

    for c_file in (TESTS_DIR / "files").glob("*.c"):
        print(c_file)
        test_file = c_file.parent.parent / c_file.with_suffix(".rs").name
        test = generate_template(c_file)

        print(f"Generating {termcolor.colored(test_file, 'green')}...")

        with test_file.open('w') as f:
            f.write(test)