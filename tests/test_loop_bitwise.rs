use std::fs;
use assert_cmd::Command;
use std::process;

#[test]
fn test_return() {
    let file = "/home/akul/rustcc/tests/files/test_loop_bitwise.c";
    let mut command = Command::cargo_bin(env!("CARGO_PKG_NAME")).unwrap();

    command
        .arg(file)
        .args(["-o", "/home/akul/rustcc/tests/files/output/test_loop_bitwise"]);
    println!("{command:?}");

    let output = command.output().unwrap();
    println!(
        "STDOUT: {:?}, STDERR: {:?}",
        String::from_utf8_lossy(&output.stderr),
        String::from_utf8_lossy(&output.stderr)
    );
    let status = output.status;
    assert!(status.success());

    let mut gcc_compile = process::Command::new("gcc");
    let gcc_compile_status = gcc_compile
        .arg(file)
        .args([
            "-o",
            "/home/akul/rustcc/tests/files/output/test_loop_bitwise_gcc",
        ])
        .status()
        .unwrap();
    assert!(gcc_compile_status.success());

    let expected_ec = process::Command::new("/home/akul/rustcc/tests/files/output/test_loop_bitwise_gcc")
        .status()
        .unwrap()
        .code()
        .unwrap();

    Command::new("/home/akul/rustcc/tests/files/output/test_loop_bitwise")
        .assert()
        .code(expected_ec);
    fs::remove_file("/home/akul/rustcc/tests/files/test_loop_bitwise.s").unwrap();
}
