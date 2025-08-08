## My solutions to Casey Muratori's [Computer Enhance](https://www.computerenhance.com/) course

# Part 1
### **Intel 8086 simulator**

To decode / simulate a 8086 binary:
```console
USAGE:
    cargo run --release -- [OPTIONS] <FILES>...

OPTIONS:
    --sim               Run in simulation mode, else just decode
    --time <CPU>        Set CPU type [possible values: 8086, 8088]
    --dump              Dump data of DS segment
    --quiet             Don't print result to stdout

ARGS:
    <FILES>...          One or more input files
```

To run the tests for the decoder:
```sh
cargo test --release
```
