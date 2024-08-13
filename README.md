# Shellculator Description
Shellculator is a simple command-line calculator that allows you to perform calculations using basic arithmetic operations and mathematical constants. Results are automatically assigned to letters, which you can reference in subsequent calculations. Ideal for quick, interactive calculations directly in your shell.

## Installation (Linux)

To compile and install the program:

1. **Compile the program:**

    ```sh
    stack build --copy-bins
    ```
    This builds the program and copies the binary to `~/.local/bin/`.
    If this directory is on your PATH installation is done.
    
    If the build fails see [Troubleshooting](#troubleshooting) section.

    Otherwise continue with step 2:

2. **Find directories on PATH**

    ```sh
    echo $PATH
    ```

3. **Move executable to any directory on PATH**
    ```sh
    mv ~/.local/bin/shellculator /usr/local/bin

    ```
    Any other directory on PATH is valid here:

    ```sh
    mv ~/.local/bin/shellculator </any/directory/on/PATH/bin>
    ```


## Usage

#### Start the calculator

```sh
shellculator
```

#### Usage

Enter any arithmetic expression using numbers, +, -, *, /, (), sqrt(), e and pi:
```sh
>> 3 * (2 + 5)
21                          [a]
```

You can use previously calculated results by referring to their letters:
```sh
>> a * 2
42                          [b]
```

#### Commands
- `help`  - Shows usage information
- `quit`  - Quit the calculator
- `q`     - Quit the calculator

#### Note
- The calculator automatically assigns results to letters starting from 'a'.
- It avoids using 'e', 'p', 'i' and so on, as they are reserved.


## Troubleshooting

Shellculator uses `haskeline` which has a (implicit?) dependency on `libtinfo-dev`, a part of `ncurses`.

To fix on Debian-bases systems install:
```sh
sudo apt install libtinfo-dev
```

For Fedora-base systems:
```sh
sudo dnf install ncurses-devel
```

For Arch-base systems:
```sh
sudo pacman -S ncurses
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.