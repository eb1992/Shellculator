# Shellculator Description
Shellculator is a simple command-line calculator that allows you to perform calculations using basic arithmetic operations and mathematical constants. Results are automatically assigned to letters, which you can reference in subsequent calculations. Ideal for quick, interactive calculations directly in your shell. The calculator is written in Haskell using monadic parser combinators.

## Installation (Linux, macOS, Windows)

To compile and install the program:

1. **Make sure stack is installed:**

    This project is easiest built with [Stack](https://docs.haskellstack.org/en/stable/).
    The easiest way to manage Haskell installations, including Stack, is to use [GHCup](https://www.haskell.org/ghcup/).

2. **Build the program:**

    ```sh
    stack build --copy-bins
    ```
    
    If the build fails see [Troubleshooting](#troubleshooting) section.

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

#### Linux

The command `stack build --copy-bins` builds the program and copies the binary to `~/.local/bin/`.
If this directory is not on your PATH you can:
    
1. **Find directories on PATH**

    ```sh
    echo $PATH
    ```

2. **Move executable to any directory on PATH**

    ```sh
    mv ~/.local/bin/shellculator /usr/local/bin

    ```

    Any other directory on PATH is valid here:

    ```sh
    mv ~/.local/bin/shellculator </any/directory/on/PATH/bin>
    ```

If the build fails in the linking stage the problem can be that Shellculator uses `haskeline` which has a (implicit?) dependency on `libtinfo-dev`, a part of `ncurses`.

To fix on Debian-based systems install:
```sh
sudo apt install libtinfo-dev
```

For Fedora-based systems:
```sh
sudo dnf install ncurses-devel
```

For Arch-based systems:
```sh
sudo pacman -S ncurses
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.