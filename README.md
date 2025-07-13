# al-conf

**al-conf** is a simple command-line tool for managing shell aliases—specifically for `zsh` and `bash`. I wrote it in Haskell as a way to get to grips with the language, so it's lightweight, minimal, and probably a bit rough around the edges. Still, it does the job nicely.

## Features

- Add, remove, and list aliases for your `bash` or `zsh` config
- Optional colourised output and full-line printing for listing
- Handy for scripting or quick alias tweaks

## Usage

al-conf <command> [options]


### Commands

#### `add`

Add a new alias.

al-conf add <shell> <alias> <command>


- `<shell>`: `bash` or `zsh`
- `<alias>`: The name of the alias (no spaces)
- `<command>`: The command the alias should run

**Example:**

```bash
al-conf add zsh ll 'ls -la'
```
#### `rm`

Remove an alias.

al-conf rm <shell> <alias>

Example:

```bash
al-conf rm bash ll
```
#### `list`

List aliases from a shell config.

al-conf list <shell> [-c] [-f]

    -c: Enable coloured output

    -f: Print the full alias lines

Example:

```bash
al-conf list zsh -cf
```

#### `--help / -h`

Display usage information.

```bash
al-conf --help
```

### Supported Shells

- bash
- zsh

### Notes

Aliases are written directly into the relevant shell config (~/.bashrc, ~/.zshrc, etc.), so back up your files if you're worried about accidental edits.

It's a learning project, so expect a few quirks. Feedback and suggestions welcome.
