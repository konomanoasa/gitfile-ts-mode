# gitfile-ts-mode

[![CI](https://github.com/konomanoasa/gitfile-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/gitfile-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
Git attributes, configuration, ignore, and mailmap files 2.55.0.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install
 '(gitfile-ts-mode :url "https://github.com/konomanoasa/gitfile-ts-mode"))
```

## Automatic Activation

Enabled for `.gitattributes`, `.gitconfig`, `.gitignore`, `.gitmodules`, and
`.mailmap` files.

## Features

- Comment Commands
- Electric Pair
- Font Lock
- Imenu
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | gitattributes                                         | gitconfig                                 | gitignore                                       | mailmap                   |
| ----- | ----------------------------------------------------- | ----------------------------------------- | ----------------------------------------------- | ------------------------- |
| 1     | Comments                                              | Comments                                  | Comments                                        | Comments                  |
| 2     | Macro markers, attribute names, values, and glob text | Section names, variable names, and values | Glob text                                       | Names and email addresses |
| 3     | Class names and escapes                               | Escapes and line continuations            | Class names and escapes                         | —                         |
| 4     | Operators, wildcards, punctuation, and brackets       | Operators, punctuation, and brackets      | Operators, wildcards, punctuation, and brackets | Address brackets          |

## Grammars

- [tree-sitter-gitattributes](https://github.com/konomanoasa/tree-sitter-gitattributes)
- [tree-sitter-gitconfig](https://github.com/konomanoasa/tree-sitter-gitconfig)
- [tree-sitter-gitignore](https://github.com/konomanoasa/tree-sitter-gitignore)
- [tree-sitter-mailmap](https://github.com/konomanoasa/tree-sitter-mailmap)

## License

[MIT](LICENSE)
