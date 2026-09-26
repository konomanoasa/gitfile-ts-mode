# git-ts-mode

[![CI](https://github.com/konomanoasa/git-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/git-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
Git ignore, attributes, and configuration files 2.55.0.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/git-ts-mode")
```

## Automatic Activation

Enabled for `.gitignore`, `.gitattributes`, and `.gitconfig` files.

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

| Level | gitignore                                       | gitattributes                                         | gitconfig                                 |
| ----- | ----------------------------------------------- | ----------------------------------------------------- | ----------------------------------------- |
| 1     | Comments                                        | Comments                                              | Comments                                  |
| 2     | Glob text                                       | Macro markers, attribute names, values, and glob text | Section names, variable names, and values |
| 3     | Class names and escapes                         | Class names and escapes                               | Escapes and line continuations            |
| 4     | Operators, wildcards, punctuation, and brackets | Operators, wildcards, punctuation, and brackets       | Operators, punctuation, and brackets      |

## Grammars

- [tree-sitter-gitignore](https://github.com/konomanoasa/tree-sitter-gitignore)
- [tree-sitter-gitattributes](https://github.com/konomanoasa/tree-sitter-gitattributes)
- [tree-sitter-gitconfig](https://github.com/konomanoasa/tree-sitter-gitconfig)

## License

[MIT](LICENSE)
