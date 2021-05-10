# Ledger Swiss Army Knife

A collection of tools for working with [Ledger files](https://plaintextaccounting.org/)
(for now only one tool though).

## Tools

### ledgerdiff

*ledgerdiff* is a chronological diffing tool for ledger files. The tool outputs
an ed-style diff of two ledger files that can be read by Vim.

This tool is useful when merging two ledger files using Vim as the tool uses
the date of transactions to provide more accurate diffs, i.e., operations done
at similar time are closer to each other.

There is already
[hledger-diff](https://hackage.haskell.org/package/hledger-diff) but that tool
fills a different niche. hledger-diff parses and interprets the content of the
ledger journal and outputs missing transactions, while ledgerdiff does mostly
textual parsing and outputs a textual diff.

#### Caveats

Unfortunately, the tool is not fully accurate with its chronological diffing
due to vim's limitations: https://github.com/neovim/neovim/issues/14522.
Once the vim issue is fixed, I can update ledgerdiff to take advantage of the
extended format.

## Installation & Usage

Use `stack run` and `stack install` to run and install the tools respectively.
