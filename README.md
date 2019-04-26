# Heading 1

This is a thing.

- List
- List
- List

\textgreek{ὁ ἄνθρωπος καλός ἐστιν.} 

Another thing.[^note]

[^note]: Footnote hereo.
i

pandoc -s README.md -t latex -V fontenc=T2A -V lang -V babel-lang=english -V babel-otherlangs=greek -o greek.pdf

sudo apt-get install texlive-lang-greek texlive-lang-european
