NoMD: LaTeX to Markdown Translator
====

![image](https://img.shields.io/badge/Gradle-7-red.svg)
![image](https://img.shields.io/badge/OpenJDK-SE11-red.svg)
![image](https://img.shields.io/badge/Scala-2.13-orange.svg)
![image](https://img.shields.io/badge/license-BSD%203--Clause-darkblue.svg)

a naive LaTeX processor written in Scala, for conversion from LaTeX to Markdown, based on PEG parsing.

## Features

- macro expansion
- user-defined commands and environments

## Usage

```sh
$ gradle build
$ java -jar build/libs/nomd.jar [cls file] [sty files] source.tex
```

## Contribution

Feel free to make issues at [nextzlog/todo](https://github.com/nextzlog/todo).
Follow [@nextzlog](https://twitter.com/nextzlog) on Twitter.

## License

### Author

[無線部開発班](https://nextzlog.dev)

### Clauses

[BSD 3-Clause License](LICENSE.md)
