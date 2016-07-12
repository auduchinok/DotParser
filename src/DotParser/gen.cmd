lib\FsYARD\YC.FsLex.exe --unicode src\DotParser\Lexer.fsl -o src\DotParser\gen\Lexer.fs

lib\FsYARD\YC.YaccConstructor.exe -f YardFrontend -i src\DotParser\Grammar.yrd -g "RNGLRGenerator -pos int -token string -module RNGLR.Grammar -translate true -o src\DotParser\gen\Parser.fs"