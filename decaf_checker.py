# Vincent Zheng
# Aawab Mahmood

import sys
import ply.lex as lex
import ply.yacc as yacc
import logging

from decaf_ast import printClassTable, initialize, typeCheck
from decaf_codegen import generateCode

def just_scan():
    fn = sys.argv[1] if len(sys.argv) > 1 else ""
    if fn == "":
        print("Missing file name for source program.")
        print("USAGE: python3 decaf_checker.py <decaf_source_file_name>")
        sys.exit()
    import decaf_lexer
    lexer = lex.lex(module = decaf_lexer)

    fh = open(fn, 'r')
    source = fh.read()
    lexer.input(source)
    next_token = lexer.token()
    while next_token != None:
        next_token = lexer.token()
# end def just_scan()


def main():
    fn = sys.argv[1] if len(sys.argv) > 1 else ""
    if fn == "":
        print("Missing file name for source program.")
        print("USAGE: python3 decaf_checker.py <decaf_source_file_name>")
        sys.exit()
    import decaf_lexer
    import decaf_parser
    try:
        lexer = lex.lex(module = decaf_lexer)
    except:
        exit(0)

    logging.basicConfig(
        level = logging.DEBUG,
        filename = "parselog.txt",
        filemode = "w",
        format = "%(filename)10s:%(lineno)4d:%(message)s"
    )
    
    log = logging.getLogger()
    
    parser = yacc.yacc(module = decaf_parser)

    fh = open(fn, 'r')
    source = fh.read()
    fh.close()
    if fn.endswith('.decaf'):
        (fn, sep, end) = fn.rpartition('.')
    try:
        initialize()
        parser.parse(source, lexer = lexer, debug=log, tracking=True)
        # If typecheck passes, print the class table and generate the output machine code to filename.ami
        if typeCheck():
            printClassTable()
            generateCode(fn)
            print("All done!")
        
    except Exception as e:
        import traceback
        traceback.print_exc()
        exit(0)

if __name__ == "__main__":
    just_scan()
    main()
