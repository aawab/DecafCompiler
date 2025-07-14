# Vincent Zheng, vinzheng, 113469839
# Aawab Mahmood, aamahmood, 113472709

# List of reserved keywords
reserved = {
   'boolean' : 'BOOLEAN',
   'break' : 'BREAK',
   'continue' : 'CONTINUE',
   'class' : 'CLASS',
   'do'    : 'DO',
   'else'  : 'ELSE',
   'extends' : 'EXTENDS',
   'false' : 'FALSE',
   'float' : 'FLOAT',
   'for'   : 'FOR',
   'if'    : 'IF',
   'int'   : 'INT',
   'new'   : 'NEW',
   'null'  : 'NULL',
   'private': 'PRIVATE',
   'public': 'PUBLIC',
   'return': 'RETURN',
   'static': 'STATIC',
   'string' : 'STRING',
   'super' : 'SUPER',
   'this'  : 'THIS',
   'true'  : 'TRUE',
   'void'  : 'VOID',
   'while' : 'WHILE',
}

# List of character literals for an easier time reading grammar productions in parser
literals = ['(', ')', '{', '}', ';', '.', ',', '[', ']']

# List of token names
tokens = ([
   'COMMENT',
   'ID',
   'LITER_INT',
   'LITER_FLOAT',
   'LITER_STRING',
   'PLUS',
   'MINUS',
   'TIMES',
   'DIVIDE',
   'NOT',
   'AND',
   'OR',
   'ASSIGNED_TO',
   'EQUAL_TO',
   'NOT_EQUAL_TO',
   'GREATER_THAN',
   'LESS_THAN',
   'GREATER_THAN_EQ',
   'LESS_THAN_EQ',
   'PLUSPLUS',
   'MINUSMINUS'] + list(reserved.values())
)

# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_NOT     = r'!'
t_AND     = r'&&'
t_OR      = r'\|\|'
t_ASSIGNED_TO  = r'='
t_EQUAL_TO     = r'=='
t_NOT_EQUAL_TO     = r'!='
t_GREATER_THAN = r'>'
t_LESS_THAN    = r'<'
t_GREATER_THAN_EQ  = r'>='
t_LESS_THAN_EQ = r'<='
t_PLUSPLUS = r'\+\+'
t_MINUSMINUS = r'\-\-'

# String to ignore spaces and tabs
t_ignore  = ' \t'

# Token definitions with action code
def t_COMMENT(t):
    r'(\/\/.*\n?|\/\*([^\/\*]|\n)*\*\/)'
    pass

def t_ID(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

def t_LITER_FLOAT(t):
    r'[0-9]+\.[0-9]+'
    try:
        t.value = float(t.value)
    except Exception as e:
        print('value could not be converted to float, %d', t.value)
        print(e)
        t.value = 0.0
    return t

def t_LITER_INT(t):
    r'[0-9]+'
    try:
        t.value = int(t.value)
    except ValueError as ve:
        print('Integer value too large, %d', t.value)
        print(ve)
        t.value = 0
    return t

def t_LITER_STRING(t):
    r'\".*\"'
    return t

# Rule to track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Error handling rule
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lexer.lineno}")
    raise Exception()