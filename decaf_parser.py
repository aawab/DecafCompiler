# Vincent Zheng, vinzheng, 113469839
# Aawab Mahmood, aamahmood, 113472709

from decaf_lexer import tokens
from decaf_ast import *

import sys

# List of precedence and associativity rules for operators from least to highest precedence
precedence = (
    ('right', 'ASSIGNED_TO'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'EQUAL_TO', 'NOT_EQUAL_TO'),
    ('nonassoc', 'GREATER_THAN','LESS_THAN','GREATER_THAN_EQ','LESS_THAN_EQ'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS', 'UPLUS', 'NOT')
)

# Global vars for keeping track of context to add to correct bodies/classes/methods etc
currContext = ""
currClass = None
currModifiers = None
currType = None
currFunction = None
formalOrLocal = None

# Program start

def p_program(p):
    '''
    program : class_decl_mult
    '''
    return True

# Class declaration and body productions

def p_class_decl_mult(p):
    '''
    class_decl_mult : class_decl class_decl_mult
                    | empty
    '''
    pass

def p_class_decl(p):
    '''
    class_decl : class_head_decl '{' class_body_decl_mult '}'
    '''
    pass

def p_class_head_decl(p):
    '''
    class_head_decl : CLASS ID
                    | CLASS ID EXTENDS ID
    '''
    global currClass, currContext

    # Grab class name, check if superclass name is valid or not
    cname = p[2]
    sc = None

    if len(p)> 3 and p[3] == 'extends':
        sc = searchForClass(p[4])
        if sc==None:
            print(f'Error! Class {p[4]} does not exist! Error at line {p.lineno(0)} and col 4')
            sys.exit(0)

    # Check if class exists, if not then create Class() and add to table
    c = searchForClass(cname)
    if c!=None:
        print(f'Error! Class {cname} already exists! Error at line {p.lineno(0)} and col 2')
        sys.exit(0)
    else:
        c = Class(cname, sc)
        addClassToTable(cname, c)
    currContext = "class"
    currClass = c
    pass

def p_class_body_decl_mult(p):
    '''
    class_body_decl_mult : class_body_decl class_body_decl_cont
    '''
    pass

def p_class_body_decl_cont(p):
    '''
    class_body_decl_cont : class_body_decl class_body_decl_cont
                         | empty
    '''
    pass

def p_class_body_decl(p):
    '''
    class_body_decl : field_decl
                    | method_decl
                    | constructor_decl
    '''
    pass

# Field, method and constructor declarations

def p_modifier(p):
    '''
    modifier : PUBLIC STATIC
             | PRIVATE STATIC
             | PUBLIC
             | PRIVATE
             | STATIC
             | empty
    '''
    global currModifiers
    if len(p)==3:   
        currModifiers = (p[1], p[2])
    else:
        if p[1] == "static":
            currModifiers = ('private', 'static')
        elif p[1] == "public":
            currModifiers = ('public', 'instance')
        else:
            currModifiers = ('private', 'instance')
    pass

def p_type(p):
    '''
    type : INT
         | FLOAT
         | BOOLEAN
         | STRING
         | ID
    '''
    global currType
    
    if p[1]!='int' and p[1]!='float' and p[1]!='boolean' and p[1]!='string':
        c = searchForClass(p[1])
        if c == None:
            print(f'Class {p[1]} does not exist! Error at line {p.lineno(0)} and col 1')
            sys.exit(0)
        p[0] = currType = Type(c)
    else:
        p[0] = currType = Type(p[1])
    

def p_field_decl(p):
    '''
    field_decl : modifier var_decl
    '''
    pass

def p_method_decl(p):
    '''
    method_decl : method_head_decl '(' formals ')' block
    '''
    # p[1] has the method obj given from below production
    p[1].body = p[5]
    pass

def p_method_head_decl(p):
    '''
    method_head_decl : modifier type ID
                     | modifier VOID ID
    '''
    global currContext, currModifiers, currType, currFunction, formalOrLocal
    if currType:
        type = currType
    if p[2] == 'void':
        type = Type('void')

    method = Method(p[3], currClass.name, currModifiers[0], currModifiers[1], type)
    currClass.addMethod(method)

    currContext = "method" 
    currFunction = method
    formalOrLocal = 'formal'
    p[0] = method


def p_constructor_decl(p):
    '''
    constructor_decl : constructor_head_decl '(' formals ')' block
    '''
    global formalOrLocal
    formalOrLocal = None
    p[1].body = p[5]
    pass

def p_constructor_head_decl(p):
    '''
    constructor_head_decl : modifier ID
    '''
    global currContext, currModifiers, currFunction, formalOrLocal

    constructor = Constructor(p[2], currModifiers[0])
    currClass.addConstructor(constructor)

    currContext = "constructor" 
    currFunction = constructor
    formalOrLocal = 'formal'
    p[0] = constructor

def p_formals(p):
    '''
    formals : formal_param formals_cont
            | empty
    '''
    global formalOrLocal
    formalOrLocal = 'local'
    pass

def p_formals_cont(p):
    '''
    formals_cont : ',' formal_param formals_cont
                 | empty
    '''
    pass

def p_formal_param(p):
    '''
    formal_param : type variable
    '''
    pass

def p_var_decl(p):
    '''
    var_decl : type variables ';'
    '''
    pass

def p_variables(p):
    '''
    variables : variable variables_cont
    '''
    pass

def p_variables_cont(p):
    '''
    variables_cont : ',' variable variables_cont
                   | empty
    '''
    pass

def p_variable(p):
    '''
    variable : ID
    '''
    global currContext, currClass, currModifiers, currType, formalOrLocal
    if currContext=="class":
        if currClass.searchForField(p[1])!=None:
            print(f'Field {p[1]} already exists in this class! Error at line {p.lineno(0)} and col 1')
            sys.exit(0)
        else:
            f = Field(p[1], currClass.name, currModifiers[0], currModifiers[1], currType)
            currClass.addField(p[1], f)
    
    elif currContext == "method" or currContext == "constructor":
        if currFunction.searchForVar(p[1]):
            print(f'Variable {p[1]} already exists in this method/constructor! Error at line {p.lineno(0)} and col 1')
            sys.exit(0)
        else:
            currFunction.addVar(Variable(p[1], formalOrLocal, currType))

# Block and statement productions

def p_block(p):
    '''
    block : '{' stmts '}'
    '''
    global formalOrLocal
    formalOrLocal = 'local'
    
    body = []
    ptr = p[2]
    while ptr:
        if ptr.val:
            body.append(ptr.val)
        
        ptr = ptr.next
    
    p[0] = Block(p.lineno(1), p.lineno(3), body)

def p_stmts(p):
    '''
    stmts : stmt stmts
          | empty
    '''
    if len(p) != 2:
        node = LinkedList(p[1])
        node.addNext(p[2])
        p[0] = node

def p_stmt(p):
    '''
    stmt : IF '(' expr ')' stmt 
         | IF '(' expr ')' stmt ELSE stmt
         | WHILE '(' expr ')' stmt
         | FOR '(' for_cond1 ';' for_cond2 ';' for_cond3 ')' stmt
         | RETURN return_val ';'
         | stmt_expr ';'
         | BREAK ';'
         | CONTINUE ';'
    '''
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    if p[1] == 'if':
        if len(p) == 6:
            body = Block(None, None, [Skip_Statement(None, None)])
            p[0] = If_Statement(start, end, p[3], p[5], body)
        else:
            p[0] = If_Statement(start, end, p[3], p[5], p[7])
    elif p[1] == 'while':
        p[0] = While_Statement(start, end, p[3], p[5])
    elif p[1] == 'for':
        p[0] = For_Statement(start, end, p[3], p[5], p[7], p[9])
    elif p[1] == 'return':
        p[0] = Return_Statement(start, end, p[2])
    elif p[1] == 'break':
        p[0] = Break_Statement(start, end)
    elif p[1] == 'continue':
        p[0] = Continue_Statement(start, end)
    elif p[1]:
        p[0] = Expr_Statement(start, end, p[1])

def p_block_stmt(p):
    '''
    stmt : block
    '''
    p[0] = p[1]

def p_var_decl_stmt(p):
    '''
    stmt : var_decl
    '''
    p[0] = Skip_Statement(None, None)

def p_empty_stmt(p):
    '''
    stmt : ';'
    '''
    p[0] = Skip_Statement(p.lineno(1))



def p_for_cond1(p):
    '''
    for_cond1 : stmt_expr
              | empty
    '''
    p[0] = p[1]

def p_for_cond2(p):
    '''
    for_cond2 : expr
              | empty
    '''
    p[0] = p[1]

def p_for_cond3(p):
    '''
    for_cond3 : stmt_expr
              | empty
    '''
    p[0] = p[1]

def p_return_val(p):
    '''
    return_val : expr
               | empty
    '''
    p[0] = p[1]

def p_literal(p):
    '''
    literal : LITER_INT
            | LITER_FLOAT
            | LITER_STRING
            | NULL
            | TRUE
            | FALSE
    '''
    start, end = p.linespan(1)

    if p[1] == 'true':
        p[0] = Const_Expression(start, end, 'true', 'TRUE')
    elif p[1] == 'false':
        p[0] = Const_Expression(start, end, 'false', 'FALSE')
    elif p[1] == 'null':
        p[0] = Const_Expression(start, end, 'null', 'NULL')
    elif type(p[1]) == int:
        p[0] = Const_Expression(start, end, p[1], 'LITER_INT')
    elif type(p[1]) == float:
        p[0] = Const_Expression(start, end, float(p[1]), 'LITER_FLOAT')
    else:
        p[0] = Const_Expression(start, end, p[1], 'LITER_STRING')

def p_primary(p):
    '''
    primary : literal
            | THIS
            | SUPER
            | '(' expr ')'
            | NEW ID '(' arguments ')'
            | lhs
            | method_invocation
    '''
    global currClass

    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]
    if p[1] == "new":
        cval = searchForClass(p[2])
        if cval:
            p[0] = New_Object_Expression(start, end, cval, p[4])
        else:
            print(f'No constructor/class that exists by name {p[2]}! Error at line {p.lineno(0)} and col 2')
            sys.exit(0)
    elif p[1] == "super":
        if currClass.superClass == None:
            print(f'Super used when current class has no superclass. Error at line {p.lineno(0)} and col 1')
            sys.exit(0)
        
        p[0] = Super_Expression(start, end)

    elif p[1] == "this":
        p[0] = This_Expression(start, end)

    elif len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[1]
        
def p_arguments(p):
    '''
    arguments : expr arguments_cont
              | empty
    '''
    if len(p) == 2:
        p[0] = []

    else:
        # Unravel result from arguments_cont and then turn into list
        params = [p[1]]

        ptr = p[2]
        while ptr != None:
            params.append(ptr.val)
            ptr = ptr.next
        
        p[0] = params

def p_arguments_cont(p):
    '''
    arguments_cont : ',' expr arguments_cont
                   | empty
    '''
    if len(p) != 2:
        node = LinkedList(p[2])
        node.addNext(p[3])
        p[0] = node

def p_lhs(p):
    '''
    lhs : field_access
    '''
    p[0] = p[1]

def p_field_access(p):
    '''
    field_access : primary '.' ID
                 | ID
    '''
    global currClass, currFunction

    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    
    if len(p) == 2:
        var = currFunction.searchForVar(p[1])
        if var:
            p[0] = Var_Expression(start, end, var)
        else:
            c = searchForClass(p[1])
            if c:
                p[0] = Class_Ref_Expression(start, end, c)
            else:
                p[0] = Field_Access_Expression(start, end, This_Expression(start, end), p[1])
        
    else:
        p[0] = Field_Access_Expression(start, end, p[1], p[3])

def p_method_invocation(p):
    '''
    method_invocation : field_access '(' arguments ')'
    '''
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    if isinstance(p[1], Field_Access_Expression):
        p[0] = Method_Call_Expression(start, end, p[1], p[3])
    else:
        print(f"Method doesn't exist! Error at line {p.lineno(0)} and col 1")
        sys.exit(0)
    pass

def p_expr(p):
    '''
    expr : primary
         | assign
    '''
    p[0] = p[1]

def p_assign(p):
    '''
    assign : lhs ASSIGNED_TO expr
           | lhs PLUSPLUS
           | PLUSPLUS lhs
           | lhs MINUSMINUS
           | MINUSMINUS lhs
    '''
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    if p[2] == '=':
        p[0] = Assign_Expression(start, end, p[1], p[3])
    
    elif p[2] == '++':
        p[0] = Auto_Expression(start, end, p[1], '+', 'post')

    elif p[1] == '++':
        p[0] = Auto_Expression(start, end, p[2], '+', 'pre')

    elif p[2] == '--':
        p[0] = Auto_Expression(start, end, p[1], '-', 'post')

    elif p[1] == '--':
        p[0] = Auto_Expression(start, end, p[2], '-', 'pre')


def p_add_expr(p):
    'expr : expr PLUS expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_sub_expr(p):
    'expr : expr MINUS expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_mult_exp(p):
    'expr : expr TIMES expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_div_expr(p):
    'expr : expr DIVIDE expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_conj_expr(p):
    'expr : expr AND expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_disj_expr(p):
    'expr : expr OR expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_equals_expr(p):
    'expr : expr EQUAL_TO expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_notequals_expr(p):
    'expr : expr NOT_EQUAL_TO expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_lt_expr(p):
    'expr : expr LESS_THAN expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_lte_expr(p):
    'expr : expr LESS_THAN_EQ expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_gt_expr(p):
    'expr : expr GREATER_THAN expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_gte_expr(p):
    'expr : expr GREATER_THAN_EQ expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Bin_Expression(start, end, p[1], p[3], p[2])
    pass

def p_pos_expr(p):
    'expr : PLUS expr %prec UPLUS'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]
    p[0] = Unary_Expression(start, end, p[2], '+' )
    pass

def p_minus_expr(p):
    'expr : MINUS expr %prec UMINUS'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]
    p[0] = Unary_Expression(start, end, p[2], '-' )
    pass

def p_not_expr(p):
    'expr : NOT expr'
    start = p.lineno(1)
    end = p.linespan(len(p) - 1)[1]

    p[0] = Unary_Expression(start, end, p[2], '!')

def p_stmt_expr(p):
    '''
    stmt_expr : assign
              | method_invocation
    '''
    p[0] = p[1]
    
def p_empty(p):
    '''
    empty :
    '''
    pass

# Error rule for syntax errors
def p_error(p):
    if p:
        # Find column number by walking lexpos backwards to find the first 
        colno = p.lexpos - p.lexer.lexdata.rfind('\n', 0, p.lexpos)
        print(f"Syntax error '{p.value}' on line {p.lineno} column {colno}")
        raise Exception()
    else:
        print("Syntax error at EOF")