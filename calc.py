# Robert Kulow

import ply.lex as lex
import ply.yacc as yacc

# Instantiate the variables needed for the lexer
tokens = ('VARIABLE', 'NUMBER', 'FLOOR')
literals = ['+', '-', '*', '/', '(', ')', '=', '%', ',']
t_VARIABLE = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_FLOOR = r'//'
t_ignore = " \t"
precedence = (('left', '+', '-'), ('left', '*', '/', '%', 'FLOOR', ','), ('right', 'NEGATIVE'))
variables = {}

# Define what a NUMBER token is, which is any real number EX. 1, .5, -22, etc.
def t_NUMBER(t):
    r'[\d.]+[.]*\d*'
    t.value = float(t.value)
    return t

# Define when a new line is made
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count('\n')

# Define what to do when there are invalid inputs given.
def t_error(t):
    print("Error! Invalid character '%s'" % t.value[0])
    t.lexer.skip(1)

# Define what expression is a variable versus a number
def p_statement_assignment(p):
    'statement : VARIABLE "=" expression'
    variables[p[1]] = p[3]

# Define what to print after the operations have been performed
def p_statement_expression(p):
    'statement : expression'
    try:
        if isinstance(p[1], list):
            print(f"({', '.join(str(int(x)) if isinstance(x, float) and x.is_integer() else str(x) for x in p[1])})")
        elif p[1] % int(p[1]) == 0:
            print(int(p[1]))
        else:
            print(p[1])
    except ZeroDivisionError:
        if p[1] == 0.0:
            print(int(p[1]))
        else:
            print(p[1])
        p[0] = 0
    except TypeError:
        p[0] = 0

# Define what expressions would cause different operations to occur
def p_statment_operation(p):
    '''expression : expression '+' expression 
                  | expression '-' expression 
                  | expression '*' expression 
                  | expression '/' expression 
                  | expression '%' expression
                  | expression FLOOR expression'''
    # If one element is a list then make the other element into a list as well
    if isinstance(p[1], list) and not isinstance(p[3], list):
        p[3] = [p[3]]
    elif isinstance(p[3], list) and not isinstance(p[1], list):
        p[1] = [p[1]]
    # Make both lists equal length by repeating the last element of the shorter list
    if isinstance(p[1], list) and isinstance(p[3], list):
        if len(p[1]) != len(p[3]):
            max_len = max(len(p[1]), len(p[3]))
            if len(p[1]) < max_len: 
                if p[1]:
                    last_elem_p1 = p[1][-1]
                p[1] = p[1] + [last_elem_p1] * (max_len - len(p[1]))
            if len(p[3]) < max_len:
                if p[3]:
                    last_elem_p2 = p[3][-1] 
                p[3] = p[3] + [last_elem_p2] * (max_len - len(p[3]))
        # Do the correct operation for the elements found in the two lists
        if p[2] == '+':
            p[0] = [a + b for a, b in zip(p[1], p[3])]
        elif p[2] == '-':
            p[0] = [a - b for a, b in zip(p[1], p[3])]
        elif p[2] == '*':
            p[0] = [a * b for a, b in zip(p[1], p[3])]
        elif p[2] == '/':
            p[0] = [a / b for a, b in zip(p[1], p[3])]
        elif p[2] == '//':
            p[0] = [a // b for a, b in zip(p[1], p[3])]
        elif p[2] == '%':
            p[0] = [a % b for a, b in zip(p[1], p[3])]
    else:
        # Do the correct operation for the two single elements
        if p[2] == '+':
            p[0] = p[1] + p[3]
        elif p[2] == '-':
            p[0] = p[1] - p[3]
        elif p[2] == '*':
            p[0] = p[1] * p[3]
        elif p[2] == '/':
            p[0] = p[1] / p[3]
        elif p[2] == '//':
            p[0] = p[1] // p[3]
        elif p[2] == '%':
            p[0] = p[1] % p[3]

# Define what to do when parsing a negative number
def p_expression_negative(p):
    "expression : '-' expression %prec NEGATIVE"
    if isinstance(p[2], list):
        p[0] = [-x for x in p[2]]
    else:
        p[0] = -p[2]

# Define what to do when parsing a parentheses
def p_expression_parentheses(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

# Define what to do when parsing a parentheses that contains a list of numbers
def p_expression_list(p):
    "expression : '(' list ')'"
    p[0] = p[2]

# Define what a list contains and how to parse if the list has 2 or more elements
def p_list(p):
    '''list : expression 
            | expression ',' list'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

# Define what to do when parsing a number
def p_expression_number(p):
    "expression : NUMBER"
    p[0] = p[1]

# Define what to do when parsing a varaible, if it already exists in memory with a number, or is being assigned a number
def p_expression_variable(p):
    "expression : VARIABLE"
    try:
        p[0] = variables[p[1]]
    except LookupError:
        print("Error! Variable not found '%s'" % p[1])
        p[0] = 0

# Deine what to do when an error ouccrs in parsing    
def p_error(p):
    if p:
        print("Error! Syntax error occured with '%s'" % p.value)
    else:
        print("Error! End of file")

# Build the lexer and parser
lexer = lex.lex()
parser = yacc.yacc()

# Ask for users input until the user does ^C
def main():
    while 1:
        try:
            algo = input('')
        except EOFError:
            break
        if not algo:
            continue
        yacc.parse(algo)

# Run the program
main()