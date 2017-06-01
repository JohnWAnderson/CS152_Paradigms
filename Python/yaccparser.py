# -----------------------------------------------------------------------------
# Name:        yaccparser
# Purpose:     CS 152 - Demonstrate the use of yacc to generate a parser
# -----------------------------------------------------------------------------
"""
yacc generated parser to recognize & evaluate simple arithmetic expressions

Supports the following grammar:
    <command> ::= <arith_expr>
    <arith_expr> ::= <arith_expr> ADD_OP <term> | <term>
    <term> ::= <term> MULT_OP <factor> | <factor>
    <factor>::= LPAREN <arith_expr> RPAREN | FLOAT | INT
"""

import yacc
import lex

# List of token names - required
tokens =  ('FLOAT', 'INT',
          'ADD_OP', 'MULT_OP',
           'EXP',
          'LPAREN', 'RPAREN')


# Regular expression rules for simple tokens
t_ADD_OP = r'\+|-'
t_MULT_OP = r'\*|/'
t_POWER = r'\*\*'
t_LPAREN = r'\('
t_RPAREN = r'\)'

# Regular expression rules with some action code
# The order matters
def t_FLOAT(t):
    r'\d*\.\d+|\d+\.\d*'
    t.value = float(t.value)  # string must be converted to float
    return t


def t_INT(t):
    r'\d+'
    t.value = int(t.value)  # string must be converted to int
    return t

# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


# Lexical error handling rule
def t_error(t):
    print("ILLEGAL CHARACTER: {}".format(t.value[0]))
    t.lexer.skip(1)

#  yacc input

def p_command(p):
    'command : arith_expr'
    p[0] = p[1]

def p_arith_expr(p):
    'arith_expr : arith_expr ADD_OP term'
    if p[2] == '+':
        p[0] = p[1] + p[3]
    else:
        p[0] = p[1] - p[3]

def p_arith_expr_term(p):
    'arith_expr : term'
    p[0] = p[1]

def p_term(p):
    'term : term MULT_OP power'
    if p[2] == '*':
        p[0] = p[1] * p[3]
    else:
        p[0] = p[1] / p[3]


def p_term_power(p):
    'term : power'
    p[0] = p[1]

def p_power(p):
    'power : factor EXP power'
    p[0] = p[1] ** p[3]

def p_power_factor(p):


def p_factor(p):
    'factor : LPAREN arith_expr RPAREN'
    p[0] = p[2]

def p_factor_float(p):
    'factor : FLOAT'
    p[0] = p[1]

def p_factor_int(p):
    'factor : INT'
    p[0] = p[1]

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")

def main():
    # Build the lexer
    lexer = lex.lex()
    # Build the parser
    parser = yacc.yacc()
    # Prompt for a command
    more_input = True
    while more_input:
        input_command = input("Pluto 1.0>>>")
        if input_command == 'q':
            more_input = False
            print('Bye for now')
        elif input_command:
            result = parser.parse(input_command)
            print(result)

if __name__ == '__main__':
    main()