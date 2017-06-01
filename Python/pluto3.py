# -----------------------------------------------------------------------------
# Name:        pluto 3
# Purpose:     CS 152 - Interpreter that supports bindings and function calls
# Author:  Rula Khayrallah
# Editer: John Anderson
# Version 2.1
# -----------------------------------------------------------------------------
"""
Simplified interpreter to demonstrate bindings and function calls

Supports the following grammar:
    <command> ::= <arith_expr> | <assignment> | <function_def>
    <arith_expr> ::= <arith_expr> ADD_OP <term> | <term>
    <term> ::= <term> MULT_OP <factor> | <factor>
    <factor>::= LPAREN <arith_expr> RPAREN | FLOAT | INT | ID
                | <function_call>
    <assignment> ::= ID EQ <arith_expression>
    <function_call> ::= ID LPAREN <expr_list> RPAREN
    <exprlist> ::=  <arith_expr> COMMA <exprlist> | <arith_expr>
    <function_def> ::= FUNCTION  ID LPAREN <paramlist> RPAREN BODY
    <paramlist> ::=  ID COMMA <paramlist> | ID

"""

import yacc
import lex

# List of token names - required
tokens = ('FLOAT', 'INT',
          'ADD_OP', 'MULT_OP',
          'LPAREN', 'RPAREN',
          'EQ', 'COMMA', 'ID', 'BODY', 'FUNCTION')

# Regular expression rules for simple tokens
t_ADD_OP = r'\+|-'
t_MULT_OP = r'\*|/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_EQ = r'='
t_ID = r'[a-z]+'  # only lower case identifiers are supported
t_COMMA = r','
t_FUNCTION = r'FUNCTION'  # must be in uppercase

# Regular expression rules with some action code
def t_FLOAT(t):
    r'\d*\.\d+|\d+\.\d*'
    t.value = float(t.value)  # string must be converted to float
    return t


def t_INT(t):
    r'\d+'
    t.value = int(t.value)  # string must be converted to int
    return t


def t_BODY(t):
    r'->.+'
    t.value = t.value[2:]  # take out the -> before saving the function body
    return t


# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


# Lexical error handling rule
def t_error(t):
    print("ILLEGAL CHARACTER: {}".format(t.value[0]))
    t.lexer.skip(1)


# yacc input

def p_command(p):
    '''command : arith_expr
               | assignment
               | function_def'''
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


def p_assignment(p):
    'assignment : ID EQ arith_expr'
    p[0] = 'ok'
    symbol_table.update({p[1]: {'role': 'VARIABLE', 'value': p[3]}})


def p_term(p):
    'term : term MULT_OP factor'
    if p[2] == '*':
        p[0] = p[1] * p[3]
    else:
        p[0] = p[1] / p[3]


def p_term_factor(p):
    'term : factor'
    p[0] = p[1]


def p_factor(p):
    'factor : LPAREN arith_expr RPAREN'
    p[0] = p[2]


def p_factor_numeric(p):
    '''factor : FLOAT
              | INT'''
    p[0] = p[1]


def p_factor_var(p):
    'factor : ID'

    if p[1] in symbol_table.keys():
        holder = symbol_table.get(p[1])
        if holder.get('role') == 'VARIABLE':
            p[0] = holder.get('value')
        else:
            """TypeError"""
            raise TypeError("unsupported operation for '{}'".format(p[1]))
    else:
        """NameError"""
        raise NameError("name '{}' is not defined".format(p[1]))




def p_factor_function_call(p):
    'factor : function_call'
    p[0] = p[1]


def p_function_def(p):
    'function_def :  FUNCTION ID LPAREN paramlist RPAREN BODY'
    p[0] = 'ok'
    symbol_table.update({p[2]: {'role': 'FUNCTION', 'param_number': len(p[4]), 'paramlist': p[4], 'body': p[6]}})


def p_function_call(p):
    'function_call : ID LPAREN exprlist RPAREN'
    name =  p[1]
    arguments = p[3]
    if valid_function_call(name, arguments):
        p[0] = apply_function(name, arguments)

def p_exprlist(p):
    'exprlist : arith_expr COMMA exprlist'
    p[0] = [p[1]] + p[3]  # add p[1] to the list


def p_exprlist_expr(p):
    'exprlist : arith_expr'
    p[0] = [p[1]]  # make it a list


def p_paramlist(p):
    'paramlist : ID COMMA paramlist'
    p[0] = [p[1]] + p[3]  # add p[1] to the list


def p_paramlist_single(p):
    'paramlist :  ID'
    p[0] = [p[1]]  # make it a list


# Error rule for syntax errors
def p_error(p):
    global syntax_error
    print("Syntax error in input!")
    syntax_error = True

# end of yacc input

def valid_function_call(name, args):
    """
    Validate the function call.
    :param name (string): function name
    :param args: (list of numbers):  function arguments
    :return: True if there are no errors
    If there is an error, the function raises exceptions:
    - a NameError exception if the function name is not in the symbol table
    - a TypeError exception if the name given is a variable-not a function name
    - a TypeError exception if the number of arguments is different than the
      number of formal parameters
    """
    if name in symbol_table.keys():
        holder = symbol_table.get(name)
        if holder.get('role') == 'FUNCTION':
            if holder.get('param_number') == len(args):
                """yes"""
                return True
            else:
                """Type Error"""
                raise TypeError("'{}' takes exactly {} arguments ({} given)".format(name, holder.get('param_number'), len(args)))
        else:
            """Type Error"""
            raise TypeError("unsupported operation for '{}'".format(name))
    else:
        """Name Error"""
        raise NameError("name '{}' is not defined".format(name))

    return False


def apply_function(name, args):
    """
    Apply the function call assuming it is a valid one.
    :param name (string): function name
    :param args: (list of numbers):  function arguments
    :return: the return value (number)
    """
    body = symbol_table[name]['body']
    paramlist = symbol_table[name]['paramlist']
    sorted_params = sorted(paramlist, key=len, reverse=True)
    for each_param in sorted_params:
        index = paramlist.index(each_param)
        body = body.replace(each_param, str(args[index]))
    return eval(body)


# This is the dictionary representing the global symbol table
# All assignments  result in a new or modified entry
# for the assigned identifier.
# All function definitions result in a new or modified  entry
# for the function identifier.
# After the two functions, square and sumofsquares have been defined
# and the variable grade has been assigned a value of 95,
# the symbol table includes the following entries:
#
# symbol_table = {'square': {'role': 'FUNCTION',
#                          'param_number': 1,
#                          'paramlist': ['x'],
#                          'body': 'x * x'},
#               'sumofsquares': {'role': 'FUNCTION',
#                                'param_number': 2,
#                                'paramlist': ['a', 'b'],
#                                'body': 'a * a + b * b'},
#               'grade': {'role': 'VARIABLE', 'value': 95}
#               }

symbol_table = {}
syntax_error = False

def main():
    global syntax_error
    # Build the lexer
    lexer = lex.lex()
    # Build the parser
    parser = yacc.yacc()
    # Prompt for a command
    more_input = True
    while more_input:
        syntax_error = False
        input_command = input("Pluto 3.0>>>")
        if input_command == 'q':
            more_input = False
            print('Bye for now')
        elif input_command:
            expression = parser.parse(input_command)
            if not syntax_error:
                print(expression)


if __name__ == '__main__':
    main()