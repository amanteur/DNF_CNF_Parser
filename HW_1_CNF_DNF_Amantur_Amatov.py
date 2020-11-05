import ply.lex as lex
import ply.yacc as yacc

tokens = [
    'VAR',
    'CONJUCTION',
    'DISJUNCTION',
    'NEGATION',
    'IMPLICATION',
    'LPARAN',
    'RPARAN'
]

t_CONJUCTION = r'/\\'
t_DISJUNCTION = r'\\/'
t_NEGATION = r'~'
t_IMPLICATION = r'->'
t_LPARAN = r'\('
t_RPARAN = r'\)'
t_ignore = ' '


def t_VAR(t):
    r'[a-zA-Z_][a-zA-Z_]*'
    t.type = 'VAR'
    return t


def t_error(t):
    print('Illegal characters!')
    t.lexer.skip(1)


lexer = lex.lex()

precedence = (
    ('left', 'DISJUNCTION'),
    ('left', 'CONJUCTION'),
    ('right', 'IMPLICATION'),
    ('left', 'NEGATION'),
    ('left', 'LPARAN'),
    ('right', 'RPARAN')
)


def p_calc(p):
    '''
    calc : expression 
         | empty
    '''

    CNF_list = []
    DNF_list = []
    print('Structure: ', p[1])

    for mask in range(0, 2 ** len(dict_test)):
        for idx, key in enumerate(dict_test):
            dict_test[key] = (mask // (2 ** idx)) % 2
        dict_test['result'] = bool(run(p[1], dict_test))
        # DNF
        if dict_test['result']:
            local_list_DNF = []
            for key in dict_test:
                if key == 'result':
                    continue
                if dict_test[key] == 0:
                    local_list_DNF.append('~' + key)
                    local_list_DNF.append(' /\\ ')
                else:
                    local_list_DNF.append(key)
                    local_list_DNF.append(' /\\ ')
            local_list_DNF = local_list_DNF[:-1]
            DNF_list.append(local_list_DNF)
        # CNF
        else:
            local_list_CNF = []
            for key in dict_test:
                if key == 'result':
                    continue
                if dict_test[key] == 0:
                    local_list_CNF.append(key)
                    local_list_CNF.append(' \\/ ')
                else:
                    local_list_CNF.append('~' + key)
                    local_list_CNF.append(' \\/ ')
            local_list_CNF = local_list_CNF[:-1]
            CNF_list.append(local_list_CNF)
    # String making
    DNF_string = ' \\/ '.join(['(' + ''.join(dnf_local) + ')' for dnf_local in DNF_list])
    CNF_string = ' /\\ '.join(['(' + ''.join(cnf_local) + ')' for cnf_local in CNF_list])
    if DNF_string == '':
        print('No DNF (not satisfiable)')
    else:
        print('DNF: {}'.format(DNF_string))
    if CNF_string == '':
        print('No CNF (tautology)')
    else:
        print('CNF: {}'.format(CNF_string))


def p_expression_paran(p):
    '''
    expression : LPARAN expression RPARAN
    '''
    p[0] = (p[2])


def p_expression_uni(p):
    '''
    expression : NEGATION expression
    '''
    p[0] = (p[1], p[2])


def p_expression_bi(p):
    '''
    expression : expression IMPLICATION expression
               | expression DISJUNCTION expression
               | expression CONJUCTION expression
    '''
    p[0] = (p[2], p[1], p[3])


def p_expression_var(p):
    '''
    expression : VAR
    '''
    p[0] = p[1]
    dict_test[p[1]] = 0


def p_empty(p):
    '''
    empty :
    '''
    p[0] = None


def p_error(p):
    print('Syntax error found!')


parser = yacc.yacc()


def run(expr, dict_test):
    if type(expr) is str:
        return bool(dict_test[expr])
    if type(expr) is tuple:
        if len(expr) == 2:
            return not run(expr[1], dict_test)
        elif len(expr) == 3:
            op = expr[0]
            if type(expr[1]) is tuple:
                left = run(expr[1], dict_test)
            else:
                left = dict_test[expr[1]]

            if type(expr[2]) is tuple:
                right = run(expr[2], dict_test)
            else:
                right = dict_test[expr[2]]

            if op == "\\/":
                return (left or right)
            if op == "/\\":
                return (left and right)
            if op == "->":
                return not left or right


while True:
    try:
        dict_test = {}
        s = input('Formula: ')
    except EOFError:
        break
    parser.parse(s)
