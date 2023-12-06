from poperror import *


class Lexical_Analyser:
    def __init__(self, file_name, text):
        self.file_name = file_name
        self.text = text
        self.pos = Position(-1, 0, -1, file_name, text)
        self.current_char = None
        self.next()

    def next(self):
        self.pos.next(self.current_char)
        self.current_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

    @property
    def generating_token(self):
        tokens = []

        while self.current_char is not None:
            # Ignoring the characters such as spaces and tabs
            if self.current_char in ' \t':
                self.next()
            # Ignoring the characters which starts with #
            elif self.current_char == '#':
                self.comments()
            # Creating Function for Digits
            elif self.current_char in DIGITS:
                tokens.append(self.generating_number())
            # Creating Function for Letters
            elif self.current_char in LETTERS:
                tokens.append(self.generating_identifier())
            # Creating Function for String "which are written in double inverted comma's"
            elif self.current_char == '"':
                tokens.append(self.generating_string())
            # Creating Function for String 'which are written in inverted comma's'
            elif self.current_char == "'":
                tokens.append(self.generating_string())
            # Creating Function for Nextline character
            elif self.current_char == '\n':
                tokens.append(Tokens(Tokentype_NEWLINE, pos_start=self.pos))

            elif self.current_char == '+':
                tokens.append(Tokens(Tokentype_PLUS, pos_start=self.pos))
                self.next()

            elif self.current_char == '-':
                tokens.append(Tokens(Tokentype_MINUS, pos_start=self.pos))
                self.next()

            elif self.current_char == '*':
                tokens.append(Tokens(Tokentype_MUL, pos_start=self.pos))
                self.next()

            elif self.current_char == '/':
                tokens.append(Tokens(Tokentype_DIV, pos_start=self.pos))
                self.next()

            elif self.current_char == '(':
                tokens.append(Tokens(Tokentype_LPAREN, pos_start=self.pos))
                self.next()

            elif self.current_char == ')':
                tokens.append(Tokens(Tokentype_RPAREN, pos_start=self.pos))
                self.next()

            elif self.current_char == '[':
                tokens.append(Tokens(Tokentype_LSQUARE))
                self.next()

            elif self.current_char == ']':
                tokens.append(Tokens(Tokentype_RSQUARE))
                self.next()

            elif self.current_char == '{':
                tokens.append(Tokens(Tokentype_LCURLY))
                self.next()

            elif self.current_char == '}':
                tokens.append(Tokens(Tokentype_RCURLY))
                self.next()

            elif self.current_char == '^':
                tokens.append(Tokens(Tokentype_BITWISE_XOR))
                self.next()

            elif self.current_char == '!':
                token, error = self.generating_not_equal()
                if error:
                    return [], error
                tokens.append(token)

            elif self.current_char == '=':
                tokens.append(self.generating_equals())

            elif self.current_char == '<':
                tokens.append(self.generating_less_than())

            elif self.current_char == '>':
                tokens.append(self.generating_greater_than())

            elif self.current_char == ',':
                tokens.append(Tokens(Tokentype_COMMA, self.current_char))
                self.next()

            elif self.current_char == ':':
                tokens.append(Tokens(Tokentype_COLON, self.current_char))
                self.next()

            elif self.current_char == ';':
                tokens.append(Tokens(Tokentype_SEMICOLON, self.current_char))
                self.next()

            elif self.current_char == '&':
                tokens.append(self.log_bit_and())

            elif self.current_char == '|':
                tokens.append(self.log_bit_or())

            elif self.current_char == '%':
                tokens.append(Tokens(Tokentype_MODULO))
                self.next()

            else:
                pos_first = self.pos.copy()
                char = self.current_char
                self.next()
                return [], IllegalCharError(pos_first, self.pos, "'" + char + "'")

        tokens.append(Tokens(Tokentype_EOL, pos_start=self.pos))
        return tokens, None

    def comments(self):
        while self.current_char is not None and self.current_char != '\n':
            self.next()

    def generating_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()

        while self.current_char is not None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dot_count == 1:
                    break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.next()

        if dot_count == 0:
            return Tokens(Tokentype_INTEGER, int(num_str), pos_start, self.pos)
        else:
            return Tokens(Tokentype_FLOAT, float(num_str), pos_start, self.pos)  # Fix this line

    def generating_identifier(self):
        identifier_str = ''

        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            identifier_str += self.current_char
            self.next()

        if identifier_str.lower() == 'true':
            return Tokens(Tokentype_BOOLEAN, True)
        elif identifier_str.lower() == 'false':
            return Tokens(Tokentype_BOOLEAN, 'False')
        elif identifier_str in KEYWORDS:
            token_type = Tokentype_KEYWORD if identifier_str in KEYWORDS else Tokentype_IDENTIFIER
            return Tokens(token_type, identifier_str)
        else:
            return Tokens(Tokentype_IDENTIFIER, identifier_str)

    def generating_string(self):
        string = ''
        pos_start = self.pos.copy()
        escape_character = False
        self.next()

        escape_characters = {
            'n': '\n',
            't': '\t'
        }

        while self.current_char is not None and (self.current_char != '"' or escape_character):
            if escape_character:
                string += escape_characters.get(self.current_char, self.current_char)
            else:
                if self.current_char == '\\':
                    escape_character = True
                else:
                    string += self.current_char
            self.next()
            escape_character = False

        self.next()
        return Tokens(Tokentype_STRING, string, pos_start, self.pos)

    def generating_not_equal(self):
        start_pos = self.pos.copy()

        self.next()

        if self.current_char == '=':
            self.next()
            return Tokens(Tokentype_NOTEQUALS), None
        else:
            return None, Error.ExpectedCharError(start_pos, self.pos, 'Expected "=" after "!"')

    def generating_equals(self):
        self.next()
        if self.current_char == '=':
            self.next()
            return Tokens(Tokentype_EE)
        else:
            return Tokens(Tokentype_EQUALS)

    def generating_less_than(self):
        self.next()
        if self.current_char == '<':
            self.next()
            return Tokens(Tokentype_LEFT_SHIFT)
        elif self.current_char == '=':
            self.next()
            return Tokens(Tokentype_LE)
        else:
            return Tokens(Tokentype_LESS)

    def generating_greater_than(self):
        self.next()
        if self.current_char == '>':
            self.next()
            return Tokens(Tokentype_RIGHT_SHIFT)
        elif self.current_char == '=':
            self.next()
            return Tokens(Tokentype_GE)
        else:
            return Tokens(Tokentype_GREATER)

    def log_bit_and(self):
        self.next()
        if self.current_char == '&':
            self.next()
            return Tokens(Tokentype_LOGICAL_AND)
        else:
            return Tokens(Tokentype_BITWISE_AND)

    def log_bit_or(self):
        self.next()
        if self.current_char == '|':
            self.next()
            return Tokens(Tokentype_LOGICAL_OR)
        else:
            return Tokens(Tokentype_BITWISE_OR)

    def generating_increment(self):
        self.next()
        if self.current_char == '+':
            self.next()
            return Tokens(Tokentype_INC)
        elif self.current_char == '=':
            self.next()
            return Tokens(Tokentype_ADD_ASSIGN)
        else:
            return Tokens(Tokentype_PLUS)

    def generating_decrement(self):
        self.next()
        if self.current_char == '-':
            self.next()
            return Tokens(Tokentype_DRC)
        elif self.current_char == '=':
            self.next()
            return Tokens(Tokentype_MIN_ASSIGN)
        else:
            return Tokens(Tokentype_MINUS)

    def generating_Exponent(self):
        self.next()
        if self.current_char == '*':
            self.next()
            return Tokens(Tokentype_EXPO)
        elif self.current_char == '=':
            self.next()
            return Tokens(Tokentype_MUL_ASSIGN)
        else:
            return Tokens(Tokentype_MUL)

    def generating_divide_assignment(self):
        self.next()
        if self.current_char == '=':
            self.next()
            return Tokens(Tokentype_DIV_ASSIGN)
        else:
            return Tokens(Tokentype_DIV)


class Tokens:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.next()

        if pos_end:
            self.pos_end = pos_end

    def __repr__(self):
        if self.value:
            return f'{self.type}:{self.value}'
        return f'{self.type}'


class Position:
    def __init__(self, index, line_number, column_number, file_name, file_text):
        self.index = index
        self.line_number = line_number
        self.column_number = column_number
        self.file_name = file_name
        self.file_text = file_text

    def next(self, current_char=None):
        self.index += 1
        self.column_number += 1

        if current_char == '\n':
            self.line_number += 1
            self.column_number += 0

        return self

    def copy(self):
        return Position(self.index, self.line_number, self.column_number, self.file_name, self.file_text)


class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}\n'
        result += f'File {self.pos_start.file_name}, line {self.pos_start.line_number + 1}'
        result += ('\n\n' + poperror(self.pos_start.file_text, self.pos_start, self.pos_end))
        return result

    @classmethod
    def ExpectedCharError(cls, start_pos, pos, details):
        return cls(start_pos, pos, 'Expected Character', details)


class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Invalid Syntax', details)


class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)



class UnaryOp:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node
        self.pos_start = self.op_tok.pos_start
        self.pos_end = node.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'

class Node:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'

class PResult:
    def __init__(self):
        self.error = None
        self.node = None

    def store(self, res):
        if isinstance(res, PResult):
            if res.error:
                self.error = res.error
            return res.node

        return res

    def passed(self, node):
        self.node = node
        return self

    def fail(self, error):
        self.error = error
        return self
class BinOP:
    def __init__(self, left_node, op_tok, right_node):
        self.l_node = left_node
        self.op_tok = op_tok
        self.r_node = right_node
        self.pos_start = self.l_node.pos_start
        self.pos_end = self.r_node.pos_end

    def __repr__(self):
        return f'({self.l_node}, {self.op_tok}, {self.r_node})'



class Parser:
    def __init__(self, tokens):
        self.curr_tok = None
        self.tokens = tokens
        self.tk_ind = -1
        self.advance()

    def advance(self, ):
        self.tk_ind += 1
        if self.tk_ind < len(self.tokens):
            self.curr_tok = self.tokens[self.tk_ind]
        return self.curr_tok

    def parse(self):
        res = self.expr()
        if not res.error and self.curr_tok.type != Tokentype_EOL:
            return res.fail(InvalidSyntaxError(
                self.curr_tok.pos_start, self.curr_tok.pos_end,
                "Expected '+', '-', '*' or '/'"))
        return res

    def factor(self):
        res = PResult()
        tok = self.curr_tok

        if tok.type in (Tokentype_PLUS, Tokentype_MINUS):
            res.store(self.advance())
            factor = res.store(self.factor())
            if res.error:
                return res
            return res.passed(UnaryOp(tok, factor))

        elif tok.type in (Tokentype_INTEGER, Tokentype_FLOAT):
            res.store(self.advance())
            return res.passed(Node(tok))

        elif tok.type == Tokentype_LPAREN:
            res.store(self.advance())
            expr = res.store(self.expr())
            if res.error:
                return res
            if self.curr_tok.type == Tokentype_RPAREN:
                res.store(self.advance())
                return res.passed(expr)
            else:
                return res.fail(InvalidSyntaxError(
                    self.curr_tok.pos_start, self.curr_tok.pos_end, "Expected ')'"))

        return res.fail(InvalidSyntaxError(
            tok.pos_start, tok.pos_end, "Expected int or float"))

    def term(self):
        return self.bin_op(self.factor, (Tokentype_MUL, Tokentype_DIV))

    def expr(self):
        return self.bin_op(self.term, (Tokentype_PLUS, Tokentype_MINUS))

    def bin_op(self, fnc, op):
        res = PResult()
        left = res.store(fnc())
        if res.error:
            return res

        while self.curr_tok.type in op:
            op_tok = self.curr_tok
            res.store(self.advance())
            right = res.store(fnc())
            if res.error:
                return res
            left = BinOP(left, op_tok, right)

        return res.passed(left)

class RTError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(pos_start, pos_end, 'Runtime Error', details)
        self.context = context

    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.error_name}: {self.details}'
        result += '\n\n' + poperror(self.pos_start.file_text, self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        con = self.context

        while con:
            result = f'  File {pos.file_name}, line {str(pos.line_number + 1)}, in {con.display_name}\n' + result
            pos = con.entry_pos
            con = con.parent

        return 'Traceback (most recent call last):\n' + result
class Num:
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def add_to(self, other):
        if isinstance(other, Num):
            return Num(self.value + other.value).set_context(self.context), None

    def sub_by(self, other):
        if isinstance(other, Num):
            return Num(self.value - other.value).set_context(self.context), None

    def mul_by(self, other):
        if isinstance(other, Num):
            return Num(self.value * other.value).set_context(self.context), None

    def div_by(self, other):
        if isinstance(other, Num):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Division by zero',
                    self.context
                )

            return Num(self.value / other.value).set_context(self.context), None

    def __repr__(self):
        return str(self.value)

class Runtime:
    def __init__(self):
        self.value = None
        self.error = None

    def register(self, res):
        if res.error:
            self.error = res.error
        return res.value

    def success(self, value):
        self.value = value
        return self

    def failure(self, error):
        self.error = error
        return self

class Context:
    def __init__(self, display_name, parent=None, entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.entry_pos = entry_pos

    def visit_UnaryOp(self, node, context):
        res = Runtime()
        number = res.register(self.visit(node.node, context))
        if res.error:
            return res

        error = None

        if node.op_tok.type == Tokentype_MINUS:
            number, error = number.mul_by(Num(-1))

        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))


def run(file_name, text):
    # Generate tokens
    lexer = Lexical_Analyser(file_name, text)
    tokens, error = lexer.generating_token
    if error:
        return None, error

    # Generate AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error:
        return None, ast.error

    interpreter = Interpreter()
    context = Context('<program>')
    result = interpreter.visit(ast.node, context)

    return result.value, result.error

class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit)
        return method(node, context)

    def no_visit(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    def visit_Node(self, node, context):
        return Runtime().success(Num(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_BinOP(self, node, context):
        res = Runtime()
        left = res.register(self.visit(node.l_node, context))
        if res.error:
            return res
        right = res.register(self.visit(node.r_node, context))
        if res.error:
            return res

        if node.op_tok.type == Tokentype_PLUS:
            result, error = left.add_to(right)
        elif node.op_tok.type == Tokentype_MINUS:
            result, error = left.sub_by(right)
        elif node.op_tok.type == Tokentype_MUL:
            result, error = left.mul_by(right)
        elif node.op_tok.type == Tokentype_DIV:
            result, error = left.div_by(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

# TOKENS

# Operators
Tokentype_INTEGER = 'INTEGER'
Tokentype_FLOAT = 'FLOAT'
Tokentype_STRING = 'STRING'
Tokentype_PLUS = 'ADDITION'
Tokentype_MINUS = 'SUBTRACTION'
Tokentype_MUL = 'MULTIPLY'
Tokentype_DIV = 'DIVIDE'
Tokentype_EXPO = 'EXPONENT'
Tokentype_IDENTIFIER = 'IDENTIFIER'
Tokentype_KEYWORD = 'KEYWORD'
Tokentype_EQUALS = 'ASSIGNMENT'
Tokentype_NOTEQUALS = 'NOT-EQUALS'
Tokentype_GREATER = 'GREATER'
Tokentype_GE = 'GREATER_THAN_EQUAL'
Tokentype_LESS = 'LESS'
Tokentype_LE = 'LESS_THAN_EQUAL'
Tokentype_POW = 'POWER'

# Scope and Blocks
Tokentype_LPAREN = 'LPAREN'
Tokentype_RPAREN = 'RPAREN'
Tokentype_LSQUARE = 'LSQUARE'
Tokentype_RSQUARE = 'RSQUARE'
Tokentype_LCURLY = 'LCURLY'
Tokentype_RCURLY = 'RCURLY'

Tokentype_NEWLINE = 'NEWLINE'

Tokentype_SEMICOLON = 'SEMICOLON'
Tokentype_COMMA = 'COMMA'
Tokentype_COLON = 'COLON'

Tokentype_EOL = 'END_OF_LINE'

# Bitwise Operators
Tokentype_BITWISE_AND = 'BITWISE_AND'
Tokentype_BITWISE_OR = 'BITWISE_OR'
Tokentype_BITWISE_XOR = 'BITWISE_XOR'
Tokentype_MODULO = 'MODULO'
Tokentype_BOOLEAN = 'BOOLEAN'

# Increment and Decrement
Tokentype_INC = 'INCREMENT'
Tokentype_DRC = 'DECREMENT'
Tokentype_EE = 'EQUALS'

# Assignment Operators
Tokentype_ADD_ASSIGN = 'ADD_ASSIGNMENT'
Tokentype_MIN_ASSIGN = 'MINUS_ASSIGNMENT'
Tokentype_MUL_ASSIGN = 'MULTIPLY_ASSIGNMENT'
Tokentype_DIV_ASSIGN = 'DIVIDE_ASSIGNMENT'

# Bitwise Shift Assignment
Tokentype_LEFT_SHIFT = 'LEFT_SHIFT'
Tokentype_RIGHT_SHIFT = 'RIGHT_SHIFT'

# Logical Assignment
Tokentype_LOGICAL_AND = 'LOGICAL_AND'
Tokentype_LOGICAL_OR = 'LOGICAL_OR'

# CONSTANTS
DIGITS = '0123456789'
LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

# KEYWORDS
KEYWORDS = ['VAR', 'AND', 'OR', 'NOT', 'IF', 'ELSE', 'ELIF', 'WHILE', 'FOR', 'FUNCTION', 'RETURN', 'DEF', 'PRINT',
            'var', 'and', 'or', 'not', 'if', 'else', 'elif', 'while', 'for', 'function', 'return', 'def', 'print']
