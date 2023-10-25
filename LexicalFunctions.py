# TOKENS
ELFIN_INT = 'INT'
ELFIN_FLOAT = 'FLOAT'
ELFIN_STRING = 'STRING'
ELFIN_PLUS = 'PLUS'
ELFIN_MINUS = 'MINUS'
ELFIN_MUL = 'MULTIPLY'
ELFIN_DIV = 'DIVIDE'
ELFIN_EXPO = 'EXPONENT'
ELFIN_IDENTIFIER = 'IDENTIFIER'
ELFIN_KEYWORD = 'KEYWORD'
ELFIN_EQUALS = 'ASSIGMENT'
ELFIN_NOTEQUALS = 'NOTEQUALS'
ELFIN_GREATER = 'GREATER'
ELFIN_GE = 'GREATER_THAN_EQUAL'
ELFIN_LESS = 'LESS'
ELFIN_LE = 'LESS_THAN_EQUAL'

# Scope and Blocks
ELFIN_LPAREN = 'LPAREN'
ELFIN_RPAREN = 'RPAREN'
ELFIN_LSQUARE = 'LSQUARE'
ELFIN_RSQUARE = 'RSQUARE'
ELFIN_LCURLY = 'LCURLY'
ELFIN_RCURLY = 'RCURLY'

ELFIN_NEWLINE = 'NEWLINE'

ELFIN_SEMICOLON = 'SEMICOLON'
ELFIN_COMMA = 'COMMA'
ELFIN_COLON = 'COLON'

ELFIN_EOF = 'END_OF_FILE'

# Bitwise Operators
ELFIN_BITWISE_AND = 'BITWISE_AND'
ELFIN_BITWISE_OR = 'BITWISE_OR'
ELFIN_BITWISE_XOR = 'BITWISE_XOR'
ELFIN_MODULO = 'MODULO'
ELFIN_BOOLEAN = 'BOOLEAN'

# Increment and Decrement
ELFIN_INC = 'INCREMENT'
ELFIN_DRC = 'DECREMENT'
ELFIN_EE = 'EQUALS'

# Assignment Operators
ELFIN_ADD_ASSIGN = 'ADD_ASSIGNMENT'
ELFIN_MIN_ASSIGN = 'MINUS_ASSIGNMENT'
ELFIN_MUL_ASSIGN = 'MULTIPLY_ASSIGNMENT'
ELFIN_DIV_ASSIGN = 'DIVIDE_ASSIGNMENT'

# Bitwise Shift Assignment
ELFIN_LEFT_SHIFT = 'LEFT_SHIFT'
ELFIN_RIGHT_SHIFT = 'RIGHT_SHIFT'

# Logical Assignment
ELFIN_LOGICAL_AND = 'LOGICAL_AND'
ELFIN_LOGICAL_OR = 'LOGICAL_OR'

# CONSTANTS
ELFIN_DIGITS = '0123456789'
ELFIN_LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

# KEYWORDS
ELFIN_KEYWORDS = ['VAR', 'AND', 'OR', 'NOT', 'IF', 'ELSE', 'ELIF', 'WHILE', 'FOR', 'FUNCTION', 'RETURN', 'TRUE', 'FALSE', 'DEF']





# ERRORS
class Error:
        def __init__(self, location_first, location_end, error_name, details):
                self.location_first = location_first
                self.location_end = location_end
                self.error_name = error_name
                self.details = details


        def stringTypeError(self):
                result = f'{self.error_name}: {self.details}'
                result += f'File {self.location_first.file_name}, line {self.location_first.line_number + 1}'
                return result

        @classmethod
        def ExpectedCharError(cls, start_location, location, details):
                return cls(start_location, location, 'Expected Character', details)


class IllegalCharError(Error):
        def __init__(self, location_first, location_end, details):
                super().__init__(location_first, location_end, 'Illegal Character', details)





class ELFIN_LEX:
        def __init__(self, file_name, text):
                self.file_name = file_name
                self.text = text
                self.location = Position(-1, 0, -1, file_name, text)
                self.current_char = None
                self.after()

        def after(self):
                self.location.after(self.current_char)
                self.current_char = self.text[self.location.index] if self.location.index < len(self.text) else None

        @property
        def Token_Maker(self):
                lexemes = []

                while self.current_char is not None:
                        # Ignoring the characters such as spaces and tabs
                        if self.current_char in ' \t':
                                self.after()
                        # Ignoring the characters which starts with #
                        elif self.current_char == '#':
                                self.Comment()
                        # Creating Function for Digits
                        elif self.current_char in ELFIN_DIGITS:
                                lexemes.append(self.digit_generator())
                        # Creating Function for Letters
                        elif self.current_char in ELFIN_LETTERS:
                                lexemes.append(self.identifier_generator())
                        # Creating Function for String "which are written in double inverted comma's"
                        elif self.current_char == '"':
                                lexemes.append(self.string_generator())
                        # Creating Function for String 'which are written in inverted comma's'
                        elif self.current_char == "'":
                                lexemes.append(self.string_generator())
                        # Creating Function for Nextline character
                        elif self.current_char == '\n':
                                lexemes.append(Tokens(ELFIN_NEWLINE, location_start=self.location))

                        elif self.current_char == '+':
                                lexemes.append(self.make_increment())
                                self.after()

                        elif self.current_char == '-':
                                lexemes.append(Tokens(self.make_decrement()))
                                self.after()

                        elif self.current_char == '*':
                                lexemes.append(Tokens(self.make_power()))
                                self.after()

                        elif self.current_char == '/':
                                lexemes.append(Tokens(self.make_divide_assignment()))
                                self.after()

                        elif self.current_char == '(':
                                lexemes.append(Tokens(ELFIN_LPAREN))
                                self.after()

                        elif self.current_char == ')':
                                lexemes.append(Tokens(ELFIN_RPAREN))
                                self.after()

                        elif self.current_char == '[':
                                lexemes.append(Tokens(ELFIN_LSQUARE))
                                self.after()

                        elif self.current_char == ']':
                                lexemes.append(Tokens(ELFIN_RSQUARE))
                                self.after()

                        elif self.current_char == '{':
                                lexemes.append(Tokens(ELFIN_LCURLY))
                                self.after()

                        elif self.current_char == '}':
                                lexemes.append(Tokens(ELFIN_RCURLY))

                        elif self.current_char == '^':
                                lexemes.append(Tokens(ELFIN_BITWISE_XOR))
                                self.after()

                        elif self.current_char == '!':
                                token, error = self.make_not_equals()
                                if error: return [], error
                                lexemes.append(token)

                        elif self.current_char == '=':
                                lexemes.append(self.make_equals())

                        elif self.current_char == '<':
                                lexemes.append(self.make_less_than())

                        elif self.current_char == '>':
                                lexemes.append(self.make_greater_than())

                        elif self.current_char == ',':
                                lexemes.append(Tokens(ELFIN_COMMA, self.current_char))
                                self.after()

                        elif self.current_char == ':':
                                lexemes.append(Tokens(ELFIN_COLON, self.current_char))
                                self.after()

                        elif self.current_char == ';':
                                lexemes.append(Tokens(ELFIN_SEMICOLON, self.current_char))
                                self.after()

                        elif self.current_char == '&':
                                lexemes.append(self.make_and())

                        elif self.current_char == '|':
                                lexemes.append(self.make_or())

                        elif self.current_char == '%':
                                lexemes.append(Tokens(ELFIN_MODULO))
                                self.after()

                        else:
                                location_first = self.location.copy()
                                char = self.current_char
                                self.after()
                                return [], IllegalCharError(location_first, self.location, "'"+ char + "'")

                lexemes.append(Tokens(ELFIN_EOF))
                return lexemes, None

        def Comment(self):
                while self.current_char is not None and self.current_char != '\n':
                        self.after()

        def digit_generator(self):
                num_str = ''
                dot_count = 0

                while self.current_char is not None and self.current_char in ELFIN_DIGITS + '.':
                        if self.current_char == '.':
                                if dot_count == 1:
                                        break
                                dot_count += 1
                                num_str += '.'
                        else:
                                num_str += self.current_char
                        self.after()

                if dot_count == 0:
                        return Tokens(Tokens(ELFIN_INT), int(num_str))
                else:
                        return Tokens(Tokens(ELFIN_FLOAT), float(num_str))

        def identifier_generator(self):
                identifier_str = ''

                while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
                        identifier_str += self.current_char
                        self.after()

                if identifier_str.lower() == 'true':
                        return Tokens(ELFIN_BOOLEAN, True)
                elif identifier_str.lower() == 'false':
                        return Tokens(ELFIN_BOOLEAN, 'False')
                elif identifier_str in ELFIN_KEYWORDS:
                        token_type = ELFIN_KEYWORD if identifier_str in ELFIN_KEYWORDS else ELFIN_IDENTIFIER
                        return Tokens(token_type, identifier_str)
                else:
                        return Tokens(ELFIN_IDENTIFIER, identifier_str)

        def string_generator(self):
                string = ''
                location_start = self.location.copy()
                escape_character = False
                self.after()

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
                        self.after()
                        escape_character = False

                self.after()
                return Tokens(ELFIN_STRING, string, location_start, self.location)

        def make_not_equals(self):
                start_location = self.location.copy()

                self.after()

                if self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_NOTEQUALS), None
                else:
                        return None, Error.ExpectedCharError(start_location, self.location, 'Expected "=" after "!"')


        def make_equals(self):
                self.after()
                if self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_EE)
                else:
                        return Tokens(ELFIN_EQUALS)

        def make_less_than(self):
                self.after()
                if self.current_char == '<':
                        self.after()
                        return Tokens(ELFIN_LEFT_SHIFT)
                elif self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_LE)
                else:
                        return Tokens(ELFIN_LESS)

        def make_greater_than(self):
                self.after()
                if self.current_char == '>':
                        self.after()
                        return Tokens(ELFIN_RIGHT_SHIFT)
                elif self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_GE)
                else:
                        return Tokens(ELFIN_GREATER)

        def make_and(self):
                self.after()
                if self.current_char == '&':
                        self.after()
                        return Tokens(ELFIN_LOGICAL_AND)
                else:
                        return Tokens(ELFIN_BITWISE_AND)

        def make_or(self):
                self.after()
                if self.current_char == '|':
                        self.after()
                        return Tokens(ELFIN_LOGICAL_OR)
                else:
                        return Tokens(ELFIN_BITWISE_OR)

        def make_increment(self):
                self.after()
                if self.current_char == '+':
                        self.after()
                        return Tokens(ELFIN_INC)
                elif self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_ADD_ASSIGN)
                else:
                        return Tokens(ELFIN_PLUS)

        def make_decrement(self):
                self.after()
                if self.current_char == '-':
                        self.after()
                        return Tokens(ELFIN_DRC)
                elif self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_MIN_ASSIGN)
                else:
                        return Tokens(ELFIN_MINUS)

        def make_power(self):
                self.after()
                if self.current_char == '*':
                        self.after()
                        return Tokens(ELFIN_EXPO)
                elif self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_MUL_ASSIGN)
                else:
                        return Tokens(ELFIN_MUL)

        def make_divide_assignment(self):
                self.after()
                if self.current_char == '=':
                        self.after()
                        return Tokens(ELFIN_DIV_ASSIGN)
                else:
                        return Tokens(ELFIN_DIV)




# POSITION
class Position:
        def __init__(self, index, line_number, column_number, file_name, file_text):
                self.index = index
                self.line_number = line_number
                self.column_number = column_number
                self.file_name = file_name
                self.file_text = file_text

        def after(self, current_char):
                self.index += 1
                self.column_number += 1

                if current_char == '\n':
                        self.line_number += 1
                        self.column_number += 0

                return self

        def copy(self):
                return Position(self.index, self.line_number, self.column_number, self.file_name, self.file_text)



class Tokens:

    def __init__(self, type_, value=None, location_start=None, location_end=None):
        self.type = type_
        self.value = value
        self.location_start = location_start
        self.location_end = location_end

    def __repr__(self):
            if self.value:
                    return f'{self.type}: {self.value}'
            return f'{self.type}'


def run(file_name, text):
        lexer = ELFIN_LEX(file_name, text)
        lexemes, error = lexer.Token_Maker

        return lexemes, error