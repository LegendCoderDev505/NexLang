import sys
import os
from enum import Enum, auto

class TokenType(Enum):
    # Basic tokens
    KEYWORD = auto()
    IDENTIFIER = auto()
    NUMBER = auto()
    STRING = auto()
    OPERATOR = auto()
    BRACKET = auto()
    EOF = auto()
    NEWLINE = auto()
    
    # Control flow
    DO = auto()
    END = auto()
    IF = auto()
    ELSEIF = auto()
    ELSE = auto()
    WHILE = auto()
    
    # Functions and classes
    FUNCTION = auto()
    CLASS = auto()
    BEGIN = auto()
    VOID = auto()
    MANUAL = auto()
    STATIC = auto()
    STACK = auto()
    OBJECT = auto()
    LIST = auto()
    NULL = auto()
    
    # I/O
    PRINT = auto()
    INPUT = auto()
    OUTPUT = auto()
    
    # Imports
    IMPORT = auto()
    FROM = auto()
    AS = auto()
    EXPORT = auto()

class Grammar:
    def __init__(self):
        self.keywords = {}
        self.operators = {}
        self.brackets = {}
        
        # Control flow
        self.add_keyword('if', {'is_control': True})
        self.add_keyword('elseif', {'is_control': True})
        self.add_keyword('else', {'is_control': True})
        self.add_keyword('do', {'is_block': True})
        self.add_keyword('end', {'is_block': True})
        self.add_keyword('while', {'is_loop': True})
        
        # Functions and classes
        self.add_keyword('function')
        self.add_keyword('class')
        self.add_keyword('begin')
        self.add_keyword('void')
        self.add_keyword('manual')
        self.add_keyword('static')
        self.add_keyword('stack')
        self.add_keyword('object')
        self.add_keyword('list')
        self.add_keyword('null')
        
        # I/O
        self.add_keyword('print', {'args': '*'})
        self.add_keyword('input', {'args': 1})
        self.add_keyword('output', {'args': 1})
        
        # Imports
        self.add_keyword('import')
        self.add_keyword('from')
        self.add_keyword('as')
        self.add_keyword('export')
        
        # Operators
        self.add_operator('+', 1)
        self.add_operator('-', 1)
        self.add_operator('*', 2)
        self.add_operator('/', 2)
        self.add_operator('>', 0)
        self.add_operator('<', 0)
        self.add_operator('==', 0)
        self.add_operator('!=', 0)
        self.add_operator('=', 0)
        
        # Brackets
        self.add_brackets('paren', '(', ')')
        self.add_brackets('brace', '{', '}')
        self.add_brackets('bracket', '[', ']')

    def add_keyword(self, name, properties=None):
        if properties is None:
            properties = {}
        self.keywords[name] = properties
        return self

    def add_operator(self, symbol, precedence, associativity='left'):
        self.operators[symbol] = {
            'precedence': precedence,
            'associativity': associativity
        }
        return self
        
    def add_brackets(self, name, open_char, close_char):
        self.brackets[name] = (open_char, close_char)
        return self

class Lexer:
    def __init__(self, grammar):
        self.grammar = grammar
        self.tokens = []
        self.current_pos = 0
        self.current_char = None
        self.source = ""
        self.line = 1
        self.column = 1
        
    def tokenize(self, source):
        self.source = source
        self.current_pos = 0
        self.current_char = self.source[0] if self.source else None
        self.tokens = []
        self.line = 1
        self.column = 1
        
        while self.current_char is not None:
            if self.current_char.isspace():
                if self.current_char == '\n':
                    self.tokens.append((TokenType.NEWLINE, '\n'))
                    self.line += 1
                    self.column = 1
                self.advance()
                continue
                
            if self.current_char == '@':
                self.tokens.append(self.read_variable())
                continue
                
            if self.current_char == '"':
                self.tokens.append(self.read_string())
                continue
                
            if self.current_char.isdigit() or (self.current_char == '.' and 
                                             self.current_pos + 1 < len(self.source) and 
                                             self.source[self.current_pos + 1].isdigit()):
                self.tokens.append(self.read_number())
                continue
                
            if self.current_char in self.grammar.operators:
                self.tokens.append((TokenType.OPERATOR, self.current_char))
                self.advance()
                continue
                
            # Handle brackets
            for bracket_name, (open_char, close_char) in self.grammar.brackets.items():
                if self.current_char == open_char:
                    self.tokens.append((TokenType.BRACKET, open_char, 'open'))
                    self.advance()
                    break
                if self.current_char == close_char:
                    self.tokens.append((TokenType.BRACKET, close_char, 'close'))
                    self.advance()
                    break
            else:  # Not a bracket
                if self.current_char.isalpha() or self.current_char == '_':
                    identifier = self.read_identifier()
                    if identifier in self.grammar.keywords:
                        # Map to specific token types
                        token_type = {
                            'if': TokenType.IF,
                            'elseif': TokenType.ELSEIF,
                            'else': TokenType.ELSE,
                            'do': TokenType.DO,
                            'end': TokenType.END,
                            'while': TokenType.WHILE,
                            'function': TokenType.FUNCTION,
                            'class': TokenType.CLASS,
                            'begin': TokenType.BEGIN,
                            'void': TokenType.VOID,
                            'manual': TokenType.MANUAL,
                            'static': TokenType.STATIC,
                            'stack': TokenType.STACK,
                            'object': TokenType.OBJECT,
                            'list': TokenType.LIST,
                            'null': TokenType.NULL,
                            'print': TokenType.PRINT,
                            'input': TokenType.INPUT,
                            'output': TokenType.OUTPUT,
                            'import': TokenType.IMPORT,
                            'from': TokenType.FROM,
                            'as': TokenType.AS,
                            'export': TokenType.EXPORT
                        }.get(identifier, TokenType.KEYWORD)
                        self.tokens.append((token_type, identifier))
                    else:
                        self.tokens.append((TokenType.IDENTIFIER, identifier))
                    continue
                    
                self.advance()  # Skip unknown characters
                
        self.tokens.append((TokenType.EOF, None))
        return self.tokens

    def read_number(self):
        result = ""
        has_decimal = False
        
        while (self.current_char is not None and 
               (self.current_char.isdigit() or 
                (self.current_char == '.' and not has_decimal))):
            if self.current_char == '.':
                has_decimal = True
            result += self.current_char
            self.advance()
            
        return (TokenType.NUMBER, float(result) if has_decimal else int(result))
        
    def read_string(self):
        self.advance()  # Skip opening quote
        result = ""
        while self.current_char != '"' and self.current_char is not None:
            if self.current_char == '\\':
                self.advance()
                if self.current_char == 'n':
                    result += '\n'
                elif self.current_char == 't':
                    result += '\t'
                elif self.current_char == '"':
                    result += '"'
                elif self.current_char == '\\':
                    result += '\\'
                else:
                    result += '\\' + self.current_char
                self.advance()
            else:
                result += self.current_char
                self.advance()
        if self.current_char == '"':
            self.advance()
        return (TokenType.STRING, result)
        
    def read_identifier(self):
        result = ""
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        return result
    
    def read_variable(self):
        var_name = self.current_char
        self.advance()
        
        while (self.current_char is not None and 
               (self.current_char.isalnum() or self.current_char == '_')):
            var_name += self.current_char
            self.advance()
            
        return (TokenType.IDENTIFIER, var_name)
        
    def advance(self):
        self.current_pos += 1
        self.column += 1
        if self.current_pos >= len(self.source):
            self.current_char = None
        else:
            self.current_char = self.source[self.current_pos]

class Parser:
    def __init__(self, grammar):
        self.grammar = grammar
        self.tokens = []
        self.current_token = None
        self.token_index = -1
        
    def parse(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.advance()
        return self.parse_program()
        
    def parse_program(self):
        statements = []
        while self.current_token[0] != TokenType.EOF:
            statements.append(self.parse_statement())
        return {'type': 'program', 'body': statements}
        
    def parse_statement(self):
        if self.current_token[0] == TokenType.IMPORT:
            return self.parse_import()
        elif self.current_token[0] == TokenType.IF:
            return self.parse_if_statement()
        elif self.current_token[0] == TokenType.WHILE:
            return self.parse_while_loop()
        elif self.current_token[0] == TokenType.FUNCTION:
            return self.parse_function()
        elif self.current_token[0] == TokenType.CLASS:
            return self.parse_class()
        elif self.current_token[0] == TokenType.PRINT:
            return self.parse_print()
        elif self.current_token[0] == TokenType.INPUT:
            return self.parse_input()
        elif self.current_token[0] == TokenType.OUTPUT:
            return self.parse_output()
        elif (self.current_token[0] == TokenType.IDENTIFIER and 
              self.current_token[1].startswith('@')):
            return self.parse_assignment()
        return self.parse_expression()
    
    def parse_import(self):
        self.advance()  # Skip 'import'
        
        if self.current_token[0] == TokenType.IDENTIFIER:
            module = self.current_token[1]
            self.advance()
            
            if self.current_token[0] == TokenType.AS:
                self.advance()
                if self.current_token[0] != TokenType.IDENTIFIER:
                    raise SyntaxError("Expected alias name after 'as'")
                alias = self.current_token[1]
                self.advance()
                return {'type': 'import', 'module': module, 'alias': alias}
            
            elif self.current_token[0] == TokenType.FROM:
                self.advance()
                if self.current_token[0] != TokenType.IDENTIFIER:
                    raise SyntaxError("Expected source after 'from'")
                source = self.current_token[1]
                self.advance()
                return {'type': 'import_from', 'module': module, 'source': source}
            
            return {'type': 'import', 'module': module}
        
        elif self.current_token[0] == TokenType.FROM:
            self.advance()
            if self.current_token[0] != TokenType.IDENTIFIER:
                raise SyntaxError("Expected package name after 'from'")
            package = self.current_token[1]
            self.advance()
            
            if self.current_token[0] != TokenType.IMPORT:
                raise SyntaxError("Expected 'import' after package name")
            self.advance()
            
            if self.current_token[0] != TokenType.IDENTIFIER:
                raise SyntaxError("Expected module name after 'import'")
            module = self.current_token[1]
            self.advance()
            
            if self.current_token[0] == TokenType.EXPORT:
                self.advance()
                exports = []
                while (self.current_token[0] != TokenType.NEWLINE and 
                       self.current_token[0] != TokenType.EOF):
                    if self.current_token[0] != TokenType.IDENTIFIER:
                        raise SyntaxError("Expected export definition")
                    exports.append(self.current_token[1])
                    self.advance()
                    if self.current_token[0] == TokenType.OPERATOR and self.current_token[1] == ',':
                        self.advance()
                
                return {
                    'type': 'import_from_export',
                    'package': package,
                    'module': module,
                    'exports': exports
                }
            
            return {
                'type': 'import_from',
                'package': package,
                'module': module
            }
        
        raise SyntaxError("Invalid import statement")
    
    def parse_if_statement(self):
        self.advance()  # Skip 'if'
        condition = self.parse_expression()
        
        if self.current_token[0] != TokenType.DO:
            raise SyntaxError("Expected 'do' after if condition")
        self.advance()
        
        if_body = []
        while (self.current_token[0] not in (TokenType.ELSEIF, TokenType.ELSE, TokenType.END) and 
               self.current_token[0] != TokenType.EOF):
            if_body.append(self.parse_statement())
        
        elseif_clauses = []
        while self.current_token[0] == TokenType.ELSEIF:
            self.advance()
            elseif_cond = self.parse_expression()
            
            if self.current_token[0] != TokenType.DO:
                raise SyntaxError("Expected 'do' after elseif condition")
            self.advance()
            
            elseif_body = []
            while (self.current_token[0] not in (TokenType.ELSEIF, TokenType.ELSE, TokenType.END) and 
                  self.current_token[0] != TokenType.EOF):
                elseif_body.append(self.parse_statement())
            elseif_clauses.append({'condition': elseif_cond, 'body': elseif_body})
        
        else_body = []
        if self.current_token[0] == TokenType.ELSE:
            self.advance()
            if self.current_token[0] != TokenType.DO:
                raise SyntaxError("Expected 'do' after else")
            self.advance()
            
            while self.current_token[0] != TokenType.END and self.current_token[0] != TokenType.EOF:
                else_body.append(self.parse_statement())
        
        if self.current_token[0] != TokenType.END:
            raise SyntaxError("Expected 'end' to close if statement")
        self.advance()
        
        return {
            'type': 'if',
            'condition': condition,
            'if_body': if_body,
            'elseif_clauses': elseif_clauses,
            'else_body': else_body
        }
    
    def parse_while_loop(self):
        self.advance()  # Skip 'while'
        condition = self.parse_expression()
        
        if self.current_token[0] != TokenType.DO:
            raise SyntaxError("Expected 'do' after while condition")
        self.advance()
        
        body = []
        while self.current_token[0] != TokenType.END and self.current_token[0] != TokenType.EOF:
            body.append(self.parse_statement())
        
        if self.current_token[0] != TokenType.END:
            raise SyntaxError("Expected 'end' to close while loop")
        self.advance()
        
        return {'type': 'while', 'condition': condition, 'body': body}
    
    def parse_function(self):
        self.advance()  # Skip 'function'
        
        # Parse modifiers
        modifiers = []
        while self.current_token[0] in (TokenType.VOID, TokenType.MANUAL):
            modifiers.append(self.current_token[1])
            self.advance()
        
        # Parse storage
        storage = None
        if self.current_token[0] in (TokenType.STATIC, TokenType.STACK, 
                                    TokenType.CLASS, TokenType.OBJECT):
            storage = self.current_token[1]
            self.advance()
        
        # Parse function name
        if self.current_token[0] != TokenType.IDENTIFIER:
            raise SyntaxError("Expected function name")
        name = self.current_token[1]
        self.advance()
        
        # Parse parameters
        if self.current_token[0] != TokenType.BRACKET or self.current_token[1] != '(':
            raise SyntaxError("Expected '(' after function name")
        self.advance()
        
        params = []
        while self.current_token[0] != TokenType.BRACKET or self.current_token[1] != ')':
            # Parse type
            if self.current_token[0] != TokenType.IDENTIFIER:
                raise SyntaxError("Expected parameter type")
            param_type = self.current_token[1]
            self.advance()
            
            # Parse array notation
            is_array = False
            if (self.current_token[0] == TokenType.BRACKET and 
                self.current_token[1] == '['):
                self.advance()
                if (self.current_token[0] != TokenType.BRACKET or 
                    self.current_token[1] != ']'):
                    raise SyntaxError("Expected ']' after array type")
                self.advance()
                is_array = True
            
            # Parse parameter name
            if self.current_token[0] != TokenType.IDENTIFIER:
                raise SyntaxError("Expected parameter name")
            param_name = self.current_token[1]
            self.advance()
            
            params.append({
                'type': param_type,
                'is_array': is_array,
                'name': param_name
            })
            
            if (self.current_token[0] == TokenType.OPERATOR and 
                self.current_token[1] == ','):
                self.advance()
        
        if self.current_token[0] != TokenType.BRACKET or self.current_token[1] != ')':
            raise SyntaxError("Expected ')' after parameters")
        self.advance()
        
        # Parse body
        if self.current_token[0] != TokenType.BRACKET or self.current_token[1] != '{':
            raise SyntaxError("Expected '{' before function body")
        self.advance()
        
        body = []
        while self.current_token[0] != TokenType.BRACKET or self.current_token[1] != '}':
            body.append(self.parse_statement())
        
        if self.current_token[0] != TokenType.BRACKET or self.current_token[1] != '}':
            raise SyntaxError("Expected '}' after function body")
        self.advance()
        
        return {
            'type': 'function',
            'modifiers': modifiers,
            'storage': storage,
            'name': name,
            'params': params,
            'body': body
        }
    
    def parse_class(self):
        self.advance()  # Skip 'class'
        
        # Parse modifiers
        modifiers = []
        while self.current_token[0] in (TokenType.OBJECT, TokenType.LIST, TokenType.NULL):
            modifiers.append(self.current_token[1])
            self.advance()
        
        # Parse class name
        if self.current_token[0] != TokenType.IDENTIFIER:
            raise SyntaxError("Expected class name")
        name = self.current_token[1]
        self.advance()
        
        if self.current_token[0] != TokenType.BEGIN:
            raise SyntaxError("Expected 'begin' after class name")
        self.advance()
        
        # Parse class body
        members = []
        while self.current_token[0] != TokenType.END:
            # Handle @definition
            if (self.current_token[0] == TokenType.IDENTIFIER and 
                self.current_token[1] == '@definition'):
                self.advance()
                if self.current_token[0] != TokenType.VOID:
                    raise SyntaxError("Expected 'void' after @definition")
                self.advance()
                if self.current_token[0] != TokenType.CLASS:
                    raise SyntaxError("Expected 'class' after @definition void")
                self.advance()
                members.append({'type': 'class_definition'})
                continue
            
            # Handle special functions
            if self.current_token[0] == TokenType.FUNCTION:
                func = self.parse_function()
                if func['name'] in ('__object_class__', '__object_init__', '__object_collect__'):
                    if len(func['params']) != 1 or func['params'][0]['name'] != 'self':
                        raise SyntaxError("Special methods must have exactly one 'self' parameter")
                members.append(func)
                continue
            
            # Handle regular members
            members.append(self.parse_statement())
        
        if self.current_token[0] != TokenType.END:
            raise SyntaxError("Expected 'end' to close class")
        self.advance()
        
        return {
            'type': 'class',
            'modifiers': modifiers,
            'name': name,
            'members': members
        }
    
    def parse_print(self):
        self.advance()  # Skip 'print'
        args = []
        while (self.current_token[0] != TokenType.NEWLINE and 
               self.current_token[0] != TokenType.EOF):
            args.append(self.parse_expression())
        return {'type': 'print', 'args': args}
    
    def parse_input(self):
        self.advance()  # Skip 'input'
        prompt = self.parse_expression() if self.current_token[0] != TokenType.NEWLINE else None
        return {'type': 'input', 'prompt': prompt}
    
    def parse_output(self):
        self.advance()  # Skip 'output'
        value = self.parse_expression()
        return {'type': 'output', 'value': value}
    
    def parse_assignment(self):
        var_name = self.current_token[1]
        self.advance()
        
        if (self.current_token[0] != TokenType.OPERATOR or 
            self.current_token[1] != '='):
            raise SyntaxError(f"Expected = after variable name, got {self.current_token}")
        self.advance()
        
        value = self.parse_expression()
        
        return {
            'type': 'assignment',
            'name': var_name,
            'value': value
        }
        
    def parse_expression(self):
        return self.parse_binary_op()
        
    def parse_binary_op(self, precedence=0):
        left = self.parse_primary()
        
        while (self.current_token[0] == TokenType.OPERATOR and 
               self.grammar.operators.get(self.current_token[1], {}).get('precedence', 0) >= precedence):
            op = self.current_token[1]
            op_info = self.grammar.operators[op]
            self.advance()
            right = self.parse_binary_op(op_info['precedence'] + 
                    (1 if op_info['associativity'] == 'right' else 0))
            left = {'type': 'binary_op', 'op': op, 'left': left, 'right': right}
            
        return left
        
    def parse_primary(self):
        token = self.current_token
        
        if token[0] == TokenType.NUMBER:
            self.advance()
            return {'type': 'number', 'value': token[1]}
        elif token[0] == TokenType.STRING:
            self.advance()
            return {'type': 'string', 'value': token[1]}
        elif token[0] == TokenType.IDENTIFIER:
            self.advance()
            return {'type': 'identifier', 'value': token[1]}
        elif token[0] == TokenType.BRACKET and token[2] == 'open':
            bracket_char = token[1]
            self.advance()
            expr = self.parse_expression()
            if (self.current_token[0] != TokenType.BRACKET or 
                self.current_token[1] != bracket_char or 
                self.current_token[2] != 'close'):
                raise SyntaxError(f"Expected closing {bracket_char}")
            self.advance()
            return expr
            
        self.advance()
        return {'type': 'unknown', 'value': token}
        
    def advance(self):
        self.token_index += 1
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]
        else:
            self.current_token = (TokenType.EOF, None)

class NLSInterpreter:
    def __init__(self, grammar, debug=False):
        self.grammar = grammar
        self.lexer = Lexer(grammar)
        self.parser = Parser(grammar)
        self.environment = {
            'true': True,
            'false': False,
            'null': None
        }
        self.debug = debug
        self.functions = {}
        self.classes = {}
        self.imported_modules = {}
        
        # Type handlers
        self.type_handlers = {
            'String': str,
            'Int': lambda x: int(float(x)),
            'Float': float,
            'Bool': lambda x: str(x).lower() in ('true', '1', 'yes'),
            'True': lambda x: True,
            'False': lambda x: False,
            'None': lambda x: None,
            'Null': lambda x: None
        }

    def run_file(self, file_path):
        if not os.path.exists(file_path):
            print(f"NLS Error: File '{file_path}' not found", file=sys.stderr)
            return
            
        with open(file_path, 'r') as file:
            code = file.read()
        
        return self.run(code)
        
    def run(self, code):
        tokens = self.lexer.tokenize(code)
        ast = self.parser.parse(tokens)
        return self.execute(ast)
        
    def execute(self, node):
        if isinstance(node, list):
            return [self.execute(item) for item in node]
            
        if not isinstance(node, dict):
            return node
            
        node_type = node.get('type')
        
        if node_type == 'program':
            results = []
            for stmt in node['body']:
                results.append(self.execute(stmt))
            return results
            
        elif node_type == 'import':
            return self._execute_import(node)
        elif node_type == 'import_from':
            return self._execute_import_from(node)
        elif node_type == 'import_from_export':
            return self._execute_import_from_export(node)
        elif node_type == 'if':
            return self._execute_if(node)
        elif node_type == 'while':
            return self._execute_while(node)
        elif node_type == 'function':
            return self._execute_function_definition(node)
        elif node_type == 'class':
            return self._execute_class_definition(node)
        elif node_type == 'print':
            return self._execute_print(node)
        elif node_type == 'input':
            return self._execute_input(node)
        elif node_type == 'output':
            return self._execute_output(node)
        elif node_type == 'assignment':
            return self._execute_assignment(node)
        elif node_type == 'binary_op':
            return self._execute_binary_op(node)
        elif node_type == 'number':
            return node['value']
        elif node_type == 'string':
            return node['value']
        elif node_type == 'identifier':
            return self._execute_identifier(node)
            
        return None

    def _execute_import(self, node):
        module_name = node['module']
        alias = node.get('alias')
        
        # In a real implementation, this would search the 'libs' folder
        try:
            with open(f"libs/{module_name}.nsl", "r") as f:
                module_code = f.read()
        except FileNotFoundError:
            raise ImportError(f"Module '{module_name}' not found in libs directory")
        
        # Create a new interpreter for the module
        module_interpreter = NLSInterpreter(self.grammar)
        module_interpreter.run(module_code)
        
        # Store the module's environment
        self.imported_modules[module_name] = module_interpreter.environment
        
        if alias:
            self.environment[alias] = module_interpreter.environment
        else:
            # Import all names into current scope
            self.environment.update(module_interpreter.environment)
        
        return module_interpreter.environment

    def _execute_import_from(self, node):
        if 'source' in node:
            # import module from source
            source = node['source']
            module = node['module']
            
            try:
                with open(f"libs/{source}.nsl", "r") as f:
                    source_code = f.read()
            except FileNotFoundError:
                raise ImportError(f"Source '{source}' not found in libs directory")
            
            source_interpreter = NLSInterpreter(self.grammar)
            source_interpreter.run(source_code)
            
            if module not in source_interpreter.environment:
                raise ImportError(f"Module '{module}' not found in source '{source}'")
            
            self.environment[module] = source_interpreter.environment[module]
            return source_interpreter.environment[module]
        
        else:
            # from package import module
            package = node['package']
            module = node['module']
            
            try:
                with open(f"libs/{package}.nsl", "r") as f:
                    package_code = f.read()
            except FileNotFoundError:
                raise ImportError(f"Package '{package}' not found in libs directory")
            
            package_interpreter = NLSInterpreter(self.grammar)
            package_interpreter.run(package_code)
            
            if module not in package_interpreter.environment:
                raise ImportError(f"Module '{module}' not found in package '{package}'")
            
            self.environment[module] = package_interpreter.environment[module]
            return package_interpreter.environment[module]

    def _execute_import_from_export(self, node):
        package = node['package']
        module = node['module']
        exports = node['exports']
        
        try:
            with open(f"libs/{package}.nsl", "r") as f:
                package_code = f.read()
        except FileNotFoundError:
            raise ImportError(f"Package '{package}' not found in libs directory")
        
        package_interpreter = NLSInterpreter(self.grammar)
        package_interpreter.run(package_code)
        
        if module not in package_interpreter.environment:
            raise ImportError(f"Module '{module}' not found in package '{package}'")
        
        module_env = package_interpreter.environment[module]
        
        # Export specific definitions
        for export in exports:
            if export not in module_env:
                raise ImportError(f"Export '{export}' not found in module '{module}'")
            self.environment[export] = module_env[export]
        
        return [module_env[e] for e in exports]

    def _execute_if(self, node):
        if self.execute(node['condition']):
            for stmt in node['if_body']:
                result = self.execute(stmt)
            return result
        
        for elseif in node.get('elseif_clauses', []):
            if self.execute(elseif['condition']):
                for stmt in elseif['body']:
                    result = self.execute(stmt)
                return result
                
        for stmt in node.get('else_body', []):
            result = self.execute(stmt)
        return result

    def _execute_while(self, node):
        condition = self.execute(node['condition'])
        result = None
        
        while condition:
            for stmt in node['body']:
                result = self.execute(stmt)
            condition = self.execute(node['condition'])
            
        return result

    def _execute_function_definition(self, node):
        name = node['name']
        self.functions[name] = node
        return name

    def _execute_class_definition(self, node):
        name = node['name']
        self.classes[name] = node
        return name

    def _execute_print(self, node):
        args = [str(self.execute(arg)) for arg in node['args']]
        print(' '.join(args))
        return None

    def _execute_input(self, node):
        prompt = self.execute(node.get('prompt'))
        if prompt:
            user_input = input(prompt)
        else:
            user_input = input()
        self.environment['_'] = user_input
        return user_input

    def _execute_output(self, node):
        value = self.execute(node['value'])
        return int(value) if isinstance(value, (int, float)) else 0

    def _execute_assignment(self, node):
        var_name = node['name']
        if not var_name.startswith('@'):
            raise SyntaxError("Variable names must start with @")
        
        value = self.execute(node['value'])
        self.environment[var_name[1:]] = value  # Store without @ prefix
        return value

    def _execute_identifier(self, node):
        value = node['value']
        if value.startswith('@'):
            var_key = value[1:]
            if var_key not in self.environment:
                raise NameError(f"Variable {value} not found")
            return self.environment[var_key]
        return value

    def _execute_binary_op(self, node):
        left = self.execute(node['left'])
        right = self.execute(node['right'])
        op = node['op']
        
        ops = {
            '+': lambda a, b: a + b,
            '-': lambda a, b: a - b,
            '*': lambda a, b: a * b,
            '/': lambda a, b: a / b,
            '>': lambda a, b: a > b,
            '<': lambda a, b: a < b,
            '==': lambda a, b: a == b,
            '!=': lambda a, b: a != b,
            '=': lambda a, b: a == b  # For comparison, not assignment
        }
        
        if op in ops:
            return ops[op](left, right)
        raise ValueError(f"Unknown operator '{op}'")

# Grammar definition
nexlang_grammar = Grammar()

if __name__ == "__main__":
    interpreter = NLSInterpreter(nexlang_grammar)
    
    # Test code demonstrating all features
    test_code = """
    # Import examples
    import math_functions
    import math_functions as math
    import advanced_math from math_extras
    from math_package import trig_functions export sin, cos
    
    # Function example
    @definition void static function greet(string[] args) {
        print "Hello," args[0]
    }
    
    # Class example
    @classes object class Person begin
        @definition void class
        
        function __object_init__(self) {
            @name = ""
            @age = 0
        }
        
        function set_name(self, string name) {
            @name = name
        }
    end
    
    # Main program
    greet(["Alice"])
    
    @p = Person()
    @p.set_name("Bob")
    print @p.name
    """
    
    interpreter.run(test_code)