package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
	"strconv"
)

type Precedence int

// We will assign a precedence to each token type.
// This will help us to know the order of operations when parsing expressions.
const (
	START Precedence = iota
	LOWEST
	ASSIGN   // <-
	EQUALS   // =
	COMPARE  // < <=
	SUM      // + -
	PRODUCT  // * /
	PREFIX   // ~ not isvoid
	DISPATCH // obj.method()
)

var precedences = map[lexer.TokenType]Precedence{
	lexer.ASSIGN: ASSIGN,
	lexer.EQ:     EQUALS,
	lexer.LT:     COMPARE,
	lexer.LE:     COMPARE,
	lexer.PLUS:   SUM,
	lexer.MINUS:  SUM,
	lexer.TIMES:  PRODUCT,
	lexer.DIVIDE: PRODUCT,
	lexer.DOT:    DISPATCH,
	lexer.AT:     DISPATCH,
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l         *lexer.Lexer
	curToken  lexer.Token
	peekToken lexer.Token
	errors    []string

	prefixParseFns map[lexer.TokenType]prefixParseFn
	infixParseFns  map[lexer.TokenType]infixParseFn
}

// This function will get the precedence of the next token.
func (p *Parser) peekPrecedence() Precedence {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

// This function will get the precedence of the current token.
func (p *Parser) curPrecedence() Precedence {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

// We also decided to add a few helper functions to make the code cleaner.

// This function will register a prefix operator.
func (p *Parser) registerPrefix(tokenType lexer.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType lexer.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

// This function will report an error when some token has not been assigned a prefix parse function.
func (p *Parser) noPrefixParseFnError(t lexer.TokenType) {
	p.errors = append(p.errors,
		fmt.Sprintf("no prefix parse function for %s found at line %d col %d",
			t, p.curToken.Line, p.curToken.Column))
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:              l,
		errors:         []string{},
		prefixParseFns: make(map[lexer.TokenType]prefixParseFn),
		infixParseFns:  make(map[lexer.TokenType]infixParseFn),
	}

	// Register prefix operators

	p.registerPrefix(lexer.INT_CONST, p.parseIntegerLiteral)
	p.registerPrefix(lexer.STR_CONST, p.parseStringLiteral)
	p.registerPrefix(lexer.BOOL_CONST, p.parseBooleanLiteral)
	p.registerPrefix(lexer.OBJECTID, p.parseIdentifier)
	p.registerPrefix(lexer.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(lexer.IF, p.parseIfExpression)
	p.registerPrefix(lexer.WHILE, p.parseWhileExpression)
	p.registerPrefix(lexer.LET, p.parseLetExpression)
	p.registerPrefix(lexer.CASE, p.parseCaseExpression)
	p.registerPrefix(lexer.NEW, p.parseNewExpression)
	p.registerPrefix(lexer.ISVOID, p.parseIsVoidExpression)
	p.registerPrefix(lexer.NEG, p.parseUnaryExpression)
	p.registerPrefix(lexer.NOT, p.parseUnaryExpression)
	p.registerPrefix(lexer.LBRACE, p.parseBlockExpression)

	// Now we will register infix operators
	p.registerInfix(lexer.PLUS, p.parseBinaryExpression)
	p.registerInfix(lexer.MINUS, p.parseBinaryExpression)
	p.registerInfix(lexer.TIMES, p.parseBinaryExpression)
	p.registerInfix(lexer.DIVIDE, p.parseBinaryExpression)
	p.registerInfix(lexer.EQ, p.parseBinaryExpression)
	p.registerInfix(lexer.LT, p.parseBinaryExpression)
	p.registerInfix(lexer.LE, p.parseBinaryExpression)
	p.registerInfix(lexer.ASSIGN, p.parseBinaryExpression)
	p.registerInfix(lexer.DOT, p.parseDispatchExpression)
	p.registerInfix(lexer.AT, p.parseDispatchExpression)

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) curTokenIs(t lexer.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t lexer.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectAndPeek(t lexer.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	p.peekError(t)
	return false
}

func (p *Parser) expectCurrent(t lexer.TokenType) bool {
	if p.curTokenIs(t) {
		p.nextToken()
		return true
	}
	p.currentError(t)
	return false
}

func (p *Parser) peekError(t lexer.TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("Expected next token to be %v, got %v line %d col %d", t, p.peekToken.Type, p.peekToken.Line, p.peekToken.Column))
}

func (p *Parser) currentError(t lexer.TokenType) {
	p.errors = append(p.errors, fmt.Sprintf("Expected current token to be %v, got %v line %d col %d", t, p.curToken.Type, p.peekToken.Line, p.peekToken.Column))
}

func (p *Parser) ParseProgram() *ast.Program {
	prog := &ast.Program{}
	for p.curToken.Type != lexer.EOF && p.curToken.Type != lexer.ERROR {
		c := p.ParseClass()

		if p.curToken.Type != lexer.SEMI {
			p.errors = append(p.errors,
				fmt.Sprintf("Expected semicolon after class definition at line %d col %d, got %s",
					p.curToken.Line, p.curToken.Column, p.curToken.Type))
		} else {
			p.nextToken()
		}

		prog.Classes = append(prog.Classes, c)
	}
	return prog
}

func (p *Parser) ParseClass() *ast.Class {
	c := &ast.Class{Token: p.curToken}

	if !p.expectCurrent(lexer.CLASS) {
		return nil
	}

	if !p.curTokenIs(lexer.TYPEID) {
		p.errors = append(p.errors,
			fmt.Sprintf("Expected class name to be TYPEID at line %d col %d, got %s",
				p.curToken.Line, p.curToken.Column, p.curToken.Type))
		return nil
	}
	c.Name = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	p.nextToken()

	if p.curTokenIs(lexer.INHERITS) {
		p.nextToken()
		if !p.curTokenIs(lexer.TYPEID) {
			p.errors = append(p.errors,
				fmt.Sprintf("Expected parent class name to be TYPEID at line %d col %d, got %s",
					p.curToken.Line, p.curToken.Column, p.curToken.Type))
			return nil
		}
		c.Parent = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
		p.nextToken()
	}

	if !p.curTokenIs(lexer.LBRACE) {
		p.errors = append(p.errors,
			fmt.Sprintf("Expected { after class declaration at line %d col %d, got %s",
				p.curToken.Line, p.curToken.Column, p.curToken.Type))
		return nil
	}
	p.nextToken()

	for !p.curTokenIs(lexer.RBRACE) && !p.curTokenIs(lexer.EOF) {
		feature := p.parseFeature()

		if feature != nil {
			c.Features = append(c.Features, feature)
			p.nextToken()
			if p.curTokenIs(lexer.SEMI) {
				p.nextToken()
			} else {
				p.errors = append(p.errors,
					fmt.Sprintf("Expected semicolon after feature at line %d col %d, got %s",
						p.curToken.Line, p.curToken.Column, p.curToken.Type))
				return nil
			}
		}
	}

	if !p.curTokenIs(lexer.RBRACE) {
		p.errors = append(p.errors,
			fmt.Sprintf("Expected } at end of class at line %d col %d, got %s",
				p.curToken.Line, p.curToken.Column, p.curToken.Type))
		return nil
	}
	p.nextToken()

	return c
}

// TODO : PARSE FEATURE (can be method ot attribute).
func (p *Parser) parseFeature() ast.Feature {
	if p.peekTokenIs(lexer.LPAREN) {
		return p.parseMethod()
	}
	return p.parseAttribute()
}

// TODO : PARSE ATTRIBUTE (Will have a name, a type and an optional init expression).
func (p *Parser) parseAttribute() *ast.Attribute {
	attr := &ast.Attribute{Token: p.curToken}

	if !p.curTokenIs(lexer.OBJECTID) {
		return nil
	}
	attr.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	attr.TypeDecl = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	if p.peekTokenIs(lexer.ASSIGN) {
		p.nextToken()
		p.nextToken()
		attr.Init = p.parseExpression(LOWEST)
	}

	return attr
}

// TODO : PARSE METHOD (Will have a set of formals and a body).
func (p *Parser) parseMethod() *ast.Method {
	method := &ast.Method{Token: p.curToken}

	// Parse method name (current token should be OBJECTID)
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors,
			fmt.Sprintf("Expected method name to be OBJECTID, got %s at line %d col %d",
				p.curToken.Type, p.curToken.Line, p.curToken.Column))
		return nil
	}
	method.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse opening parenthesis
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}

	// Parse formal parameters
	method.Formals = []*ast.Formal{}
	if !p.peekTokenIs(lexer.RPAREN) {
		p.nextToken()
		firstFormal := p.parseFormal()
		if firstFormal != nil {
			method.Formals = append(method.Formals, firstFormal)
		}

		for p.peekTokenIs(lexer.COMMA) {
			p.nextToken() // consume the comma
			p.nextToken() // move to the next formal
			formal := p.parseFormal()
			if formal != nil {
				method.Formals = append(method.Formals, formal)
			}
		}
	}

	// Parse closing parenthesis
	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}

	// Parse return type
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	method.TypeDecl = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse method body
	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}

	p.nextToken()

	// Handle empty method body
	if p.curTokenIs(lexer.RBRACE) {
		return method
	}

	// Parse method body expression
	method.Body = p.parseExpression(LOWEST)

	// Expect closing brace
	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}

	return method
}

func (p *Parser) parseFormals() []*ast.Formal {
	var formals []*ast.Formal

	// Handle empty formals list
	if p.curTokenIs(lexer.RPAREN) {
		return formals
	}

	// Parse first formal
	formal := p.parseFormal()
	if formal != nil {
		formals = append(formals, formal)
	}

	// Parse remaining formals
	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken() // consume comma
		p.nextToken() // move to next identifier
		formal = p.parseFormal()
		if formal != nil {
			formals = append(formals, formal)
		}
	}

	return formals
}

func (p *Parser) parseFormal() *ast.Formal {
	formal := &ast.Formal{Token: p.curToken}

	// current token should be OBJECTID
	if !p.curTokenIs(lexer.OBJECTID) {
		p.errors = append(p.errors,
			fmt.Sprintf("Expected formal parameter name to be OBJECTID, got %s at line %d col %d",
				p.curToken.Type, p.curToken.Line, p.curToken.Column))
		return nil
	}

	formal.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse colon
	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	// Parse type
	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	formal.TypeDecl = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	return formal
}

func (p *Parser) parseExpression(precedence Precedence) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()

	for precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken}

	// Move past 'if'
	p.nextToken()
	// Parse condition
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.THEN) {
		return nil
	}

	// Parse consequence (true branch)
	p.nextToken()
	expression.Consequence = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.ELSE) {
		return nil
	}

	// Parse alternative (false branch)
	p.nextToken()
	expression.Alternative = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.FI) {
		return nil
	}

	return expression
}

func (p *Parser) parseWhileExpression() ast.Expression {
	expression := &ast.WhileExpression{Token: p.curToken}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.LOOP) {
		return nil
	}

	p.nextToken()
	expression.Body = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.POOL) {
		return nil
	}

	return expression
}

func (p *Parser) parseBlockExpression() ast.Expression {
	block := &ast.BlockExpression{Token: p.curToken}

	p.nextToken()
	block.Expressions = []ast.Expression{}

	for !p.curTokenIs(lexer.RBRACE) {
		expr := p.parseExpression(LOWEST)
		if expr != nil {
			block.Expressions = append(block.Expressions, expr)
		}
		p.nextToken()

		if !p.curTokenIs(lexer.SEMI) && !p.curTokenIs(lexer.RBRACE) {
			return nil
		}
		if p.curTokenIs(lexer.SEMI) {
			p.nextToken()
		}
	}

	return block
}

func (p *Parser) parseLetExpression() ast.Expression {
	expr := &ast.LetExpression{Token: p.curToken}
	expr.Bindings = []*ast.LetBinding{}

	for {
		if !p.expectAndPeek(lexer.OBJECTID) {
			return nil
		}

		binding := &ast.LetBinding{
			Identifier: &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal},
		}

		if !p.expectAndPeek(lexer.COLON) {
			return nil
		}

		if !p.expectAndPeek(lexer.TYPEID) {
			return nil
		}
		binding.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

		if p.peekTokenIs(lexer.ASSIGN) {
			p.nextToken() // move to <-
			p.nextToken() // move to first token of init expression

			// Parse initialization with higher precedence than comma
			binding.Init = p.parseExpression(LOWEST)
			if binding.Init == nil {
				return nil
			}
		}

		expr.Bindings = append(expr.Bindings, binding)

		if !p.peekTokenIs(lexer.COMMA) {
			break
		}
		p.nextToken() // consume the comma
	}

	if !p.expectAndPeek(lexer.IN) {
		return nil
	}

	p.nextToken() // move past IN
	expr.In = p.parseExpression(LOWEST)

	return expr
}

func (p *Parser) parseCaseExpression() ast.Expression {
	expr := &ast.CaseExpression{Token: p.curToken}

	p.nextToken()
	expr.Expr = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.OF) {
		return nil
	}

	expr.Branches = []*ast.CaseBranch{}

	for !p.peekTokenIs(lexer.ESAC) {
		if !p.expectAndPeek(lexer.OBJECTID) {
			return nil
		}

		branch := &ast.CaseBranch{
			Token:   p.curToken,
			Pattern: &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal},
		}

		if !p.expectAndPeek(lexer.COLON) {
			return nil
		}

		if !p.expectAndPeek(lexer.TYPEID) {
			return nil
		}
		branch.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

		if !p.expectAndPeek(lexer.DARROW) {
			return nil
		}

		p.nextToken()
		branch.Expression = p.parseExpression(LOWEST)

		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}

		expr.Branches = append(expr.Branches, branch)
	}

	if !p.expectAndPeek(lexer.ESAC) {
		return nil
	}

	return expr
}

func (p *Parser) parseDispatchExpression(object ast.Expression) ast.Expression {
	dispatch := &ast.DispatchExpression{
		Token:  p.curToken,
		Object: object,
	}

	// Check for static dispatch (@Type)
	if p.peekTokenIs(lexer.AT) {
		p.nextToken() // consume the '.'
		p.nextToken() // move to the type

		if !p.curTokenIs(lexer.TYPEID) {
			p.errors = append(p.errors,
				fmt.Sprintf("Expected type identifier after @ at line %d col %d, got %s",
					p.curToken.Line, p.curToken.Column, p.curToken.Type))
			return nil
		}

		dispatch.StaticType = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

		// Expect and consume the '.'
		if !p.expectAndPeek(lexer.DOT) {
			return nil
		}
	}

	// Parse method name
	if !p.expectAndPeek(lexer.OBJECTID) {
		return nil
	}
	dispatch.Method = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	// Parse argument list
	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}
	dispatch.Arguments = p.parseExpressionList(lexer.RPAREN)

	return dispatch
}

func (p *Parser) parseExpressionList(end lexer.TokenType) []ast.Expression {
	list := []ast.Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectAndPeek(end) {
		return nil
	}

	return list
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()

	exp := p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}

	return exp
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		p.errors = append(p.errors,
			fmt.Sprintf("could not parse %q as integer at line %d col %d",
				p.curToken.Literal, p.curToken.Line, p.curToken.Column))
		return nil
	}

	return &ast.IntegerLiteral{Token: p.curToken, Value: value}
}

func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseBooleanLiteral() ast.Expression {
	return &ast.BooleanLiteral{
		Token: p.curToken,
		Value: p.curToken.Literal == "true",
	}
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseNewExpression() ast.Expression {
	expr := &ast.NewExpression{Token: p.curToken}

	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}

	expr.Type = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}
	return expr
}

func (p *Parser) parseIsVoidExpression() ast.Expression {
	expr := &ast.IsVoidExpression{Token: p.curToken}

	p.nextToken()
	expr.Expression = p.parseExpression(PREFIX)

	return expr
}

func (p *Parser) parseUnaryExpression() ast.Expression {
	expr := &ast.UnaryExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}

	p.nextToken()
	expr.Right = p.parseExpression(PREFIX)

	return expr
}

func (p *Parser) parseBinaryExpression(left ast.Expression) ast.Expression {
	expr := &ast.BinaryExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
		Left:     left,
	}

	precedence := p.curPrecedence()
	p.nextToken()
	expr.Right = p.parseExpression(precedence)

	return expr
}
