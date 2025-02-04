package parser

import (
	"cool-compiler/ast"
	"cool-compiler/lexer"
	"fmt"
)

type Precedence int


// We will assign a precedence to each token type. 
//This will help us to know the order of operations when parsing expressions.
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
}

type Parser struct {
	l         *lexer.Lexer
	curToken  lexer.Token
	peekToken lexer.Token
	errors    []string
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

// This function will register a prefix operator.
func (p *Parser) registerPrefix(tokenType lexer.TokenType, fn prefixParseFn) {
    p.prefixParseFns[tokenType] = fn
}

func New(l *lexer.Lexer) *Parser {
    p := &Parser{
        l:             l,
        errors:        []string{},
        prefixParseFns: make(map[lexer.TokenType]prefixParseFn),
        infixParseFns:  make(map[lexer.TokenType]infixParseFn),
    }
    
    // Register prefix operators

    p.registerPrefix(lexer.IF, p.parseIfExpression)
    p.registerPrefix(lexer.WHILE, p.parseWhileExpression)


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

		if !p.expectAndPeek(lexer.SEMI) {
			continue
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
		// Add errors
		return nil
	}

	c.Name = p.curToken.Literal
	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}

	for !p.peekTokenIs(lexer.RBRACE) {
		p.nextToken()
		c.Features = append(c.Features, p.parseFeature())
		if !p.expectAndPeek(lexer.SEMI) {
			return nil
		}
	}

	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}

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

	if !p.curTokenIs(lexer.OBJECTID) {
		return nil
	}
	method.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectAndPeek(lexer.LPAREN) {
		return nil
	}

	method.Formals = p.parseFormals()

	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

	if !p.expectAndPeek(lexer.TYPEID) {
		return nil
	}
	method.TypeDecl = &ast.TypeIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectAndPeek(lexer.LBRACE) {
		return nil
	}

	p.nextToken()
	method.Body = p.parseExpression(LOWEST)

	if !p.expectAndPeek(lexer.RBRACE) {
		return nil
	}

	return method
}

// TODO : PARSE FORMALS (Will return a set of formals).
func (p *Parser) parseFormals() []*ast.Formal {
	var formals []*ast.Formal

	if p.peekTokenIs(lexer.RPAREN) {
		p.nextToken()
		return formals
	}

	p.nextToken()
	formals = append(formals, p.parseFormal())

	for p.peekTokenIs(lexer.COMMA) {
		p.nextToken()
		p.nextToken()
		formals = append(formals, p.parseFormal())
	}

	if !p.expectAndPeek(lexer.RPAREN) {
		return nil
	}

	return formals
}

func (p *Parser) parseFormal() *ast.Formal {
	formal := &ast.Formal{Token: p.curToken}

	if !p.curTokenIs(lexer.OBJECTID) {
		return nil
	}
	formal.Name = &ast.ObjectIdentifier{Token: p.curToken, Value: p.curToken.Literal}

	if !p.expectAndPeek(lexer.COLON) {
		return nil
	}

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

    if !p.expectAndPeek(lexer.THEN) {
        return nil
    }

    p.nextToken()
    expression.Condition = p.parseExpression(LOWEST)

    if !p.expectAndPeek(lexer.THEN) {
        return nil
    }

    p.nextToken()
    expression.Consequence = p.parseExpression(LOWEST)

    if !p.expectAndPeek(lexer.ELSE) {
        return nil
    }

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