implement Lfmt;

include "sys.m";
include "draw.m";
include "string.m";
include "bufio.m";
include "arg.m";
include "lfmt.m";

sys: Sys;
str: String;

TkEOF,
TkIdent,
TkNumber,
TkString,
TkChar,
TkKeyword,
TkOp,
TkPunct,
TkComment: con iota;

NProgram,
NBlock,
NFn,
NIf,
NFor,
NWhile,
NDo,
NCase,
NAlt,
NPick,
NClause,
NReturn,
NBreak,
NContinue,
NExit,
NRaise,
NSpawn,
NExpr,
NDecl,
NDeclBlock,
NImplement,
NInclude,
NRaw: con iota;

Token: adt
{
	kind: int;
	text: string;
	line: int;
	col: int;
};

Node: adt
{
	kind: int;
	header: array of Token;
	body: list of ref Node;
	elsebody: list of ref Node;
	comments: list of Token;
	needsemi: int;
};

Lexer: adt
{
	src: array of byte;
	pos: int;
	line: int;
	col: int;

	peek: fn(l: self ref Lexer): int;
	next: fn(l: self ref Lexer): int;
	lex: fn(l: self ref Lexer): Token;
};

Parser: adt
{
	toks: array of Token;
	i: int;
	pending: list of Token;
	lastline: int;

	peek: fn(p: self ref Parser): Token;
	next: fn(p: self ref Parser): Token;
	matchkw: fn(p: self ref Parser, s: string): int;
	matchp: fn(p: self ref Parser, s: string): int;
	expectp: fn(p: self ref Parser, s: string): Token;
	leading: fn(p: self ref Parser): list of Token;
};

keywords := array[] of
{
	"adt",
	"alt",
	"array",
	"big",
	"break",
	"byte",
	"case",
	"chan",
	"con",
	"continue",
	"cyclic",
	"do",
	"dynamic",
	"else",
	"exception",
	"exit",
	"fixed",
	"fn",
	"for",
	"hd",
	"if",
	"implement",
	"import",
	"include",
	"int",
	"len",
	"list",
	"load",
	"module",
	"nil",
	"of",
	"or",
	"pick",
	"raise",
	"raises",
	"real",
	"ref",
	"return",
	"self",
	"spawn",
	"string",
	"tagof",
	"tl",
	"to",
	"type",
	"while"
};

ops := array[] of
{
	"<<=",
	">>=",
	"**=",
	"&&",
	"||",
	"::",
	"==",
	"!=",
	"<=",
	">=",
	"<<",
	">>",
	"<-",
	"++",
	"--",
	"->",
	"=>",
	"**",
	":=",
	"+=",
	"-=",
	"*=",
	"/=",
	"%=",
	"&=",
	"|=",
	"^="
};

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	str = load String String->PATH;

	arg := load Arg Arg->PATH;
	if(arg == nil){
		sys->fprint(sys->fildes(2), "lfmt: can't load arg: %r\n");
		return;
	}

	arg->init(args);
	arg->setusage("lfmt [-w] file.b");
	write := 0;
	while((c := arg->opt()) != 0){
		case c {
		'w' =>
			write = 1;
		* =>
			arg->usage();
		}
	}
	argv := arg->argv();
	if(argv == nil){
		arg->usage();
		return;
	}
	file := hd argv;
	buf := readfile(file);
	if(buf == nil){
		sys->fprint(sys->fildes(2), "lfmt: can't read %s: %r\n", file);
		return;
	}

	l := ref Lexer(buf, 0, 1, 1);
	toks := lexall(l);
	p := ref Parser(toks, 0, nil, 0);
	prog := parseprogram(p);
	out := formatnode(prog, 0);
	if(write){
		if(!writefile(file, out))
			sys->fprint(sys->fildes(2), "lfmt: can't write %s: %r\n", file);
		return;
	}
	sys->print("%s", out);
}

readfile(path: string): array of byte
{
	fd := sys->open(path, Sys->OREAD);
	if(fd == nil)
		return nil;

	buf := array[0] of byte;
	tmp := array[8192] of byte;
	for(;;){
		n := sys->read(fd, tmp, len tmp);
		if(n <= 0)
			break;
		all := array[len buf + n] of byte;
		all[0:] = buf;
		all[len buf:] = tmp[0:n];
		buf = all;
	}
	return buf;
}

writefile(path: string, s: string): int
{
	fd := sys->open(path, Sys->OWRITE|Sys->OTRUNC);
	if(fd == nil)
		return 0;
	buf := array of byte s;
	if(sys->write(fd, buf, len buf) != len buf){
		return 0;
	}
	return 1;
}

lexall(l: ref Lexer): array of Token
{
	toks := array[0] of Token;
	for(;;){
		tok := l.lex();
		toks = appendtok(toks, tok);
		if(tok.kind == TkEOF)
			break;
	}
	return toks;
}

appendtok(a: array of Token, t: Token): array of Token
{
	n := len a;
	b := array[n+1] of Token;
	b[0:] = a;
	b[n] = t;
	return b;
}

Lexer.peek(l: self ref Lexer): int
{
	if(l.pos >= len l.src)
		return -1;
	return int l.src[l.pos];
}

Lexer.next(l: self ref Lexer): int
{
	if(l.pos >= len l.src)
		return -1;
	c := int l.src[l.pos++];
	if(c == '\n'){
		l.line++;
		l.col = 1;
	}else
		l.col++;
	return c;
}

Lexer.lex(l: self ref Lexer): Token
{
	for(;;){
		c := l.peek();
		if(c < 0)
			return Token(TkEOF, "", l.line, l.col);
		if(c == ' ' || c == '\t' || c == '\r' || c == '\v'){
			l.next();
			continue;
		}
		if(c == '\n'){
			l.next();
			continue;
		}
		break;
	}

	startl := l.line;
	startc := l.col;
	c := l.next();

	if(c == '#'){
		text := "#";
		for(;;){
			n := l.peek();
			if(n < 0 || n == '\n')
				break;
			text[len text] = l.next();
		}
		return Token(TkComment, text, startl, startc);
	}

	if(c == '/'){
		n := l.peek();
		if(n == '/'){
			l.next();
			text := "//";
			for(;;){
				n = l.peek();
				if(n < 0 || n == '\n')
					break;
				text[len text] = l.next();
			}
			return Token(TkComment, text, startl, startc);
		}
		if(n == '*'){
			l.next();
			text := "/*";
			for(;;){
				n = l.next();
				if(n < 0)
					break;
				text[len text] = n;
				if(n == '*' && l.peek() == '/'){
					text[len text] = l.next();
					break;
				}
			}
			return Token(TkComment, text, startl, startc);
		}
		return Token(TkOp, "/", startl, startc);
	}

	if(c == '"' || c == '`'){
		term := c;
		text := "";
		text[0] = term;
		for(;;){
			n := l.next();
			if(n < 0)
				break;
			text[len text] = n;
			if(n == term)
				break;
			if(term == '"' && n == '\\'){
				n = l.next();
				if(n < 0)
					break;
				text[len text] = n;
			}
		}
		return Token(TkString, text, startl, startc);
	}

	if(c == '\''){
		text := "'";
		for(;;){
			n := l.next();
			if(n < 0)
				break;
			text[len text] = n;
			if(n == '\\'){
				n = l.next();
				if(n < 0)
					break;
				text[len text] = n;
				continue;
			}
			if(n == '\'')
				break;
		}
		return Token(TkChar, text, startl, startc);
	}

	if(isalpha(c) || c == '_'){
		text := "";
		text[0] = c;
		for(;;){
			n := l.peek();
			if(!(isalpha(n) || isdigit(n) || n == '_'))
				break;
			text[len text] = l.next();
		}
		if(iskeyword(text))
			return Token(TkKeyword, text, startl, startc);
		return Token(TkIdent, text, startl, startc);
	}

	if(isdigit(c)){
		text := "";
		text[0] = c;
		for(;;){
			n := l.peek();
			if(!(isalpha(n) || isdigit(n) || n == '.' || n == '_'))
				break;
			text[len text] = l.next();
		}
		return Token(TkNumber, text, startl, startc);
	}

	# operators and punctuation
	for(i := 0; i < len ops; i++){
		op := ops[i];
		if(op[0] != c)
			continue;
		if(matchop(l, op[1:]))
			return Token(TkOp, op, startl, startc);
	}

	if(ispunct(c))
		return Token(TkPunct, tokchar(c), startl, startc);

	return Token(TkOp, tokchar(c), startl, startc);
}

matchop(l: ref Lexer, rest: string): int
{
	spos := l.pos;
	sline := l.line;
	scol := l.col;
	for(i := 0; i < len rest; i++){
		c := l.peek();
		if(c < 0 || c != rest[i]){
			l.pos = spos;
			l.line = sline;
			l.col = scol;
			return 0;
		}
		l.next();
	}
	return 1;
}

iskeyword(s: string): int
{
	for(i := 0; i < len keywords; i++)
		if(keywords[i] == s)
			return 1;
	return 0;
}

isalpha(c: int): int
{
	return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

isdigit(c: int): int
{
	return c >= '0' && c <= '9';
}

ispunct(c: int): int
{
	return c == '(' || c == ')' || c == '[' || c == ']' || c == '{' || c == '}' ||
		c == ',' || c == ';' || c == ':' || c == '.';
}

tokchar(c: int): string
{
	return sys->sprint("%c", c);
}

Parser.peek(p: self ref Parser): Token
{
	if(p.i >= len p.toks)
		return Token(TkEOF, "", 0, 0);
	return p.toks[p.i];
}

Parser.next(p: self ref Parser): Token
{
	t := p.peek();
	p.i++;
	if(t.line != 0)
		p.lastline = t.line;
	return t;
}

Parser.matchkw(p: self ref Parser, s: string): int
{
	t := p.peek();
	if(t.kind == TkKeyword && t.text == s){
		p.next();
		return 1;
	}
	return 0;
}

Parser.matchp(p: self ref Parser, s: string): int
{
	t := p.peek();
	if(t.kind == TkPunct && t.text == s){
		p.next();
		return 1;
	}
	return 0;
}

Parser.expectp(p: self ref Parser, s: string): Token
{
	t := p.next();
	if(t.kind != TkPunct || t.text != s)
		return t;
	return t;
}

Parser.leading(p: self ref Parser): list of Token
{
	c: list of Token;
	t := p.peek();
	if(p.lastline != 0 && t.line != 0){
		gap := t.line - p.lastline - 1;
		for(; gap > 0; gap--)
			c = Token(TkComment, "", 0, 0) :: c;
	}
	for(;;){
		t = p.peek();
		if(t.kind != TkComment)
			break;
		p.next();
		c = t :: c;
	}
	return revcomments(c);
}

revcomments(c: list of Token): list of Token
{
	out: list of Token;
	for(; c != nil; c = tl c)
		out = hd c :: out;
	return out;
}

parseprogram(p: ref Parser): ref Node
{
	n := ref Node(NProgram, nil, nil, nil, nil, 0);
	for(;;){
		if(p.peek().kind == TkEOF)
			break;
		item := parsetop(p);
		if(item == nil)
			break;
		n.body = item :: n.body;
	}
	n.body = revnodes(n.body);
	return n;
}

parsetop(p: ref Parser): ref Node
{
	com := p.leading();
	if(p.matchkw("implement")){
		n := ref Node(NImplement, nil, nil, nil, com, 1);
		n.header = appendtok(n.header, Token(TkKeyword, "implement", 0, 0));
		n.header = appendarr(n.header, collectuntil(p, ";", 0));
		p.matchp(";");
		return n;
	}
	if(p.matchkw("include")){
		n := ref Node(NInclude, nil, nil, nil, com, 1);
		n.header = appendtok(n.header, Token(TkKeyword, "include", 0, 0));
		n.header = appendarr(n.header, collectuntil(p, ";", 0));
		p.matchp(";");
		return n;
	}
	if(lookaheadfndef(p)){
		return parsefndef(p, com);
	}
	return parsedeclblock(p, com);
}

lookaheadfndef(p: ref Parser): int
{
	idx := p.i;
	if(!isname(p)){
		p.i = idx;
		return 0;
	}
	if(!lookaheadparen(p)){
		p.i = idx;
		return 0;
	}
	skiprettype(p);
	if(p.peek().kind == TkKeyword && p.peek().text == "raises"){
		p.next();
		if(p.matchp("("))
			skipbalanced(p, "(", ")");
		else
			p.next();
	}
	ok := p.peek().kind == TkPunct && p.peek().text == "{";
	p.i = idx;
	return ok;
}

isname(p: ref Parser): int
{
	t := p.peek();
	if(t.kind != TkIdent)
		return 0;
	p.next();
	for(;;){
		t = p.peek();
		if(t.kind == TkPunct && t.text == "."){
			p.next();
			t = p.peek();
			if(t.kind != TkIdent)
				return 0;
			p.next();
			continue;
		}
		break;
	}
	return 1;
}

lookaheadparen(p: ref Parser): int
{
	t := p.peek();
	if(t.kind != TkPunct || t.text != "(")
		return 0;
	p.next();
	skipbalanced(p, "(", ")");
	return 1;
}

skipbalanced(p: ref Parser, open: string, close: string)
{
	depth := 1;
	for(; depth > 0; ){
		t := p.next();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkPunct && t.text == open)
			depth++;
		else if(t.kind == TkPunct && t.text == close)
			depth--;
	}
}

parsefndef(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NFn, nil, nil, nil, com, 0);
	n.header = collectnameandargs(p);
	n.header = appendarr(n.header, collectrettype(p));
	if(p.matchkw("raises")){
		t := Token(TkKeyword, "raises", 0, 0);
		n.header = appendtok(n.header, t);
		n.header = appendarr(n.header, collectraise(p));
	}
	n.body = parseblock(p);
	return n;
}

collectnameandargs(p: ref Parser): array of Token
{
	arr := array[0] of Token;
	arr = appendarr(arr, collectname(p));
	if(p.matchp("(")){
		arr = appendtok(arr, Token(TkPunct, "(", 0, 0));
		arr = appendarr(arr, collectuntil(p, ")", 1));
		p.matchp(")");
		arr = appendtok(arr, Token(TkPunct, ")", 0, 0));
	}
	return arr;
}

collectname(p: ref Parser): array of Token
{
	arr := array[0] of Token;
	arr = appendtok(arr, p.next());
	for(;;){
		t := p.peek();
		if(t.kind == TkPunct && t.text == "."){
			arr = appendtok(arr, p.next());
			arr = appendtok(arr, p.next());
			continue;
		}
		break;
	}
	return arr;
}

collectraise(p: ref Parser): array of Token
{
	arr := array[0] of Token;
	if(p.matchp("(")){
		arr = appendtok(arr, Token(TkPunct, "(", 0, 0));
		arr = appendarr(arr, collectuntil(p, ")", 1));
		p.matchp(")");
		arr = appendtok(arr, Token(TkPunct, ")", 0, 0));
		return arr;
	}
	arr = appendtok(arr, p.next());
	return arr;
}

collectrettype(p: ref Parser): array of Token
{
	arr := array[0] of Token;
	if(!p.matchp(":"))
		return arr;
	arr = appendtok(arr, Token(TkPunct, ":", 0, 0));
	arr = appendarr(arr, collectuntilkworpunct(p, "raises", "{"));
	return arr;
}

skiprettype(p: ref Parser)
{
	if(!p.matchp(":"))
		return;
	skipuntilkworpunct(p, "raises", "{");
}

parsedeclblock(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NDecl, nil, nil, nil, com, 1);
	n.header = collectuntilblock(p);
	if(p.peek().kind == TkPunct && p.peek().text == "{"){
		if(shouldblock(n.header)){
			n.kind = NDeclBlock;
			n.body = parseblock(p);
			p.matchp(";");
		}else{
			n.header = appendarr(n.header, collectblockexpr(p));
			n.header = appendarr(n.header, collectuntil(p, ";", 0));
			p.matchp(";");
		}
	}
	else if(p.matchp(";"))
		n.needsemi = 1;
	return n;
}

collectuntilblock(p: ref Parser): array of Token
{
	arr := array[0] of Token;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkPunct && (t.text == "{" || t.text == ";"))
			break;
		arr = appendtok(arr, p.next());
	}
	return arr;
}

shouldblock(hdr: array of Token): int
{
	for(i := 0; i < len hdr; i++){
		t := hdr[i];
		if(t.kind == TkKeyword && (t.text == "module" || t.text == "adt"))
			return 1;
	}
	return 0;
}

collectblockexpr(p: ref Parser): array of Token
{
	arr := array[0] of Token;
	if(!p.matchp("{"))
		return arr;
	arr = appendtok(arr, Token(TkPunct, "{", 0, 0));
	depth := 1;
	for(; depth > 0; ){
		t := p.next();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkPunct && t.text == "{")
			depth++;
		else if(t.kind == TkPunct && t.text == "}")
			depth--;
		arr = appendtok(arr, t);
		if(depth == 0)
			break;
	}
	return arr;
}

parseblock(p: ref Parser): list of ref Node
{
	p.matchp("{");
	stmts: list of ref Node;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkPunct && t.text == "}"){
			p.next();
			break;
		}
		com := p.leading();
		t = p.peek();
		if(t.kind == TkPunct && t.text == "}"){
			if(com != nil)
				stmts = ref Node(NRaw, nil, nil, nil, com, 0) :: stmts;
			p.next();
			break;
		}
		if(com != nil)
			stmts = parsestmtwith(p, com) :: stmts;
		else
			stmts = parsestmt(p) :: stmts;
	}
	return revnodes(stmts);
}

parsestmt(p: ref Parser): ref Node
{
	com := p.leading();
	return parsestmtwith(p, com);
}

parsestmtwith(p: ref Parser, com: list of Token): ref Node
{
	if(p.matchp("{")){
		n := ref Node(NBlock, nil, parseblockafteropen(p), nil, com, 0);
		return n;
	}
	if(p.matchkw("if"))
		return parseif(p, com);
	if(p.matchkw("for"))
		return parsefor(p, com);
	if(p.matchkw("while"))
		return parsewhile(p, com);
	if(p.matchkw("do"))
		return parsedo(p, com);
	if(p.matchkw("return"))
		return parsereturn(p, com);
	if(p.matchkw("break"))
		return parsecontrol(p, com, NBreak);
	if(p.matchkw("continue"))
		return parsecontrol(p, com, NContinue);
	if(p.matchkw("exit"))
		return parsecontrol(p, com, NExit);
	if(p.matchkw("raise"))
		return parseraise(p, com);
	if(p.matchkw("spawn"))
		return parsespawn(p, com);
	if(p.matchkw("case"))
		return parsecaseblock(p, com, "case");
	if(p.matchkw("alt"))
		return parsecaseblock(p, com, "alt");
	if(p.matchkw("pick"))
		return parsecaseblock(p, com, "pick");
	return parserawstmt(p, com);
}

parseblockafteropen(p: ref Parser): list of ref Node
{
	stmts: list of ref Node;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkPunct && t.text == "}"){
			p.next();
			break;
		}
		com := p.leading();
		t = p.peek();
		if(t.kind == TkPunct && t.text == "}"){
			if(com != nil)
				stmts = ref Node(NRaw, nil, nil, nil, com, 0) :: stmts;
			p.next();
			break;
		}
		if(com != nil)
			stmts = parsestmtwith(p, com) :: stmts;
		else
			stmts = parsestmt(p) :: stmts;
	}
	return revnodes(stmts);
}

parseif(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NIf, nil, nil, nil, com, 0);
	n.header = collectparen(p);
	n.body = parsestmt(p) :: nil;
	if(p.matchkw("else"))
		n.elsebody = parsestmt(p) :: nil;
	return n;
}

parsefor(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NFor, nil, nil, nil, com, 0);
	n.header = collectparen(p);
	n.body = parsestmt(p) :: nil;
	return n;
}

parsewhile(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NWhile, nil, nil, nil, com, 0);
	n.header = collectparen(p);
	n.body = parsestmt(p) :: nil;
	return n;
}

parsedo(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NDo, nil, nil, nil, com, 0);
	n.body = parsestmt(p) :: nil;
	if(p.matchkw("while"))
		n.header = collectparen(p);
	p.matchp(";");
	return n;
}

parsereturn(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NReturn, nil, nil, nil, com, 1);
	n.header = collectuntil(p, ";", 0);
	p.matchp(";");
	return n;
}

parsecontrol(p: ref Parser, com: list of Token, kind: int): ref Node
{
	n := ref Node(kind, nil, nil, nil, com, 1);
	n.header = collectuntil(p, ";", 0);
	p.matchp(";");
	return n;
}

parseraise(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NRaise, nil, nil, nil, com, 1);
	n.header = collectuntil(p, ";", 0);
	p.matchp(";");
	return n;
}

parsespawn(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NSpawn, nil, nil, nil, com, 1);
	n.header = collectuntil(p, ";", 0);
	p.matchp(";");
	return n;
}

parserawstmt(p: ref Parser, com: list of Token): ref Node
{
	n := ref Node(NRaw, nil, nil, nil, com, 1);
	n.header = collectuntil(p, ";", 0);
	p.matchp(";");
	return n;
}

parsecaseblock(p: ref Parser, com: list of Token, kw: string): ref Node
{
	kind := NCase;
	if(kw == "alt")
		kind = NAlt;
	else if(kw == "pick")
		kind = NPick;
	n := ref Node(kind, nil, nil, nil, com, 0);
	n.header = appendtok(n.header, Token(TkKeyword, kw, 0, 0));
	n.header = appendarr(n.header, collectuntil(p, "{", 1));
	if(!p.matchp("{"))
		return n;
	clauses: list of ref Node;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkPunct && t.text == "}"){
			p.next();
			break;
		}
		ccom := p.leading();
		qual := collectuntilop(p, "=>");
		if(p.peek().kind == TkOp && p.peek().text == "=>")
			p.next();
		clause := ref Node(NClause, qual, nil, nil, ccom, 0);
		body: list of ref Node;
		for(;;){
			t = p.peek();
			if(t.kind == TkEOF)
				break;
			if(t.kind == TkPunct && t.text == "}")
				break;
			if(isclausestart(p))
				break;
			body = parsestmt(p) :: body;
		}
		clause.body = revnodes(body);
		clauses = clause :: clauses;
	}
	n.body = revnodes(clauses);
	return n;
}

collectparen(p: ref Parser): array of Token
{
	arr := array[0] of Token;
	if(!p.matchp("("))
		return arr;
	arr = appendtok(arr, Token(TkPunct, "(", 0, 0));
	arr = appendarr(arr, collectuntil(p, ")", 1));
	p.matchp(")");
	arr = appendtok(arr, Token(TkPunct, ")", 0, 0));
	return arr;
}

collectuntil(p: ref Parser, stop: string, nested: int): array of Token
{
	arr := array[0] of Token;
	depth := 0;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkPunct && t.text == stop && depth == 0)
			break;
		if(nested && t.kind == TkPunct){
			if(t.text == "(" || t.text == "[" || t.text == "{")
				depth++;
			else if(t.text == ")" || t.text == "]" || t.text == "}")
				depth--;
		}
		arr = appendtok(arr, p.next());
	}
	return arr;
}

collectuntilkworpunct(p: ref Parser, kw: string, punct: string): array of Token
{
	arr := array[0] of Token;
	depth := 0;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(depth == 0){
			if(t.kind == TkKeyword && t.text == kw)
				break;
			if(t.kind == TkPunct && t.text == punct)
				break;
		}
		if(t.kind == TkPunct){
			if(t.text == "(" || t.text == "[" || t.text == "{")
				depth++;
			else if(t.text == ")" || t.text == "]" || t.text == "}")
				depth--;
		}
		arr = appendtok(arr, p.next());
	}
	return arr;
}

skipuntilkworpunct(p: ref Parser, kw: string, punct: string)
{
	depth := 0;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(depth == 0){
			if(t.kind == TkKeyword && t.text == kw)
				break;
			if(t.kind == TkPunct && t.text == punct)
				break;
		}
		if(t.kind == TkPunct){
			if(t.text == "(" || t.text == "[" || t.text == "{")
				depth++;
			else if(t.text == ")" || t.text == "]" || t.text == "}")
				depth--;
		}
		p.next();
	}
}

collectuntilop(p: ref Parser, op: string): array of Token
{
	arr := array[0] of Token;
	depth := 0;
	for(;;){
		t := p.peek();
		if(t.kind == TkEOF)
			break;
		if(t.kind == TkOp && t.text == op && depth == 0)
			break;
		if(t.kind == TkPunct){
			if(t.text == "(" || t.text == "[" || t.text == "{")
				depth++;
			else if(t.text == ")" || t.text == "]" || t.text == "}")
				depth--;
		}
		arr = appendtok(arr, p.next());
	}
	return arr;
}

isclausestart(p: ref Parser): int
{
	t := p.peek();
	if(t.kind == TkKeyword){
		case t.text {
		"if" or "for" or "while" or "do" or "case" or "alt" or "pick" or
		"return" or "break" or "continue" or "exit" or "raise" or "spawn" =>
			return 0;
		}
	}
	if(t.kind == TkPunct && t.text == "{")
		return 0;
	depth := 0;
	for(i := p.i; i < len p.toks; i++){
		t = p.toks[i];
		if(t.kind == TkEOF)
			return 0;
		if(t.kind == TkPunct){
			if(t.text == "{" || t.text == "(" || t.text == "[")
				depth++;
			else if(t.text == "}" || t.text == ")" || t.text == "]"){
				if(depth == 0)
					return 0;
				depth--;
			}else if(t.text == ";" && depth == 0)
				return 0;
		}
		if(t.kind == TkOp && t.text == "=>" && depth == 0)
			return 1;
	}
	return 0;
}

appendarr(a: array of Token, b: array of Token): array of Token
{
	n := len a;
	c := array[n + len b] of Token;
	c[0:] = a;
	c[n:] = b;
	return c;
}

revnodes(a: list of ref Node): list of ref Node
{
	out: list of ref Node;
	for(; a != nil; a = tl a)
		out = hd a :: out;
	return out;
}

formatnode(n: ref Node, indent: int): string
{
	if(n == nil)
		return "";
	case n.kind {
	NProgram =>
		return formatlist(n.body, indent);
	NFn =>
		return formatfn(n, indent);
	NDeclBlock =>
		return formatdeclblock(n, indent);
	NDecl or NImplement or NInclude or NRaw =>
		return formatline(n, indent, 1);
	NBlock =>
		return formatblock(n, indent, 1);
	NIf or NFor or NWhile or NDo or NReturn or NBreak or NContinue or NExit or NRaise or NSpawn =>
		return formatstmt(n, indent);
	NCase or NAlt or NPick =>
		return formatcase(n, indent);
	* =>
		return formatline(n, indent, 1);
	}
}

formatlist(items: list of ref Node, indent: int): string
{
	out := "";
	for(; items != nil; items = tl items)
		out += formatnode(hd items, indent);
	return out;
}

formatfn(n: ref Node, indent: int): string
{
	out := formatcomments(n.comments, indent);
	out += tabs(indent) + formattokens(n.header) + "\n";
	out += tabs(indent) + "{\n";
	out += formatlist(n.body, indent+1);
	out += tabs(indent) + "}\n";
	return out;
}

formatdeclblock(n: ref Node, indent: int): string
{
	out := formatcomments(n.comments, indent);
	out += tabs(indent) + formattokens(n.header) + "\n";
	out += tabs(indent) + "{\n";
	out += formatlist(n.body, indent+1);
	out += tabs(indent) + "}";
	if(n.needsemi)
		out += ";";
	out += "\n";
	return out;
}

formatblock(n: ref Node, indent: int, same: int): string
{
	out := formatcomments(n.comments, indent);
	if(same)
		out += tabs(indent) + "{\n";
	else
		out += tabs(indent) + "{\n";
	out += formatlist(n.body, indent+1);
	out += tabs(indent) + "}\n";
	return out;
}

formatstmt(n: ref Node, indent: int): string
{
	out := formatcomments(n.comments, indent);
	case n.kind {
	NIf =>
		out += formatif(n, indent, 1);
	NFor =>
		out += tabs(indent) + "for" + formattokens(n.header);
		out += formatstmtbody(n.body, indent);
	NWhile =>
		out += tabs(indent) + "while" + formattokens(n.header);
		out += formatstmtbody(n.body, indent);
	NDo =>
		out += tabs(indent) + "do";
		out += formatstmtbody(n.body, indent);
		out += tabs(indent) + "while" + formattokens(n.header) + ";\n";
	NReturn =>
		out += tabs(indent) + "return" + formatsuffix(n.header) + ";\n";
	NBreak =>
		out += tabs(indent) + "break" + formatsuffix(n.header) + ";\n";
	NContinue =>
		out += tabs(indent) + "continue" + formatsuffix(n.header) + ";\n";
	NExit =>
		out += tabs(indent) + "exit" + formatsuffix(n.header) + ";\n";
	NRaise =>
		out += tabs(indent) + "raise" + formatsuffix(n.header) + ";\n";
	NSpawn =>
		out += tabs(indent) + "spawn" + formatsuffix(n.header) + ";\n";
	* =>
		out += tabs(indent) + formattokens(n.header) + ";\n";
	}
	return out;
}

formatif(n: ref Node, indent: int, leadtabs: int): string
{
	out := "";
	if(leadtabs)
		out += tabs(indent);
	out += "if" + formattokens(n.header);
	out += formatstmtbody(n.body, indent);
	if(n.elsebody != nil){
		m := hd n.elsebody;
		if(m.kind == NIf && m.comments == nil){
			out += tabs(indent) + "else ";
			out += formatif(m, indent, 0);
		}else{
			out += tabs(indent) + "else";
			out += formatstmtbody(n.elsebody, indent);
		}
	}
	return out;
}

formatcase(n: ref Node, indent: int): string
{
	out := formatcomments(n.comments, indent);
	out += tabs(indent) + formattokens(n.header) + " {\n";
	out += formatclauses(n.body, indent);
	out += tabs(indent) + "}\n";
	return out;
}

formatclauses(items: list of ref Node, indent: int): string
{
	out := "";
	for(; items != nil; items = tl items){
		c := hd items;
		out += formatcomments(c.comments, indent);
		out += tabs(indent) + formattokens(c.header) + " =>\n";
		out += formatlist(c.body, indent+1);
	}
	return out;
}
formatstmtbody(body: list of ref Node, indent: int): string
{
	if(body == nil)
		return "\n";
	n := hd body;
	if(n.kind == NBlock){
		return formatinlineblock(n, indent);
	}
	out := "\n";
	out += formatnode(n, indent+1);
	return out;
}

formatline(n: ref Node, indent: int, addsemi: int): string
{
	out := formatcomments(n.comments, indent);
	if(len n.header == 0)
		return out;
	out += tabs(indent) + formattokens(n.header);
	if(addsemi && n.needsemi)
		out += ";";
	out += "\n";
	return out;
}

formatcomments(c: list of Token, indent: int): string
{
	out := "";
	for(; c != nil; c = tl c){
		t := hd c;
		if(t.text == "")
			out += "\n";
		else
			out += tabs(indent) + t.text + "\n";
	}
	return out;
}

formatsuffix(toks: array of Token): string
{
	if(len toks == 0)
		return "";
	return " " + formattokens(toks);
}

formattokens(toks: array of Token): string
{
	if(len toks == 0)
		return "";
	out := "";
	prev: Token;
	prevok := 0;
	prevunary := 0;
	brack := 0;
	for(i := 0; i < len toks; i++){
		t := toks[i];
		curunary := isunaryprefix(prevok, prev, t);
		if(prevok && needspace(prev, t, brack, prevunary, curunary))
			out += " ";
		out += t.text;
		prev = t;
		prevok = 1;
		prevunary = curunary;
		if(t.kind == TkPunct){
			if(t.text == "[")
				brack++;
			else if(t.text == "]" && brack > 0)
				brack--;
		}
	}
	return out;
}

needspace(a: Token, b: Token, brack: int, aunary: int, bunary: int): int
{
	if(a.kind == TkOp && isincdec(a.text))
		return 0;
	if(b.kind == TkOp && isincdec(b.text))
		return 0;
	if(aunary)
		return 0;
	if(bunary)
		if(!ispostop(a))
			return 0;
	if(a.kind == TkOp && isassignop(a.text))
		return 1;
	if(b.kind == TkOp && isassignop(b.text))
		return 1;
	if(brack > 0){
		if(a.kind == TkOp && istightop(a.text))
			return 0;
		if(b.kind == TkOp && istightop(b.text))
			return 0;
	}
	if(b.kind == TkPunct && b.text == ":")
		return 0;
	if(a.kind == TkPunct && a.text == ":"){
		if(brack > 0)
			return 0;
		return 1;
	}
	if(a.kind == TkPunct && (a.text == "(" || a.text == "[" || a.text == "{"))
		return 0;
	if(b.kind == TkPunct && (b.text == ")" || b.text == "]" || b.text == "}" || b.text == "," || b.text == ";"))
		return 0;
	if(b.kind == TkPunct && b.text == "[")
		return 0;
	if(a.kind == TkPunct && (a.text == "," || a.text == ";"))
		return 1;
	if(a.kind == TkPunct && a.text == ".")
		return 0;
	if(b.kind == TkPunct && b.text == ".")
		return 0;
	if(a.kind == TkOp && (a.text == "->" || a.text == "::"))
		return 0;
	if(b.kind == TkOp && (b.text == "->" || b.text == "::"))
		return 0;
	if(b.kind == TkPunct && b.text == "("){
		if(a.kind == TkKeyword)
			return 0;
		if(a.kind == TkIdent)
			return 0;
	}
	if(a.kind == TkOp || b.kind == TkOp)
		return 1;
	return 1;
}

isunaryprefix(prevok: int, prev: Token, cur: Token): int
{
	if(cur.kind != TkOp)
		return 0;
	if(isunaryop(cur.text))
		return 1;
	if(cur.text != "-")
		return 0;
	if(!prevok)
		return 1;
	if(prev.kind == TkOp)
		return !ispostop(prev);
	if(prev.kind == TkPunct && (prev.text == "(" || prev.text == "[" || prev.text == "{" || prev.text == "," || prev.text == ":"))
		return 1;
	if(prev.kind == TkKeyword)
		return 1;
	return 0;
}

ispostop(t: Token): int
{
	return t.kind == TkOp && (t.text == "=" || t.text == "+=" || t.text == "-=" || t.text == "*=" ||
		t.text == "/=" || t.text == "%=" || t.text == "&=" || t.text == "|=" || t.text == "^=" ||
		t.text == "<<=" || t.text == ">>=" || t.text == ":=");
}

istightop(s: string): int
{
	return s == "<<" || s == ">>" || s == "&" || s == "|" || s == "^" || s == "++" || s == "--";
}

isunaryop(s: string): int
{
	return s == "!" || s == "~";
}

isassignop(s: string): int
{
	return s == "=" || s == "+=" || s == "-=" || s == "*=" || s == "/=" || s == "%=" ||
		s == "&=" || s == "|=" || s == "^=" || s == "<<=" || s == ">>=" || s == ":=";
}

isincdec(s: string): int
{
	return s == "++" || s == "--";
}

tabs(n: int): string
{
	out := "";
	for(i := 0; i < n; i++)
		out += "\t";
	return out;
}

formatinlineblock(n: ref Node, indent: int): string
{
	out := " {\n";
	out += formatcomments(n.comments, indent+1);
	out += formatlist(n.body, indent+1);
	out += tabs(indent) + "}\n";
	return out;
}
