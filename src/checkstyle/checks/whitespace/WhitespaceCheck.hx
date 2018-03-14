package checkstyle.checks.whitespace;

import checkstyle.checks.whitespace.WhitespaceCheckBase.WhitespacePolicy;
import checkstyle.token.TokenTree;
import checkstyle.utils.TokenTreeCheckUtils;

//TODO: Short lambdas (check every use of KwdFunction and FUNCTION)

@name("Whitespace")
@desc("Checks that whitespace is present or absent around a token in a specific context.")
class WhitespaceCheck extends WhitespaceCheckBase {

	public var policy:WhitespacePolicy;
	public var tokens:Array<WhitespaceToken>;
	public var contexts:Array<String>;

	var convertedContexts:Array<ContextSelector>;

	public function new() {
		super();

		tokens = [
			MAP_ARROW, ASSIGN, UNARY, COMPARE, BITWISE, BOOL
		];
		policy = AROUND;
		contexts = [OBJECT_DECL, FUNCTION, FIELD, SWITCH, TRY_CATCH, ARRAY_ACCESS, BLOCK, CLASS, INTERFACE, TYPEDEF, ABSTRACT, ENUM];
	}

	override function violation(tok:TokenTree, p:String) {
		logPos('Whitespace policy "$p" violated by "${TokenDefPrinter.toString(tok.tok)}"', tok.pos);
	}

	override function actualRun() {
		var tokenList:Array<TokenDef> = [];

		for (token in tokens) {
			switch (token) {
				case ASSIGN:
					tokenList = tokenList.concat([
						Binop(OpAssign),
						Binop(OpAssignOp(OpAdd)),
						Binop(OpAssignOp(OpSub)),
						Binop(OpAssignOp(OpMult)),
						Binop(OpAssignOp(OpDiv)),
						Binop(OpAssignOp(OpMod)),
						Binop(OpAssignOp(OpShl)),
						Binop(OpAssignOp(OpShr)),
						Binop(OpAssignOp(OpUShr)),
						Binop(OpAssignOp(OpOr)),
						Binop(OpAssignOp(OpAnd)),
						Binop(OpAssignOp(OpXor))
					]);
				case UNARY:
					tokenList = tokenList.concat([
						Unop(OpNot),
						Unop(OpIncrement),
						Unop(OpDecrement)
					]);
				case COMPARE:
					tokenList = tokenList.concat([
						Binop(OpGt),
						Binop(OpLt),
						Binop(OpGte),
						Binop(OpLte),
						Binop(OpEq),
						Binop(OpNotEq)
					]);
				case ARITHMETIC:
					tokenList = tokenList.concat([
						Binop(OpAdd),
						Binop(OpSub),
						Binop(OpMult),
						Binop(OpDiv),
						Binop(OpMod)
					]);
				case BITWISE:
					tokenList = tokenList.concat([
						Binop(OpAnd),
						Binop(OpOr),
						Binop(OpXor),
						Binop(OpShl),
						Binop(OpShr),
						Binop(OpUShr)
					]);
				case BOOL:
					tokenList.push(Binop(OpBoolAnd));
					tokenList.push(Binop(OpBoolOr));
				case MAP_ARROW:
					tokenList.push(Binop(OpArrow));
				case COMMA:
					tokenList.push(Comma);
				case SEMICOLON:
					tokenList.push(Semicolon);
				case POPEN:
					tokenList.push(POpen);
				case PCLOSE:
					tokenList.push(PClose);
				case BROPEN:
					tokenList.push(BrOpen);
				case BRCLOSE:
					tokenList.push(BrClose);
				case BKOPEN:
					tokenList.push(BkOpen);
				case BKCLOSE:
					tokenList.push(BkClose);
				case DBLDOT:
					tokenList.push(DblDot);
				case DOT:
					tokenList.push(Dot);
				case INTERVAL:
					tokenList.push(Binop(OpInterval));
				case FUNCTION_ARROW:
					tokenList.push(Arrow);
			}
		}

		if (tokenList.length <= 0) return;
		trace(checker.getTokenTree().printTokenTree());

		convertedContexts = contexts.map(function (s) return ContextSelector.fromString(s));
		checkTokens(checker.getTokenTree(), tokenList, policy);
	}

	override function checkTokenList(tokenList:Array<TokenTree>, p:WhitespacePolicy) {
		for (tok in tokenList) {
			if (isPosSuppressed(tok.pos)) continue;
			if (!checkContext(tok)) continue;

			checkWhitespace(tok, p);
		}
	}

	function checkContext(token:TokenTree):Bool {
		// if (TokenTreeCheckUtils.isImportMult(token)) return false;
		// if (TokenTreeCheckUtils.filterOpSub(token)) return false;
		//TODO: handle different minuses
		var contextQueue:List<TokenContext> = determineContext(token);
		return contextQueue != null && hasContext(contextQueue);
	}

	function determineContext(token:TokenTree):List<TokenContext> {
		var stack = new List<TokenContext>();

		function addSinglelineIfNeeded() {
			if (!token.hasChildren() || stack.isEmpty()) return; //should not happen
			if (stack.last() == COND || stack.last() == FUNCTION_PARAM || stack.last() == BLOCK) return;

			for (child in token.children) { //check if there is a block
				switch (child.tok) {
					case BrOpen:
						if (contextOfBrOpen(child) == BLOCK) return;
						else break;
					default:
				}
			}
			stack.add(SINGLELINE);
		}
		// var originalToken = token;
		while (token.tok != null) {
			switch (token.tok) {
				case At: stack.add(META);
				case Dollar(_): stack.add(REIFICATION);
				case Kwd(KwdImport): stack.add(IMPORT);
				case Kwd(KwdUsing): stack.add(USING);
				case Kwd(KwdPackage): stack.add(PACKAGE);
				case Kwd(KwdClass): stack.add(CLASS);
				case Kwd(KwdInterface): stack.add(INTERFACE);
				case Kwd(KwdEnum): stack.add(ENUM);
				case Kwd(KwdAbstract): stack.add(ABSTRACT);
				case Kwd(KwdTypedef): stack.add(TYPEDEF);
				case Kwd(KwdSwitch): stack.add(SWITCH);
				case Kwd(KwdCase), Kwd(KwdDefault): stack.add(CASE);
				case Kwd(KwdTry): stack.add(TRY_CATCH);
				case Kwd(KwdIf), Kwd(KwdElse): addSinglelineIfNeeded(); stack.add(IF_ELSE);
				case Kwd(KwdWhile), Kwd(KwdDo): addSinglelineIfNeeded(); stack.add(WHILE);
				case Kwd(KwdFor): addSinglelineIfNeeded(); stack.add(FOR);
				case Kwd(KwdFunction): addSinglelineIfNeeded(); stack.add(FUNCTION);
				case BkOpen: stack.add(ARRAY_ACCESS);
				case BrOpen: stack.add(contextOfBrOpen(token));
				case POpen: stack.add(contextOfPOpen(token));
				case Binop(OpLt):
					if (TokenTreeCheckUtils.isTypeParameter(token)) stack.add(TYPE_PARAMETER);
				default:
			}
			token = token.parent;
		}
		// logPos(originalToken.tok + " " + stack, originalToken.pos, INFO);
		return stack;
	}

	function contextOfBrOpen(token:TokenTree):TokenContext {
		//count number of children without comments

		var numChildren = 0;
		if (token.hasChildren()) {
			//anonymous structure can only contain a sequence of identifiers and commas
			for (child in token.children) {
				switch (child.tok) {
					case Const(CIdent(_)):
						numChildren++;
						if (child.children[0].tok != DblDot) return BLOCK;
					case Comma: numChildren++;
					case Comment(_), CommentLine(_):
					case BrClose:
					default: return BLOCK;
				}
			}
		}

		if (numChildren > 0) { //only valid object children
			return OBJECT_DECL;
		}
		else { //empty, look at what is done with it
			var parent = token.parent;
			//TODO: token tree seems to be incorrect if there are comments inbetween object and assignment
			//ignore comments
			while (parent.tok != null) {
				switch (parent.tok) {
					case Comment(_), CommentLine(_): parent = parent.parent;
					default: break;
				}
			}
			if (parent.tok == null) return BLOCK; //should not happen
			switch (parent.tok) {
				case Binop(OpAssign), Binop(OpAssignOp(_)): return OBJECT_DECL;
				case POpen: return OBJECT_DECL;
				case Question: return OBJECT_DECL;
				case DblDot: return OBJECT_DECL;
				case Arrow: return BLOCK; //this case is ambiguous
				case Binop(OpArrow): return OBJECT_DECL;
				case BkOpen: return OBJECT_DECL;
				case Comma: return OBJECT_DECL;
				default: return BLOCK;
			}
		}
	}

	function contextOfPOpen(token:TokenTree):TokenContext {
		while (token.tok != null) {
			switch (token.tok) {
				case Kwd(KwdFunction): return FUNCTION_PARAM;
				case POpen: return contextOfPOpen(token.parent);
				case Binop(OpAssign), Binop(OpAssignOp(_)): return COND;
				case Kwd(KwdVar): return PROPERTY;
				case Kwd(KwdIf), Kwd(KwdFor), Kwd(KwdWhile), Kwd(KwdSwitch), Kwd(KwdCase): return COND;
				case Const(CIdent(_)):
					if (token.parent.tok == null || !token.parent.tok.match(Kwd(KwdFunction)) || token.parent.children[0].tok == POpen) return FUNCTION_CALL;
				default:
			}
			token = token.parent;
		}
		return null;
	}

	function hasContext(context:List<TokenContext>):Bool {
		for (c in convertedContexts) {
			if (c.matches(context)) return true;
		}
		return false;
	}
}

@:enum
abstract WhitespaceMode(String) {
	var BEFORE = "before";
	var AFTER = "after";
	var AROUND = "around";
	var NONE = "none";
}

enum ContextSelectorEnum {
	CONTEXT(context:TokenContext);
	PLACEHOLDER;
	DIRECT_CHILD(parent:ContextSelectorEnum, child:ContextSelectorEnum);
	CHILD(parent:ContextSelectorEnum, child:ContextSelectorEnum);
	//maye add :empty and :not()
}

abstract ContextSelector(ContextSelectorEnum) to ContextSelectorEnum {
	inline function new(query:ContextSelectorEnum) {
		this = query;
	}

	@:from
	public static function fromString(s:String):ContextSelector {
		return new ContextSelector(parseQuery(s));
	}

	public function matches(context : List<TokenContext>):Bool {
		var context = context.map(function (t) return t); //copy context

		switch (this) {
			case CHILD(p, c):
				var childCorrect = new ContextSelector(c).matchesSingle(context.pop()); //child can only be Context(_)
				if (!childCorrect) return false; //first child must match directly
				return matchesParentSomewhere(context, p);
			default: return matchesSomewhere(context);
		}
		return false;
	}

	function matchesSomewhere(context:List<TokenContext>):Bool {
		switch (this) {
			case CONTEXT(c): return matchesSingle(context.pop());
			case DIRECT_CHILD(p, c): return new ContextSelector(c).matchesSingle(context.pop()) && new ContextSelector(p).matchesSomewhere(context);
			case CHILD(p, c):
				while (!context.isEmpty()) { //remove elements until we have a match
					var childCorrect = new ContextSelector(c).matchesSingle(context.pop()); //child can only be Context(_)
					if (childCorrect) break;
				}
				return matchesParentSomewhere(context, p);
			default: return matchesSingle(context.pop());
		}
		return false;
	}

	function matchesParentSomewhere(context:List<TokenContext>, p:ContextSelectorEnum):Bool {
		while (!context.isEmpty()) {
			if (new ContextSelector(p).matchesSomewhere(context)) return true;
		}
		return false;
	}

	function matchesSingle(check:TokenContext):Bool {
		switch (this) {
			case CONTEXT(c): return check == c;
			case PLACEHOLDER: return true;
			case null: return true;
			default: return false;
		}
	}

	static inline function parseQuery(s:String):ContextSelectorEnum {
		var tokens = checkstyle.utils.StringUtils.splitAll(s, [" ", ">"], true);
		if (tokens.length == 0) {
			var e = "Error: Invalid context query";
			#if debug
			Sys.println(e);
			#end
			#if unittest
			throw e;
			#end
		}
		return parseToken(null, tokens.length, tokens);
	}

	static function parseToken(parsed:ContextSelectorEnum, current:Int, tokens:Array<String>):ContextSelectorEnum {
		if (current < 0) return parsed;

		var token = tokens[current];
		switch (token) {
			case " ":
				return CHILD(parseToken(parsed, current - 1, tokens), parsed);
			case ">":
				return DIRECT_CHILD(parseToken(parsed, current - 1, tokens), parsed);
			case "*":
				return parseToken(PLACEHOLDER, current - 1, tokens);
			default:
				return parseToken(CONTEXT(cast token), current - 1, tokens); //TODO: validate token is an existing context
		}
	}
}

@:enum
abstract TokenContext(String) to String {
	var OBJECT_DECL = "Object";
	var CLASS = "Class";
	var INTERFACE = "Interface";
	var TYPEDEF = "Typedef";
	var ABSTRACT = "Abstract";
	var ENUM = "Enum";
	var FUNCTION = "Function"; //only applies to the function header
	var FUNCTION_PARAM = "Parameters";
	var FUNCTION_CALL = "Call";
	var FIELD = "Field";
	var PROPERTY = "Property";
	var BLOCK = "Block";
	var IF_ELSE = "IfElse";
	var WHILE = "While";
	var FOR = "For";
	var COND = "Condition";
	var SWITCH = "Switch";
	var CASE = "Case";
	var TRY_CATCH = "TryCatch";
	var ARRAY_ACCESS = "Array";
	var REIFICATION = "Reification";
	var TYPE_PARAMETER = "TypeParameter";
	var META = "Meta";
	var SINGLELINE = "Singleline";
	var PACKAGE = "Package";
	var IMPORT = "Import";
	var USING = "Using";
}

@:enum
abstract WhitespaceToken(String) {
	var ASSIGN = "Assign";
	var UNARY = "Unary";
	var COMPARE = "Compare";
	var ARITHMETIC = "Arithmetic";
	var BITWISE = "Bitwise";
	var BOOL = "Bool";
	var MAP_ARROW = "=>";
	var COMMA = ",";
	var SEMICOLON = ";";
	var POPEN = "(";
	var PCLOSE = ")";
	var BROPEN = "{";
	var BRCLOSE = "}";
	var BKOPEN = "[";
	var BKCLOSE = "]";
	var DBLDOT = ":";
	var DOT = ".";
	var INTERVAL = "...";
	var FUNCTION_ARROW = "->";
	//TODO: should all possible tokens be added seperately? (e.g. += or <=)
}