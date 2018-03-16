package checkstyle.checks.whitespace;

import checkstyle.token.TokenTree;

using Lambda;
using checkstyle.utils.TokenTreeCheckUtils;

//TODO: Short lambdas (check every use of KwdFunction and FUNCTION)

@name("Whitespace")
@desc("Checks that whitespace is present or absent around a token in a specific context.")
class WhitespaceCheck extends WhitespaceCheckBase {

	//TODO: cannot differentiate between: ") {" and ")"

	public var policies:Array<AdvancedWhitespacePolicy>;
	public var tokens:Array<WhitespaceToken>;
	public var contexts:Array<String>;

	var convertedContexts:Array<ContextSelector>;

	public function new() {
		super();

		tokens = [
			MAP_ARROW, ASSIGN, UNARY, COMPARE, BITWISE, BOOL
		];
		policies = [AROUND];
		contexts = [OBJECT_DECL, FUNCTION, VAR, SWITCH, TRY_CATCH, ARRAY_ACCESS, BLOCK, CLASS, INTERFACE, TYPEDEF, ABSTRACT, ENUM];
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
				case LT:
					tokenList.push(Binop(OpLt));
				case GT:
					tokenList.push(Binop(OpGt));
			}
		}
		if (tokenList.length <= 0) return;

		convertedContexts = contexts.map(function (s) return ContextSelector.fromString(s));
		checkTokensAdvanced(checker.getTokenTree(), tokenList);
	}

	function checkTokensAdvanced(root:TokenTree, toks:Array<TokenDef>) {
		if (policies == null || policies.empty() || policies.contains(IGNORE)) return;
		var tokenList:Array<TokenTree> = root.filter(toks, ALL);
		trace(root.printTokenTree());
		checkTokenListAdvanced(tokenList);
	}

	function checkTokenListAdvanced(tokenList:Array<TokenTree>) {
		for (tok in tokenList) {
			if (isPosSuppressed(tok.pos)) continue;
			if (!checkContext(tok)) continue;

			checkWhitespaceAdvanced(tok);
		}
	}

	function checkWhitespaceAdvanced(tok:TokenTree) {
		checkWhitespaceExt(tok, function(before:Bool, after:Bool) {
			var v = function (p:AdvancedWhitespacePolicy) violation(tok, Std.string(p));
			for (p in policies) {
				switch (p) {
					case AROUND:
						if (!before || !after) {
							v(p);
							return;
						}
					case BEFORE:
						if (!before) {
							v(p);
							return;
						}
					case AFTER:
						if (!after) {
							v(p);
							return;
						}
					case NONE:
						if (before || after) {
							v(p);
							return;
						}
					case NONE_BEFORE:
						if (before) {
							v(p);
							return;
						}
					case NONE_AFTER:
						if (after) {
							v(p);
							return;
						}
					default:
						return;
				}
			}
		});
	}

	function checkContext(token:TokenTree):Bool {
		if (token.filterOpSub()) return false;
		//TODO: handle different minuses
		var contextQueue:List<TokenContext> = determineContext(token);
		return contextQueue != null && hasContext(contextQueue);
	}

	function determineContext(token:TokenTree):List<TokenContext> {
		var stack = new List<TokenContext>();

		function addSinglelineIfNeeded() {
			if (!token.hasChildren() || stack.isEmpty()) return;
			var notSingleLines = [COND, FUNCTION_PARAM, BLOCK, TYPE_PARAMETER];
			if (notSingleLines.contains(stack.last())) return;

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
		var originalToken = token;
		/*while (token.tok != null) {
			switch (token.tok) {
		}*/
		
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
				case Arrow: addSinglelineIfNeeded(); stack.add(SHORT_LAMBDA);
				case Kwd(KwdVar): stack.add(contextOfVar(token));
				case BkOpen: stack.add(contextOfBkOpen(token));
				case BrOpen: stack.add(contextOfBrOpen(token));
				case POpen: stack.add(contextOfPOpen(token));
				//case Question: stack.add(TERNARY);
				case Binop(OpLt):
					if (token.isTypeParameter()) stack.add(TYPE_PARAMETER);
				default:
			}
			token = token.parent;
		}
		// logPos("" + stack, originalToken.pos, INFO);
		return stack;
	}

	function contextOfBkOpen(token:TokenTree):TokenContext {
		if (token.parent.tok != null) {
			switch (token.parent.tok) {
				case Const(CIdent(_)): return ARRAY_ACCESS;
				default:
			}
		}

		if (!token.hasChildren()) return ARRAY;
		if (token.children.oneHasChild(Binop(OpArrow))) return MAP;

		return ARRAY;
	}

	function contextOfVar(token:TokenTree):TokenContext {
		if (!token.hasChildren()) return VAR; //should not happen

		// logPos(token.printTokenTree(), token.pos, INFO);
		for (child in token.children) {
			switch (child.tok) {
				case Const(CIdent(_)):
					if (child.children.exists(function (t) return t.is(POpen))) return PROPERTY;
				default:
			}
		}
		return VAR;
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
						if (!child.children.exists(function (t) return t.tok == DblDot)) return BLOCK;
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
		if (token.hasChild(Arrow)) return FUNCTION_PARAM; //short lambda parameter
		while (token.tok != null) {
			token = token.parent;
			switch (token.tok) {
				case At: return META;
				case Kwd(KwdFunction): return FUNCTION_PARAM;
				case POpen: return PARENTHESES;
				case Binop(OpAssign), Binop(OpAssignOp(_)): return COND;
				case Kwd(KwdVar): return PROPERTY;
				case Kwd(KwdIf), Kwd(KwdFor), Kwd(KwdWhile), Kwd(KwdSwitch), Kwd(KwdCase): return COND;
				case BrOpen: return contextOfBrOpen(token);
				case Const(CIdent(_)):
					if (token.hasChildren() &&
						token.children.exists(function (t) return t.tok == POpen) &&
						!token.parent.tok.match(Kwd(KwdFunction)) &&
						!token.parent.tok.match(Kwd(KwdVar)) &&
						!token.parent.tok.match(At)) {
							return FUNCTION_CALL;
					}
				default:
			}
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
				return parseToken(CONTEXT(cast token), current - 1, tokens);
		}
	}
}

@:enum
abstract AdvancedWhitespacePolicy(String) {
	var BEFORE = "before";
	var AFTER = "after";
	var AROUND = "around"; // = BEFORE + AFTER
	var NONE = "none"; // = NONE_BEFORE + NONE_AFTER
	var NONE_BEFORE = "noneBefore";
	var NONE_AFTER = "noneAfter";
	var IGNORE = "ignore";
}

@:enum
abstract TokenContext(String) to String {
	var OBJECT_DECL = "Object";
	var CLASS = "Class";
	var INTERFACE = "Interface";
	var TYPEDEF = "Typedef";
	var ABSTRACT = "Abstract";
	var ENUM = "Enum";
	var FUNCTION = "Function"; //only applies to the function header (excluding parameters)
	var SHORT_LAMBDA = "ShortLambda";
	var FUNCTION_PARAM = "Parameters";
	var FUNCTION_CALL = "Call";
	var VAR = "Var";
	var PROPERTY = "Property";
	var BLOCK = "Block";
	var IF_ELSE = "IfElse";
	var WHILE = "While";
	var FOR = "For";
	var COND = "Condition";
	var SWITCH = "Switch";
	var CASE = "Case";
	var TRY_CATCH = "TryCatch";
	var ARRAY = "Array";
	var MAP = "Map";
	var ARRAY_ACCESS = "ArrayAccess";
	var REIFICATION = "Reification";
	var TYPE_PARAMETER = "TypeParameter";
	var META = "Meta";
	var SINGLELINE = "Singleline";
	var PACKAGE = "Package";
	var IMPORT = "Import";
	var USING = "Using";
	//var TERNARY = "Ternary"; //TODO: check how to implement this properly
	var PARENTHESES = "Parentheses"; //generic parentheses (e.g. for grouping boolean expressions)
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
	var LT = "<";
	var GT = ">";
	//should all possible tokens be added seperately? (e.g. += or <=)
}