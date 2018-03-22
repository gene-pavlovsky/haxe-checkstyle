package checkstyle.checks.whitespace;

import checkstyle.utils.TokenTreeCheckUtils;

@name("TypeHeaderWrap")
@desc("Checks line wrapping of extends and implements keywords.")
class TypeHeaderWrapCheck extends WrapCheckBase {

	public var forceIndent:Bool;

	public function new() {
		super();
		option = NL;
		forceIndent = false;
		tokens = [
			"extends",
			"implements"
			//That's it for now
		];
	}

	override function actualRun() {
		var tokenList:Array<TokenDef> = [];

		if (hasToken("extends")) tokenList.push(Kwd(KwdExtends));
		if (hasToken("implements")) tokenList.push(Kwd(KwdImplements));

		if (tokenList.length <= 0) return;
		checkTokens(tokenList);
	}

	override function checkTokens(tokenList:Array<TokenDef>) {
		super.checkTokens(tokenList);

		var root:TokenTree = checker.getTokenTree();
		var allTokens:Array<TokenTree> = root.filter(tokenList, ALL);

		for (tok in allTokens) {
			if (isPosSuppressed(tok.pos)) continue;
			if (TokenTreeCheckUtils.isTypeParameter(tok)) continue;
			if (TokenTreeCheckUtils.isImportMult(tok)) continue;
			if (TokenTreeCheckUtils.filterOpSub(tok)) continue;

			var linePos:LinePos = checker.getLinePos(tok.pos.min);
			var parentLine:String = tok.parent.tok != null ? checker.lines[checker.getLinePos(tok.parent.pos.max).line] : "";
			var nextLine:String = linePos.line + 1 < checker.lines.length ? checker.lines[linePos.line + 1] : "";
			var whitespaceRE:EReg = ~/(^\s*)/;
			whitespaceRE.match(parentLine);
			var parentIndention = whitespaceRE.matched(1).length;
			whitespaceRE.match(nextLine);
			var nextLineIndention = whitespaceRE.matched(1).length;

			if (forceIndent && option == NL && linePos.ofs <= parentIndention) {
				logPos('Token "$tok" must be indented more', tok.pos);
				continue;
			}
			if (forceIndent && option == EOL && nextLineIndention <= parentIndention ) {
				logPos("Next line must be indented more", tok.pos);
				continue;
			}
		}
	}
}