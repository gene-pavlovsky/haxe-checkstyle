package checkstyle.token.walk;

class WalkTypeNameDef {
	public static function walkTypeNameDef(stream:TokenStream, parent:TokenTree):TokenTree {
		WalkComment.walkComment(stream, parent);
		var tempStore:Array<TokenTree> = WalkAt.walkAts(stream);
		if (stream.is(Question)) {
			var questTok:TokenTree = stream.consumeTokenDef(Question);
			parent.addChild(questTok);
			parent = questTok;
		}
		var name:TokenTree;
		var bAdd:Bool = true;
		switch (stream.token()) {
			case BrOpen:
				WalkTypedefBody.walkTypedefBody(stream, parent);
				return parent.getFirstChild();
			case BkOpen:
				WalkArrayAccess.walkArrayAccess(stream, parent);
				return parent.getFirstChild();
			case Kwd(KwdMacro), Kwd(KwdExtern), Kwd(KwdNew):
				name = stream.consumeToken();
			case Const(_):
				name = stream.consumeConst();
			case Dollar(_):
				name = stream.consumeToken();
			case POpen:
				name = WalkPOpen.walkPOpen(stream, parent);
				if (stream.is(Question)) {
					WalkQuestion.walkQuestion(stream, name);
				}
				bAdd = false;
			case Sharp(_):
				WalkSharp.walkSharp(stream, parent, WalkStatement.walkStatement);
				if (!stream.hasMore()) return parent.getFirstChild();
				switch (stream.token()) {
					case Const(_):
						name = stream.consumeConst();
					default:
						return parent.getFirstChild();
				}
			default:
				name = stream.consumeToken();
		}
		for (tok in tempStore) name.addChild(tok);
		if (bAdd) parent.addChild(name);
		walkTypeNameDefContinue(stream, name);
		return name;
	}

	static function walkTypeNameDefContinue(stream:TokenStream, parent:TokenTree) {

		if (stream.is(Dot)) {
			var dot:TokenTree = stream.consumeTokenDef(Dot);
			parent.addChild(dot);
			WalkTypeNameDef.walkTypeNameDef(stream, dot);
			return;
		}
		if (stream.is(Arrow)) {
			var arrow:TokenTree = stream.consumeTokenDef(Arrow);
			parent.addChild(arrow);
			WalkTypeNameDef.walkTypeNameDef(stream, arrow);
			return;
		}
		if (stream.is(Binop(OpLt))) WalkLtGt.walkLtGt(stream, parent);
		if (stream.is(BkOpen)) WalkArrayAccess.walkArrayAccess(stream, parent);
		WalkComment.walkComment(stream, parent);
	}
}