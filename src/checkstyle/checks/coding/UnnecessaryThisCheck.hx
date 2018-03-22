package checkstyle.checks.coding;

import haxe.macro.ExprTools;

using checkstyle.utils.TokenTreeCheckUtils;
using Lambda;

@name("UnnecessaryThis")
@desc("Checks that `this` is only used if actually necessary.")
class UnnecessaryThisCheck extends Check {

	public function new() {
		super(AST);
	}

	override function actualRun() {
		var classes:Array<Array<Field>> = [];
		for (decl in checker.ast.decls) {
			switch (decl.decl) {
				case EClass(d):
					if (d.flags.contains(HInterface) || d.flags.contains(HExtern)) continue;
					classes.push(d.data);
				default:
			}
		}

		for (clazz in classes) {
			for (field in clazz) {
				switch (field.kind) {
					case FFun(fun): checkFunction(fun);
					default:
				}
			}
		}
	}

	inline function checkFunction(fun:Function) {
		checkExpr(fun.expr, fun);
	}

	function checkExpr(expr:Expr, context:Function) {
		switch (expr.expr) {
			case EFunction(_, f):
				checkExpr(f.expr, f);
			case EField({expr: EConst(CIdent("this")), pos: pos}, name):
				if (isPosSuppressed(pos)) return;
				if (!context.args.exists(function(a) return a.name == name)) {
					logPos('Unnecessary "this" detected', pos);
				}
			default:
				ExprTools.iter(expr, checkExpr.bind(_, context));
		}
	}

	function checkThis(token:TokenTree) {
		if (!token.hasChildren()) return;

		trace(token.children);
		var dot = token.getChild(Dot);
		if (dot != null && dot.hasChildren()) {
			for (c in dot.children) {
				switch (c.tok) {
					case Const(CIdent(name)):
						//name is only allowed if it is shadowed

					default: //allowed
				}
			}
		}
	}
}