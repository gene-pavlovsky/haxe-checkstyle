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
					case FFun(fun): checkExpr(fun.expr, fun.args);
					default:
				}
			}
		}
	}

	function checkExpr(expr:Expr, context:Array<FunctionArg>) {
		switch (expr.expr) {
			case EFunction(_, f):
				checkExpr(f.expr, context.concat(f.args));
			case EField({expr: EConst(CIdent("this")), pos: pos}, name):
				checkThisAccess(name, context, pos);
			default:
				ExprTools.iter(expr, checkExpr.bind(_, context));
		}
	}

	function checkThisAccess(name:String, context:Array<FunctionArg>, pos:Position) {
		if (isPosSuppressed(pos)) return;
		if (!context.exists(function(a) return a.name == name)) {
			logPos('Unnecessary "this" detected', pos);
		}
	}
}