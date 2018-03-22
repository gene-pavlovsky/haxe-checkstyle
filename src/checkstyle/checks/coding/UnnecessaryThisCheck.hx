package checkstyle.checks.coding;

import haxe.macro.ExprTools;

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
					case FFun(fun): checkExpr(fun.expr, varsToString(fun.args));
					default:
				}
			}
		}
	}

	function checkExpr(expr:Expr, context:Array<String>) {
		switch (expr.expr) {
			case EVars(vars):
				for (v in vars) {
					context.push(v.name);
					checkExpr(v.expr, context);
				}
			case EFunction(_, f):
				var newContext = context.concat(varsToString(f.args));
				checkExpr(f.expr, newContext);
			case EField({expr: EConst(CIdent("this")), pos: pos}, name):
				checkThisAccess(name, context, pos);
			default:
				ExprTools.iter(expr, checkExpr.bind(_, context));
		}
	}

	function varsToString(vars:Array<{name:String}>):Array<String> {
		return vars.map(function (v) return v.name);
	}

	function checkThisAccess(name:String, context:Array<String>, pos:Position) {
		if (isPosSuppressed(pos)) return;

		if (!context.contains(name)) {
			logPos('Unnecessary "this" detected', pos);
		}
	}
}