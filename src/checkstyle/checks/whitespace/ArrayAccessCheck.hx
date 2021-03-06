package checkstyle.checks.whitespace;

@name("ArrayAccess")
@desc("Checks for spaces before array access or inside array elements. Finds code like `a [0], a[ 0]`, etc.")
class ArrayAccessCheck extends Check {

	public var spaceBefore:Bool;
	public var spaceInside:Bool;

	public function new() {
		super(AST);
		spaceBefore = false;
		spaceInside = false;
		categories = [Category.STYLE, Category.CLARITY];
	}

	override function actualRun() {
		var lastExpr = null;

		checker.ast.walkFile(function(e:Expr) {
			if (lastExpr == null) {
				lastExpr = e;
				return;
			}

			switch (e.expr) {
				case EArray(e1, e2):
					if (!spaceBefore) {
						var e1length = e1.pos.max - e1.pos.min;
						var eString = checker.getString(e.pos.min, e.pos.max);
						if (eString.substr(e1length, 1) == " ") logPos("Space between array and [", e.pos);
					}

					if (!spaceInside) {
						if (checker.getString(e2.pos.min - 1, e2.pos.min) == " ") logPos("Space between [ and index", e.pos);
						if (checker.getString(e2.pos.max, e2.pos.max + 1) == " ") logPos("Space between index and ]", e.pos);
					}
				default:
			}

			lastExpr = e;
		});
	}

	override public function detectableInstances():DetectableInstances {
		return [{
			fixed: [],
			properties: [{
				propertyName: "spaceBefore",
				values: [true, false]
			},
			{
				propertyName: "spaceInside",
				values: [true, false]
			}]
		}];
	}
}