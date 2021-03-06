package checkstyle.checks.size;

@name("MethodCount")
@desc("Checks the number of methods declared in each type. This includes the number of each scope (`private` and `public`) as well as an overall total.")
class MethodCountCheck extends Check {

	static var DEFAULT_MAX_COUNT:Int = 100;

	public var maxTotal:Int;
	public var maxPrivate:Int;
	public var maxPublic:Int;

	public function new() {
		super(TOKEN);
		maxTotal = DEFAULT_MAX_COUNT;
		maxPrivate = DEFAULT_MAX_COUNT;
		maxPublic = DEFAULT_MAX_COUNT;
		categories = [Category.COMPLEXITY];
		points = 21;
	}

	override function actualRun() {
		var root:TokenTree = checker.getTokenTree();
		var acceptableTokens:Array<TokenTree> = root.filter([Kwd(KwdFunction)], FIRST);

		if (acceptableTokens.length > maxTotal) {
			logPos('Total number of methods is ${acceptableTokens.length} (max allowed is $maxTotal)', acceptableTokens[maxTotal].pos);
			return;
		}

		var privateTokens = [];
		var publicTokens = [];
		for (token in acceptableTokens) {
			if (token.filter([Kwd(KwdPublic)], FIRST).length > 0) publicTokens.push(token);
			else privateTokens.push(token);
		}

		if (privateTokens.length > maxPrivate) {
			logPos('Number of private methods is ${privateTokens.length} (max allowed is $maxPrivate)', privateTokens[maxPrivate].pos);
			return;
		}
		if (publicTokens.length > maxPublic) {
			logPos('Number of public methods is ${publicTokens.length} (max allowed is $maxPublic)', publicTokens[maxPublic].pos);
			return;
		}
	}

	override public function detectableInstances():DetectableInstances {
		return [{
			fixed: [],
			properties: [{
				propertyName: "maxTotal",
				values: [for (i in 0...21) 10 + i * 5]
			},
			{
				propertyName: "maxPrivate",
				values: [for (i in 0...21) 10 + i * 5]
			},
			{
				propertyName: "maxPublic",
				values: [for (i in 0...21) 10 + i * 5]
			}]
		}];
	}
}