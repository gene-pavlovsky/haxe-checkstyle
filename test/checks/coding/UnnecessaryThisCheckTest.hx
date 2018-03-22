package checks.coding;

import checkstyle.checks.coding.UnnecessaryThisCheck;
/**
 * ...
 * @author Christoph Otter
 */
class UnnecessaryThisCheckTest extends CheckTestCase<UnnecessaryThisCheckTests> {

	static inline var MSG:String = 'Unnecessary "this" detected';

	public function testCorrectThis() {
		var check = new UnnecessaryThisCheck();
		assertNoMsg(check, CORRECT_USE_OF_THIS);
	}

	public function testIncorrectThis() {
		var check = new UnnecessaryThisCheck();
		assertMsg(check, INCORRECT_THIS_ASSIGNMENT_CONSTRUCTOR, MSG);
		assertMsg(check, INCORRECT_THIS_ASSIGNMENT_FUNCTION, MSG);
		assertMsg(check, INCORRECT_THIS_RETURN, MSG);
	}

	public function testNestedFunction() {
		var check = new UnnecessaryThisCheck();
		assertNoMsg(check, CORRECT_NESTED_FUNCTION_THIS);
		assertMsg(check, INCORRECT_NESTED_FUNCTION_THIS, MSG);
	}
}

@:enum
abstract UnnecessaryThisCheckTests(String) to String {
	var CORRECT_USE_OF_THIS = "
	class Test {
		var field1:Int;
		var field2:Int = 1;
		public function new(field2:String) {
			this.field2 = 4;
		}
		public function setField1(field1:Int) {
			return this.field1;
		}
		public function test(value:Int) {
			field2 = value;
		}
		@SuppressWarnings('checkstyle:UnnecessaryThis')
		public function test(val:Int) {
			this.field1 = val;
		}
	}";

	var CORRECT_NESTED_FUNCTION_THIS = "
	class Test {
		var field1:Int;
		var field2:Int = 1;
		public function test(field1:Int) {
			var f = function (field2:Int) {
				this.field2 = field2;
				this.field1 = field2;
			}

			f(this.field1);
		}
	}";

	var INCORRECT_THIS_ASSIGNMENT_CONSTRUCTOR = "
	class Test {
		var field1:Int;
		var field2:Int = 1;
		public function new(value:Int) {
			this.field1 = value;
		}
	}";

	var INCORRECT_THIS_ASSIGNMENT_FUNCTION = "
	class Test {
		var field1:Int;
		var field2:Int = 1;
		public function test(fieldVal:String) {
			this.field1 = fieldVal;
		}
	}";

	var INCORRECT_THIS_RETURN = "
	class Test {
		var field1:Int;
		var field2:Int = 1;
		public function getField2() {
			return this.field2;
		}
	}";

	var INCORRECT_NESTED_FUNCTION_THIS = "
	class Test {
		var field1:Int;
		var field2:Int = 1;
		public function test(field1:Int) {
			var f = function (value:Int) {
				this.field2 = value;
			}

			f(field1);
		}
	}";
}
