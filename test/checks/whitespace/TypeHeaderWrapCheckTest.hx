package checks.whitespace;

import checkstyle.checks.whitespace.TypeHeaderWrapCheck;

class TypeHeaderWrapCheckTest extends CheckTestCase<TypeHeaderWrapCheckTests> {

	static inline var MSG_EXTENDS_EOL:String = 'Token "extends" must be at the end of the line';
	static inline var MSG_IMPLEMENTS_EOL:String = 'Token "implements" must be at the end of the line';
	static inline var MSG_EXTENDS_NL:String = 'Token "extends" must be on a new line';
	static inline var MSG_IMPLEMENTS_NL:String = 'Token "implements" must be on a new line';
	static inline var MSG_INDENTATION_NL_IMPLEMENTS:String = 'Token "implements" must be indented more';

	public function testCorrectWrap() {
		var check = new TypeHeaderWrapCheck();
		assertNoMsg(check, CORRECT_NL_WRAP);
		assertNoMsg(check, MISSING_INDENTATION_NL_WRAP_IMPLEMENTS);
	}

	public function testIncorrectWrap() {
		var check = new TypeHeaderWrapCheck();
		assertMsg(check, CORRECT_EOL_WRAP_EXTENDS, MSG_EXTENDS_NL);
		assertMsg(check, INCORRECT_NL_WRAP_IMPLEMENTS, MSG_IMPLEMENTS_NL);
	}

	public function testOptionEOL() {
		var check = new TypeHeaderWrapCheck();
		check.option = EOL;
		assertNoMsg(check, CORRECT_EOL_WRAP);
		assertMsg(check, CORRECT_EOL_WRAP_EXTENDS, MSG_IMPLEMENTS_EOL);
	}

	public function testForceIndent() {
		var check = new TypeHeaderWrapCheck();
		check.forceIndent = true;
		assertNoMsg(check, CORRECT_NL_WRAP);
		assertMsg(check, CORRECT_EOL_WRAP_EXTENDS, MSG_EXTENDS_NL);
		assertMsg(check, MISSING_INDENTATION_NL_WRAP_IMPLEMENTS, MSG_INDENTATION_NL_IMPLEMENTS);
	}
}

@:enum
abstract TypeHeaderWrapCheckTests(String) to String {
	var CORRECT_NL_WRAP = "
	class Test
		extends A
		implements B
	{
	}";

	var CORRECT_EOL_WRAP = "
	class Test extends
		A implements
		B
	{
	}";

	var CORRECT_EOL_WRAP_EXTENDS = "
	class Test extends
		A 
		implements B
	{
	}";

	var INCORRECT_NL_WRAP_IMPLEMENTS = "
	class Test extends A implements
		B
	{
	}";

	var MISSING_INDENTATION_NL_WRAP_IMPLEMENTS = "
	class Test
	implements Test2 {
		
	}";
}